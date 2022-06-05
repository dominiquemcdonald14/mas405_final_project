
options(width=90, xtable.comment = FALSE)

if(!dir.exists("_assets")) {
    dir.create("_assets")
}

###############################################
#list of libraries that we use
library(RMySQL)
library(DBI)

library(dplyr)
library(stringr)

library(tm)
library(syuzhet)
library(wordcloud)

library(cluster)
library(factoextra)

library(ggplot2)

########################################################
#access database to acquire datasets

drv <- dbDriver("MySQL")
xdbsock <- ""

#Marianas ROUSER Account
xdbuser <-'ROuser'
xpw     <- 'Nom59trpy03'
xdbname <- 'MyDB' 
xdbhost <- 'database-1.cjv4ba2ytv1n.us-west-1.rds.amazonaws.com'
xdbport <- 3306

con <-
  dbConnect(
    drv,
    user=xdbuser,
    password=xpw,
    dbname=xdbname,
    host=xdbhost,
    port=xdbport,
    unix.sock=xdbsock
  )


dbListTables(con)

original_sonnets <- dbGetQuery(con, "SELECT * FROM OG")

artists_complete <- dbGetQuery(con, "SELECT * FROM Artists_noDuplicates")

dbDisconnect(con)

############################################
#DATA CLEANUP

#FIRST UP: OG SONNETS
#going to remove of the column of row numbers
og <- select(original_sonnets,-c(1)) 

og[1,1] #it appears that there are special characters

names(og)
og <- rename(og, "og_sonnets" = "rep.NA..154.")

og <- 
  og %>% 
  mutate_at("og_sonnets", str_replace_all, "â€™", "\'") 


#I manually removed the punctuation except for apostrophes
#creating an empty dataframe first
og_sonnets <- data.frame(matrix(ncol=1,nrow=154, 
                                 dimnames=list(NULL, "Sonnets")))

n <- 154
for (i in 1:154) {
  og_sonnets[i,]<- gsub("[,.;:?!]", "", og[i,1])
  rbind(og_sonnets[i,])
} 

og_sonnets[1,1] #checking to see if it worked



#################################
#NOW CLEAN UP OF THE ARTIST DATA

#removing the column of row numbers
artists_complete <- select(artists_complete,-c(1))

###this code will take all the non-lyrical strings contained in a set of 
#parenthesis and compile it into a vector of lists
#
df  <- vector(mode = "list", length = 45)
names(df) <- artists_complete$Artist

for (i in 1:45) {
  
  df[[i]] <- gsub("[\\(\\)]", "", unlist(regmatches(artists_complete$Lyrics[i],
                                            gregexpr("\\(.*?\\)", 
                                            artists_complete$Lyrics[i]))))
}

df[["adele"]] #look at the first element as an example

#I looked through the lists and roughly 10 artists contained these wannabe lyrics
#Of those 10, about 17 wannabe lyrics were discovered and now must be eliminated

artists_complete <- 
  artists_complete %>% 
  mutate_at("Lyrics", str_remove_all, pattern = c("Verse 1|Chorus 2|Verse 3|Chorus 3|x2|vocal solo|Saxophone solo|fadeout|chorus|Repeat 4 times|bv=|scat singing|4x|x8|x7|x4 |2x")) 

#can run this line of code again to verify if the strings are gone
#We can check the first element:adele
#she had "Verse 1, Chorus 2, Verse 3, and Chorus 3
#you'll find that those phrases are no longer there and what remains is just empty quotations

df2  <- vector(mode = "list", length = 45)
names(df2) <- artists_complete$Artist

for (i in 1:45) {
  
  df2[[i]] <- gsub("[\\(\\)]", "", unlist(regmatches(artists_complete$Lyrics[i],
                                            gregexpr("\\(.*?\\)", 
                                            artists_complete$Lyrics[i]))))
}

df2[["adele"]]

#the last thing to do is to remove unwanted punctuation

#this removes everything inside brackets, brackets included
artists_complete$Lyrics <- gsub("\\[.+?\\]", "", 
                                      artists_complete$Lyrics)

#now with parenthesis that includes lyrics being sung...
#this removes all the parenthesis BUT without removing the text inside
artists_complete$Lyrics <- gsub("\\s*\\([^\\)]+\\)","", 
                                        artists_complete$Lyrics)


#This replaces everything that's not alphanumeric signs, space or apostrophe with an empty string
artists_complete$Lyrics <- gsub("[^[:alnum:][:space:]']", "", 
                                      artists_complete$Lyrics)
artists_complete
#clean up is all done!

##############################################################
# METHODOLOGY
##################
#finalized cleaned datasets 
og_sonnets
artists_complete
###################
#Key Word extraction

##### Further data cleaning before keyword extraction
og_sonnets <- og_sonnets %>% 
  mutate_at("Sonnets", str_replace_all, "�???T", "\'") #the pattern for the special character may vary from computer to computer

#need to load vector of text objects as a corpus
#VectorSource() interprets each element of a vec as a document
x_text <- Corpus(VectorSource(og_sonnets$Sonnets))
x_text

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (y , pattern ) gsub(pattern, " ", y))
x_text <- tm_map(x_text, toSpace, "/")
x_text <- tm_map(x_text, toSpace, "@")
x_text <- tm_map(x_text, toSpace, "\\|")

# Remove numbers
x_text <- tm_map(x_text, removeNumbers)

# Remove English common stop words
x_text <- tm_map(x_text, removeWords, stopwords("english"))

# Removing custom Shakespearean Stop words
# specify your custom stop words as a character vector
x_text <- tm_map(x_text, removeWords, c("thi", "thee", "thou", "may", "still", "thus", "though", "can", "will", "hath", "doth", "thine", "like", "much", "let",
                                        "upon", "from", "dost", "shall", "thy")) 
# Remove punctuation
x_text <- tm_map(x_text, removePunctuation)

# Eliminate extra white spaces
x_text <- tm_map(x_text, stripWhitespace)

x_text #NO DOCUMENTS DROPPED

#### Creating The document term matrix and extracting keywords
# The term matrix contains all the words in your "documents" and their frequencies
# Build a term-document matrix
x_text_dtm <- TermDocumentMatrix(x_text)
x_text_dtm

#number of total terms is the non sparse entries
mat_dtm <- as.matrix(x_text_dtm)

# Sort by decreasing value of frequency
dtm_v <- sort(rowSums(mat_dtm),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 50 most frequent words
head(dtm_d, 50)

####### Generate word cloud of Shakespeare Keywords from Sonnets and Write it out as a PNG 
set.seed(314)
png("_assets/Sonnets_Keywords_WC.png", width=1920, height=1080, pointsize=35)
plot.new()

wordcloud(words = dtm_d$word, 
          freq = dtm_d$freq, 
          min.freq = 5,
          max.words=100, 
          random.order=FALSE, 
          rot.per=0.20, 
          colors=brewer.pal(8, "Dark2"))

dev.off()

#################
# Extracting Shakespears sentiments from the 154 sonets 


#############################do we need this???############################################
# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
#### NOTE YOU DO NOT HAVE TO USE A CORPUS FOR THE SYUZHET PACKAGE HERE. THIS IS JUST THE VECTOR OF SONNETS
s_vector <- get_sentiment(og_sonnets$Sonnets, method="syuzhet") 
# see the first row of the vector
head(s_vector, n = 10)
# see summary statistics of the vector
summary(s_vector)
hist(s_vector, col = "pink", main = "Sentiments Scores for each Sonnet", xlab = "Sentiment Scores") #normal distribution 

####################################################
#porportion calculation 
head(dtm_d)
num_terms <- length(x_text_dtm$i); num_terms #total number of terms - 7611
nTerms(x_text_dtm) # unique terms in sonnets - 3089
head(Terms(x_text_dtm)) #just a list of all the terms that show up

prop_terms<-  dtm_d$freq/num_terms; head(prop_terms)

dtm_prop <- cbind(dtm_d, prop_terms)













##################
#Sentiment Analysis for the Sonnets
emotions <- data.frame(matrix(ncol=10,nrow=0, dimnames=list(NULL, 
      c("anger", "anticipation", "disgust", "fear", "joy", "sadness", 
        "surprise", "trust", "negative", "positive"))))

n <- 154

#ran a for loop to conduct sentiment analysis to each individual sonnet
for (i in 1:n) {
  emotions[i,]<- get_nrc_sentiment(og_sonnets[i,1])
  rbind(emotions[i,])
}

head(emotions)

#creating barplots to visualize the sonnets sentiments dataframe
#gathering the sums of each emotion and sentiment
s <- colSums(emotions)
ss <- s[c("anger", "anticipation", "disgust", "fear", "joy", "sadness", 
        "surprise", "trust")]
sss <- s[c("negative", "positive")]


png("_assets/Barplot_Sonnets_8_Emotions.png", width=1920, height=1080, pointsize=35)
plot.new()

barplot(ss,
        main = "Frequency of Each Emotion",
        las = 2,
        col = terrain.colors(15),
        ylim = c(0,700))

dev.off()

png("_assets/Barplot_Sonnets_2_Sentiments.png", width=1920, height=1080, pointsize=35)
plot.new()
barplot(sss,
        main = "Frequency of Positive and Negative Sentiments",
        las = 2,
        col = rainbow(15),
        ylim = c(0, 1200))


dev.off()

#IMPORTANT: need to change the colors so they're not that bold/glaring

################
# Cluster Analysis




