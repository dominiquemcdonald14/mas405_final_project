
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
  mutate_at("og_sonnets", str_replace_all, "Ã¢â‚¬â„¢", "\'") 


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
#                     METHODOLOGY
##############################################################
#finalized cleaned datasets 
og_sonnets
artists_complete

##############################################################
#                 Key Word Extraction
##############################################################
# Shakespeare Key Word Extraction
##########################################
# Further data cleaning before keyword extraction
og_sonnets <- og_sonnets %>% 
  mutate_at("Sonnets", str_replace_all, "â???T", "\'") #the pattern for the special character may vary from computer to computer

# need to load vector of text objects as a corpus. VectorSource() interprets each element of a vec as a document
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

##### Creating The document term matrix and extracting keywords
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
png("_assets/Shakespear_Keywords_WordCloud.png", width=1920, height=1080, pointsize=35)

plot.new()

set.seed(314)
wordcloud(words = dtm_d$word, 
          freq = dtm_d$freq, 
          min.freq = 5,
          max.words=100, 
          random.order=FALSE, 
          rot.per=0.20, 
          colors=brewer.pal(8, "Dark2"))

dev.off()

##### proportion calculation for Shakespear
head(dtm_d)
num_terms <- length(x_text_dtm$i); num_terms #total number of terms - 7611
nTerms(x_text_dtm) # unique terms in sonnets - 3089
head(Terms(x_text_dtm)) #just a list of all the terms that show up

prop_terms<-  dtm_d$freq/num_terms; head(prop_terms)

dtm_prop <- cbind(dtm_d, prop_terms)

##########################################
# Music Artist Key Word Extraction
##########################################
# This loop performs key word extraction on each artist's work 
# Result: - A list containing a df for each artists.
#         - Each artist's df will contain: top 10 keywords, frequencies, proportions

#creating list that will be populated by loop
artist_keyword <- list()
artist_keyword

artist_text <- Corpus(VectorSource(artists_complete[,2])); artist_text

for(i in 1:length(artists_complete[,2])){
  kw <- rep(NA, 10)
  frq <- rep(NA, 10)
  prp <- rep(NA, 10)
  temp_mat <- cbind(kw, frq, prp)
  artist_keyword[[i]] <-as.data.frame(temp_mat) # ea. element will be df with 10 rows and 3 columns, chose not to                                                                                           # include artist because it would just be repeated, but could be                                                                                            # useful. discuss???
  colnames(artist_keyword[[i]]) <- c("keyword","frequency", "proportion")
  
  art_doc <- artist_text[i]
  
  #Replacing "/", "@" and "|" with space
  toSpace <- content_transformer(function (y , pattern ) gsub(pattern, " ", y))
  art_doc <- tm_map(art_doc, toSpace, "/")
  art_doc <- tm_map(art_doc, toSpace, "@")
  art_doc <- tm_map(art_doc, toSpace, "\\|")
  #art_doc <- tm_map(art_doc, to_e, "â???T")
  
  # Convert the text to lower case
  art_doc <- tm_map(art_doc, content_transformer(tolower))
  
  # Remove numbers
  art_doc <- tm_map(art_doc, removeNumbers)
  
  # Remove common English stop words
  art_doc <- tm_map(art_doc, removeWords, stopwords("english"))
  
  # Removing custom stop words, specify stop words as a character vector
  art_doc <- tm_map(art_doc, removeWords, c("aint", "ooh", "thou", "never", "yeah", "hey", "though", "just", "will", "dont", "gonna", "can",                                                            "let", "thing", "every", "cause", "Since", "along",  "always", "many" , "eighteen", "hundred",
                                            "upon", "from", "nah", "aint", "now", "one", "two", "cant", "dont", "wont", "like", "much")) 
  # Remove punctuation
  art_doc <- tm_map(art_doc, removePunctuation)
  
  # Eliminate extra white spaces
  art_doc <- tm_map(art_doc, stripWhitespace)

  #art_doc #NO DOCUMENTS DROPPED
  
  #number of total terms is the non sparse entries
  art_doc_dtm <- TermDocumentMatrix(art_doc)
  artist_mat_dtm <- as.matrix(art_doc_dtm)
  
  # Sort by decreasing value of frequency
  artisit_dtm_v <- sort(rowSums(artist_mat_dtm),decreasing=TRUE)
  artist_dtm_d <- data.frame(word = names(artisit_dtm_v),freq=artisit_dtm_v)
  artist_dtm_d
  
  artist_keyword[[i]]$keyword[1:10] <- artist_dtm_d$word[1:10] #populating keywords
  artist_keyword[[i]]$frequency[1:10] <- artist_dtm_d$freq[1:10]; artist_keyword #populating frequencies
  
  art_num_terms <- length(art_doc_dtm$i); art_num_terms #total number of terms
  artist_keyword[[i]]$proportion[1:10] <- artist_dtm_d$freq[1:10]/art_num_terms #populating proportions

  }

names(artist_keyword) <- artists_complete$artist_names
artist_keyword[3] #look at the data frame one at a time otherwise your computer won't like you
artist_keyword[1:45] #run this from your console to see results more easily


#########################################
#
##
############### Check that these are the top artists #### Loop to generate top artists key words World Clouds
##
##
#########################################
top_artists_df <- artists_complete[artists_complete$Artist %in% c("al-green", "amy-winehouse", "adele", "beiber", "bjork", "cake", "alicia-keys", 
                                                                        "joni-mitchell", "paul-simon"), ]
top_artists_df #check check check !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

for(i in 1:nrow(top_artists_df)){
  
  art_doc <- Corpus(VectorSource(top_artists_df[i,2])) 
  
  #Replacing "/", "@" and "|" with space
  toSpace <- content_transformer(function (y , pattern ) gsub(pattern, " ", y))
  art_doc <- tm_map(art_doc, toSpace, "/")
  art_doc <- tm_map(art_doc, toSpace, "@")
  art_doc <- tm_map(art_doc, toSpace, "\\|")
  #art_doc <- tm_map(art_doc, to_e, "â???T")
  
  # Convert the text to lower case
  art_doc <- tm_map(art_doc, content_transformer(tolower))
  
  # Remove numbers
  art_doc <- tm_map(art_doc, removeNumbers)
  
  # Remove common English stop words
  art_doc <- tm_map(art_doc, removeWords, stopwords("english"))
  
  # Removing custom stop words, specify stop words as a character vector
  art_doc <- tm_map(art_doc, removeWords, c("aint", "ooh", "thou", "never", "yeah", "hey", "though", "just", "will", "dont", "gonna", "can",                                                            "let", "thing", "every", "cause", "Since", "along",  "always", "many" , "eighteen", "hundred",
                                            "upon", "from", "nah", "aint", "now", "one", "two", "cant", "dont", "wont", "like", "much")) 
  # Remove punctuation
  art_doc <- tm_map(art_doc, removePunctuation)
  
  # Eliminate extra white spaces
  art_doc <- tm_map(art_doc, stripWhitespace)
  
  #art_doc #NO DOCUMENTS DROPPED
  
  #number of total terms is the non sparse entries
  art_doc_dtm <- TermDocumentMatrix(art_doc)
  artist_mat_dtm <- as.matrix(art_doc_dtm)
  
  # Sort by decreasing value of frequency
  artisit_dtm_v <- sort(rowSums(artist_mat_dtm),decreasing=TRUE)
  artist_dtm_d <- data.frame(word = names(artisit_dtm_v),freq=artisit_dtm_v)
  artist_dtm_d
  
  #word cloud generation for artist 
  artistName <- top_artists_df$Artist[i]
  wc_Filename <- paste0("_assets/", artistName, "_KeyWord_WordCloud1.png")
  
  png(wc_Filename, width=1920, height=1080, pointsize=35)
  
  plot.new()
  
  set.seed(314)
  wordcloud(words = artist_dtm_d$word, freq = artist_dtm_d$freq, min.freq = 5,
            max.words=100, random.order=FALSE, rot.per=0.20, 
            colors=brewer.pal(8, "Dark2"))
  
  dev.off()
}




###########################








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


#####################
#dominiques sentiment code 


sent_scores<- get_sentiment(artists_complete$Lyrics) #returns overall sentiment value
summary(sent_scores)

artists_8sent <- get_nrc_sentiment(artists_complete$Lyrics) #returns classification into each 8 sentiments + pos/neg
artists_8sent


all_sonnets <- paste(og_sonnets$Sonnets[1:154], collapse = " ") #the collapse argument allows you to create a single string by concatenating ea element of the vector
all_sonnets

shake_8sent <- get_nrc_sentiment(all_sonnets) #getting shakespeare sentiments

shake_8sent


all_8sent <- rbind(artists_8sent, shake_8sent) #appending shakespeare's sentiments to table with those of other artists

names_8sent <- c(artists_complete$Artist, "shakespeare") #getting artist names for table
names_8sent

all_8sent <- cbind(names_8sent, all_8sent) #adding names
all_8sent #quick check

###################################
#creating a table of proportions instead of frequencies
prop_8sent <- data.frame(matrix(ncol = 9, nrow = 46))

prop_8sent[,1] <- all_8sent$names_8sent


for(i in 1:46){ #this loop divides each element in a row bu the row sum
  
  
  prop_8sent[i,2:9] <- all_8sent[i,2:9]/sum(all_8sent[i,2:9])
  
}

prop_8sent #another quick checl


#Now that we have the sentiments in a table kmeans clustering will be straightforward. I will use MG's lovely code for this part :*

#identifying optimal number of clusters
k <- 12
vpc <- NULL #vpc here means "variance per cluster"
for (i in 1:k) {
    kfit <- kmeans(prop_8sent[,2:9], i)
    vpc <- c(vpc, kfit$betweenss/kfit$totss)
}
vpc

plot(1:k, vpc, xlab = "# of clusters", ylab = "explained variance")


kfit7 <- kmeans(prop_8sent[,2:9], 7)
kfit8 <- kmeans(prop_8sent[,2:9], 8)  
kfit10 <- kmeans(prop_8sent[,2:9], 10)
kfit12 <- kmeans(prop_8sent[,2:9], 12)


#clustering means were given for each of the eight emotions...
kfit7$centers
kfit8$centers
kfit10$centers
kfit12$centers

#install.packages("factoextra")

library(factoextra)

row.names(prop_8sent) <- all_8sent$names_8sent ###*changing the row names so they will show up as labels for each poin


fviz_cluster(kfit7, data = prop_8sent[,2:9],
             palette = c("deepskyblue", "cyan3", "violet", "pink3", "red", "purple", "green3", "lightpink", "salmon3", "gray", "orange3", "olivedrab3"), 
             geom = c("point", "text"),
             ellipse = F,
             ellipse.type = "t", 
             ggtheme = theme_bw()
             )
fviz_cluster(kfit8, data = prop_8sent[,2:9],
             palette = c("deepskyblue", "cyan3", "violet", "pink3", "red", "purple", "green3", "lightpink", "salmon3", "gray", "orange3", "olivedrab3"), 
             geom = c("point", "text"),
             ellipse = F,
             ellipse.type = "t", 
             ggtheme = theme_bw()
             )

fviz_cluster(kfit10, data = prop_8sent[,2:9],
             palette = c("deepskyblue", "cyan3", "violet", "pink3", "red", "purple", "green3", "lightpink", "salmon3", "gray", "orange3", "olivedrab3"), 
             geom = c("point", "text"),
             ellipse = F,
             ellipse.type = "t", 
             ggtheme = theme_bw()
             )

fviz_cluster(kfit12, data = prop_8sent[,2:9],
             palette = c("deepskyblue", "cyan3", "violet", "pink3", "red", "purple", "green3", "lightpink", "salmon3", "gray", "orange", "olivedrab3"), 
             geom = c("point", "text"),
             ellipse = F,
             ellipse.type = "t", 
             ggtheme = theme_bw()
             )

## Not sure what plot we want to go with for the results 


test <- cbind (all_8sent, kfit$cluster) #Appended the clusters to all artists sentiments to get table of artist in same group
#need to check which cluster shakespeare is in each time, because different groups might be formed each time you run the cluster analysis

# test[test$`kfit$cluster` == 4,] #these are the artists in shakespeare's cluster



################
# Dannys code 

x_text <- Corpus(VectorSource(og_sonnets$Sonnets))

x_text

#Clean the data

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (y , pattern ) gsub(pattern, " ", y))
x_text <- tm_map(x_text, toSpace, "/")
x_text <- tm_map(x_text, toSpace, "@")
x_text <- tm_map(x_text, toSpace, "\\|")
x_text <- tm_map(x_text, removeNumbers)
x_text <- tm_map(x_text, removeWords, stopwords("english"))
x_text <- tm_map(x_text, removeWords, c("thi", "thee", "thou", "may", "still", "thus", "though", "can", "will", "hath", "doth", "thine", "like", "much", "let", "upon", "from", "dost", "shall", "thy")) 
x_text <- tm_map(x_text, removePunctuation)
x_text <- tm_map(x_text, stripWhitespace)

#The document term matrix just contains all the words in your "documents" and their frequencies and maybe their stem, gotta check


# Build a term-document matrix
x_text_dtm <- TermDocumentMatrix(x_text)
x_text_dtm
#number of total terms is the non sparse entries

mat_dtm <- as.matrix(x_text_dtm)
# mat_dtm
# Sort by decreasing value of frequency
dtm_v <- sort(rowSums(mat_dtm),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 50 most frequent words
head(dtm_d, 50)

num_terms <- length(x_text_dtm$i); num_terms #total number of terms - 7611
nTerms(x_text_dtm) # unique terms in sonnets - 3089
head(Terms(x_text_dtm)) #just a list of all the terms that show up

prop_terms<-  dtm_d$freq/num_terms; head(prop_terms)

dtm_prop <- cbind(dtm_d, prop_terms)

sonnet_data <- head(dtm_prop, 10); sonnet_data

## Idea: divide each by top 10 frequencies, instead of using total

#Here, we have the table summarizing Shakepeare's top 10 most used words. 


## Sentiment Analysis



#data cleanup using dplyr
sonnets <- 
  og_sonnets %>% 
  mutate_at("Sonnets", str_replace, "â???T", "\'") 

sonnets_df1 <- data.frame(matrix(ncol=1,nrow=154, 
                                 dimnames=list(NULL, "Sonnets")))

#I manually removed the punctuation except for apostrophes
n <- 154
for (i in 1:n) {
  sonnets_df1[i,]<- gsub("[,.;:?!]", "", sonnets[i,1])
  rbind(sonnets_df1[i,])
} 


#create an empty dataframe
emotions <- data.frame(matrix(ncol=8,nrow=0, dimnames=list(NULL, 
      c("anger", "anticipation", "disgust", "fear", "joy", "sadness", 
        "surprise", "trust"))))

#n <- 154

#ran a for loop to conduct sentiment analysis to each individual psuedosonnet
#beware: it might take a lil while to run
#for (i in 1:n) {
  #emotions[i,]<- get_nrc_sentiment(sonnets_df1[i,1])
  #rbind(emotions[i,])
#}

#s <- colSums(emotions)
all_sonnets <- paste(sonnets_df1, collapse = " ") #the collapse argument allows you to create a single string by concatenating ea element of the vector

emotions <- get_nrc_sentiment(all_sonnets) #getting shakespeare sentiments

s_prop <- emotions[1,1:8] / sum(emotions[1,1:8])


## Now create the table for 49 artists

#Gonna write a loop that performs key word extraction on each artist's work. 
#Result: - A list containing a df for each artists.
#- Each artist's df will contain: top 10 keywords, frequencies, proportions
        
#creating list that will be populated by loop
artist_keyword <- data.frame(artist = artists_complete$Artist, sent = rep(NA, 45), word = rep(NA,45), freq_df = rep(NA,45), prop_df = rep(NA,45), keyword = rep(NA,45))


artist_text <- Corpus(VectorSource(artists_complete[,2])); artist_text

for(i in 1:length(artists_complete[,2])){
 
  kw <- rep(NA, 10)
  frq <- rep(NA, 10)
  prp <- rep(NA, 10)
  temp_mat <- cbind(kw, frq, prp) #temp_mat is the current artist
  #artist_keyword[[i]] <-as.data.frame(temp_mat) 
  #colnames(artist_keyword[[i]]) <- c("keyword","frequency", "proportion" )
  temp_df <- as.data.frame(temp_mat)
  colnames(temp_df) <- c("word","frequency", "proportion" )
  
  art_doc <- artist_text[i]
  
  ## Data cleaning
  toSpace <- content_transformer(function (y , pattern ) gsub(pattern, " ", y))
  art_doc <- tm_map(art_doc, toSpace, "/")
  art_doc <- tm_map(art_doc, toSpace, "@")
  art_doc <- tm_map(art_doc, toSpace, "\\|")
  art_doc <- tm_map(art_doc, content_transformer(tolower))
  art_doc <- tm_map(art_doc, removeNumbers)
  art_doc <- tm_map(art_doc, removeWords, stopwords("english"))
  art_doc <- tm_map(art_doc, removeWords, c("aint", "ooh", "thou", "never", "yeah", "hey", "though", "just", "will", "dont", "gonna", "can", "let", "thing", "every", "cause", "Since", "along",  "always", "many" , "eighteen", "hundred", "upon", "from", "nah", "aint", "now", "one", "two", "cant", "dont", "wont", "like", "much")) 
  art_doc <- tm_map(art_doc, removePunctuation)
  art_doc <- tm_map(art_doc, stripWhitespace)
  x_text <- tm_map(art_doc, stemDocument)

  art_doc_dtm <- TermDocumentMatrix(art_doc)
  

  #art_doc_dtm
  #number of total terms is the non sparse entries
  
  artist_mat_dtm <- as.matrix(art_doc_dtm)
  # mat_dtm
  # Sort by decreasing value of frequency
  artisit_dtm_v <- sort(rowSums(artist_mat_dtm),decreasing=TRUE)
  artist_dtm_d <- data.frame(word = names(artisit_dtm_v),freq=artisit_dtm_v)
  artist_dtm_d
  
  
  #artist_keyword[[i]]$keyword[1:10] <- artist_dtm_d$word[1:10] #populating keywords
  #artist_keyword[[i]]$frequency[1:10] <- artist_dtm_d$freq[1:10]; artist_keyword #populating frequencies
  temp_df$word[1:10] <- artist_dtm_d$word[1:10]
  temp_df$frequency[1:10] <- artist_dtm_d$freq[1:10]; 
  
  art_num_terms <- length(art_doc_dtm$i); art_num_terms #total number of terms
 # artist_keyword[[i]]$proportion[1:10] <- artist_dtm_d$freq[1:10]/art_num_terms #populating proportions
  temp_df$proportion[1:10] <- artist_dtm_d$freq[1:10]/art_num_terms
  
  compare <- sonnet_data %>%
    left_join(temp_df, by = "word") %>%
    filter(!is.na(frequency))
  
  artist_keyword[i,]$word <- nrow(compare)
  artist_keyword[i,]$freq_df <- sum((compare$freq - compare$frequency)^2)
  artist_keyword[i,]$prop_df <- sum((compare$prop_terms - compare$proportion)^2)
  
  temp_word <- ""
  for (j in 1:length(compare$word)) {
    temp_word <- paste0(temp_word," ",compare$word[j])
  }

  artist_keyword[i,]$keyword <- temp_word
  
  ## Sentiment Analysis
  art_data <- gsub("[,.;:?!]", "", artists_complete[i,2])
  
  emotions <- data.frame(matrix(ncol=8,nrow=0, dimnames=list(NULL, 
      c("anger", "anticipation", "disgust", "fear", "joy", "sadness", 
        "surprise", "trust"))))

  emotions <- get_nrc_sentiment(art_data)
  emotions_prop <- emotions[1,1:8] / sum(emotions[1,1:8])
  
  artist_keyword[i,]$sent <- sum((s_prop - emotions_prop)^2)
}

table_word <- artist_keyword[order(artist_keyword$word, decreasing = TRUE),] %>%
  mutate(rank_word = 1:nrow(artist_keyword))
table_sent <- artist_keyword[order(artist_keyword$sent),]  %>%
  mutate(rank_sent = 1:nrow(artist_keyword))

rank_table <- table_word %>%
  left_join(table_sent, by = "artist") %>%
  select(artist, rank_word, rank_sent) %>%
  mutate(rank = rank_word + rank_sent)

rank_table[order(rank_table$rank),]

