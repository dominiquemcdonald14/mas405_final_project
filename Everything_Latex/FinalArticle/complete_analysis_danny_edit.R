
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
library(RColorBrewer)

library(cluster)
library(factoextra)

library(ggplot2)
library(xtable)
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
#               DATA CLEANUP
############################################
# Cleaning Shakespeare sonnets: OG SONNETS
############################################
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

############################################
#   CLEAN UP OF THE ARTIST DATA
############################################
#removing the column of row numbers
artists_complete <- select(artists_complete,-c(1))

###this code will take all the non-lyrical strings contained in a set of 
#parenthesis and compile it into a vector of lists
df  <- vector(mode = "list", length = 45)
names(df) <- artists_complete$Artist

for (i in 1:45) {
  
  df[[i]] <- gsub("[\\(\\)]", "", unlist(regmatches(artists_complete$Lyrics[i],
                                            gregexpr("\\(.*?\\)", 
                                            artists_complete$Lyrics[i]))))
}

df[["adele"]] #look at the first element as an example


# had to remove "Verse 1, Chorus 2, Verse 3, and Chorus 3
#you'll find that those phrases are no longer there and what remains is just empty quotations
artists_complete <- 
  artists_complete %>% 
  mutate_at("Lyrics", str_remove_all, pattern = c("Verse 1|Chorus 2|Verse 3|Chorus 3|x2|vocal solo|Saxophone solo|fadeout|chorus|Repeat 4 times|bv=|scat singing|4x|x8|x7|x4 |2x")) 


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

#the pattern for the special character may vary from computer to computer
# here are other patterns incase is the one bellow wont work
# ????T, ????T, 
og_sonnets <- og_sonnets %>% 
  mutate_at("Sonnets", str_replace_all, "????T", "\'") #the pattern for the special character may vary from computer to computer

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
png("_assets/Shakespeare_Keywords_WordCloud.png", width=1980, height=1080, pointsize=35)

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()

text(x=0.5, y=0.5, "Shakespeare Top Keywords Wordcloud")

set.seed(314)
wordcloud(words = dtm_d$word, 
          freq = dtm_d$freq, 
          min.freq = 5,
          max.words=100, 
          random.order=FALSE, 
          rot.per=0.20, 
          colors=brewer.pal(8, "Set1"),
          main = "Title")

dev.off()

#### world coud no title
png("_assets/Shakespeare_Keywords_WordCloud_No_title.png", width=1980, height=1080, pointsize=35)

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()

set.seed(314)
wordcloud(words = dtm_d$word, 
          freq = dtm_d$freq, 
          min.freq = 5,
          max.words=100, 
          random.order=FALSE, 
          rot.per=0.20, 
          colors=brewer.pal(8, "Set1"),
          main = "Title")

dev.off()

##### proportion calculation for Shakespeare
head(dtm_d)
num_terms <- length(x_text_dtm$i); num_terms #total number of terms - 7611
nTerms(x_text_dtm) # unique terms in sonnets - 3089
head(Terms(x_text_dtm)) #just a list of all the terms that show up

prop_terms<-  dtm_d$freq/num_terms; head(prop_terms)

dtm_prop <- cbind(dtm_d, prop_terms)

##############################################################
#                 Sentiment Analysis
##############################################################
# Sentiment Analysis for Shakespeare's Sonnets
#############################################

# exporting plots as png 
all_sonnets <- paste(og_sonnets$Sonnets[1:154], collapse = " ") #the collapse argument allows you to create a single string by concatenating ea element of the vector
all_sonnets

shake_8sent <- get_nrc_sentiment(all_sonnets) #getting shakespeare sentiments
shake_8sent

s_prop <- shake_8sent[1,1:8] / sum(shake_8sent[1,1:8])

shake_8sent_emotions <-shake_8sent[1:8]
shake_8sent_sents <- shake_8sent[9:10]

shake_8sent_emotions <- as.matrix(shake_8sent_emotions)
shake_8sent_sents <- as.matrix(shake_8sent_sents)

png("_assets/Barplot_Sonnets_8_Emotions.png", width=1920, height=1080, pointsize=35)
plot.new()

barplot(shake_8sent_emotions,
        main = "Frequency of Each Emotion",
        las = 2,
        col = brewer.pal(8, "Set1"))

dev.off()

png("_assets/Barplot_Sonnets_2_Sentiments.png", width=1920, height=1080, pointsize=35)
plot.new()
barplot(shake_8sent_sents,
        main = "Frequency of Positive and Negative Sentiments",
        las = 2,
        col = brewer.pal(3, "Set1"))

dev.off()

##################################
#  Music Artist Sentient Analysis 
####################################
sent_scores<- get_sentiment(artists_complete$Lyrics) #returns overall sentiment value
summary(sent_scores)

artists_8sent <- get_nrc_sentiment(artists_complete$Lyrics) #returns classification into each 8 sentiments + pos/neg
artists_8sent


all_8sent <- rbind(artists_8sent, shake_8sent) #appending shakespeare's sentiments to table with those of other artists

names_8sent <- c(artists_complete$Artist, "shakespeare") #getting artist names for table
names_8sent

all_8sent <- cbind(names_8sent, all_8sent) #adding names
all_8sent #quick check

####### Creating a table of proportions instead of frequencies
prop_8sent <- data.frame(matrix(ncol = 9, nrow = 46))

prop_8sent[,1] <- all_8sent$names_8sent

for(i in 1:46){ #this loop divides each element in a row bu the row sum
  
  prop_8sent[i,2:9] <- all_8sent[i,2:9]/sum(all_8sent[i,2:9])
  
}

prop_8sent #another quick check

#Now that we have the sentiments in a table kmeans clustering will be straightforward. 
#I will use MG's lovely code for this part :*
#identifying optimal number of clusters
k <- 12
vpc <- NULL #vpc here means "variance per cluster"
for (i in 1:k) {
    kfit <- kmeans(prop_8sent[,2:9], i)
    vpc <- c(vpc, kfit$betweenss/kfit$totss)
}
vpc

#writing plot out as a png 
png("_assets/Variance_Explained_VS_Number_Clusters.png", width=1920, height=1080, pointsize=35)
plot.new()
plot(1:k, vpc, xlab = "# of clusters", ylab = "explained variance", main = "Explained Variance Based on Number of Clusters")

dev.off()

kfit7 <- kmeans(prop_8sent[,2:9], 7)
kfit8 <- kmeans(prop_8sent[,2:9], 8)  
kfit10 <- kmeans(prop_8sent[,2:9], 10)
kfit12 <- kmeans(prop_8sent[,2:9], 12)

#clustering means were given for each of the eight emotions
kfit7$centers
kfit8$centers
kfit10$centers
kfit12$centers

########### Making Cluster Plots
##changing the row names so they will show up as labels for each point
row.names(prop_8sent) <- all_8sent$names_8sent 

{ #Run this to generate plot all at once 
  
png("_assets/ClusterAnalysis_Fit7.png", width=1920, height=1080, pointsize=35)
plot.new()
fviz_cluster(kfit7, data = prop_8sent[,2:9],
             palette = brewer.pal(7, "Set1"), 
             geom = c("point", "text"),
             ellipse = F,
             ellipse.type = "t", 
             ggtheme = theme_bw()
             )
dev.off()

png("_assets/ClusterAnalysis_Fit8.png", width=1920, height=1080, pointsize=35)
plot.new()
fviz_cluster(kfit8, data = prop_8sent[,2:9],
             palette = brewer.pal(8, "Set1"),
              geom = c("point", "text"),
             ellipse = F,
             ellipse.type = "t", 
             ggtheme = theme_bw()
             )
dev.off()

#this is the plot we decided to use for final article
png("_assets/ClusterAnalysis_Fit10.png", width=1920, height=1080, pointsize=35)
plot.new()
fviz_cluster(kfit10, data = prop_8sent[,2:9],
             palette = brewer.pal(10, "Paired"),
             geom = c("point", "text"),
             ellipse = F,
             ellipse.type = "t",
             labelsize = 35 ,
             ggtheme = theme_bw()) + ggtitle("Cluster Analysis of all Artist and Shakespeare") + theme(plot.title = element_text(size = 40), 
                                                                                                       legend.title = element_text(size = 30), 
                                                                                                       legend.text=element_text(size=30), 
                                                                                                       axis.title = element_text(size = 30),
                                                                                                       axis.text = element_text(size = 20)) 
dev.off()


png("_assets/ClusterAnalysis_Fit12.png", width=1920, height=1080, pointsize=35)
plot.new()
fviz_cluster(kfit12, data = prop_8sent[,2:9],
             palette = brewer.pal(12, "Paired"),
              geom = c("point", "text"),
             ellipse = F,
             ellipse.type = "t", 
             ggtheme = theme_bw()
             )
dev.off()

}
test <- cbind (all_8sent, kfit$cluster) #Appended the clusters to all artists sentiments to get table of artist in same group
#need to check which cluster shakespeare is in each time, because different groups might be formed each time you run the cluster analysis

# test[test$`kfit$cluster` == 4,] #these are the artists in shakespeare's cluster

####################################
# Sentiment Ranking of Shakespeare vs Artists
####################################

artist_keyword <- data.frame(artist = artists_complete$Artist, sent = rep(NA, 45), word = rep(NA,45), freq_df = rep(NA,45), prop_df = rep(NA,45), keyword = rep(NA,45))

artist_text <- Corpus(VectorSource(artists_complete[,2])); artist_text

for(i in 1:length(artists_complete[,2])){
 
  kw <- rep(NA, 10)
  frq <- rep(NA, 10)
  prp <- rep(NA, 10)
  temp_mat <- cbind(kw, frq, prp) 
  
  #temp_mat is the information of the current artist
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
  #number of total terms is the non sparse entries
  #art_doc_dtm
  artist_mat_dtm <- as.matrix(art_doc_dtm)
  # mat_dtm
  # Sort by decreasing value of frequency
  artisit_dtm_v <- sort(rowSums(artist_mat_dtm),decreasing=TRUE)
  artist_dtm_d <- data.frame(word = names(artisit_dtm_v),freq=artisit_dtm_v)
  head(artist_dtm_d)
  
  temp_df$word[1:10] <- artist_dtm_d$word[1:10]
  temp_df$frequency[1:10] <- artist_dtm_d$freq[1:10]; 
  art_num_terms <- length(art_doc_dtm$i); art_num_terms #total number of terms
  temp_df$proportion[1:10] <- artist_dtm_d$freq[1:10]/art_num_terms
  
  compare <- head(dtm_prop,10) %>%
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

table_word <- artist_keyword[order(artist_keyword$word,-rank(artist_keyword$freq_df), decreasing = TRUE),] %>%
  mutate(rank_word = 1:nrow(artist_keyword))
table_sent <- artist_keyword[order(artist_keyword$sent),]  %>%
  mutate(rank_sent = 1:nrow(artist_keyword))
  
rank_table <- table_word %>%
  left_join(table_sent, by = "artist") %>%
  select(artist, rank_word, rank_sent) %>%
  mutate(rank = rank_word + rank_sent)

rank_table  
table_word
table_sent

## Writing resulting top 10 most similar artist to Shakespeare out as a write able .tex file 
t <- rank_table[order(rank_table$rank),]
t
#selecting top ten
t1 <- t[1:10,]
t1
####make bottom 5 least similar to shakespeare 

t1 <- as.data.frame(t1)
#rename column name so table looks nice in latex
names(t1) <- c('Artist', 'Word Rank', 'Sentiment Rank', 'Overall Rank') 
#making xtable to .tex file 
xttbl <- xtable(t1, caption="Ranked Top 10 Most Similar Music Artist to Shakespeare", label="tab:overallranktable")
xxx <- print(xttbl, include.rownames=FALSE)
writeLines( xxx, file.path("_assets", "overall_rankTable_top10_Similar.tex") )

##writing bottom 5 just in case we want to use it

b_t1 <- t[40:45,]
names(b_t1) <- c('Artist', 'Word Rank', 'Sentiment Rank', 'Overall Rank')

xttbl <- xtable(b_t1, caption="Ranked Bottom 5 Least Similar Music Artist to Shakespeare", label="tab:bottom5_ranktable")
xxx <- print(xttbl, include.rownames=FALSE)
writeLines( xxx, file.path("_assets", "bottom5_rankTable_top10_Similar.tex") )

##Writing top 10 Keyword Rankings

t2 <- table_word %>% select(artist, word, freq_df, keyword, rank_word)
t2 <- t2[1:10,]
names(t2) <- c('Artist', 'Word Count', 'Frequency', 'Keyword', 'Word Rank')
t2

xttbl <- xtable(t2, caption="Ranked Top 10 Most Similar Music Artist to Shakespeare Based on Keywords", label="tab:wordranktable")
xxx <- print(xttbl, include.rownames=FALSE)
writeLines( xxx, file.path("_assets", "word_rankTable_top10_Similar.tex") )

##Writing top 10 Sentiment Rankings

t3 <- table_sent %>% select(artist, sent, rank_sent)
t3 <- t3[1:10,]
names(t3) <- c('Artist', 'Sent. Euclidean Distance', 'Sentiment Rank')
t3
xttbl <- xtable(t3, digits = 6, caption="Ranked Top 10 Most Similar Music Artist to Shakespeare Based on Sentiments", label="tab:wordranktable")
xxx <- print(xttbl, include.rownames=FALSE)
writeLines( xxx, file.path("_assets", "sent_rankTable_top10_Similar.tex") )
