
options(width=90, xtable.comment = FALSE)

if(!dir.exists("_assets")) {
    dir.create("_assets")
}

####might need to remove this later, ask Carina later about hard coding
pathtoRenviron <- "C:/Users/Mariana/Documents/.Renviron"
readRenviron(pathtoRenviron)

##########
#list of libraries that we might use
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

##########
#access my database to get the datasets
drv <- dbDriver("MySQL")
xdbsock <- ""


##########
#access database to acquire datasets

xdbuser <- Sys.getenv("MAS405_AWS_MARIANA_DB_ROUSER_USER")
xpw     <- Sys.getenv("MAS405_AWS_MARIANA_DB_ROUSER_PW")
xdbname <- Sys.getenv("MAS405_AWS_MARIANA_DB_ROUSER_DBNAME")
xdbhost <- Sys.getenv("MAS405_AWS_MARIANA_DB_ROUSER_HOST")
xdbport <- as.integer( Sys.getenv("MAS405_AWS_MARIANA_DB_ROUSER_PORT") )



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

##########
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

#NOW THE ARTIST DATA
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

#clean up is all done!




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






