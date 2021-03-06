#I'm putting literally every package I've used for this project 

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)
library(RMySQL)
library(dplyr)
library(stringr)



###need og and artist data - gonna grab from AS bc he has artist data with no duplicates


#dbDisconnect(con)
drv <- dbDriver("MySQL")
xdbsock <- ""

xdbuser <- Sys.getenv("MAS405_AWS_ANGEL_DB_ROUSER_USER")
xpw     <- Sys.getenv("MAS405_AWS_ANGEL_DB_ROUSER_PW")
xdbname <- Sys.getenv("MAS405_AWS_ANGEL_DB_ROUSER_DBNAME")
xdbhost <- Sys.getenv("MAS405_AWS_ANGEL_DB_ROUSER_HOST")
xdbport <- as.integer( Sys.getenv("MAS405_AWS_ANGEL_DB_ROUSER_PORT") )

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
qry1 <- "SELECT * FROM OG"

og <- dbGetQuery(con, qry1)

names(og) <-  "Sonnets"
head(og)

#clean up sonnets if you haven't already


og <- og %>% 
  mutate_at("Sonnets", str_replace_all, "�", "\'") #the pattern for the special character may vary from computer to computer


artists_complete <- dbGetQuery(con, "SELECT * FROM Artists_noDuplicates")
names(artists_complete)


dbDisconnect(con)

#############
#*sigh* idk why dbWriteTable creates a column of row numbers
#table is loaded with an extra and useless column


#artists_complete <- select(artists_complete,-c(1)) ##*****YOU ONLY NEED THIS LINE IF YOU WROTE THE DATA TO YOUR OWN DB*****##

###this code will take all the non-lyrical strings contained in a set of 
#parenthesis and compile it into some vector of lists thing...trust me, you'll see
#
df  <- vector(mode = "list", length = 49)
names(df) <- artists_complete$Artist

for (i in 1:49) {
  
  df[[i]] <- gsub("[\\(\\)]", "", unlist(regmatches(artists_complete$Lyrics[i],
                                                    gregexpr("\\(.*?\\)", 
                                                             artists_complete$Lyrics[i]))))
}

head(df) #it's good practice to use head/tail/subset when checking results if there's a whole bunch to print


#############
###need to input code that will remove the phrases that are not lyrics

#I looked through the lists and roughly 10 artists contained these wannabe lyrics
#Of those 10, about 17 wannabe lyrics were discovered and now must be eliminated

artists_complete <- 
  artists_complete %>% 
  mutate_at("Lyrics", str_remove_all, pattern = c("Verse 1|Chorus 2|Verse 3|Chorus 3|x2|vocal solo|Saxophone solo|fadeout|chorus|Repeat 4 times|bv=|scat singing|4x|x8|x7|x4|2x")) 

#can run this line of code again to verify if the strings are gone
#I would check adele, she had "Verse 1, Chorus 2, Verse 3, and Chorus 3
#you'll find that those phrases are no longer there and what remains is just 
#empty quotations...it's like you can still feel its presence but it's GONE

df2  <- vector(mode = "list", length = 49)
names(df2) <- artists_complete$Artist

for (i in 1:49) {
  
  df2[[i]] <- gsub("[\\(\\)]", "", unlist(regmatches(artists_complete$Lyrics[i],
                                                     gregexpr("\\(.*?\\)", 
                                                              artists_complete$Lyrics[i]))))
}

df2[["adele"]]

############
#the last thing to do is to remove unwanted punctuation

#this removes everything inside brackets, brackets included
artists_complete$Lyrics <- gsub("\\[.+?\\]", "", 
                                artists_complete$Lyrics)

#now with parenthesis includes lyrics being sung...
#this removes all the parenthesis BUT without removing the text inside
artists_complete$Lyrics <- gsub("\\s*\\([^\\)]+\\)","", 
                                artists_complete$Lyrics)


#This replaces everything that's not alphanumeric signs, space or apostrophe 
#with an empty string...yes please
artists_complete$Lyrics <- gsub("[^[:alnum:][:space:]']", "", 
                                artists_complete$Lyrics)
artists_complete$Lyrics[2]



#using syuzhet package for sentiment analysis

sent_scores<- get_sentiment(artists_complete$Lyrics) #returns overall sentiment value
sent_scores

artists_8sent <- get_nrc_sentiment(artists_complete$Lyrics) #returns classification into each 8 sentiments + pos/neg
artists_8sent


all_sonnets <- paste(og$Sonnets[1:154], collapse = " ") #the collapse argument allows you to create a single string by concatenating ea element of the vector
all_sonnets

shake_8sent <- get_nrc_sentiment(all_sonnets)

shake_8sent


all_8sent <- rbind(artists_8sent, shake_8sent)

names_8sent <- c(artists_complete$Artist, "shakespeare")
names_8sent

all_8sent <- cbind(names_8sent, all_8sent)
all_8sent


prop_8sent <- data.frame(matrix(ncol = 9, nrow = 46))

prop_8sent[,1] <- all_8sent$names_8sent


for(i in 1:46){
  
  
  prop_8sent[i,2:9] <- all_8sent[i,2:9]/sum(all_8sent[i,2:9])
  
}

names(prop_8sent) <- names(all_8sent)[1:9] #making col names the 8 sentiments

names(prop_8sent)[1] <- "artist"

prop_8sent





## Now that we have the sentiments in a table kmeans clustering will be straightforward. I will use MG's lovely code for this part 


k <- 8
vpc <- NULL #vpc here means "variance per cluster"
for (i in 1:k) {
  kfit <- kmeans(prop_8sent[,2:9], i)
  vpc <- c(vpc, kfit$betweenss/kfit$totss)
}
vpc

plot(1:k, vpc, xlab = "# of clusters", ylab = "explained variance")


kfit3 <- kmeans(prop_8sent[,2:9], 3)


kfit4 <- kmeans(prop_8sent[,2:9], 4)  

kfit7 <- kmeans(prop_8sent[,2:9],7) #7 clusters performs better

#clustering means were given for each of the eight emotions...
kfit$centers

#install.packages("factoextra")

library(factoextra)

row.names(prop_8sent) <- all_8sent$names_8sent #changing the row names so they will show up as labels for each point


fviz_cluster(kfit7, data = prop_8sent[,2:9],
             palette = c("deepskyblue", "cyan3", "gold1", "pink3", "purple2", "red", "salmon4"), 
             geom = c("point", "text"),
             ellipse = F,
             ellipse.type = "t", 
             ggtheme = theme_bw()
)







test <- cbind (all_8sent, kfit$cluster)
test[test$`kfit$cluster` == 4,]
test[test$`kfit$cluster` == 3,]
test[test$`kfit$cluster` == 2,]  
test[test$`kfit$cluster` == 1,] #these are the artists in shakespeare's cluster

########## Trying out linear regression for shits and gigs ############

#essentially going to calculate ea. artists euclidian distance from shakespeare. This will be my response, the 8 sentiments proportions
#will be the explanatory vars. Obvi gonna remove shakespeare from the artist data



test <- sum(prop_8sent[46,2:9] - prop_8sent[22,2:9])^2
test

ed_shake <- rep(NA, nrow(prop_8sent)) #euclidean distance from shakespeare

for(i in 1:46){ 
  ed_shake[i] <- (sum(prop_8sent[46, 2:9] - prop_8sent[i, 2:9])^2) 
  
}
ed_shake
unique(ed_shake) #there are only 5 unique values which is.... interesting 

prop_8sent[7,1]


reg_data <- cbind(prop_8sent[,2:9], ed_shake)
reg_data[-25,]

mod1 <- lm(data = reg_data, reg_data$ed_shake ~ .)

summary(mod1)  


par(mfrow = c(2,2))
plot(mod1)
#two or more of your predictor variables have an exact linear relationship between them - known as perfect multicollinearity.

cor(reg_data)


plot(ed_shake, reg_data$anger)
plot(ed_shake, reg_data$anticipation)
plot(ed_shake, reg_data$trust)


prop_8sent[25,1]
max(ed_shake)
