
#loading necessary packages
library(RMySQL)
library(DBI)

library(dplyr)
library(stringr)

drv <- dbDriver("MySQL")
xdbsock <- ""


#############
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

artists_complete <- dbGetQuery(con, "SELECT * FROM Artists_noDuplicates")

dbDisconnect(con)

#############
#*sigh* idk why dbWriteTable creates a column of row numbers
#table is loaded with an extra and useless column
artists_complete <- select(artists_complete,-c(1))

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

df


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


