
#loading necessary packages
library(RMySQL)
library(DBI)

library(dplyr)
library(stringr)

drv <- dbDriver("MySQL")
xdbsock <- ""


#############
xdbuser <- Sys.getenv("MAS405_AWS_MY_DB_ROUSER_USER")
xpw     <- Sys.getenv("MAS405_AWS_MY_DB_ROUSER_PW")
xdbname <- Sys.getenv("MAS405_AWS_MY_DB_ROUSER_DBNAME")
xdbhost <- Sys.getenv("MAS405_AWS_MY_DB_ROUSER_HOST")
xdbport <- as.integer( Sys.getenv("MAS405_AWS_MY_DB_ROUSER_PORT") )

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

artists_complete <- dbGetQuery(con, "SELECT * FROM artists_complete")

dbDisconnect(con)

#############
#*sigh* idk why dbWriteTable creates a column of row numbers
#table is loaded with an extra and useless column
artists_complete <- select(artists_complete,-c(1))

df  <- vector(mode = "list", length = 49)
names(df) <- artists_complete$artist_names

for (i in 1:49) {
  
  df[[i]] <- gsub("[\\(\\)]", "", unlist(regmatches(artists_complete$artist_songs[i],
                                            gregexpr("\\(.*?\\)", 
                                            artists_complete$artist_songs[i]))))
}

df


#############
###need to input code that will remove the phrases that are not lyrics


############
#the last thing to do is to remove unwanted punctuation

#this removes everything inside brackets, brackets included
artists_complete$artist_songs <- gsub("\\[.+?\\]", "", 
                                      artists_complete$artist_songs)

#now with parenthesis includes lyrics being sung...
#this removes all the parenthesis BUT without removing the text inside
artists_complete$artist_songs <- gsub("\\s*\\([^\\)]+\\)","", 
                                        artists_complete$artist_songs)


#This replaces everything that's not alphanumeric signs, space or apostrophe 
#with an empty string...yes please
artists_complete$artist_songs <- gsub("[^[:alnum:][:space:]']", "", 
                                      artists_complete$artist_songs)


