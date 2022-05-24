#GETTING THE ORIGINAL SONNETS FROM ANGELS DB

library(RMySQL)


drv <- dbDriver("MySQL")
xdbsock <- ""


#############
xdbuser <- Sys.getenv("MAS405_AWS_ANGEL_DB_ROUSER_USER")
xpw     <- Sys.getenv("MAS405_AWS_ANGEL_DB_ROUSER_PW")
xdbname <- Sys.getenv("MAS405_AWS_ANGEL_DB_ROUSER_DBNAME")
xdbhost <- Sys.getenv("MAS405_AWS_ANGEL_DB_ROUSER_HOST")
xdbport <- as.integer( Sys.getenv("MAS405_AWS_ANGEL_DB_ROUSER_PORT") )

#dbDisconnect(con)

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


#Getting Artist data from table called Lyrics. DO NOT SELECT FROM THE TABLE TITLED ARTISTS!!!
qry1 <- "SELECT * FROM  Lyrics"
Artists <- dbGetQuery(con, qry1) 
View(Artists)
dbDisconnect(con) #Thanks Angel. Bye!



#### Connect to your own db



drv <- dbDriver("MySQL")
xdbsock <- ""


#############
xdbuser <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_USER")
xpw     <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_PW")
xdbname <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_DBNAME")
xdbhost <- Sys.getenv("MAS405_AWS_MY_DB_ADMIN_HOST")
xdbport <- as.integer( Sys.getenv("MAS405_AWS_MY_DB_ADMIN_PORT") )



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


dbWriteTable(con, "Lyrics", Artists)

