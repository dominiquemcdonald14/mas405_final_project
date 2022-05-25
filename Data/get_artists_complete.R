#GETTING THE ORIGINAL SONNETS FROM ANGELS DB

library(RMySQL)


drv <- dbDriver("MySQL")
xdbsock <- ""


#############
xdbuser <- Sys.getenv("MAS405_AWS_DOMINIQUE_DB_ROUSER_USER")
xpw     <- Sys.getenv("MAS405_AWS_DOMINIQUE_DB_ROUSER_PW")
xdbname <- Sys.getenv("MAS405_AWS_DOMINIQUE_DB_ROUSER_DBNAME")
xdbhost <- Sys.getenv("MAS405_AWS_DOMINIQUE_DB_ROUSER_HOST")
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

#Getting correct table with artists and all of their lyrics

qry1 <- "SELECT * FROM artists_complete"
artists_complete <- dbGetQuery(con, qry1)



dbDisconnect(con)



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



dbWriteTable(con, "artists_complete", artists_complete)



