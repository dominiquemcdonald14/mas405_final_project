###CONNECTING TO DOMINIQUE'S DB TO GET NETFLIX DATA

drv <- dbDriver("MySQL")
xdbsock <- ""


#############
xdbuser <- Sys.getenv("MAS405_AWS_DOMINIQUE_DB_ROUSER_USER")
xpw     <- Sys.getenv("MAS405_AWS_DOMINIQUE_DB_ROUSER_PW")
xdbname <- Sys.getenv("MAS405_AWS_DOMINIQUE_DB_ROUSER_DBNAME")
xdbhost <- Sys.getenv("MAS405_AWS_DOMINIQUE_DB_ROUSER_HOST")
xdbport <- as.integer( Sys.getenv("MAS405_AWS_DOMINIQUE_DB_ROUSER_PORT") )



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

qry1 <- "SELECT * FROM netflix"
netflix <- dbGetQuery(con, qry1)
dbWriteTable(con, "netlfix", netflix)


#Get out of my db losers
dbDisconnect(con)


### CONNECT TO YOUR OWN DB 


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
