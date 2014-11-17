library(DBI)
library(methods)
library(stringr)

# helpers
ifnull = function(x, v) {
  ifelse(is.na(x), x, v)
}


db.db <- methods::setRefClass("db.db",
  fields = list(
    tables = "list",
    con = "ANY",
    hostname = "ANY", 
    port = "ANY", 
    username = "ANY", 
    password = "ANY",
    dbname = "ANY",
    filename = "ANY",
    dbtype = "ANY",
    schema = "ANY",
    query.templates = "ANY"
  ),
  methods = list(
    init = function(profile=NA) {
      if (! is.na(profile)) {
        load_credentials(profile)
      }
      if (dbtype=="postgres") {
        library(RPostgreSQL)
        drv <- DBI::dbDriver("PostgreSQL")
        port <<- ifnull(port, 5432)
        query.templates <<- queries.pg
      } else if (dbtype=="mysql") {
        library(RMySQL)
        drv <- DBI::dbDriver("MySQL")
        port <<- ifnull(port, 3309)
        query.templates <<- queries.mysql
      } else if (dbtype=="sqlite") {
        library(RSQLite)
        drv <- DBI::dbDriver("SQLite")
        query.templates <<- queries.sqlite
      } else if (dbtype=="redshift") {
        library(RPostgreSQL)
        drv <- DBI::dbDriver("PostgreSQL")
        port <<- ifnull(port, 5439)
        query.templates <<- queries.redshift
      } else {
        stop(sprintf("Database %s not implemented!", dbtype))
      }
      if (dbtype=="sqlite") {
        con <<- DBI::dbConnect(drv, filename) 
        ..create_sqlite_metatable()
      } else {
        # DBI will "fill up" with connections. it's best to mind
        # your manners and clean up after yourself
        lapply(DBI::dbListConnections(drv), DBI::dbDisconnect)
        args <- list(drv, user=username, password=password,
                     dbname=dbname, host=hostname, port=port)
        suppressWarnings(args[sapply(args, is.na)] <- NULL)
        con <<- do.call(DBI::dbConnect, args)
      }
      refresh_schema()
      methods <- getRefClass(class(.self))$methods()
      eval(parse(text=paste(".self$", methods)))
      .self
    },
    show = function() {
      'visual representation of the db'
      cat(paste(string(), "\n", sep=""))
    },
    string = function() {
      'string representation of the db'
      sprintf("DB[%s][%s]:%d > %s@%s",
        dbtype, hostname, port, username, dbname)
    },
    save_credentials = function(profile="default") {
      'Save your credentials. You can specify a database profile in the event
      that you\'re using more than 1 database'
      creds <- list(
        username = username,
        password = password,
        hostname = hostname,
        port = port,
        dbname = dbname,
        dbtype = dbtype
      )
      write(rjson::toJSON(creds), paste0("~/.db.r_", profile))
    },
    load_credentials = function(profile="default") {
      'Load saved credentials. You can specify a database profile to load from'
      f <- paste0("~/.db.r_", profile)
      creds <- rjson::fromJSON(paste(readLines(f), sep=""))
      username <<- creds$username
      password <<- creds$password
      hostname <<- creds$hostname
      port <<- creds$port
      dbname <<- creds$dbname
      dbtype <<- creds$dbtype
    },
    query = function(q) {
      'Execute a query (q) on your database.'
      DBI::dbGetQuery(con, q)
    },
    query_from_file = function(filename) {
      'Execute a query from a file (filename) on your database.'
      q <- paste(readLines(filename), sep="", collapse="")
      DBI::dbGetQuery(con, q)
    },
    ..dev_query = function(q) {
      df <- DBI::dbGetQuery(con, q)
      if (nrow(df) > 0) {
        rownames(df) <- 1:nrow(df)
      }
      df
    },
    find_table = function(s) {
      'Search for a table in your database with a query string `s`'
      # make into a regex or glob
      idx <- grep(s, names(tables))
      tables[idx]
    },
    find_column = function(s, type=NA) {
      'Search for a column in your database with a query string `s`. You can 
      optionally specify datatypes to filter by.'
      if (is.na(type)) {
        schema[grep(s, schema$column_name),]
      } else {
        schema[schema$column_name==s & schema$udt_name %in% type,]
      }
    },
    ..create_sqlite_metatable = function() {
      all.tables <- ..dev_query("select name from sqlite_master where type='table';")
      all.tables <- lapply(all.tables$name, function(table_name) {
        result <- DBI::dbGetQuery(con, sprintf("pragma table_info(%s)", table_name)) 
        result$table_name <- table_name
        result
      })
      if (DBI::dbExistsTable(con, "tmp_dbpy_schema")) {
        DBI::dbRemoveTable(con, "tmp_dbpy_schema")
      }
      DBI::dbSendQuery(con, "create temp table tmp_dbpy_schema(table_name varchar, column_name varchar, data_type varchar);")
      lapply(all.tables, function(rows) {
        for(i in 1:nrow(rows)) {
          row <- rows[i,]
          q <- sprintf("insert into tmp_dbpy_schema(table_name, column_name, data_type) values('%s', '%s', '%s');", 
              row$table_name, row$name, row$type)
          DBI::dbSendQuery(con, q)
        }
      })

      if (DBI::dbExistsTable(con, "tmp_dbpy_foreign_keys")) {
        DBI::dbRemoveTable(con, "tmp_dbpy_foreign_keys")
      }
      DBI::dbSendQuery(con, "create temp table tmp_dbpy_foreign_keys(table_name varchar, column_name varchar, foreign_table varchar, foreign_column varchar);")
      all.sqls <- DBI::dbGetQuery(con, "SELECT name, sql  FROM sqlite_master ;")
      result <- stringr::str_match_all(all.sqls$sql, "FOREIGN KEY ..([A-Za-z]+).. REFERENCES .([A-Za-z]+). ..([A-Za-z]+)..")
      for(i in 1:length(result)) {
        r <- result[[i]]
        if(length(r) > 0) {
          q <- sprintf("insert into tmp_dbpy_foreign_keys(table_name, column_name, foreign_table, foreign_column) values('%s', '%s', '%s', '%s');",
            all.sqls$name[i], r[2], r[3], r[4])
          DBI::dbSendQuery(con, q)
        }
      }
    },
    refresh_schema = function() {
      'Updates and adds/removes any new/old tables/columns from the schema'
      schema <<- DBI::dbGetQuery(con, query.templates$system$schema_no_system)
      all.tables <- unique(schema$table_name)
      for (tname in all.tables) {
        schema.table <- subset(schema, table_name==tname)
        tbl <- db.table.new(tname, .self, schema.table)
        tables[tname] <<- tbl
      }
    }
  )
)

db.column <- methods::setRefClass("db.column",
  fields = list( name = "character", table_name = "character", db = "db.db"),
  methods = list(
    init = function() {
      methods <- getRefClass(class(.self))$methods()
      eval(parse(text=paste(".self$", methods)))
      .self
    },
    show = function() {
      'representation of the column'
      cat(paste(string(), "\n", sep=""))
    },
    string = function() {
      'string representation of the column'
      sprintf("Column <%s>", name)
    },
    head = function(n=10) {
      q <- sprintf(db$query.templates$column$head, name, table_name,  n)
      db$..dev_query(q)
    },
    all = function() {
      'returns all records for column'
      q <- sprintf(db$query.templates$column$all, name, table_name)
      db$..dev_query(q)
    },
    sample = function(n=10) {
      'returns a random sample of N records'
      q <- sprintf(db$query.templates$column$sample, name, table_name, n)
      db$..dev_query(q)
    },
    unique = function() {
      'returns all unique instances of the column'
      q <- sprintf(db$query.templates$column$unique, name, table_name)
      db$..dev_query(q)
    }
  )
)


db.table <- methods::setRefClass("db.table",
  fields = list( name = "ANY", db = "db.db", schema = "ANY", columns = "list", foreign.keys = "data.frame", ref.keys = "data.frame"),
  methods = list(
    init = function() {
      row.names(schema) <<- 1:nrow(schema)
      for(colname in schema$column_name) {
        columns[colname] <<- db.column.new(name=colname, table_name=name, db=db)
      }
      foreign.keys <<- db$..dev_query(sprintf(db$query.templates$system$foreign_keys_for_table, name))
      ref.keys <<- db$..dev_query(sprintf(db$query.templates$system$ref_keys_for_table, name))

      # initialize type ahead
      methods <- getRefClass(class(.self))$methods()
      eval(parse(text=paste(".self$", methods)))
      .self
    },
    show = function() {
      'visual representation of the table'
      foreign.key.str <- sapply(names(columns), function(col) {
        t0 <- foreign.keys[foreign.keys$column_name==col,]
        paste(t0$foreign_table_name, t0$foreign_column_name, sep=".", collapse=",")
      })
      ref.key.str <- sapply(names(columns), function(col) {
        t0 <- ref.keys[ref.keys$column_name==col,]
        paste(t0$foreign_table_name, t0$foreign_column_name, sep=".", collapse=",")
      })
      schema$foreign.keys <<- foreign.key.str
      schema$ref.keys <<- ref.key.str
      # TODO: this can be a bit much...
      print(schema)
    },
    string = function() {
      'string epresentation of the table'
      sprintf("Table <%s>", name)
    },
    head = function(n=6) {
      'returns the first N rows of the table'
      q <- sprintf(db$query.templates$table$head, name, n)
      db$..dev_query(q)
    },
    all = function() {
      'returns all the rows in the table'
      q <- sprintf(db$query.templates$table$all, name)
      db$..dev_query(q)
    },
    sample = function(n=10) {
      'returns a random sample of N rows in the table'
      q <- sprintf(db$query.templates$table$sample, name, n)
      db$..dev_query(q)
    },
    unique = function(...) {
      'returns all unique instance of specified columns in the table'
      cols <- c(...)
      cols <- paste(cols, collapse=", ")
      q <- sprintf(db$query.templates$table$unique, cols, name)
      db$..dev_query(q)
    },
    select = function(...) {
      'returns all rows from the table with the specified columns'
      cols <- c(...)
      cols <- paste(cols, collapse=", ")
      q <- sprintf(db$query.templates$table$select, cols, name)
      db$..dev_query(q)
    }
  )
)

db.column.new <- function(name, table_name, db) {
  newColumn <- db.column$new(name=name, table_name=table_name, db=db)
  newColumn$init()
}

db.table.new <- function(name, db, schema) {
  newTable <- db.table$new(name=name, db=db, schema=schema)
  newTable$init()
}

#'Connection to a database
#'
#'Returns a structure that helps you query and interact with your database.
#' 
#'@param hostname the hostname for your database (i.e. dw.muppets.com)
#'@param port the port for your database (i.e. 5432 or 3309)
#'@param username the username for your database (i.e. kermit)
#'@param password the password for your database (i.e. supersecret)
#'@param filename filepath to SQLite db 
#'@param dbname the dbname for your database (i.e. dw, production, staging)
#'@param dbtype the dbtype for your database (postgres, mysql, sqlite, or redshift)
#'@param profile the profile for your database (a connection profile)
#'
#'@keywords db, object, new
#'
#'@export
#'@examples
#' db <- DemoDB()
#' db$query("select * from Artist;")
#' db$tables
#' db$tables$Artist
#' db$tables$Artist$head()
#' db$tables$Artist$all()
#' db$tables$Artist$sample()
#' db$query("select * from Artist;")
#' db$find_table("A*")
#' db$find_column("*Id*")
#' \dontrun{
#' db <- db.new(username="kermit", password="rainbowconnection", 
#"  hostname="localhost", dbname="muppetdb", dbtype="postgres")
#' db$save_credentails(profile="muppetdb")
#'
#' db <- db.new(profile="mysql_local")
#' db$query("select * from foo limit 10;")
#' }
db.new <- function(hostname=NA, port=NA, username=NA, password=NA, dbname=NA,
                   filename=NA, dbtype=NA, profile=NA) {
  newDB <- db.db$new(hostname=hostname, port=port, username=username, 
                     password=password, dbname=dbname, dbtype=dbtype,
                     filename=filename)
  newDB$init(profile)
}

#'Connection to a database
#'
#'Returns a structure that helps you query and interact with your database.
#' 
#'@param hostname the hostname for your database (i.e. dw.muppets.com)
#'@param port the port for your database (i.e. 5432 or 3309)
#'@param username the username for your database (i.e. kermit)
#'@param password the password for your database (i.e. supersecret)
#'@param filename filepath to SQLite db 
#'@param dbname the dbname for your database (i.e. dw, production, staging)
#'@param dbtype the dbtype for your database (postgres, mysql, sqlite, or redshift)
#'@param profile the profile for your database (a connection profile)
#'
#'@keywords db, object, new
#'
#'@export
#'@examples
#' db <- DemoDB()
#' db$query("select * from Artist;")
#' db$tables
#' db$tables$Artist
#' db$tables$Artist$head()
#' db$tables$Artist$all()
#' db$tables$Artist$sample()
#' db$query("select * from Artist;")
#' db$find_table("A*")
#' db$find_column("*Id*")
#' \dontrun{
#' db <-DB(username="kermit", password="rainbowconnection", 
#'  hostname="localhost", dbname="muppetdb", dbtype="postgres")
#' db$save_credentails(profile="muppetdb")
#'
#' db <-DB(profile="mysql_local")
#' db$query("select * from foo limit 10;")
#' }
DB <- db.new

#'Demo database for testing and examples.
#' 
#'Psuedo-function that returns an example database. We're using the Chinook  
#'database which is a commonly used example database.
#' 
#'@export 
#'@examples 
#'db <- DemoDB() 
#'db$query("select * from Artist;") 
#'db$tables
DemoDB <- function() {
  file.chinook <- paste(system.file(package="db.r"), "extdata", "chinook.sqlite",
                        sep="/")
  db.new(filename = file.chinook, dbtype="sqlite")
}
