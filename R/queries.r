library(rjson)

# load query templates
loadTemplate <- function(name) {
  filename <- paste(system.file(package="db.r"), "extdata",
                    paste0(name, ".json"), sep="/")
  rjson::fromJSON(paste(readLines(filename), collapse=""))
}

#' Queries for Postgres
queries.pg <- loadTemplate("postgres")
#' Queries for Redshift
queries.redshift <- loadTemplate("redshift")
#' Queries for SQLite
queries.sqlite <- loadTemplate("sqlite")
#' Queries for Mysql
queries.mysql <- loadTemplate("mysql")
