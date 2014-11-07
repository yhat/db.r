# load query templates
loadTemplate <- function(name) {
  # filename <- system.file("include", paste0(name, ".json"))
  # TODO: obviously this doesn't work...
  filename <- paste("/Users/glamp/repos/yhat/opensource/db.r/inst/extdata", paste0(name, ".json"), sep="/")
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

DemoDB <- DB(filename="/Users/glamp/repos/yhat/opensource/db.r/inst/extdata/chinook.sqlite", dbtype="sqlite")