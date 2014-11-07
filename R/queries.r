library(rjson)

# load query templates
loadTemplate <- function(name) {
  filename <- system.file("include", paste0(name, ".json"))
  rjson::fromJSON(paste(readLines(filename), collapse=""))
}

queries.pg <- loadTemplate("postgres")
queries.redshift <- loadTemplate("redshift")
queries.sqlite <- loadTemplate("sqlite")
queries.mysql <- loadTemplate("mysql")
