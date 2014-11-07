library(rjson)


queries.pg <- rjson::fromJSON(paste(readLines("./postgres.json"), collapse=""))
queries.redshift <- rjson::fromJSON(paste(readLines("./redshift.json"), collapse=""))
queries.sqlite <- rjson::fromJSON(paste(readLines("./sqlite.json"), collapse=""))
queries.mysql <- rjson::fromJSON(paste(readLines("./mysql.json"), collapse=""))
