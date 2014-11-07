library(rjson)

queries.pg <- rjson::fromJSON(paste(readLines(system.file("include", "postgres.json")), collapse=""))
queries.redshift <- rjson::fromJSON(paste(readLines(system.file("include", "redshift.json")), collapse=""))
queries.sqlite <- rjson::fromJSON(paste(readLines(system.file("include", "sqlite.json")), collapse=""))
queries.mysql <- rjson::fromJSON(paste(readLines(system.file("include", "mysql.json")), collapse=""))
