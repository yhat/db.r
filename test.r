library(devtools)
devtools::install()
library(db.r)

db = DB(filename="./inst/extdata/chinook.sqlite", dbtype="sqlite")
(db)
