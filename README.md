# db.r

## Installation

```r
> library(devtools)
> devtools::install_github("db.r", "yhat")
```


## Whirlwind Tour 

```r
> library(db.r)
> db <- DB(username="kermit", password="rainbowconnection",
    hostname="dw.muppets.com", dbname="muppetdb", dbtype="postgres")
> db$tables
> db$tables$jokes
> db$tables$jokes$head()
> db$tables$jokes$all()
> db$tables$jokes$sample()
> db$query("select * from jokes;")
> db$query_from_file("joke_query.sql")
> db$find_table("jok*")
> db$find_column("*id*")
```


## TODO
- [ ] Database Support
    - [x] postgres
    - [x] redshift
    - [x] mysql
    - [ ] sqlite
    - [ ] mssql
    - [ ] oracle
