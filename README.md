# db.r

## Installation
On CRAN
```r
> install.packages("db.r")
```
Using `devtools`
```r
> library(devtools)
> devtools::install_github("yhat/db.r")
```
Database specific packages. You'll probably need one or many of these.
```r
> devtools::install_url("http://cran.r-project.org/src/contrib/RSQLite_1.0.0.tar.gz")
> devtools::install_url("http://cran.r-project.org/src/contrib/RMySQL_0.9-3.tar.gz")
# not sure why but Postgres needs to be downloaded manually
# http://cran.r-project.org/src/contrib/RPostgreSQL_0.4.tar.gz
# unzip, then
> devtools::install("/path/to/RPostgreSQL_0.4")
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
    - [x] sqlite
    - [ ] mssql
    - [ ] oracle
