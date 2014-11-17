\name{db.r}
\alias{db.r}
\alias{db.r}
\docType{package}
\title{
  Explore databases interactively using the R shell
}
\description{
    \code{db.r} provides a way to interactively explore databases in R. It
    uses the Reference classes to create Python/Ruby/Javascript-esque objects
    in R that dynamically load database schemas, execute queries, and manage
    credentials.
}
\details{
    DB

    \code{\link{DB}} is a simple way to connect to a database. This is your 
    contstructor object that handles authentication, managing the connection, 
    querying, etc.

    db.new
    
    \code{\link{db.new}} is an alternate name for \code{\link{DB}}. It is just a
    wrapper around the same function/object.

    DemoDB

    \code{\link{DemoDB}} is a demo database that ships with \code{db.r}. The 
    data and schema come from the \href{http://chinookdatabase.codeplex.com/}{Chinook Database}
    which is commonly used as an example relational database.
    

See \url{https://github.com/yhat/db.r} for more detials.
}
\author{
Greg Lamp <greg@yhathq.com>
}
\references{
  \url{http://www.yhathq.com/}
  \tabular{ll}{
	Package: \tab \code{db.r}\cr
	Type: \tab Package\cr
	Version: \tab 0.1.1\cr
	Date: \tab 2014-11-17\cr
	License: \tab FreeBSD\cr
	}
}
\keyword{ package }
\seealso{
  \url{http://blog.yhathq.com/posts/introducing-db-r.html}
}
\examples{
  db <- DemoDB()
  db$tables
  db$tables$Track
  db$tables$Track$head()
  db$tables$Track$all()
  db$tables$Track$sample()
  db$find_table("A*")
  db$find_column("*Id*")
}

