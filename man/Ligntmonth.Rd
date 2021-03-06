% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Ligntmonth}
\alias{Ligntmonth}
\title{Relationship between damage reports and deaths caused by lightning}
\format{A data frame with 12 observations on the following 4 variables.
\describe{ 
\item{Month}{a factor with levels \code{1/01/2000}
\code{10/01/2000} \code{11/01/2000} \code{12/01/2000} \code{2/01/2000}
\code{3/01/2000} \code{4/01/2000} \code{5/01/2000} \code{6/01/2000}
\code{7/01/2000} \code{8/01/2000} \code{9/01/2000}} 
\item{deaths}{a numeric vector} 
\item{injuries}{a numeric vector}
\item{damage}{a numeric vector} 
}}
\description{
Data for Exercise 2.6
}
\examples{

str(Ligntmonth)
attach(Ligntmonth)
plot(damage,deaths)
detach(Ligntmonth)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Duxbury
}
\keyword{datasets}

