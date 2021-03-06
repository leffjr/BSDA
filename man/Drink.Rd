% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Drink}
\alias{Drink}
\title{Opinion on referendum by view on moral issue of selling alcoholic beverages}
\format{A data frame with 3 observations on the following 4 variables.
\describe{ 
\item{drink}{a factor with levels \code{immoral}
\code{ok} \code{tolerated}} 
\item{For}{a numeric vector}
\item{Against}{a numeric vector} 
\item{undecide}{a numeric vector} 
}}
\description{
Data for Exercise 8.53
}
\examples{

str(Drink)
attach(Drink)
Drink
chisq.test(Drink[,2:4])
detach(Drink)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Duxbury
}
\keyword{datasets}

