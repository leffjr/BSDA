% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Trucks}
\alias{Trucks}
\title{Miles per gallon for standard 4-wheel drive trucks manufactured by
Chevrolet, Dodge and Ford}
\format{A data frame with 15 observations on the following 5 variables.
\describe{ 
\item{chevy}{a numeric vector} 
\item{dodge}{a numeric vector} 
\item{ford}{a numeric vector}
\item{gas.mileage}{a numeric vector} 
\item{truck}{a factor with levels \code{chevy} \code{dodge} \code{ford}} 
}}
\description{
Data for Example 10.2
}
\examples{

str(Trucks)
attach(Trucks)
anova(lm(gas.mileage~truck))
detach(Trucks)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Duxbury
}
\keyword{datasets}

