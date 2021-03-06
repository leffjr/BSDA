% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Nascar}
\alias{Nascar}
\title{Efficiency of pit crews for three major NASCAR teams}
\format{A data frame with 36 observations on the following 6 variables.
\describe{ 
\item{TeamA}{a numeric vector} 
\item{TeamB}{a numeric vector} 
\item{TeamC}{a numeric vector} 
\item{Time}{a numeric vector} 
\item{Team}{a numeric vector} 
\item{Ranks}{a numeric vector} 
}}
\description{
Data for Example 10.53
}
\examples{

str(Nascar)
attach(Nascar)
boxplot(Time~Team)
anova(lm(Time~as.factor(Team)))
detach(Nascar)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Duxbury
}
\keyword{datasets}

