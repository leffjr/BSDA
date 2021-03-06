% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Toxic}
\alias{Toxic}
\title{Hazardous waste sites near minority communities}
\format{A data frame with 51 observations on the following 5 variables.
\describe{ 
\item{state}{a factor with levels \code{alabama}
\code{alaska} \code{arizona} \code{arkansas} \code{california}
\code{colorado} \code{connecticut} \code{delaware} \code{dist of columbia}
\code{florida} \code{georgia} \code{hawaii} \code{idaho} \code{illinois}
\code{indiana} \code{iowa} \code{kansas} \code{kentucky} \code{louisiana}
\code{maine} \code{maryland} \code{massachusetts} \code{michigan}
\code{minnesota} \code{mississippi} \code{missouri} \code{montana}
\code{nebraska} \code{nevada} \code{new hampshire} \code{new jersey}
\code{new mexico} \code{new york} \code{north carolina} \code{north dakota}
\code{ohio} \code{oklahoma} \code{oregon} \code{pennsylvania} \code{rhode
island} \code{south carolina} \code{south dakota} \code{tennessee}
\code{texas} \code{utah} \code{vermont} \code{virginia} \code{washington}
\code{west virginia} \code{wisconsin} \code{wyoming}}
\item{region}{a factor with levels \code{midwest} \code{northeast}
\code{south} \code{west}} 
\item{sites}{a numeric vector}
\item{minority}{a numeric vector} 
\item{percent}{a numeric vector} 
}}
\description{
Data for Exercises 1.55, 5.08, 5.109, 8.58, and 10.35
}
\examples{

str(Toxic)
attach(Toxic)
hist(minority,prob=TRUE)
lines(density(minority))
SIGN.test(sites,conf.level=.98)
boxplot(sites~region)
kruskal.test(sites~as.factor(region))
detach(Toxic)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Duxbury
}
\keyword{datasets}

