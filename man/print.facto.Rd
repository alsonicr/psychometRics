% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factorial.R
\name{print.facto}
\alias{print.facto}
\title{Facto print method}
\usage{
\method{print}{facto}(x)
}
\arguments{
\item{x}{a facto class object}
}
\description{
formating result for the facto class
}
\examples{
data("inference")
items <- c(paste0("item_0",3:9),"item_10")
f <- factor_explorer(inference, items)
print(t)
}
