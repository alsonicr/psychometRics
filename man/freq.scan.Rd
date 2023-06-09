% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freq_scan.R
\name{freq.scan}
\alias{freq.scan}
\title{Frequency scam}
\usage{
freq.scan(data, items)
}
\arguments{
\item{data}{a data.frame containing the test or scale response in columns}

\item{items}{The name of the items (should be >1 )}
}
\value{
data.frame with the item name and their frequency of correct response
}
\description{
Frequency scan provide the frequency of correct answer for dichotomic item (0 or 1) in a data frame
}
\examples{
data("inference")
items <- c(paste0("item_0",3:9),"item_10")
freq.scan(inference, items)

inference[1,1] <- 4
freq.scan(inference, items)
}
