% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NNT.R
\name{NNT}
\alias{NNT}
\title{The number needed to treat (NNT)}
\usage{
NNT(d = NULL, CER = NULL, EER = NULL, type = "classic")
}
\arguments{
\item{d}{a cohen's d effect size or equivalent}

\item{CER}{the experimental event rate  or a vector of value (between 0 and 1)}

\item{EER}{the control event rate between (0 and 1)}

\item{type}{the type of NNT could be "classic", "Furukawa" or "Kraemer"}
}
\value{
an NNT score or a table of NNT score
}
\description{
The number needed to treat (NNT)
}
\examples{

NNT(d=0.20,CER=0.20,type="Furukawa")
NNT(d=0.20,type="Kraemer")
NNT(CER=.20,EER=.40)

}
