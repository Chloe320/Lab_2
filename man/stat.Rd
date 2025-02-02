% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics - DRG.R
\name{stat}
\alias{stat}
\title{A function that can calculate the mean, median and standard deviation of Average Medicare Payments}
\usage{
stat(df, Fun)
}
\arguments{
\item{df}{a dataframe that we want to analysis (DRG)}

\item{Fun}{choose a function (mean,median,std) to analysis DRG data}
}
\value{
a matrix of DRG code and the related statistics of Average Medicare Payments
}
\description{
A function that can calculate the mean, median and standard deviation of Average Medicare Payments
}
\examples{
stat(DRG,'mean')

}
