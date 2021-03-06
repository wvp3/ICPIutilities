% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename_official.R
\name{rename_official}
\alias{rename_official}
\title{Cleanup Mechanism and Partner Names to their Official Names}
\usage{
rename_official(df)
}
\arguments{
\item{df}{identify the MER Structured DataSet to clean}
}
\description{
Some mechanisms and partners are recorded in FACTSInfo with multiple names over different time period. This function replaces all partner and mechanism names the most recent name for each mechanism ID pulling from a DATIM SQL View.
}
\examples{
\dontrun{
df_psnu_im <- rename_official(df_psnu_im) }
}
