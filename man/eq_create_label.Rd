% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping.R
\name{eq_create_label}
\alias{eq_create_label}
\title{Construct `leaflet` popup text labels in html given data frame of earthquake data}
\usage{
eq_create_label(df)
}
\arguments{
\item{df}{Data frame containing cleaned earthquake data.}
}
\value{
Character vector of the html.
}
\description{
Construct `leaflet` popup text labels in html given data frame of earthquake data
}
\examples{
# return a character vector of the html code for leaflet labels,
# given a data frame of the cleaned data
df = data.frame(LOCATION_NAME='USA', EQ_PRIMARY=1, TOTAL_DEATHS=0,
                stringsAsFactors=FALSE)
labels = eq_create_label(df)
}
