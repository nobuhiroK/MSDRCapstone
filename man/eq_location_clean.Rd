% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_data.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{cleans the LOCATION_NAME}
\usage{
eq_location_clean(dat = eq_clean_data())
}
\arguments{
\item{dat, }{default for cleaned signif.txt from eq_clean_data()}
}
\value{
a cleand tibble of 48 variables
}
\description{
stripping out the country name (including the colon)
and converts names to title case (as opposed to all caps)
}
\examples{
clean_location <- eq_location_clean()

}
