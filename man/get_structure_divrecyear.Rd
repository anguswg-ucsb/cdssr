% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_structures_divrec_ts.R
\name{get_structure_divrecyear}
\alias{get_structure_divrecyear}
\title{Return Structure Yearly Diversion/Release Records}
\usage{
get_structure_divrecyear(
  wdid = NULL,
  wc_identifier = NULL,
  start_date = "1900-01-01",
  end_date = Sys.Date(),
  api_key = NULL
)
}
\arguments{
\item{wdid}{character vector or list of characters indicating WDID code of structure}

\item{wc_identifier}{wc_identifier character, series of water class codes that provide the location of the diversion, the SOURCE of water, the USE of the water and the administrative operation required to make the diversion. Provide "diversion" or "release" to retrieve diversion/release records. Default is NULL which will return diversions records.}

\item{start_date}{character date to request data start point YYYY-MM-DD. Default start date is "1900-01-01".}

\item{end_date}{character date to request data end point YYYY-MM-DD. Default end date is the current date the function is run.}

\item{api_key}{character, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.}
}
\value{
dataframe with yearly structure data for the CDSS structure of interest
}
\description{
Make a request to the api/v2/structures/divrec/divrecyear/ endpoint to retrieve annual structure diversion/release data for a specified WDID within a specified date range.
}
