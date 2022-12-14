% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_resource_meta}
\alias{get_resource_meta}
\title{Request meta data for a CDSS API endpoint}
\usage{
get_resource_meta(endpoint_url = NULL, endpoint_path = NULL)
}
\arguments{
\item{endpoint_url}{character. URL to CDSS API REST Help page detailing the return fields for each endpoint. This URL can be found in the dataframe returned by the browse_api function}

\item{endpoint_path}{character. full path name of CDSS API resource}
}
\value{
dataframe with the endpoint name, field name, a description, the data type, and the endpoint URL
}
\description{
Returns the names, descriptions, and types for each response field for a given API endpoint
}
