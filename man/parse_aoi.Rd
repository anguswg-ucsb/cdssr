% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{parse_aoi}
\alias{parse_aoi}
\title{Internal function for extracting lat/lng points from SF/Terra geometries}
\usage{
parse_aoi(aoi)
}
\arguments{
\item{aoi}{list of length 2 containing an XY coordinate pair, 2 column matrix/dataframe of XY coordinates, sf or Terra SpatVector point/polygon/linestring geometry}
}
\value{
dataframe with lat/long columns
}
\description{
Internal function for extracting lat/lng points from SF/Terra geometries
}
