% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_water_classes.R
\name{get_water_classes}
\alias{get_water_classes}
\title{Returns list of waterclasses}
\usage{
get_water_classes(
  wdid = NULL,
  county = NULL,
  division = NULL,
  water_district = NULL,
  wc_identifier = NULL,
  aoi = NULL,
  radius = NULL,
  gnis_id = NULL,
  start_date = NULL,
  end_date = NULL,
  divrectype = NULL,
  ciu_code = NULL,
  timestep = NULL,
  api_key = NULL
)
}
\arguments{
\item{wdid}{character vector or list of characters indicating WDID code of structure}

\item{county}{character, county to query}

\item{division}{numeric, water division to query}

\item{water_district}{numeric, water district to query}

\item{wc_identifier}{character, series of water class codes that provide the location of the diversion, the SOURCE of water, the USE of the water and the administrative operation required to make the diversion. The Water Class, combined with a daily, monthly or annual volume, constitutes a Diversion Record.}

\item{aoi}{list of length 2 containing an XY coordinate pair, 2 column matrix/dataframe of XY coordinates, sf or Terra SpatVector point/polygon/linestring geometry}

\item{radius}{numeric, search radius in miles around given point (or the centroid of a polygon). If an AOI is given, radius defaults to 20 miles. If no AOI is given, then default is NULL.}

\item{gnis_id}{character, water source - Geographic Name Information System ID}

\item{start_date}{character date of first measurement in the well’s period of record (YYYY-MM-DD). Default is NULL.}

\item{end_date}{character date of last measurement in the well’s period of record (YYYY-MM-DD). Default is NULL.}

\item{divrectype}{character, type of record: "DivComment", "DivTotal", "RelComment", "RelTolal", "StageVolume", or "WaterClass".}

\item{ciu_code}{character, current in use code of structure}

\item{timestep}{character, timestep, one of "day", "month", "year"}

\item{api_key}{character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS. Defaults to NULL.}
}
\value{
dataframe of water class data for administrative structures
}
\description{
Make a request to the /structures/divrec/waterclasses endpoint to identify water classes via a spatial search or by division, county, water_district, GNIS, or WDID.
}
\examples{
\dontrun{
# Request endpoint: /structures/divrec/waterclasses
wc <- get_water_classes(
                   county     = "Boulder",
                   start_date = "1999-01-01",
                   end_date   = "2005-01-01",
                   timestep   = "year"
                   )
}
}
