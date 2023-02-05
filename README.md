
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **cdssr** <img src="man/figures/cdssr_logo_custom2.png" align="right" width="25%" />

<!-- badges: start -->

[![Dependencies](https://img.shields.io/badge/dependencies-9/02-orange?style=flat)](#)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit/)
<!-- badges: end -->

<div align="left">

<p align="left">
<a href="https://dwr.state.co.us/Tools"><strong>« CDSS »</strong></a>
<br /> <a href="https://dwr.state.co.us/Rest/GET/Help">CDSS REST Web
Services</a>
</p>

</div>

<hr>

The goal of [**`cdssr`**](https://anguswg-ucsb.github.io/cdssr/) is to
provide functions that help R users to navigate, explore, and make
requests to the [CDSS REST API web
service](https://dwr.state.co.us/Rest/GET/Help).

The Colorado’s Decision Support Systems (CDSS) is a water management
system created and developed by the [Colorado Water Conservation Board
(CWCB)](https://cwcb.colorado.gov/) and the [Colorado Division of Water
Resources (DWR)](https://dwr.colorado.gov/).

Thank you to those at CWCB and DWR for providing an accessible and well
documented REST API!

<br>

> See [**`cdsspy`**](https://github.com/anguswg-ucsb/cdsspy), for the
> **Python** version of this package

------------------------------------------------------------------------

-   [**cdssr (R)**](https://github.com/anguswg-ucsb/cdssr)

-   [**cdssr documentation**](https://anguswg-ucsb.github.io/cdssr/)

-   [**cdsspy (Python)**](https://github.com/anguswg-ucsb/cdsspy)

-   [**cdsspy documentation**](https://pypi.org/project/cdsspy/)

------------------------------------------------------------------------

<br>

## Installation

You can install the development version of **`cdssr`** from
[GitHub](https://github.com/anguswg-ucsb/cdssr) with:

``` r
# install.packages("devtools")
devtools::install_github("anguswg-ucsb/cdssr")
```

``` r
# Load package
library(cdssr)
```

## **Available endpoints**

Below is a table of all of the CDSS API endpoints that **`cdssr`**
provides functions for.

| **-** | **Function**                     | **Description**                                                                                                                                                                     | **Endpoint**                                                                                                                                                                                                                                 |
|-------|----------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1     | **get_admin_calls()**            | Returns list of active/historic administrative calls                                                                                                                                | [administrativecalls/active](https://dwr.state.co.us/rest/get/help#Datasets&#AdministrativeCallsController&https://dnrweblink.state.co.us/dwr/ElectronicFile.aspx?docid=3600964&dbid=0&#gettingstarted&#jsonxml)                             |
| 2     | **get_structures()**             | Returns list of administrative structures                                                                                                                                           | [structures](https://dwr.state.co.us/rest/get/help#Datasets&#StructuresController&#gettingstarted&#jsonxml)                                                                                                                                  |
| 3     | **get_structures_divrec_ts()**   | Returns list of diversion/release records based on WDID                                                                                                                             | [structures/divrec/divrec](https://dwr.state.co.us/rest/get/help#Datasets&#DiversionRecordsController&https://dnrweblink.state.co.us/dwr/ElectronicFile.aspx?docid=3600965&dbid=0&#gettingstarted&#jsonxml)                                  |
| 4     | **get_structures_stage_ts()**    | Returns list of stage/volume records based on WDID                                                                                                                                  | [structures/divrec/stagevolume](https://dwr.state.co.us/rest/get/help#Datasets&#DiversionRecordsController&https://dnrweblink.state.co.us/dwr/ElectronicFile.aspx?docid=3600965&dbid=0&#gettingstarted&#jsonxml)                             |
| 5     | **get_climate_stations()**       | Returns Climate Stations                                                                                                                                                            | [climatedata/climatestations](https://dwr.state.co.us/rest/get/help#Datasets&#ClimateStationsController&https://www.ncdc.noaa.gov/cdo-web/webservices&https://www.northernwater.org/our-data/weather-data&#gettingstarted&#jsonxml)          |
| 6     | **get_climate_ts()**             | Returns Climate Station Time Series (day, month, year)                                                                                                                              | [climatedata/climatestationts](https://dwr.state.co.us/rest/get/help#Datasets&#ClimateStationsController&https://www.ncdc.noaa.gov/cdo-web/webservices&https://www.northernwater.org/our-data/weather-data&#gettingstarted&#jsonxml)         |
| 7     | **get_climate_frostdates()**     | Returns Climate Station Frost Dates                                                                                                                                                 | [climatedata/climatestationfrostdates](https://dwr.state.co.us/rest/get/help#Datasets&#ClimateStationsController&https://www.ncdc.noaa.gov/cdo-web/webservices&https://www.northernwater.org/our-data/weather-data&#gettingstarted&#jsonxml) |
| 8     | **get_gw_gplogs_wells()**        | Returns Groundwater GeophysicalLogsWell from filters                                                                                                                                | [groundwater/geophysicallogs/](https://dwr.state.co.us/rest/get/help#Datasets&#GroundwaterGeophysicalLogsController&#gettingstarted&#jsonxml)                                                                                                |
| 9     | **get_gw_gplogs_geologpicks()**  | Returns Groundwater Geophysical Log picks by well ID                                                                                                                                | [groundwater/geophysicallogs/](https://dwr.state.co.us/rest/get/help#Datasets&#GroundwaterGeophysicalLogsController&#gettingstarted&#jsonxml)                                                                                                |
| 10    | **get_gw_wl_wells()**            | Returns WaterLevelsWell from filters                                                                                                                                                | [groundwater/waterlevels/wells](https://dwr.state.co.us/rest/get/help#Datasets&#GroundwaterLevelsController&#gettingstarted&#jsonxml)                                                                                                        |
| 11    | **get_gw_wl_wellmeasures()**     | Returns Groundwater Measurements                                                                                                                                                    | [groundwater/waterlevels/wellmeasurements](https://dwr.state.co.us/rest/get/help#Datasets&#GroundwaterLevelsController&#gettingstarted&#jsonxml)                                                                                             |
| 12    | **get_reference_tbl()**          | Returns reference tables list                                                                                                                                                       | [referencetables/](https://dwr.state.co.us/rest/get/help#Datasets&#ReferenceTablesController&#gettingstarted&#jsonxml)                                                                                                                       |
| 13    | **get_sw_stations()**            | Returns Surface Water Station info                                                                                                                                                  | [surfacewater/surfacewaterstations](https://dwr.state.co.us/rest/get/help#Datasets&#SurfaceWaterController&#gettingstarted&#jsonxml)                                                                                                         |
| 14    | **get_sw_ts()**                  | Returns Surface Water Time Series                                                                                                                                                   | [surfacewater/surfacewaterts](https://dwr.state.co.us/rest/get/help#Datasets&#SurfaceWaterController&#gettingstarted&#jsonxml)                                                                                                               |
| 15    | **get_telemetry_stations()**     | Returns telemetry stations and their most recent parameter reading                                                                                                                  | [telemetrystations/telemetrystation](https://dwr.state.co.us/rest/get/help#Datasets&#TelemetryStationsController&#gettingstarted&#jsonxml)                                                                                                   |
| 16    | **get_telemetry_ts()**           | Returns telemetry time series data (raw, hour, day)                                                                                                                                 | [telemetrystations/telemetrytimeseries](https://dwr.state.co.us/rest/get/help#Datasets&#TelemetryStationsController&#gettingstarted&#jsonxml)                                                                                                |
| 17    | **get_water_rights_netamount()** | Returns current status of a water right based on all of its court decreed actions                                                                                                   | [waterrights/netamount](https://dwr.state.co.us/rest/get/help#Datasets&#WaterRightsController&#gettingstarted&#jsonxml)                                                                                                                      |
| 18    | **get_water_rights_trans()**     | Returns court decreed actions that affect amount and use(s) that can be used by each water right                                                                                    | [waterrights/transaction](https://dwr.state.co.us/rest/get/help#Datasets&#WaterRightsController&#gettingstarted&#jsonxml)                                                                                                                    |
| 19    | **get_call_analysis_wdid()**     | Performs a call analysis that returns a time series showing the percentage of each day that the specified WDID and priority was out of priority and the downstream call in priority | [analysisservices/callanalysisbywdid](https://dwr.state.co.us/rest/get/help#Datasets&#AnalysisServicesController&#gettingstarted&#jsonxml)                                                                                                   |
| 20    | **get_source_route_framework()** | Returns the DWR source route framework reference table for the criteria specified                                                                                                   | [analysisservices/watersourcerouteframework](https://dwr.state.co.us/rest/get/help#Datasets&#AnalysisServicesController&#gettingstarted&#jsonxml)                                                                                            |
| 21    | **get_parceluse_ts()**           | Returns list of Parcel Use Time Series                                                                                                                                              | [structures/parcelusets](https://dwr.state.co.us/rest/get/help#Datasets&#ParcelUseTSController&#gettingstarted&#jsonxml)                                                                                                                     |

#### **Example: Explore endpoint**

To check out the various CDSS API endpoint, **`cdssr`** comes packaged
with an **`api_endpoint`** table which details endpoint names,
descriptions, and relevant URLs.

``` r
dplyr::tibble(cdssr::api_endpoints)
#> # A tibble: 61 × 5
#>    resource                                      descrip…¹ endpo…² url   endpo…³
#>    <chr>                                         <chr>     <chr>   <chr> <chr>  
#>  1 Active Administrative Calls Generator         Returns … api/v2… http… https:…
#>  2 Historical Administrative Calls Generator     Returns … api/v2… http… https:…
#>  3 Water Source Route Analysis Info Generator    Returns … api/v2… http… https:…
#>  4 Water Source Route Framework Info Generator   Returns … api/v2… http… https:…
#>  5 Call Analysis Structure Info Generator        Performs… api/v2… http… https:…
#>  6 Call Analysis Stream Mile Info Generator      Performs… api/v2… http… https:…
#>  7 Climate Stations Generator                    Returns … api/v2… http… https:…
#>  8 Climate Station Data Types Generator          Returns … api/v2… http… https:…
#>  9 Climate Station Time Series - Day Generator   Returns … api/v2… http… https:…
#> 10 Climate Station Time Series - Month Generator Returns … api/v2… http… https:…
#> # … with 51 more rows, and abbreviated variable names ¹​description, ²​endpoint,
#> #   ³​endpoint_url
```

<br>

#### **Example: View meta data**

**`cdssr`** also comes packaged with a **`resource_meta`** dataset which
provides meta data for the data retrieved by **`cdssr`** (via the CDSS
REST API)

``` r
dplyr::tibble(cdssr::resource_meta)
#> # A tibble: 1,031 × 5
#>    endpoint                          name                  descr…¹ type  endpo…²
#>    <chr>                             <chr>                 <chr>   <chr> <chr>  
#>  1 api/v2/administrativecalls/active boundingStructureLat… Latitu… deci… https:…
#>  2 api/v2/administrativecalls/active boundingStructureLon… Longit… deci… https:…
#>  3 api/v2/administrativecalls/active boundingStructureName Struct… stri… https:…
#>  4 api/v2/administrativecalls/active boundingWdid          WDID o… stri… https:…
#>  5 api/v2/administrativecalls/active callNumber            Unique… inte… https:…
#>  6 api/v2/administrativecalls/active callType              The ty… stri… https:…
#>  7 api/v2/administrativecalls/active dateTimeReleased      Date a… date  https:…
#>  8 api/v2/administrativecalls/active dateTimeSet           Date a… date  https:…
#>  9 api/v2/administrativecalls/active division              DWR Wa… inte… https:…
#> 10 api/v2/administrativecalls/active locationStructureLat… Latitu… deci… https:…
#> # … with 1,021 more rows, and abbreviated variable names ¹​description,
#> #   ²​endpoint_url
```

<br> <br> <br>

## **Identify query inputs using reference tables**

The **`get_reference_tbl()`** function will return tables that makes it
easier to know what information should be supplied to the data retrieval
functions in **`cdssr`**. For more information on the exact reference
tables click
[here](https://dwr.state.co.us/rest/get/help#Datasets&#ReferenceTablesController&#gettingstarted&#jsonxml).

Let’s locate the parameters available at telemetry stations.

``` r
# available parameters for telemetry stations
telemetry_params <- cdssr::get_reference_tbl(
  table_name = "telemetryparams"
  )
#> Retrieving telemetry parameter reference table
```

    #> # A tibble: 46 × 1
    #>    parameter
    #>    <chr>    
    #>  1 AIRTEMP  
    #>  2 BAR_P    
    #>  3 BATTERY  
    #>  4 COND     
    #>  5 D1       
    #>  6 D2       
    #>  7 DISCHRG  
    #>  8 DISCHRG1 
    #>  9 DISCHRG2 
    #> 10 DISCHRG3 
    #> # … with 36 more rows

<br> <br>

## **Locate structures**

**`cdssr`** provides functions for locating
structures/stations/wells/sites by providing a spatial extent, water
district, division, county, designated basin, or management district to
the functions in the table below. Site data can also be retrieved by
providing the site specific abbreviations, GNIS IDs, USGS IDs, WDIDs, or
Well IDs.

| **-** | **Function**                 | **Description**                                                    | **Endpoint**                                                                                                                                                                                                                        |
|-------|------------------------------|--------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1     | **get_structures()**         | Returns list of administrative structures                          | [structures](https://dwr.state.co.us/rest/get/help#Datasets&#StructuresController&#gettingstarted&#jsonxml)                                                                                                                         |
| 2     | **get_climate_stations()**   | Returns Climate Stations                                           | [climatedata/climatestations](https://dwr.state.co.us/rest/get/help#Datasets&#ClimateStationsController&https://www.ncdc.noaa.gov/cdo-web/webservices&https://www.northernwater.org/our-data/weather-data&#gettingstarted&#jsonxml) |
| 3     | **get_gw_gplogs_wells()**    | Returns Groundwater GeophysicalLogsWell from filters               | [groundwater/geophysicallogs/](https://dwr.state.co.us/rest/get/help#Datasets&#GroundwaterGeophysicalLogsController&#gettingstarted&#jsonxml)                                                                                       |
| 4     | **get_gw_wl_wells()**        | Returns WaterLevelsWell from filters                               | [groundwater/waterlevels/wells](https://dwr.state.co.us/rest/get/help#Datasets&#GroundwaterLevelsController&#gettingstarted&#jsonxml)                                                                                               |
| 5     | **get_sw_stations()**        | Returns Surface Water Station info                                 | [surfacewater/surfacewaterstations](https://dwr.state.co.us/rest/get/help#Datasets&#SurfaceWaterController&#gettingstarted&#jsonxml)                                                                                                |
| 6     | **get_telemetry_stations()** | Returns telemetry stations and their most recent parameter reading | [telemetrystations/telemetrystation](https://dwr.state.co.us/rest/get/help#Datasets&#TelemetryStationsController&#gettingstarted&#jsonxml)                                                                                          |

<br>

#### **Example: Locating telemetry stations by county**

``` r
# identify telemetry stations in Boulder county
stations <- cdssr::get_telemetry_stations(
  county = "Boulder"
  )
#> Retrieving telemetry station data
```

    #> # A tibble: 110 × 35
    #>    divis…¹ water…² county stati…³ data_…⁴ data_…⁵ water…⁶ gnis_id strea…⁷ abbrev
    #>      <int>   <int> <chr>  <chr>   <chr>   <chr>   <chr>   <chr>     <dbl> <chr> 
    #>  1       1       5 BOULD… "CLOUG… NCWCD   Northe… SAINT … 002050…   NA    05005…
    #>  2       1       6 BOULD… "ANDER… DWR     Co. Di… BOULDE… 001783…   23.6  ANDDI…
    #>  3       1       6 BOULD… "ANDRE… DWR     Co. Di… SOUTH … 001809…    1.5  ANFDI…
    #>  4       1       6 BOULD… "BASEL… DWR     Co. Di… BOULDE… 001783…   19.2  BASOU…
    #>  5       1       6 BOULD… "BOULD… NCWCD   Northe… BOULDE… 001783…   15.5  BCSCB…
    #>  6       1       5 BOULD… "BOULD… NCWCD   Northe… LEFT H… 001782…    8.66 BFCIN…
    #>  7       1       5 BOULD… "BOULD… NCWCD   Northe… SAINT … 002050…   31.7  BFCLY…
    #>  8       1       5 BOULD… "BOULD… NCWCD   Northe… SAINT … 002050…   31.7  BFCLY…
    #>  9       1       5 BOULD… "LYONS… DWR     Co. Di… SOUTH … 001782…    0.03 BHNPR…
    #> 10       1       6 BOULD… "BOULD… DWR     Co. Di… BOULDE… 001783…   22.3  BLDLH…
    #> # … with 100 more rows, 25 more variables: usgs_station_id <chr>,
    #> #   station_status <chr>, station_type <chr>, structure_type <chr>,
    #> #   meas_date_time <dttm>, parameter <chr>, stage <dbl>, meas_value <dbl>,
    #> #   units <chr>, flag_a <chr>, flag_b <chr>, contr_area <dbl>,
    #> #   drain_area <dbl>, huc10 <chr>, utm_x <dbl>, utm_y <dbl>, latitude <dbl>,
    #> #   longitude <dbl>, location_accuracy <chr>, wdid <chr>, modified <chr>,
    #> #   more_information <chr>, station_por_start <dttm>, station_por_end <dttm>, …

![](https://cdsspy-images.s3.us-west-1.amazonaws.com/county_telem_stations2.png)

<br>

#### **Example: Locating telemetry stations around a point**

``` r
# identify telemetry stations 10 miles around a point
stations <- cdssr::get_telemetry_stations(
  aoi    = c(-105.358164, 40.092608),
  radius = 10
  )
```

    #> # A tibble: 110 × 35
    #>    divis…¹ water…² county stati…³ data_…⁴ data_…⁵ water…⁶ gnis_id strea…⁷ abbrev
    #>      <int>   <int> <chr>  <chr>   <chr>   <chr>   <chr>   <chr>     <dbl> <chr> 
    #>  1       1       5 BOULD… "CLOUG… NCWCD   Northe… SAINT … 002050…   NA    05005…
    #>  2       1       6 BOULD… "ANDER… DWR     Co. Di… BOULDE… 001783…   23.6  ANDDI…
    #>  3       1       6 BOULD… "ANDRE… DWR     Co. Di… SOUTH … 001809…    1.5  ANFDI…
    #>  4       1       6 BOULD… "BASEL… DWR     Co. Di… BOULDE… 001783…   19.2  BASOU…
    #>  5       1       6 BOULD… "BOULD… NCWCD   Northe… BOULDE… 001783…   15.5  BCSCB…
    #>  6       1       5 BOULD… "BOULD… NCWCD   Northe… LEFT H… 001782…    8.66 BFCIN…
    #>  7       1       5 BOULD… "BOULD… NCWCD   Northe… SAINT … 002050…   31.7  BFCLY…
    #>  8       1       5 BOULD… "BOULD… NCWCD   Northe… SAINT … 002050…   31.7  BFCLY…
    #>  9       1       5 BOULD… "LYONS… DWR     Co. Di… SOUTH … 001782…    0.03 BHNPR…
    #> 10       1       6 BOULD… "BOULD… DWR     Co. Di… BOULDE… 001783…   22.3  BLDLH…
    #> # … with 100 more rows, 25 more variables: usgs_station_id <chr>,
    #> #   station_status <chr>, station_type <chr>, structure_type <chr>,
    #> #   meas_date_time <dttm>, parameter <chr>, stage <dbl>, meas_value <dbl>,
    #> #   units <chr>, flag_a <chr>, flag_b <chr>, contr_area <dbl>,
    #> #   drain_area <dbl>, huc10 <chr>, utm_x <dbl>, utm_y <dbl>, latitude <dbl>,
    #> #   longitude <dbl>, location_accuracy <chr>, wdid <chr>, modified <chr>,
    #> #   more_information <chr>, station_por_start <dttm>, station_por_end <dttm>, …

![](https://cdsspy-images.s3.us-west-1.amazonaws.com/poi_telem_stations.png)

<br>

#### **Example: Locating telemetry stations within a spatial extent**

A masking operation is performed when a location search is done using a
**polygon**. This ensures that the function only returns points that are
***within*** the given polygon.

``` r
# load AOI to retrieve county polygons
library(AOI)

# identify telemetry stations 15 miles around the centroid of a polygon
stations <- cdssr::get_telemetry_stations(
  aoi    = AOI::aoi_get(county = "Boulder", state = "CO"),
  radius = 15
  )
#> Retrieving telemetry station data
#> Location search: 
#> Latitude: 40.0843975
#> Longitude: -105.345242774525
#> Radius (miles): 15
```

    #> # A tibble: 108 × 35
    #>    divis…¹ water…² county stati…³ data_…⁴ data_…⁵ water…⁶ gnis_id strea…⁷ abbrev
    #>      <int>   <int> <chr>  <chr>   <chr>   <chr>   <chr>   <chr>     <dbl> <chr> 
    #>  1       1       5 BOULD… "CLOUG… NCWCD   Northe… SAINT … 002050…   NA    05005…
    #>  2       1       6 BOULD… "ANDER… DWR     Co. Di… BOULDE… 001783…   23.6  ANDDI…
    #>  3       1       6 BOULD… "ANDRE… DWR     Co. Di… SOUTH … 001809…    1.5  ANFDI…
    #>  4       1       6 BOULD… "BASEL… DWR     Co. Di… BOULDE… 001783…   19.2  BASOU…
    #>  5       1       6 BOULD… "BOULD… NCWCD   Northe… BOULDE… 001783…   15.5  BCSCB…
    #>  6       1       5 BOULD… "BOULD… NCWCD   Northe… LEFT H… 001782…    8.66 BFCIN…
    #>  7       1       5 BOULD… "BOULD… NCWCD   Northe… SAINT … 002050…   31.7  BFCLY…
    #>  8       1       5 BOULD… "BOULD… NCWCD   Northe… SAINT … 002050…   31.7  BFCLY…
    #>  9       1       5 BOULD… "LYONS… DWR     Co. Di… SOUTH … 001782…    0.03 BHNPR…
    #> 10       1       6 BOULD… "BOULD… DWR     Co. Di… BOULDE… 001783…   22.3  BLDLH…
    #> # … with 98 more rows, 25 more variables: usgs_station_id <chr>,
    #> #   station_status <chr>, station_type <chr>, structure_type <chr>,
    #> #   meas_date_time <dttm>, parameter <chr>, stage <dbl>, meas_value <dbl>,
    #> #   units <chr>, flag_a <chr>, flag_b <chr>, contr_area <dbl>,
    #> #   drain_area <dbl>, huc10 <chr>, utm_x <dbl>, utm_y <dbl>, latitude <dbl>,
    #> #   longitude <dbl>, location_accuracy <chr>, wdid <chr>, modified <chr>,
    #> #   more_information <chr>, station_por_start <dttm>, station_por_end <dttm>, …

<br>

This gif highlights the masking process that happens when the **`aoi`**
argument is given a **polygon**

![](https://cdsspy-images.s3.us-west-1.amazonaws.com/boulder_telem_stations_poly2.gif)

<br> <br> <br>

## **Retrieve time series data**

The functions in the table below retrieve time series data from the
various time series related CDSS API endpoints.

| **-** | **Function**                   | **Description**                                         | **Endpoint**                                                                                                                                                                                                                         |
|-------|--------------------------------|---------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1     | **get_structures_divrec_ts()** | Returns list of diversion/release records based on WDID | [structures/divrec/divrec](https://dwr.state.co.us/rest/get/help#Datasets&#DiversionRecordsController&https://dnrweblink.state.co.us/dwr/ElectronicFile.aspx?docid=3600965&dbid=0&#gettingstarted&#jsonxml)                          |
| 2     | **get_structures_stage_ts()**  | Returns list of stage/volume records based on WDID      | [structures/divrec/stagevolume](https://dwr.state.co.us/rest/get/help#Datasets&#DiversionRecordsController&https://dnrweblink.state.co.us/dwr/ElectronicFile.aspx?docid=3600965&dbid=0&#gettingstarted&#jsonxml)                     |
| 3     | **get_climate_ts()**           | Returns Climate Station Time Series (day, month, year)  | [climatedata/climatestationts](https://dwr.state.co.us/rest/get/help#Datasets&#ClimateStationsController&https://www.ncdc.noaa.gov/cdo-web/webservices&https://www.northernwater.org/our-data/weather-data&#gettingstarted&#jsonxml) |
| 4     | **get_gw_wl_wellmeasures()**   | Returns Groundwater Measurements                        | [groundwater/waterlevels/wellmeasurements](https://dwr.state.co.us/rest/get/help#Datasets&#GroundwaterLevelsController&#gettingstarted&#jsonxml)                                                                                     |
| 5     | **get_sw_ts()**                | Returns Surface Water Time Series                       | [surfacewater/surfacewaterts](https://dwr.state.co.us/rest/get/help#Datasets&#SurfaceWaterController&#gettingstarted&#jsonxml)                                                                                                       |
| 6     | **get_telemetry_ts()**         | Returns telemetry time series data (raw, hour, day)     | [telemetrystations/telemetrytimeseries](https://dwr.state.co.us/rest/get/help#Datasets&#TelemetryStationsController&#gettingstarted&#jsonxml)                                                                                        |
| 7     | **get_parceluse_ts()**         | Returns list of Parcel Use Time Series                  | [structures/parcelusets](https://dwr.state.co.us/rest/get/help#Datasets&#ParcelUseTSController&#gettingstarted&#jsonxml)                                                                                                             |

<br>

#### **Example: Daily discharge at a telemetry station**

We can then take a station abbreviations from the
**`get_telemetry_stations()`** call, a parameter from the
**`get_reference_tbl()`** call, and use this information as inputs into
the **`get_telemetry_ts()`** function.

<br>

The function below returns a dataframe of daily discharge for the
“ANDDITCO” site between 2015-2022.

``` r
# Daily discharge at "ANDDITCO" telemetry station
discharge_ts <- cdssr::get_telemetry_ts(
                      abbrev              = "ANDDITCO",     # Site abbreviation
                      parameter           = "DISCHRG",      # Desired parameter
                      start_date          = "2015-01-01",   # Starting date
                      end_date            = "2022-01-01",   # Ending date
                      timescale           = "day"           # select daily timescale 
                               )
#> Retrieving telemetry station time series data (day - DISCHRG)
```

    #> # A tibble: 572 × 11
    #>    abbrev   parameter meas_date    meas_…¹ meas_…² flag_a flag_b meas_…³ modif…⁴
    #>    <chr>    <chr>     <chr>          <dbl> <chr>   <chr>  <chr>    <int> <chr>  
    #>  1 ANDDITCO DISCHRG   2020-05-07 …    3.05 cfs     O      <NA>        33 2020-0…
    #>  2 ANDDITCO DISCHRG   2020-05-08 …    3.04 cfs     O      <NA>        96 2020-0…
    #>  3 ANDDITCO DISCHRG   2020-05-09 …    2.98 cfs     O      <NA>        96 2020-0…
    #>  4 ANDDITCO DISCHRG   2020-05-10 …    2.95 cfs     O      <NA>        96 2020-0…
    #>  5 ANDDITCO DISCHRG   2020-05-11 …    2.95 cfs     O      <NA>        96 2020-0…
    #>  6 ANDDITCO DISCHRG   2020-05-12 …    2.95 cfs     O      <NA>        96 2020-0…
    #>  7 ANDDITCO DISCHRG   2020-05-13 …    2.95 cfs     O      <NA>        96 2020-0…
    #>  8 ANDDITCO DISCHRG   2020-05-14 …    2.95 cfs     O      <NA>         9 2020-0…
    #>  9 ANDDITCO DISCHRG   2020-05-15 …    2.95 cfs     O      <NA>        63 2020-0…
    #> 10 ANDDITCO DISCHRG   2020-05-16 …    2.95 cfs     O      <NA>        96 2020-0…
    #> # … with 562 more rows, 2 more variables: datetime <dttm>, timescale <chr>, and
    #> #   abbreviated variable names ¹​meas_value, ²​meas_unit, ³​meas_count, ⁴​modified

![](https://cdsspy-images.s3.us-west-1.amazonaws.com/discharge_timeseries_plot2.png)

<br> <br>

#### **Example: Retrieve Diversion records for multiple structures**

Some of the CDSS API endpoints allow users to request data from multiple
structures if you provide a list of IDs. If we want to get diversion
data from multiple structure locations, we’ll need to get a list of
WDIDs. We can get a list WDIDs within a given area by:

1.  Executing a spatial search using **`get_structures()`**
2.  Selecting the WDIDs of interest from the search results
3.  Providing the WDIDs as a vector to **`get_structures_divrec_ts()`**

**Note:** Data availability can vary between structures (i.e. Missing
data, not all structures have every data type/temporal resolution
available, etc.)

``` r
# 1. Executing a spatial search
structures <- cdssr::get_structures(
  aoi    = c(-105.3578, 40.09244),
  radius = 5
)
#> Retreiving administrative structures
#> Location search: 
#> Latitude: 40.09244
#> Longitude: -105.3578
#> Radius (miles): 5

# 2. Selecting the WDID's of interest from our search results
ditch_wdids <-
  structures %>%
    dplyr::filter(ciu_code == "A", structure_type == "DITCH") %>%
  .$wdid

# 3. Providing the WDID's as a vector to get_structures_divrec_ts()
diversion_rec <-
  cdssr::get_structures_divrec_ts(
                        wdid           = ditch_wdids,
                        wc_identifier  = "diversion",
                        start_date     = "1990-01-01",
                        end_date       = "2022-01-01",
                        timescale      = "month"
                        )
#> Retrieving monthly diversion data
```

    #> # A tibble: 495 × 12
    #>    wdid  water…¹ wc_id…² meas_…³ meas_…⁴ data_…⁵ data_…⁶ meas_…⁷ obs_c…⁸ appro…⁹
    #>    <chr>   <int> <chr>   <chr>     <int> <chr>     <dbl> <chr>   <chr>   <chr>  
    #>  1 0500…  1.05e7 050056… Daily        18 1992-04    165. ACFT    U       Approv…
    #>  2 0500…  1.05e7 050056… Daily        31 1992-05    745. ACFT    U       Approv…
    #>  3 0500…  1.05e7 050056… Daily        30 1992-06    430. ACFT    U       Approv…
    #>  4 0500…  1.05e7 050056… Daily        31 1992-07    372. ACFT    U       Approv…
    #>  5 0500…  1.05e7 050056… Daily        31 1992-08    611. ACFT    U       Approv…
    #>  6 0500…  1.05e7 050056… Daily        29 1992-09    315. ACFT    U       Approv…
    #>  7 0500…  1.05e7 050056… Daily        29 1992-10    120. ACFT    U       Approv…
    #>  8 0500…  1.05e7 050056… Daily        25 2004-04    244. ACFT    U       Approv…
    #>  9 0500…  1.05e7 050056… Daily        40 2004-05    699. ACFT    U       Approv…
    #> 10 0500…  1.05e7 050056… Daily        25 2004-06    294. ACFT    U       Approv…
    #> # … with 485 more rows, 2 more variables: modified <chr>, datetime <date>, and
    #> #   abbreviated variable names ¹​water_class_num, ²​wc_identifier,
    #> #   ³​meas_interval, ⁴​meas_count, ⁵​data_meas_date, ⁶​data_value, ⁷​meas_units,
    #> #   ⁸​obs_code, ⁹​approval_status

![](https://cdsspy-images.s3.us-west-1.amazonaws.com/divrec_facet_plot.png)

<br> <br> <br>

## **Example: Groundwater well data**

#### Retrieve groundwater well data

The **`get_gw_()`** set of functions lets users make get requests to the
various CDSS API groundwater endpoints shown in the table below:

| **-** | **Function**                    | **Endpoint**                                                                                                                                     |
|-------|---------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| 1     | **get_gw_wl_wellmeasures()**    | [api/v2/groundwater/waterlevels/wellmeasurements](https://dwr.state.co.us/Rest/GET/Help/Api/GET-api-v2-groundwater-waterlevels-wellmeasurements) |
| 2     | **get_gw_wl_wells()**           | [api/v2/groundwater/waterlevels/wells](https://dwr.state.co.us/Rest/GET/Help/Api/GET-api-v2-groundwater-waterlevels-wells)                       |
| 3     | **get_gw_gplogs_wells()**       | [api/v2/groundwater/geophysicallogs/wells](https://dwr.state.co.us/Rest/GET/Help/Api/GET-api-v2-groundwater-geophysicallogs-wells)               |
| 4     | **get_gw_gplogs_geologpicks()** | [api/v2/groundwater/geophysicallogs/geoplogpicks](https://dwr.state.co.us/Rest/GET/Help/Api/GET-api-v2-groundwater-geophysicallogs-geoplogpicks) |

<br>

Here we will retrieve groundwater well measurement data for Well ID 1274
between 1990-2022.

``` r
# Request wellmeasurements endpoint (api/v2/groundwater/waterlevels/wellmeasurements)
well_measure <- cdssr::get_gw_wl_wellmeasures(
  wellid     = 1274,
  start_date = "1990-01-01",
  end_date   = "2022-01-01"
  )
#> Retrieving groundwater well measurements
```

    #> # A tibble: 1,469 × 18
    #>    well_id well_name      divis…¹ water…² county manag…³ desig…⁴ publi…⁵ measu…⁶
    #>      <int> <chr>            <int>   <int> <chr>  <lgl>   <lgl>   <chr>   <chr>  
    #>  1    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1990-0…
    #>  2    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1990-1…
    #>  3    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1991-0…
    #>  4    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1991-1…
    #>  5    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1992-0…
    #>  6    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1992-0…
    #>  7    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1992-1…
    #>  8    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1993-0…
    #>  9    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1993-1…
    #> 10    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1994-0…
    #> # … with 1,459 more rows, 9 more variables: depth_to_water <dbl>,
    #> #   measuring_point_above_land_surface <dbl>,
    #> #   depth_water_below_land_surface <dbl>, elevation_of_water <dbl>,
    #> #   delta <dbl>, data_source <chr>, published <chr>, modified <chr>,
    #> #   datetime <dttm>, and abbreviated variable names ¹​division, ²​water_district,
    #> #   ³​management_district, ⁴​designated_basin, ⁵​publication, ⁶​measurement_date

![](https://cdsspy-images.s3.us-west-1.amazonaws.com/gw_depth_to_water_plot2.png)
<br> <br> <br>
