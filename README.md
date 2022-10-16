
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cdssr <img src="man/figures/cdssr_logo.png" align="right" width="25%" />

<!-- badges: start -->

[![Dependencies](https://img.shields.io/badge/dependencies-9/02-orange?style=flat)](#)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit/)
<!-- badges: end -->

<div align="left">

<p align="left">
<a href="https://dwr.state.co.us/Tools"><strong>« CDSS »</strong></a>
<br /> <a href="https://dwr.state.co.us/Rest/GET/Help">CDSS API</a>
</p>

</div>

<hr>

The goal of `cdssr` is to provide functions that help R users to
navigate, explore, and make requests to the [CDSS REST API web
service](https://dwr.state.co.us/Rest/GET/Help).

The Colorado’s Decision Support Systems (CDSS) is a water management
system created and developed by the [Colorado Water Conservation Board
(CWCB)](https://cwcb.colorado.gov/) and the [Colorado Division of Water
Resources (DWR)](https://dwr.colorado.gov/).

Thank you to those at CWCB and DWR for providing an accessible and well
documented REST API!

<hr>

## Installation

You can install the development version of `cdssr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("anguswg-ucsb/cdssr")
```

## Browse avaliable endpoints

To browse all the available endpoints shown
[here](https://dwr.state.co.us/Rest/GET/Help) use `browse_api()`.

``` r
# Load package
library(cdssr)
```

``` r
# View the catalog of avaliable endpoints
catalog <- cdssr::browse_api()

catalog
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

## View the return fields

Use `preview_endpoint()` Inspect what fields will be returned for a
given endpoint by entering a `endpoint_url` (a column in the
`browse_api()` output pointing to the endpoints help page)

``` r
# Return expected data fields for a given endpoint
return_fields <- cdssr::preview_endpoint(endpoint_url = catalog$endpoint_url[3])

return_fields
#> # A tibble: 6 × 3
#>   name          description                     type          
#>   <chr>         <chr>                           <chr>         
#> 1 featureType   Feature Type                    string        
#> 2 gnisId        GNIS ID                         string        
#> 3 streamMile    Stream Mile number              decimal number
#> 4 structureName Structure Name                  string        
#> 5 structureType Structure Type                  string        
#> 6 wdid          DWR unique structure identifier string
```

<br>

# Example: Telemetry site data

## Identify station locations and information

We can use the `_stations` functions to identify what stations are
within a given AOI, water district, division, or county.

``` r
stations <- cdssr::get_telemetry_stations(
  water_district = 6
  )
#> Retrieving telemetry station data from CDSS API...

dplyr::tibble(stations)
#> # A tibble: 70 × 35
#>    divis…¹ water…² county stati…³ data_…⁴ data_…⁵ water…⁶ gnis_id strea…⁷ abbrev
#>      <int>   <int> <chr>  <chr>   <chr>   <chr>   <chr>   <chr>     <dbl> <chr> 
#>  1       1       6 BOULD… "ANDER… DWR     Co. Di… BOULDE… 001783…   23.6  ANDDI…
#>  2       1       6 BOULD… "BASEL… DWR     Co. Di… BOULDE… 001783…   19.2  BASOU…
#>  3       1       6 BOULD… "BOULD… NCWCD   Northe… BOULDE… 001783…   15.5  BCSCB…
#>  4       1       6 BOULD… "BOULD… DWR     Co. Di… BOULDE… 001783…   22.3  BLDLH…
#>  5       1       6 BOULD… "BUTTE… DWR     Co. Di… BOULDE… 001783…   18.6  BMLDI…
#>  6       1       6 BOULD… "BOULD… DWR     Co. Di… BOULDE… 001783…    9.52 BOC10…
#>  7       1       6 BOULD… "MIDDL… DWR     Co. Di… MIDDLE… 001785…    5.92 BOCBB…
#>  8       1       6 BOULD… "SOUTH… DWR     Co. Di… SOUTH … 001809…   16.5  BOCBG…
#>  9       1       6 BOULD… "BOULD… DWR     Co. Di… BOULDE… 001783…   22.3  BOCBR…
#> 10       1       6 BOULD… "SOUTH… DWR     Co. Di… SOUTH … 001809…   12.1  BOCEL…
#> # … with 60 more rows, 25 more variables: usgs_station_id <chr>,
#> #   station_status <chr>, station_type <chr>, structure_type <chr>,
#> #   meas_date_time <dttm>, parameter <chr>, stage <dbl>, meas_value <dbl>,
#> #   units <chr>, flag_a <chr>, flag_b <chr>, contr_area <dbl>,
#> #   drain_area <dbl>, huc10 <chr>, utm_x <dbl>, utm_y <dbl>, latitude <dbl>,
#> #   longitude <dbl>, location_accuracy <chr>, wdid <chr>, modified <chr>,
#> #   more_information <chr>, station_por_start <dttm>, station_por_end <dttm>, …
```

## Retrieve Reference Tables to help generate queries

The `get_reference_tbl()` function will return information that makes it
easier to know what information should be supplied to the data retrieval
functions in `cdssr`. For more information on the exact reference tables
click
[here](https://dwr.state.co.us/rest/get/help#Datasets&#ReferenceTablesController&#gettingstarted&#jsonxml).

Let’s locate the possible telemetry station parameter names that we can
use in a query.

``` r
# # Daily discharge at "CLAFTCCO" telemetry station
telemetry_params <- cdssr::get_reference_tbl(
  table_name = "telemetryparams"
  )
#> Retrieving telemetry parameter reference table from CDSS API...

dplyr::tibble(telemetry_params)
#> # A tibble: 45 × 1
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
#> # … with 35 more rows
```

## Retrieve Telemetry station timeseries data

Use `get_` function to make requests to the CDSS API and return the
results in a tidy dataframe.

We’ll use the station abbreviations from the `get_telemetry_stations`
call, a parameter from the `get_reference_tbl()` call, select a starting
and ending date and a temporal resolution.

``` r
# Daily discharge at "CLAFTCCO" telemetry station
discharge_ts <- cdssr::get_telemetry_ts(
                      abbrev              = stations$abbrev[1],
                      parameter           = telemetry_params$parameter[7],
                      start_date          = "2015-01-01",
                      end_date            = "2022-01-01",
                      timescale           = "day"
                               )
#> Downloading data from CDSS API...
#> Telemetry station abbreviation: ANDDITCO
#> Parameter: DISCHRG
#> Timescale: day

dplyr::tibble(discharge_ts)
#> # A tibble: 572 × 7
#>    abbrev   parameter date               value unit  datetime            times…¹
#>    <chr>    <chr>     <chr>              <dbl> <chr> <dttm>              <chr>  
#>  1 ANDDITCO DISCHRG   2020-05-07 00:00:…  3.05 cfs   2020-05-07 00:00:00 day    
#>  2 ANDDITCO DISCHRG   2020-05-08 00:00:…  3.04 cfs   2020-05-08 00:00:00 day    
#>  3 ANDDITCO DISCHRG   2020-05-09 00:00:…  2.98 cfs   2020-05-09 00:00:00 day    
#>  4 ANDDITCO DISCHRG   2020-05-10 00:00:…  2.95 cfs   2020-05-10 00:00:00 day    
#>  5 ANDDITCO DISCHRG   2020-05-11 00:00:…  2.95 cfs   2020-05-11 00:00:00 day    
#>  6 ANDDITCO DISCHRG   2020-05-12 00:00:…  2.95 cfs   2020-05-12 00:00:00 day    
#>  7 ANDDITCO DISCHRG   2020-05-13 00:00:…  2.95 cfs   2020-05-13 00:00:00 day    
#>  8 ANDDITCO DISCHRG   2020-05-14 00:00:…  2.95 cfs   2020-05-14 00:00:00 day    
#>  9 ANDDITCO DISCHRG   2020-05-15 00:00:…  2.95 cfs   2020-05-15 00:00:00 day    
#> 10 ANDDITCO DISCHRG   2020-05-16 00:00:…  2.95 cfs   2020-05-16 00:00:00 day    
#> # … with 562 more rows, and abbreviated variable name ¹​timescale
```

And a plot of the daily discharge…

``` r
# Plot daily discharge at "CLAFTCCO"
plot(discharge_ts$value~discharge_ts$datetime, type = "l")
```

<img src="man/figures/README-plot_ts-1.png" width="100%" style="display: block; margin: auto;" />

<br>

## Retrieve groundwater well data

The `get_groundwater()` function lets users make get requests to the
various CDSS API groundwater endpoints using the **type** parameter.

Groundwater endpoints:

-   [api/v2/groundwater/waterlevels/wellmeasurements](https://dwr.state.co.us/Rest/GET/Help/Api/GET-api-v2-groundwater-waterlevels-wellmeasurements)
-   [api/v2/groundwater/waterlevels/wells](https://dwr.state.co.us/Rest/GET/Help/Api/GET-api-v2-groundwater-waterlevels-wells)
-   [api/v2/groundwater/geophysicallogs/wells](https://dwr.state.co.us/Rest/GET/Help/Api/GET-api-v2-groundwater-geophysicallogs-wells)

``` r
# Use type = "wellmeasurements" to request wellmeasurements endpoint (api/v2/groundwater/waterlevels/wellmeasurements)
well_measure <- cdssr::get_groundwater(
  type    = "wellmeasurements",
  wellid  = 1274
  )
#> Downloading data from CDSS API...
#> Groundwater well measurements

dplyr::tibble(well_measure)
#> # A tibble: 1,567 × 18
#>    well_id well_name      divis…¹ water…² county manag…³ desig…⁴ publi…⁵ measu…⁶
#>      <int> <chr>            <int>   <int> <chr>  <lgl>   <lgl>   <chr>   <chr>  
#>  1    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1989-0…
#>  2    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1989-1…
#>  3    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1990-0…
#>  4    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1990-1…
#>  5    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1991-0…
#>  6    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1991-1…
#>  7    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1992-0…
#>  8    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1992-0…
#>  9    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1992-1…
#> 10    1274 LSP-020  03N6…       1       2 WELD   NA      NA      LOWER … 1993-0…
#> # … with 1,557 more rows, 9 more variables: depth_to_water <dbl>,
#> #   measuring_point_above_land_surface <dbl>,
#> #   depth_water_below_land_surface <dbl>, elevation_of_water <dbl>,
#> #   delta <dbl>, data_source <chr>, published <chr>, modified <chr>,
#> #   datetime <dttm>, and abbreviated variable names ¹​division, ²​water_district,
#> #   ³​management_district, ⁴​designated_basin, ⁵​publication, ⁶​measurement_date
```

And a plot of the depth to water over time…

``` r
# plot depth to water
plot(well_measure$depth_to_water~well_measure$datetime, type = "l")
```

<img src="man/figures/README-plot_gw-1.png" width="100%" style="display: block; margin: auto;" />

<br> <br>

> **More functions for more endpoints coming soon!**
