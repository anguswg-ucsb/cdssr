## code to prepare `resource_meta` dataset goes here
#' @title Meta data for CDSS REST API resources
#' @description Dataframe containing meta data for the data returned by CDSS REST API endpoints
#' @export

# get a dataframe with all the API endpoint URLs
api_catalog <- browse_api()

# loop through each endpoint and retrieve meta data, then bind all the dataframes together
resource_meta <- lapply(1:nrow(api_catalog), function(i) {

  get_resource_meta(
    endpoint_url  = api_catalog$endpoint_url[i],
    endpoint_path = api_catalog$endpoint[i]
  )

}) %>%
  do.call(rbind, .)

usethis::use_data(resource_meta, overwrite = TRUE)

