
get_IDs <- function(file_name="R/nokia_HERE.txt"){
  ids <- scan(file_name, what="character", quiet=T)
  return(list(App_id=ids[2], App_code=ids[4]))
}

#' Attempt to geocode a string
#'
#' Enter a string and have latitude and longitude returned using the HERE API
#' @param search A string to search
#' @param format json or xml. Defaults to json.
#' @param ids The output from the get_IDs function. This contains the App_id and App_code required for authentication by the HERE API
#' @keywords geocode
#' @export
#' @examples
#' \dontrun{
#' ids = get_IDs(file_name="~/nokia_HERE.txt")
#' geocodeHERE_simple("chicago")
#' geocodeHERE_simple("wrigley field chicago IL")
#' geocodeHERE_simple("233 S Wacker Dr, Chicago, IL 60606")
#' }
#' geocodeHERE_simple
geocodeHERE_simple <- function(search, format="json", ids=ids){
  # get the lat lon for somewhere...
  #ids <- get_IDs()
  base_url <- "http://geocoder.api.here.com/6.2/geocode."
  search <- RCurl::curlEscape(search)
  final_url <- paste0(base_url, format, "?app_id=", ids$App_id, "&app_code=",
                      ids$App_code, "&searchtext=", search)

  response <- RCurl::getURL(final_url)
  response_parsed <- RJSONIO::fromJSON(response)
  if(length(response_parsed$Response$View) > 0){
    ret <- response_parsed$Response$View[[1]]$Result[[1]]$Location$DisplayPosition
  }else{
    ret <- NA
  }
  return(ret)
}

#ids <- get_IDs()
#geocodeHERE_simple("the bean chicago", ids=ids)
