
#' Attempt to geocode a string
#'
#' Enter a string and have latitude and longitude returned using the HERE API
#' @param search A string to search
#' @param App_id App_id to use the production HERE API. Get one here... http://developer.here.com/get-started. If left blank, will default to demo key with an unknown usage limit.
#' @param App_code App_code to use the production HERE API. Get one here... http://developer.here.com/get-started. If left blank, will default to demo key with an unknown usage limit.
#' @keywords geocode
#' @export
#' @examples
#' \dontrun{
#' geocodeHERE_simple("chicago")
#' geocodeHERE_simple("wrigley field chicago IL")
#' geocodeHERE_simple("233 S Wacker Dr, Chicago, IL 60606")
#' }
#' geocodeHERE_simple
geocodeHERE_simple <- function(search, App_id="", App_code=""){
  if(!is.character(search)){stop("'search' must be a character string")}
  if(!is.character(App_id)){stop("'App_id' must be a character string")}
  if(!is.character(App_code)){stop("'App_code' must be a character string")}

  if(App_id=="" & App_code==""){
    App_id <- "DemoAppId01082013GAL"
    App_code <- "AJKnXv84fjrb0KIHawS0Tg"
    base_url <- "http://geocoder.cit.api.here.com/6.2/geocode."
  }else{
    base_url <- "http://geocoder.api.here.com/6.2/geocode."
  }

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


