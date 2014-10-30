
#' Check the status on a batch geocoding job
#'
#' Check the status on a batch geocoding job
#' @param request_id Character string containing a request_id. This is returned from geocodeHERE_batch_upload(...)
#' @param full_list TRUE / FALSE indicating whether to return the full response from Nokia HERE or just the "Status" portion of the response
#' @param App_id App_id to use the production HERE API. Get one here... http://developer.here.com/get-started. If left blank, will default to demo key with an unknown usage limit.
#' @param App_code App_code to use the production HERE API. Get one here... http://developer.here.com/get-started. If left blank, will default to demo key with an unknown usage limit.
#' @keywords geocode batch
#' @export
#' @examples
#' \dontrun{
#' request_id <- geocodeHERE_batch_upload(file_name = "example.txt", email_address = "youremail<at>domain.com")
#' geocodeHERE_batch_status(request_id)
#' }
#' geocodeHERE_batch_status
geocodeHERE_batch_status <- function(request_id="", full_list=FALSE, App_id="", App_code=""){
  if(!is.character(request_id)){stop("'request_id' must be a character string")}
  if(request_id==""){stop("'request_id' must be have a value")}
  if(!is.logical(full_list)){stop("'full_list' must be a logical value")}

  if(App_id=="" & App_code==""){
    App_id <- "DemoAppId01082013GAL"
    App_code <- "AJKnXv84fjrb0KIHawS0Tg"
    base_url <- "http://batch.geocoder.cit.api.here.com/6.2/jobs"
  }else{
    base_url <- "http://batch.geocoder.api.here.com/6.2/jobs"
  }

  status_url <- paste0(base_url, "/",
                       request_id,
                       "?action=status",
                       "&app_id=", App_id,
                       "&app_code=", App_code)
  a <- httr::GET(status_url)
  response <- httr::content(a)

  if(length(response$Response) > 0){
    request_id <- response$Response$Status
    if(full_list){
      ret <- response
    }else{
      ret <- request_id
    }
  }else{
    stop(paste("ERROR: ", response))
  }

  return(ret)
}
