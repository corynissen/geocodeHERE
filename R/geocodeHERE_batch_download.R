
#' Download the result of a batch geocoding job
#'
#' Download the result of a batch geocoding job
#' @param request_id Character string containing a request_id. This is returned from geocodeHERE_batch_upload(...)
#' @param full_list TRUE / FALSE indicating whether to return the full response from Nokia HERE or just the "Status" portion of the response
#' @param App_id App_id to use the production HERE API. Get one here... http://developer.here.com/get-started. If left blank, will default to demo key with an unknown usage limit.
#' @param App_code App_code to use the production HERE API. Get one here... http://developer.here.com/get-started. If left blank, will default to demo key with an unknown usage limit.
#' @keywords geocode batch
#' @export
#' @examples
#' \dontrun{
#' addresses <- chicago_landmarks[,"Address"]
#' addresses <- paste(addresses, "chicago IL")
#' addresses_df <- data.frame(id=1:length(addresses), addresses=addresses)
#' address_str <- df_to_string(addresses_df)
#' request_id <- geocodeHERE_batch_upload(address_string = address_str, email_address = "youremail<at>domain.com")
#' geocodeHERE_batch_status(request_id)
#' result_data_path <- geocodeHERE_batch_download(request_id)
#' geocode_data <- geocodeHERE_batch_final_data(result_data_path)
#' addresses_df <- merge(addresses_df, geocode_data, by.x="id", by.y="recId", all.x=T)#' }
#' geocodeHERE_batch_download
geocodeHERE_batch_download <- function(request_id="", download_path=getwd(),
                                       App_id="", App_code=""){
  if(!is.character(request_id)){stop("'request_id' must be a character string")}
  if(request_id==""){stop("'request_id' must be have a value")}
  if(!is.character(download_path)){stop("'download_path' must be a character string")}
  if(!file.exists(download_path)){stop("'download_path' does not exist")}
  if(geocodeHERE_batch_status(request_id) != "completed"){
    stop("Batch geocoding is not completed yet")}

  if(App_id=="" & App_code==""){
    App_id <- "DemoAppId01082013GAL"
    App_code <- "AJKnXv84fjrb0KIHawS0Tg"
    base_url <- "http://batch.geocoder.cit.api.here.com/6.2/jobs"
  }else{
    base_url <- "http://batch.geocoder.api.here.com/6.2/jobs"
  }

  download_url <- paste0(base_url, "/",
                         request_id,
                        "/all",
                        "?app_id=", App_id,
                        "&app_code=", App_code)
  file_path <- paste0(download_path, "/", request_id, ".zip")
  a <- httr::GET(download_url, httr::write_disk(file_path, overwrite=TRUE))
  response <- httr::content(a)

  if(is.list(response)){
    stop(paste("ERROR: ", response$Details))
  }else{
    ret <- file_path
  }

  return(ret)
}
