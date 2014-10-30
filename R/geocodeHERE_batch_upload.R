
#' Upload a file to Nokia HERE batch geocoding API
#'
#' Upload a file to Nokia HERE batch geocoding API
#' @param file_name Character string containing the path to the file to be uploaded
#' @param email_address Character string containing an email address. Nokia will email you here when the job is done.
#' @param App_id App_id to use the production HERE API. Get one here... http://developer.here.com/get-started. If left blank, will default to demo key with an unknown usage limit.
#' @param App_code App_code to use the production HERE API. Get one here... http://developer.here.com/get-started. If left blank, will default to demo key with an unknown usage limit.
#' @param quiet TRUE / FALSE indicating whether to write the POST information to the console
#' @keywords geocode batch
#' @export
#' @examples
#' \dontrun{
#' geocodeHERE_batch_upload(file_name = "example.txt", email_address = "youremail<at>domain.com")
#' }
#' geocodeHERE_batch_upload
geocodeHERE_batch_upload <- function(file_name, email_address, App_id="",
                                     App_code="", quiet=TRUE){
  if(!is.character(file_name)){stop("'file_name' must be a character string")}
  if(!is.character(email_address)){stop("'file_name' must be a character string")}
  if(!is.character(App_id)){stop("'App_id' must be a character string")}
  if(!is.character(App_code)){stop("'App_code' must be a character string")}
  if(!file.exists(file_name)){stop("File does not exist, check your path or working directory")}

  if(App_id=="" & App_code==""){
    App_id <- "DemoAppId01082013GAL"
    App_code <- "AJKnXv84fjrb0KIHawS0Tg"
    base_url <- "http://batch.geocoder.cit.api.here.com/6.2/jobs"
  }else{
    base_url <- "http://batch.geocoder.api.here.com/6.2/jobs"
  }

  v <- ifelse(quiet, httr::verbose(), NULL)

  bod <- paste(readLines(file_name, warn=F), collapse="\n")

  a <- httr::POST(base_url, encode="multipart",
            body=bod,
            query=list(
              action="run",
              mailto=email_address,
              maxresults="1",
              language="es-ES",
              header="true",
              indelim="|",
              outdelim="|",
              outcols="displayLatitude,displayLongitude,houseNumber,street,district,city,postalCode,county,state,country,matchLevel,relevance", # i shortened this for the example
              outputCombined="false",
              app_code=App_code,
              app_id=App_id),
            v)
  response <- httr::content(a)

  if(length(response$Response) > 0){
    request_id <- response$Response$MetaInfo$RequestId
    ret <- request_id
  }else{
    stop(paste("ERROR: ", response))
  }

  return(ret)
}
