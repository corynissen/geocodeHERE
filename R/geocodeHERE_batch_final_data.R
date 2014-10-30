
#' Read in result geocoded data
#'
#' Read in the data and format nicely
#' @param request_id Character string containing a request_id. This is returned from geocodeHERE_batch_upload(...)
#' @param full_list TRUE / FALSE indicating whether to return the full response from Nokia HERE or just the "Status" portion of the response
#' @param App_id App_id to use the production HERE API. Get one here... http://developer.here.com/get-started. If left blank, will default to demo key with an unknown usage limit.
#' @param App_code App_code to use the production HERE API. Get one here... http://developer.here.com/get-started. If left blank, will default to demo key with an unknown usage limit.
#' @keywords geocode batch
#' @export
#' @examples
#' \dontrun{
#' #' data(chicago_landmarks)
#' addresses <- chicago_landmarks[,"Address"]
#' addresses <- paste(addresses, "chicago IL")
#' addresses_df <- data.frame(id=1:length(addresses), addresses=addresses)
#' address_str <- df_to_string(addresses_df)
#' request_id <- geocodeHERE_batch_upload(address_string = address_str, email_address = "youremail<at>domain.com")
#' geocodeHERE_batch_status(request_id)
#' result_data_path <- geocodeHERE_batch_download(request_id)
#' geocode_data <- geocodeHERE_batch_final_data(result_data_path)
#' addresses_df <- merge(addresses_df, geocode_data, by.x="id", by.y="recId", all.x=T)
#'
#' }
#' geocodeHERE_batch_final_data
geocodeHERE_batch_final_data <- function(data_path){
  if(!is.character(data_path)){stop("'data_path' must be a character string")}
  if(!file.exists(data_path)){stop(paste0(data_path ," doesn't exist"))}

  extract_path <- substring(data_path, 1, nchar(data_path) - 4)
  unzip(data_path, exdir = extract_path)

  result_files <- list.files(extract_path)

  good_geocodes_file <- result_files[substring(result_files,
                                       nchar(result_files) - 6,
                                       nchar(result_files)) == "out.txt"]
  good_geocodes_path <- paste0(extract_path, "/", good_geocodes_file)

  if(!file.exists(good_geocodes_path)){stop("can't find result geocode file")}

  good_geocodes_data <- read.delim(good_geocodes_path, stringsAsFactors=F,
                                   sep="|")
  return(good_geocodes_data)
}
