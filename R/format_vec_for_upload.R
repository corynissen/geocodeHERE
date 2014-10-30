
#' Format a vector of addresses for upload
#'
#' Format a vector of addresses for upload
#' @param addresses A vector of character strings to be geocoded
#' @keywords geocode batch
#' @export
#' @examples
#' \dontrun{
#' data(chicago_landmarks)
#' addresses <- chicago_landmarks[,"Address"]
#' addresses <- paste(addresses, "chicago IL")
#' big_string <- format_vec_for_upload(addresses)
#' }
#' format_vec_for_upload
format_vec_for_upload <- function(addresses){
  if(length(addresses) > 9999){stop("'addresses' must be less than 10,000 rows")}

  header <- "recID|searchText"
  therest <- paste(paste(1:length(addresses), addresses, sep="|"), collapse="\n")
  final <- paste(header, therest, sep="\n")

  return(final)
}



