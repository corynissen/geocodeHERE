
#' Format a df of addresses for upload
#'
#' Format a df of addresses for upload
#' @param addresses A df with two columns, an id and address character strings to be geocoded
#' @keywords geocode batch
#' @export
#' @examples
#' \dontrun{
#' data(chicago_landmarks)
#' addresses <- chicago_landmarks[,"Address"]
#' addresses <- paste(addresses, "chicago IL")
#' address_str <- vec_to_string(addresses)
#' }
#' df_to_string
df_to_string <- function(addresses_df){
  if(nrow(addresses_df) > 9999){stop("'addresses_df' must be less than 10,000 rows")}

  header <- "recID|searchText"
  therest <- paste(paste(addresses_df[,1], addresses_df[,2], sep="|"), collapse="\n")
  final <- paste(header, therest, sep="\n")

  return(final)
}



