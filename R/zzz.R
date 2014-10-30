
.onLoad <- function(lib, pkg, ...) {
  packageStartupMessage("testing onLoad function")
  demo_geocode_base_url <<- "http://geocoder.cit.api.here.com/6.2/geocode.json"
  prod_geocode_base_url <- "http://geocoder.api.here.com/6.2/geocode.json"

  demo_batch_geocode_base_url <- "http://batch.geocoder.cit.api.here.com/6.2/jobs"
  prod_batch_geocode_base_url <- "http://batch.geocoder.api.here.com/6.2/jobs"

  demo_App_id <- "DemoAppId01082013GAL"
  demo_App_code <- "AJKnXv84fjrb0KIHawS0Tg"
}
