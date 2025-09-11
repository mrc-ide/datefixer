date_to_int <- function(date, origin = "1970-01-01") {
  as.integer(as.Date(date) - as.Date(origin))
}


int_to_date <- function(int, origin = "1970-01-01") {
  int + as.Date(origin)
}
