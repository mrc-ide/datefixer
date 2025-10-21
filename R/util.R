date_to_int <- function(date, origin = "1970-01-01") {
  int <- as.integer(as.Date(date) - as.Date(origin))
  
  d <- dim(date)
  if (!is.null(d)) {
    int <- array(int, d)
  }
  
  int
}


int_to_date <- function(int, origin = "1970-01-01") {
  date <- int + as.Date(origin)
  
  d <- dim(int)
  if (!is.null(d)) {
    date <- array(date, d)
  }
  
  date
}


data_frame_to_array <- function(df) {
  array(unlist(df), dim(df))
}
