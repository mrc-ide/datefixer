date_to_int <- function(date, origin = "1970-01-01") {
  array(as.integer(as.Date(date) - as.Date(origin)), dim2(date))
}


int_to_date <- function(int, origin = "1970-01-01") {
  array(int + as.Date(origin), dim2(int))
}


data_frame_to_array <- function(df) {
  array(unlist(df), dim(df))
}

dim2 <- function(x) {
  dim(x) %||% length(x)
}
