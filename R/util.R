date_to_int <- function(date, origin = "1970-01-01") {
  int <- as.integer(as.Date(date) - as.Date(origin))
  
  d <- dim(date)
  if (!is.null(d)) {
    int <- array(int, d)
  }
  
  int
}


int_to_date <- function(int, origin = "1970-01-01") {
  int + as.Date(origin)
}


data_frame_to_array <- function(df) {
  array(unlist(df), dim(df))
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


squote <- function(x) {
  sprintf("'%s'", x)
}


visFALSE <- function(x) {
  !is.na(x) & x == FALSE
}

#' Clean group names from delay map
#' @param raw_group The group element from a delay_map row
#' @noRd
clean_group_name <- function(raw_group) {
  raw_group <- as.character(raw_group)
  
  if (length(raw_group) == 1 && grepl("^c\\(", raw_group)) {
    raw_group <- gsub("^c\\(|\\)$", "", raw_group)
    raw_group <- gsub("[\"']", "", raw_group)
    raw_group <- trimws(strsplit(raw_group, ",")[[1]])
  }
  
  clean_group <- paste(raw_group, collapse = ", ")
  clean_group <- gsub("[-_]", " ", clean_group)
  
  tools::toTitleCase(clean_group)
}

#' Clean event names from delay map
#' @param raw_event The from/to element from a delay_map row
#' @noRd
clean_event_name <- function(raw_event) {
  clean_name <- gsub("[-_]", " ", as.character(raw_event))
  tools::toTitleCase(clean_name)
}

vnapply <- function(...) {
  vapply(..., FUN.VALUE = 1)
}
