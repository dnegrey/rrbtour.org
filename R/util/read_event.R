read_event <- function(f) {
  x <- jsonlite::read_json(f)
  y <- list(
    name = x$name,
    date = as.Date(x$date),
    tag = x$tag,
    location = x$location,
    loc_short = x$loc_short
  )
  y$members <- names(x)[!(names(x) %in% names(y))]
  return(y)
}
