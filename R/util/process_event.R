process_event <- function(f) {
  # Read event data
  x <- read_event(f)

  # Append event data to tour data if necessary
  xd <- data.frame(
    EventName = x$name,
    EventDate = x$date,
    EventTag = x$tag,
    EventLocation = x$location,
    EventLocationShort = x$loc_short,
    EventNumGames = x$num_games
  )
  td <- read_tour_data()
  tmp <- anti_join(xd, td, c("EventName", "EventDate", "EventLocation"))
  if (nrow(tmp) > 0) {
    xd <- cbind(xd, x$scores)
    write.csv(
      x = xd,
      file = "var/tour_data.csv",
      append = TRUE,
      row.names = FALSE
    )
  }
}
