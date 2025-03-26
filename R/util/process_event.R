process_event <- function(f) {
  # Read event data
  x <- read_event(f)
  xid <- substr(basename(f), 7, nchar(basename(f)))
  xid <- as.integer(gsub(".json", "", xid, fixed = TRUE))

  # Append event data to tour data if necessary
  xd <- data.frame(
    EventId = xid,
    EventName = x$name,
    EventDate = x$date,
    EventTag = x$tag,
    EventLocation = x$location,
    EventLocationShort = x$loc_short,
    EventNumGames = x$num_games
  )
  td <- read_tour_data()
  tmp <- anti_join(xd, td, "EventId")
  if (nrow(tmp) > 0) {
    xd <- cbind(xd, x$scores)
    write.table(
      x = xd,
      file = "var/tour_data.csv",
      append = TRUE,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE
    )
  }

  # Create event report if necessary
  report <- sprintf("events/_event_%s.qmd", xid)
  if (!file.exists(report)) {
    tmp <- paste(readLines("events/_event_template.qmd"), collapse = "\n")
    xr <- sprintf(
      fmt = tmp,
      paste0(x$name, " {#", x$tag, "}"),
      x$location,
      x$loc_short,
      format(x$date, "%B %d, %Y"),
      paste(x$members[order(x$members)], collapse = ", "),
      xid,
      paste(x$tag, "result", sep = "-"),
      generate_game_table_code(x$tag, x$num_games)
    )
    write(xr, report)
  }
}
