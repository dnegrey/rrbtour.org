get_member_stats <- function(member, year = NULL) {
  x <- read_tour_data() |>
    filter(Member == member)
  if (!is.null(year)) {
    x <- x |>
      filter(format(EventDate, "%Y") == as.character(year))
  }
  y <- list(
    date_min = min(x$EventDate),
    event_count = n_distinct(x$EventId),
    game_count = nrow(x),
    game_avg = mean(x$GameScore),
    game_max = max(x$GameScore),
    strike_count = sum(x$GameStrikes),
    spare_count = sum(x$GameSpares),
    mark_count = sum(x$GameMarks)
  )
  return(y)
}
