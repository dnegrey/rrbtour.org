read_tour_data <- function(f = "var/tour_data.csv") {
  x <- read.csv(
    file = f,
    header = TRUE,
    na.strings = "NA",
    colClasses = "character"
  )
  x$EventId <- as.integer(x$EventId)
  x$EventDate <- as.Date(x$EventDate)
  x$EventNumGames <- as.integer(x$EventNumGames)
  x$Game <- as.integer(x$Game)
  x$GameScore <- as.integer(x$GameScore)
  x$GameStrikes <- as.integer(x$GameStrikes)
  x$GameSpares <- as.integer(x$GameSpares)
  x$GameMarks <- as.integer(x$GameMarks)
  for (i in 1:10) {
    x[, sprintf("Frame%s_Score", i)] <- as.integer(x[, sprintf("Frame%s_Score", i)])
  }
  return(x)
}
