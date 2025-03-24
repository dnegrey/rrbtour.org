read_event <- function(f) {
  x <- read_json(f)
  y <- list(
    name = x$name,
    date = as.Date(x$date),
    tag = x$tag,
    location = x$location,
    loc_short = x$loc_short
  )
  y$members <- names(x)[!(names(x) %in% names(y))]
  games <- integer()
  for (mem in y$members) {
    games <- c(games, as.integer(names(x[[mem]])))
  }
  y$num_games <- max(games)
  rm(mem, games)
  for (m in y$members) {
    for (g in 1:y$num_games) {
      tmp <- transpose_score(score_game(x[[m]][[g]]))
      tmp <- cbind(
        data.frame(Member = m, Game = g, tmp)
      )
      if (!("scores" %in% ls())) {
        scores <- tmp
      } else {
        scores <- rbind(scores, tmp)
      }
    }
  }
  y$scores <- scores
  return(y)
}
