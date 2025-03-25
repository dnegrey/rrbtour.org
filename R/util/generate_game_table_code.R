generate_game_table_code <- function(num_games) {
  x <- "#### Game %s\n\n```{r}\n#| output: true\ngame_table(filter(x, Game == %s))\n```"
  y <- character()
  for (i in 1:num_games) {
    y <- c(y, sprintf(x, i, i))
  }
  z <- paste(y, collapse = "\n\n")
  return(z)
}
