summarize_event <- function(x) {
  y <- select(x, Member, Game, GameScore)
  yt <- y |>
    group_by(Member) |>
    summarize(TotalScore = sum(GameScore)) |>
    ungroup() |>
    data.frame()
  ng <- max(y$Game)
  for (i in 1:ng) {
    tmp <- filter(y, Game == i) |>
      select(Member, GameScore)
    names(tmp)[2] <- paste("Game", i)
    yt <- left_join(yt, tmp, "Member")
  }
  yt <- yt |>
    mutate(Total = TotalScore) |>
    select(-TotalScore) |>
    arrange(desc(Total))
  yt$Place <- min_rank(-1*yt$Total)
  return(yt)
}
