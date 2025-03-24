transpose_score <- function(x) {
  stopifnot(nrow(x) == 10)
  m1 <- as.data.frame(t(x$Mark1))
  names(m1) <- paste0("M1_", 1:10)
  m2 <- as.data.frame(t(x$Mark2))
  names(m2) <- paste0("M2_", 1:10)
  s <- as.data.frame(t(x$ScoreTotal))
  names(s) <- paste0("S_", 1:10)
  y <- cbind(m1, m2)
  y$M3_10 <- x[10, ]$Mark3
  marks <- data.frame(
    Total = x[10, ]$ScoreTotal,
    Strikes = sum(x$Mark1 == "X" | x$Mark2 == "X" | x$Mark3 == "X"),
    Spares = sum(x$Mark1 == "/" | x$Mark2 == "/" | x$Mark3 == "/")
  )
  marks$Marks <- marks$Strikes + marks$Spares
  y <- cbind(
    marks,
    y,
    s
  ) |>
    select(
      GameScore = Total,
      GameStrikes = Strikes, GameSpares = Spares, GameMarks = Marks,
      Frame1_Mark1 = M1_1, Frame1_Mark2 = M2_1, Frame1_Score = S_1,
      Frame2_Mark1 = M1_2, Frame2_Mark2 = M2_2, Frame2_Score = S_2,
      Frame3_Mark1 = M1_3, Frame3_Mark2 = M2_3, Frame3_Score = S_3,
      Frame4_Mark1 = M1_4, Frame4_Mark2 = M2_4, Frame4_Score = S_4,
      Frame5_Mark1 = M1_5, Frame5_Mark2 = M2_5, Frame5_Score = S_5,
      Frame6_Mark1 = M1_6, Frame6_Mark2 = M2_6, Frame6_Score = S_6,
      Frame7_Mark1 = M1_7, Frame7_Mark2 = M2_7, Frame7_Score = S_7,
      Frame8_Mark1 = M1_8, Frame8_Mark2 = M2_8, Frame8_Score = S_8,
      Frame9_Mark1 = M1_9, Frame9_Mark2 = M2_9, Frame9_Score = S_9,
      Frame10_Mark1 = M1_10, Frame10_Mark2 = M2_10, Frame10_Mark3 = M3_10,
      Frame10_Score = S_10,
    )
  return(y)
}
