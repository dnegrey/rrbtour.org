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
      Total, Strikes, Spares, Marks,
      M1_1, M2_1, S_1,
      M1_2, M2_2, S_2,
      M1_3, M2_3, S_3,
      M1_4, M2_4, S_4,
      M1_5, M2_5, S_5,
      M1_6, M2_6, S_6,
      M1_7, M2_7, S_7,
      M1_8, M2_8, S_8,
      M1_9, M2_9, S_9,
      M1_10, M2_10, M3_10, S_10,
    )
  return(y)
}
