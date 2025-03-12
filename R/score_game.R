score_game <- function(x) {
  # Check values
  ux <- unlist(x)
  stopifnot(
    class(ux) == "integer",
    all(ux %in% 0:10)
  )
  # Create list to output data
  y <- list()
  # Loop through frames
  for (i in 1:10) {
    # Get list indices
    l1 <- i*2 - 1
    l2 <- i*2
    # Get ball results
    b1 <- x[[l1]]
    b2 <- x[[l2]]
    b3 <- NULL
    if (i == 10) {
      b3 <- unlist(x[21])
    }
    stopifnot(
      !is.null(b1),
      b1 %in% 0:10,
      !(is.null(b2) & b1 != 10),
      !(b1 == 10 & !is.null(b2)),
      is.null(b2) || b2 %in% 0:10,
      is.null(b3) || b3 %in% 0:10,
      !(i == 10 & is.null(b2))
    )
    # Determine pins
    p1 <- b1
    p2 <- b2
    if (is.null(b2)) {
      p2 <- NA_integer_
    }
    p3 <- b3
    if (is.null(b3)) {
      p3 <- NA_integer_
    }
    # Determine marks
    m1 <- ""
    m2 <- ""
    m3 <- ""
    if (i < 10) {
      if (p1 == 10) {
        m1 <- "X"
        m2 <- ""
      } else {
        m1 <- as.character(p1)
        m2 <- ifelse(p1 + p2 == 10, "/", as.character(p2))
      }
    } else {
      if (p1 == 10) {
        m1 <- "X"
        m2 <- ifelse(p2 == 10, "X", as.character(p2))
      } else {
        m1 <- as.character(p1)
        m2 <- ifelse(p1 + p2 == 10, "/", as.character(p2))
      }
      if (p1 + p2 >= 10) {
        stopifnot(p3 %in% 0:10)
        if (m2 %in% c("X", "/")) {
          m3 <- ifelse(p3 == 10, "X", as.character(p3))
        } else {
          m3 <- ifelse(p2 + p3 == 10, "/", as.character(p3))
        }
      }
    }
    m1 <- ifelse(m1 == "0", "-", m1)
    m2 <- ifelse(m2 == "0", "-", m2)
    m3 <- ifelse(m3 == "0", "-", m3)
    # Determine scores
    x1 <- unlist(x[l1:length(x)])
    if (m1 == "X") {
      s1 <- sum(x1[1:3], na.rm = TRUE)
    } else {
      s1 <- x1[1]
    }
    x2 <- unlist(x[l2:length(x)])
    if (m2 %in% c("X", "/")) {
      stopifnot(m2 == "/" || (m2 == "X" & i == 10))
      s2 <- sum(x2[1:2], na.rm = TRUE)
    } else if (m2 == "") {
      s2 <- 0
    } else {
      s2 <- x2[1]
    }
    sf <- sum(s1, s2, na.rm = TRUE)
    # Return row for data frame
    y[[i]] <- data.frame(
      frame = i,
      p1 = p1, p2 = p2,
      m1 = m1, m2 = m2,
      s1 = s1, s2 = s2,
      p3 = p3, m3 = m3,
      sf = sf
    )
  }
  # Bind data frames and return
  z <- do.call(rbind, y)
  row.names(z) <- NULL
  z$score <- cumsum(z$sf)
  return(z)
}
