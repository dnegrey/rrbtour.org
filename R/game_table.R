game_table <- function() {
  x <- data.frame(
    member = "Dan",
    m1 = "7",
    m2 = "/",
    sf = 18
  )
  x$frame <- sprintf(
    fmt = "<p><span style='width:33%%;'>%s</span><span style='width:33%%;'>%s</span></p><p>%s</p>",
    x$m1, x$m2, x$sf
  )
  datatable(
    data = x,
    options = list(
      dom = "t"
    ),
    rownames = FALSE,
    escape = FALSE
  )
}
