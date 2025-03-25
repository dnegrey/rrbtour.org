event_table <- function(x) {
  y <- summarize_event(x)
  datatable(
    data = y,
    class = "compact nowrap cell-border event_table",
    options = list(
      dom = "t",
      pageLength = nrow(y),
      ordering = FALSE
    ),
    rownames = FALSE
  ) |>
    formatStyle(
      columns = c("Total", "Place"),
      fontWeight = "bold",
      color = "#FFFFFF"
    )
}
