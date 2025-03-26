event_table <- function(x) {
  event_name <- x$EventName[1]
  event_date <- x$EventDate[1]
  location <- x$EventLocation[1]
  y <- summarize_event(x)
  datatable(
    data = y,
    class = "compact nowrap cell-border event_table",
    options = list(
      dom = "t",
      pageLength = nrow(y),
      ordering = FALSE
    ),
    rownames = FALSE,
    caption = tags$caption(paste(
      event_name,
      location,
      format(event_date, "%B %d, %Y"),
      sep = " | "
    ))
  ) |>
    formatStyle(
      columns = c("Total", "Place"),
      fontWeight = "bold",
      color = "#FFFFFF"
    )
}
