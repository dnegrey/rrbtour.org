plot_member_scores <- function(member) {
  x <- read_tour_data() |>
    filter(Member == member) |>
    arrange(EventId, Game) |>
    mutate(GameNum = row_number())
  y <- plot_ly(x) |>
    add_trace(
      x = ~GameNum,
      y = ~GameScore,
      type = "scatter",
      mode = "markers+lines",
      marker = list(
        line = list(
          color = "#FFFFFF",
          width = 3
        ),
        color = "#762F3E",
        size = 15
      ),
      line = list(
        color = "#FFFFFF",
        width = 3
      ),
      hoverinfo = "text",
      text = ~paste(
        sprintf("<b style='text-decoration: underline;'>%s</b>", EventName),
        EventLocation,
        gsub("  ", " ", format(EventDate, "%B %e, %Y"), fixed = TRUE),
        sprintf("Game: %s", Game),
        sprintf("Score: %s", GameScore),
        sep = "<br>"
      )
    ) |>
    layout(
      margin = list(t = 65),
      title = list(
        text = "Member Score History",
        xanchor = "left",
        x = 0,
        xref = "paper",
        font = list(
          color = "#FFFFFF",
          size = 18
        )
      ),
      xaxis = list(
        title = NA_character_,
        range = c(-0.2, nrow(x) + 1),
        showgrid = FALSE,
        showticklabels = FALSE,
        zerolinecolor = "#FFFFFF"
      ),
      yaxis = list(
        title = NA_character_,
        range = c(0, 305),
        gridcolor = "#555555",
        zerolinecolor = "#FFFFFF",
        tickfont = list(color = "#FFFFFF")
      ),
      plot_bgcolor = "#333333",
      paper_bgcolor = "#333333"
    ) |>
    config(
      displayModeBar = FALSE
    )
  return(y)
}
