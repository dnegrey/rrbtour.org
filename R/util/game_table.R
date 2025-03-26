game_table <- function(x) {
  event_name <- x$EventName[1]
  event_date <- x$EventDate[1]
  location <- x$EventLocation[1]
  game_num <- x$Game[1]
  member_format <- "<span class=game_member>%s</span>"
  frame_format <- "<p><span class=frame_mark>%s</span><span class=frame_mark>%s</span></p><p class=frame_score>%s</p>"
  frame_format10 <- "<p><span class=frame_mark>%s</span><span class=frame_mark>%s</span><span class=frame_mark>%s</span></p><p class=frame_score>%s</p>"
  y <- x |>
    mutate(
      Frame1_Mark2 = ifelse(Frame1_Mark2 == "", "&nbsp;", Frame1_Mark2),
      Frame2_Mark2 = ifelse(Frame2_Mark2 == "", "&nbsp;", Frame2_Mark2),
      Frame3_Mark2 = ifelse(Frame3_Mark2 == "", "&nbsp;", Frame3_Mark2),
      Frame4_Mark2 = ifelse(Frame4_Mark2 == "", "&nbsp;", Frame4_Mark2),
      Frame5_Mark2 = ifelse(Frame5_Mark2 == "", "&nbsp;", Frame5_Mark2),
      Frame6_Mark2 = ifelse(Frame6_Mark2 == "", "&nbsp;", Frame6_Mark2),
      Frame7_Mark2 = ifelse(Frame7_Mark2 == "", "&nbsp;", Frame7_Mark2),
      Frame8_Mark2 = ifelse(Frame8_Mark2 == "", "&nbsp;", Frame8_Mark2),
      Frame9_Mark2 = ifelse(Frame9_Mark2 == "", "&nbsp;", Frame9_Mark2),
      Frame10_Mark3 = ifelse(Frame10_Mark3 == "", "&nbsp;", Frame10_Mark3),
      Member = sprintf(member_format, Member),
      F1 = sprintf(frame_format, Frame1_Mark1, Frame1_Mark2, Frame1_Score),
      F2 = sprintf(frame_format, Frame2_Mark1, Frame2_Mark2, Frame2_Score),
      F3 = sprintf(frame_format, Frame3_Mark1, Frame3_Mark2, Frame3_Score),
      F4 = sprintf(frame_format, Frame4_Mark1, Frame4_Mark2, Frame4_Score),
      F5 = sprintf(frame_format, Frame5_Mark1, Frame5_Mark2, Frame5_Score),
      F6 = sprintf(frame_format, Frame6_Mark1, Frame6_Mark2, Frame6_Score),
      F7 = sprintf(frame_format, Frame7_Mark1, Frame7_Mark2, Frame7_Score),
      F8 = sprintf(frame_format, Frame8_Mark1, Frame8_Mark2, Frame8_Score),
      F9 = sprintf(frame_format, Frame9_Mark1, Frame9_Mark2, Frame9_Score),
      F10 = sprintf(frame_format10, Frame10_Mark1, Frame10_Mark2, Frame10_Mark3, Frame10_Score)
    ) |>
    select(
      Member, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, GameScore
    )
  datatable(
    data = y,
    class = "compact nowrap cell-border game_table",
    options = list(
      dom = "t",
      pageLength = nrow(y),
      ordering = FALSE
    ),
    rownames = FALSE,
    colnames = c("", 1:10, "Total"),
    caption = tags$caption(paste(
      event_name,
      location,
      format(event_date, "%B %d, %Y"),
      paste("Game", game_num),
      sep = " | "
    )),
    escape = FALSE
  ) |>
    formatStyle(
      columns = "GameScore",
      fontWeight = "bold",
      color = "#FFFFFF",
      textAlign = "center"
    )
}
