source_functions <- function(path, recursive = TRUE) {
  x <- list.files(
    path = path,
    pattern = ".R$",
    full.names = TRUE,
    recursive = recursive
  )
  invisible(
    lapply(x, source)
  )
}
