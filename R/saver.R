# tableR
## Save Output

#' Save printed output to a file without printing to console
#'
#' @param expr An expression that prints output (e.g., `style_console(tbl)`).
#' @param file Path to the file where output should be saved.
#' @return Invisibly returns the file path.
#'
#' @export
save_output <- function(expr, file) {
  output <- capture.output(expr)
  writeLines(output, file)
  invisible(file)
}
