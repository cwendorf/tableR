# tableR
## Save Output

#' Save a Styled Table to a File
#'
#' Writes a table to a text file. The table can be either a `"formatted"` object created
#' with [format_table()] or a character vector of pre-rendered lines (e.g., from [style_console()],
#' [style_markdown()], or [style_apa()]).
#'
#' @param styled A `"formatted"` object or a character vector representing styled table lines.
#' @param file Character string specifying the file path where the table will be saved.
#' Defaults to `"table_output.txt"`.
#' @param append Logical. If `TRUE`, appends the output to the file. If `FALSE`, overwrites the file.
#'
#' @return Invisibly returns the input `styled` object.
#'
#' @seealso [format_table()], [style_console()], [style_markdown()], [style_apa()]
#'
#' @examples
#' df <- data.frame(a = c(1.1, 2.2), b = c("x", "y"))
#' tbl <- format_table(df)
#' styled <- style_console(tbl)
#' save_output(styled, file = "my_table.txt")
#'
#' @export
save_output <- function(styled, file = "table_output.txt", append = FALSE) {
  output_lines <- NULL

  # If input is a tableR or tableR_list object, capture the printed output
  if (inherits(styled, "tableR") || inherits(styled, "tableR_list")) {
    con <- textConnection("tmp_output", "w", local = TRUE)
    sink(con)
    print(styled)
    sink()
    close(con)
    output_lines <- tmp_output
  } else if (is.character(styled)) {
    output_lines <- styled
  } else {
    stop("Input must be a 'tableR' object, 'tableR_list', or a character vector.")
  }

  con <- file(file, open = if (append) "a" else "w", encoding = "UTF-8")
  writeLines(output_lines, con = con, useBytes = TRUE)
  close(con)

  invisible(styled)
}
