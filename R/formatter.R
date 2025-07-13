# tableR
## Format Output

#' Format a Data Frame for Aligned Printing (with Padding)
#'
#' Formats a data frame into a list structure with aligned, width-controlled,
#' padded, and optionally rounded or significant numeric columns, suitable for custom
#' table rendering or printing.
#'
#' @param x A data frame or object coercible to a data frame.
#' @param digits Integer or numeric vector. Number of decimal places (or significant digits if `signif = TRUE`) to use for numeric columns. If a single value, it is recycled for all columns.
#' @param signif Logical. If `TRUE`, numbers are formatted using significant digits instead of decimal places.
#' @param zeros Logical. If `TRUE`, retains trailing zeros in numeric formatting.
#' @param width NULL, single integer, or integer vector. Column widths to apply before padding. If `NULL`, widths are inferred from content and column names.
#' @param padding Integer. Extra spaces to pad to each column's width.
#' @param align Character string or character vector. Text alignment per column: `"left"`, `"center"`, or `"right"`. Recycled to match number of columns if needed.
#'
#' @return An object of class `"formatted"`: a list with elements `header`, `rows`, `row_names`, `align`, and `width`. This object is suitable for custom printing or table formatting.
#'
#' @examples
#' df <- data.frame(a = c(1.234, 5.678), b = c("foo", "bar"))
#' format_table(df, digits = 2, padding = 2)
#'
#' @export
#' 
format_table <- function(x, 
                         digits = 2, 
                         signif = FALSE, 
                         zeros = TRUE,
                         width = NULL,
                         padding = 1,
                         align = "right") {
  
  # Strip classes and convert to data.frame
  if (!is.data.frame(x)) {
    x <- as.data.frame(unclass(x))
  }
  
  n_cols <- ncol(x)

  digits_vec <- if (length(digits) == 1) rep(digits, n_cols) else digits
  if (length(digits_vec) != n_cols) stop("Length of 'digits' must be 1 or ncol(x)")

  x[] <- mapply(function(col, d) {
    if (is.numeric(col)) {
      val <- if (signif) signif(col, digits = d) else round(col, digits = d)
      if (zeros) format(val, nsmall = d, trim = FALSE) else as.character(val)
    } else as.character(col)
  }, x, digits_vec, SIMPLIFY = FALSE)

  if (is.null(width)) {
    width <- sapply(x, function(col) max(nchar(col), na.rm = TRUE))
    width <- pmax(width, nchar(colnames(x)))
  } else if (length(width) == 1) {
    width <- rep(width, n_cols)
  }
  
  width <- width + (padding * 2)

  if (length(align) == 1) align <- rep(align, n_cols)
  if (length(align) != n_cols) stop("Length of 'align' must match ncol(x)")

  structure(
    list(
      header = colnames(x),
      rows = split(x, seq(nrow(x))),
      row_names = rownames(x),
      align = align,
      width = width
    ),
    class = "formatted"
  )
}


#' Print a Formatted Table
#'
#' S3 method for printing objects of class `"formatted"` as aligned tables in the console.
#' Includes optional row names, caption, and vertical spacing.
#'
#' @param x An object of class `"formatted"` as returned by [format_table()].
#' @param caption Optional character string. A caption to print above the table.
#' @param space Integer vector of length 2. Number of blank lines to print before and after the table, respectively. Defaults to `c(1, 1)`.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object `x`. Used for its side effect of printing to the console.
#'
#' @seealso [format_table()]
#'
#' @method print formatted
#' @export
print.formatted <- function(x, caption = NULL, space = c(1, 1), ...) {
  tbl <- x
  header <- tbl$header
  rows <- tbl$rows
  row_names <- tbl$row_names
  align <- tbl$align
  width <- tbl$width

  if (!is.null(row_names)) {
    header <- c("", header)
    align <- c("left", align)
    name_width <- max(nchar(row_names), nchar(""))
    width <- c(name_width, width)
  }

  pad_cell <- function(cell, width, align) {
    pad <- width - nchar(cell)
    if (pad < 0) return(substr(cell, 1, width))
    switch(align,
           left = paste0(cell, strrep(" ", pad)),
           right = paste0(strrep(" ", pad), cell),
           center = paste0(strrep(" ", floor(pad / 2)), cell, strrep(" ", ceiling(pad / 2))),
           cell)
  }

  header_line <- paste(mapply(pad_cell, header, width, align), collapse = " ")
  data_lines <- vapply(seq_along(rows), function(i) {
    row <- unname(unlist(rows[[i]]))
    if (!is.null(row_names)) row <- c(row_names[i], row)
    paste(mapply(pad_cell, row, width, align), collapse = " ")
  }, character(1))

  output <- c()
  output <- c(output, rep("", space[1]))
  if (!is.null(caption)) {
    output <- c(output, caption, "")
  }
  output <- c(output, header_line, data_lines, rep("", space[2]))

  cat(paste(output, collapse = "\n"), "\n")
  invisible(tbl)
}
