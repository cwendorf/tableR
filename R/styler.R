# tableR
## Style Output

#' Safe fallback operator for NULL values
#'
#' Returns `a` if it is not NULL, otherwise returns `b`.
#' 
#' @param a First value.
#' @param b Second value to use if `a` is NULL.
#' @return Value of `a` if not NULL, else `b`.
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Print a formatted table object in console style
#'
#' @param formatted A formatted table or list of formatted tables.
#' @param caption Caption(s) for the table(s).
#' @param space Numeric vector of length 2 for spacing before and after output.
#' @return Invisibly returns the formatted object.
#' @examples
#' tbl <- format_table(head(mtcars))
#' style_console(tbl)
#' 
#' @export
style_console <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (inherits(formatted, "tableR_list")) {
    n <- length(formatted)
    captions <- if (!is.null(caption)) rep_len(caption, n) else
      vapply(formatted, function(x) attr(x, "caption") %||% "", character(1))
    for (i in seq_len(n)) {
      style_console(formatted[[i]], caption = captions[[i]], space = space)
    }
    return(invisible(formatted))
  }
  
  if (!inherits(formatted, "tableR")) stop("Input must be a 'tableR' object.")
  if (length(space) == 1) space <- rep(space, 2)
  
  header <- names(formatted)
  rows_df <- as.data.frame(formatted, stringsAsFactors = FALSE)
  row_names <- attr(formatted, "row_names")
  align <- attr(formatted, "align")
  width <- attr(formatted, "width")
  
  if (!is.null(row_names)) {
    header <- c("", header)
    align <- c("left", align)
    name_width <- max(1, max(nchar(row_names), na.rm = TRUE))
    width <- c(name_width, width)
  }
  
  pad_cell <- function(cell, width, align) {
    cell <- as.character(cell)
    pad <- width - nchar(cell)
    pad <- max(0, pad)
    switch(align,
           left = paste0(cell, strrep(" ", pad)),
           right = paste0(strrep(" ", pad), cell),
           center = paste0(strrep(" ", floor(pad / 2)), cell, strrep(" ", ceiling(pad / 2))),
           cell)
  }
  
  header_line <- paste(mapply(pad_cell, header, width, align), collapse = "   ")
  
  row_lines <- vapply(seq_len(nrow(rows_df)), function(i) {
    row <- as.character(rows_df[i, ])
    if (!is.null(row_names)) row <- c(row_names[i], row)
    paste(mapply(pad_cell, row, width, align), collapse = "   ")
  }, character(1))
  
  output <- c()
  output <- c(output, rep("", space[1]))
  caption <- caption %||% attr(formatted, "caption")
  if (!is.null(caption) && nzchar(caption)) output <- c(output, caption, "")
  output <- c(output,
              header_line,
              row_lines,
              rep("", space[2]))
  
  cat(paste(output, collapse = "\n"), "\n")
  invisible(formatted)
}

#' Print a formatted table object as Markdown
#'
#' @param formatted A formatted table or list of formatted tables.
#' @param caption Caption(s) for the table(s).
#' @param space Numeric vector of length 2 for spacing before and after output.
#' @return Invisibly returns the formatted object.
#' @examples
#' tbl <- format_table(head(mtcars))
#' style_markdown(tbl)
#' 
#' @export
style_markdown <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (inherits(formatted, "tableR_list")) {
    n <- length(formatted)
    captions <- if (!is.null(caption)) rep_len(caption, n) else
      vapply(formatted, function(x) attr(x, "caption") %||% "", character(1))
    invisible(lapply(seq_len(n), function(i) {
      style_markdown(formatted[[i]], caption = captions[[i]], space = space)
    }))
    return(invisible(formatted))
  }
  
  if (!inherits(formatted, "tableR")) stop("Input must be a 'tableR' object.")
  if (length(space) == 1) space <- rep(space, 2)
  
  header <- names(formatted)
  rows_df <- as.data.frame(formatted, stringsAsFactors = FALSE)
  row_names <- attr(formatted, "row_names")
  align <- attr(formatted, "align")
  width <- attr(formatted, "width")
  
  if (!is.null(row_names)) {
    header <- c("", header)
    align <- c("left", align)
    name_width <- max(1, max(nchar(row_names), na.rm = TRUE))
    width <- c(name_width, width)
  }
  
  pad_cell <- function(cell, width, align) {
    cell <- as.character(cell)
    pad <- width - nchar(cell)
    pad <- max(0, pad)
    switch(align,
           left = paste0(cell, strrep(" ", pad)),
           right = paste0(strrep(" ", pad), cell),
           center = paste0(strrep(" ", floor(pad / 2)), cell, strrep(" ", ceiling(pad / 2))),
           cell)
  }
  
  align_marker <- function(a, w) {
    w <- max(1, w)
    if (a == "left")   return(paste0(":", strrep("-", w - 1)))
    if (a == "right")  return(paste0(strrep("-", w - 1), ":"))
    if (a == "center") return(paste0(":", strrep("-", w - 2), ":"))
    strrep("-", w)
  }
  
  header_line <- paste0("| ", paste(mapply(pad_cell, header, width, align), collapse = " | "), " |")
  align_line  <- paste0("| ", paste(mapply(align_marker, align, width), collapse = " | "), " |")
  
  data_lines <- vapply(seq_len(nrow(rows_df)), function(i) {
    row <- as.character(rows_df[i, ])
    if (!is.null(row_names)) row <- c(row_names[i], row)
    paste0("| ", paste(mapply(pad_cell, row, width, align), collapse = " | "), " |")
  }, character(1))
  
  md <- c()
  md <- c(md, rep("", space[1]))
  caption <- caption %||% attr(formatted, "caption")
  if (!is.null(caption) && nzchar(caption)) {
    md <- c(md, caption, "")
  }
  md <- c(md, header_line, align_line, data_lines, rep("", space[2]))
  
  cat(paste(md, collapse = "\n"), "\n")
  invisible(formatted)
}

#' Print a formatted table object in APA style
#'
#' @param formatted A formatted table or list of formatted tables.
#' @param caption Caption(s) for the table(s).
#' @param space Numeric vector of length 2 for spacing before and after output.
#' @return Invisibly returns the formatted object.
#' @examples
#' tbl <- format_table(head(mtcars))
#' style_apa(tbl)
#' 
#' @export
style_apa <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (inherits(formatted, "tableR_list")) {
    n <- length(formatted)
    captions <- if (!is.null(caption)) rep_len(caption, n) else
      vapply(formatted, function(x) attr(x, "caption") %||% "", character(1))
    invisible(lapply(seq_len(n), function(i) {
      style_apa(formatted[[i]], caption = captions[[i]], space = space)
    }))
    return(invisible(formatted))
  }
  
  if (!inherits(formatted, "tableR")) stop("Input must be a 'tableR' object.")
  if (length(space) == 1) space <- rep(space, 2)
  
  header <- names(formatted)
  rows_df <- as.data.frame(formatted, stringsAsFactors = FALSE)
  row_names <- attr(formatted, "row_names")
  align <- attr(formatted, "align")
  width <- attr(formatted, "width")
  
  if (!is.null(row_names)) {
    header <- c("", header)
    align <- c("left", align)
    name_width <- max(1, max(nchar(row_names), na.rm = TRUE))
    width <- c(name_width, width)
  }
  
  pad_cell <- function(cell, width, align) {
    cell <- as.character(cell)
    pad <- width - nchar(cell)
    pad <- max(0, pad)
    switch(align,
           left = paste0(cell, strrep(" ", pad)),
           right = paste0(strrep(" ", pad), cell),
           center = paste0(strrep(" ", floor(pad / 2)), cell, strrep(" ", ceiling(pad / 2))),
           stop("Invalid alignment"))
  }
  
  total_width <- sum(width) + 3 * (length(width) - 1)
  sep_line <- strrep("-", total_width)
  
  header_line <- paste(mapply(pad_cell, header, width, align), collapse = "   ")
  
  row_lines <- vapply(seq_len(nrow(rows_df)), function(i) {
    row <- as.character(rows_df[i, ])
    if (!is.null(row_names)) row <- c(row_names[i], row)
    paste(mapply(pad_cell, row, width, align), collapse = "   ")
  }, character(1))
  
  output <- c()
  output <- c(output, rep("", space[1]))
  caption <- caption %||% attr(formatted, "caption")
  if (!is.null(caption) && nzchar(caption)) output <- c(output, caption, "")
  output <- c(output, sep_line, header_line, sep_line, row_lines, sep_line, rep("", space[2]))
  
  cat(paste(output, collapse = "\n"), "\n")
  invisible(formatted)
}



#' Print a formatted table object as Unicode box
#'
#' @param formatted A formatted table or list of formatted tables.
#' @param caption Caption(s) for the table(s).
#' @param space Numeric vector of length 2 for spacing before and after output.
#' @return Invisibly returns the formatted object.
#' @examples
#' tbl <- format_table(head(mtcars))
#' style_box(tbl)
#' 
#' @export
style_box <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (inherits(formatted, "tableR_list")) {
    n <- length(formatted)
    captions <- if (!is.null(caption)) rep_len(caption, n) else
      vapply(formatted, function(x) attr(x, "caption") %||% "", character(1))
    invisible(lapply(seq_len(n), function(i) {
      style_box(formatted[[i]], caption = captions[[i]], space = space)
    }))
    return(invisible(formatted))
  }

  if (!inherits(formatted, "tableR")) stop("Input must be a 'tableR' object.")
  if (length(space) == 1) space <- rep(space, 2)

  padding <- attr(formatted, "padding") %||% 1  # padding spaces left and right
  header <- names(formatted)
  rows_df <- as.data.frame(formatted, stringsAsFactors = FALSE)
  row_names <- attr(formatted, "row_names")
  width <- attr(formatted, "width")
  align <- attr(formatted, "align")

  if (!is.null(row_names)) {
    header <- c("", header)
    align <- c("left", align)
    name_width <- max(1, max(nchar(row_names), na.rm = TRUE))
    width <- c(name_width, width)
  }

  pad_width <- width + 2 * padding
  pad_cell <- function(cell, width, align) {
    cell <- as.character(cell)
    pad <- width - nchar(cell)
    pad <- max(0, pad)
    text <- switch(align,
                   left = paste0(cell, strrep(" ", pad)),
                   right = paste0(strrep(" ", pad), cell),
                   center = paste0(strrep(" ", floor(pad / 2)), cell, strrep(" ", ceiling(pad / 2))),
                   cell)
    paste0(strrep(" ", padding), text, strrep(" ", padding))
  }

  make_row <- function(cells, l = "│", r = "│", sep = "│") {
    paste0(l, paste(mapply(pad_cell, cells, width, align), collapse = sep), r)
  }

  make_line <- function(left, sep, right, fill = "─") {
    pieces <- mapply(function(w) strrep(fill, w + 2 * padding), width)
    paste0(left, paste(pieces, collapse = sep), right)
  }

  top_line    <- make_line("┌", "┬", "┐")
  header_line <- make_row(header)
  mid_line    <- make_line("├", "┼", "┤")
  bottom_line <- make_line("└", "┴", "┘")

  row_lines <- vapply(seq_len(nrow(rows_df)), function(i) {
    row <- as.character(rows_df[i, ])
    if (!is.null(row_names)) row <- c(row_names[i], row)
    make_row(row)
  }, character(1))

  output <- c()
  output <- c(output, rep("", space[1]))
  caption <- caption %||% attr(formatted, "caption")
  if (!is.null(caption) && nzchar(caption)) output <- c(output, caption, "")
  output <- c(output,
              top_line,
              header_line,
              mid_line,
              row_lines,
              bottom_line,
              rep("", space[2]))

  cat(paste(output, collapse = "\n"), "\n")
  invisible(formatted)
}


#' Print a formatted table object as LaTeX code
#'
#' @param formatted A formatted table or list of formatted tables.
#' @param caption Optional caption(s) for the table(s).
#' @param space Numeric vector of length 2 for spacing before and after output.
#' @return Invisibly returns the formatted object.
#'
#' @examples
#' tbl <- format_table(head(mtcars))
#' style_latex(tbl)
#'
#' # For a list of formatted tables
#' tbl_list <- list(
#'   format_table(head(mtcars)),
#'   format_table(head(iris))
#' )
#' style_latex(tbl_list, caption = c("Mtcars Table", "Iris Table"))
#' 
#' @export
style_latex <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (inherits(formatted, "tableR_list")) {
    n <- length(formatted)
    captions <- if (!is.null(caption)) rep_len(caption, n) else
      vapply(formatted, function(x) attr(x, "caption") %||% "", character(1))
    invisible(lapply(seq_len(n), function(i) {
      style_latex(formatted[[i]], caption = captions[[i]], space = space)
    }))
    return(invisible(formatted))
  }
  
  if (!inherits(formatted, "tableR")) stop("Input must be a 'tableR' object.")
  if (length(space) == 1) space <- rep(space, 2)
  
  escape_latex <- function(text) {
    text <- gsub("\\\\", "\\textbackslash{}", text)
    text <- gsub("&", "\\&", text)
    text <- gsub("%", "\\%", text)
    text <- gsub("\\$", "\\$", text)
    text <- gsub("#", "\\#", text)
    text <- gsub("_", "\\_", text)
    text <- gsub("\\{", "\\{", text)
    text <- gsub("\\}", "\\}", text)
    text <- gsub("~", "\\textasciitilde{}", text)
    text <- gsub("\\^", "\\textasciicircum{}", text)
    text
  }
  
  header <- names(formatted)
  rows_df <- as.data.frame(formatted, stringsAsFactors = FALSE)
  row_names <- attr(formatted, "row_names")
  align <- attr(formatted, "align")
  
  if (!is.null(row_names)) {
    header <- c("", header)
  }
  
  col_align <- sapply(align, function(a) {
    switch(a,
           left = "l",
           right = "r",
           center = "c",
           "l")
  })
  if (!is.null(row_names)) col_align <- c("l", col_align)
  
  output <- c()
  output <- c(output, rep("", space[1]))
  output <- c(output, "\\begin{table}[ht]")
  output <- c(output, "\\centering")
  
  cap <- caption %||% attr(formatted, "caption")
  if (!is.null(cap) && nzchar(cap)) {
    output <- c(output, paste0("\\caption{", escape_latex(cap), "}"))
  }
  
  output <- c(output, paste0("\\begin{tabular}{", paste(col_align, collapse = ""), "}"))
  output <- c(output, "\\hline")
  
  output <- c(output, paste(escape_latex(header), collapse = " & "), "\\\\")
  output <- c(output, "\\hline")
  
  for (i in seq_len(nrow(rows_df))) {
    row <- as.character(rows_df[i, ])
    if (!is.null(row_names)) row <- c(row_names[i], row)
    output <- c(output, paste(escape_latex(row), collapse = " & "), "\\\\")
  }
  
  output <- c(output, "\\hline")
  output <- c(output, "\\end{tabular}")
  output <- c(output, "\\end{table}")
  output <- c(output, rep("", space[2]))
  
  cat(paste(output, collapse = "\n"), "\n")
  invisible(formatted)
}


#' Print a formatted table object as HTML
#'
#' @param formatted A formatted table or list of formatted tables.
#' @param caption Optional caption(s) for the table(s).
#' @param space Numeric vector of length 2 for spacing before and after output.
#' @return Invisibly returns the formatted object.
#' @examples
#' tbl <- format_table(head(mtcars))
#' style_html(tbl)
#'
#' # For a list of formatted tables
#' tbl_list <- list(
#'   format_table(head(mtcars)),
#'   format_table(head(iris))
#' )
#' style_html(tbl_list, caption = c("Mtcars Table", "Iris Table"))
#' @export
style_html <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (inherits(formatted, "tableR_list")) {
    n <- length(formatted)
    captions <- if (!is.null(caption)) rep_len(caption, n) else
      vapply(formatted, function(x) attr(x, "caption") %||% "", character(1))
    invisible(lapply(seq_len(n), function(i) {
      style_html(formatted[[i]], caption = captions[[i]], space = space)
    }))
    return(invisible(formatted))
  }

  if (!inherits(formatted, "tableR")) stop("Input must be a 'tableR' object.")
  if (length(space) == 1) space <- rep(space, 2)

  escape_html <- function(text) {
    text <- gsub("&", "&amp;", text)
    text <- gsub("<", "&lt;", text)
    text <- gsub(">", "&gt;", text)
    text <- gsub("\"", "&quot;", text)
    text <- gsub("'", "&#39;", text)
    text
  }

  header <- names(formatted)
  rows_df <- as.data.frame(formatted, stringsAsFactors = FALSE)
  row_names <- attr(formatted, "row_names")

  if (!is.null(row_names)) {
    header <- c("", header)
  }

  output <- c()
  output <- c(output, rep("", space[1]))
  output <- c(output, "<table border=\"1\" cellspacing=\"0\" cellpadding=\"4\">")

  cap <- caption %||% attr(formatted, "caption")
  if (!is.null(cap) && nzchar(cap)) {
    output <- c(output, paste0("<caption>", escape_html(cap), "</caption>"))
  }

  # Header row
  header_cells <- paste0("<th>", escape_html(header), "</th>", collapse = "")
  output <- c(output, paste0("<thead><tr>", header_cells, "</tr></thead>"))

  # Body rows
  output <- c(output, "<tbody>")
  for (i in seq_len(nrow(rows_df))) {
    row <- as.character(rows_df[i, ])
    if (!is.null(row_names)) row <- c(row_names[i], row)
    row_cells <- paste0("<td>", escape_html(row), "</td>", collapse = "")
    output <- c(output, paste0("<tr>", row_cells, "</tr>"))
  }
  output <- c(output, "</tbody>")

  output <- c(output, "</table>")
  output <- c(output, rep("", space[2]))

  cat(paste(output, collapse = "\n"), "\n")
  invisible(formatted)
}
