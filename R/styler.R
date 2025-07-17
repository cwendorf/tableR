# tableR
## Style Output

#' Safe fallback operator for NULL values
#'
#' Returns `a` if it is not NULL, otherwise returns `b`.
#' 
#' @param a First value.
#' @param b Second value to use if `a` is NULL.
#' @return Value of `a` if not NULL, else `b`.
#'
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Render a Formatted Table to the Console with Styling
#'
#' Prints a table object created by [format_table()] to the console with aligned columns,
#' optional caption, and customizable vertical spacing. Useful for clean, readable console output.
#'
#' @param formatted An object of class `"tableR"` or `"tableR_list"` as returned by [format_table()].
#' @param caption Optional character string to override the object's caption.
#' @param space Integer of length 1 or 2. Blank lines before and after the table.
#'
#' @return Invisibly returns the input `formatted` object.
#'
#' @export
style_console <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (inherits(formatted, "tableR_list")) {
    for (tbl in formatted) {
      style_console(tbl, caption = attr(tbl, "caption"), space = attr(tbl, "space"))
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
  padding <- attr(formatted, "padding") %||% 0

  width <- width + padding * 2

  if (!is.null(row_names)) {
    header <- c("", header)
    align <- c("left", align)
    name_width <- max(1, max(nchar(row_names)))
    width <- c(name_width + padding * 2, width)
  }

  pad_cell <- function(cell, width, align) {
    inner_width <- width - 2 * padding
    cell <- as.character(cell)
    pad <- inner_width - nchar(cell)
    padded <- switch(align,
                     left = paste0(cell, strrep(" ", pad)),
                     right = paste0(strrep(" ", pad), cell),
                     center = paste0(strrep(" ", floor(pad / 2)), cell, strrep(" ", ceiling(pad / 2))),
                     cell)
    paste0(strrep(" ", padding), padded, strrep(" ", padding))
  }

  header_line <- paste(mapply(pad_cell, header, width, align), collapse = " ")

  row_lines <- vapply(seq_len(nrow(rows_df)), function(i) {
    row <- as.character(rows_df[i, ])
    if (!is.null(row_names)) row <- c(row_names[i], row)
    paste(mapply(pad_cell, row, width, align), collapse = " ")
  }, character(1))

  output <- c(rep("", space[1]))
  caption <- caption %||% attr(formatted, "caption")
  if (!is.null(caption) && nzchar(caption)) output <- c(output, caption, "")
  output <- c(output, header_line, row_lines, rep("", space[2]))

  cat(paste(output, collapse = "\n"), "\n")
  invisible(output)
}

#' Format a 'tableR' Object as a Markdown Table
#'
#' Converts a formatted table (of class \code{"tableR"}) into a markdown-formatted table string.
#' Supports optional captions and spacing around the output.
#'
#' @param formatted A \code{"tableR"} object (or \code{"tableR_list"}) created by \code{format_table()}.
#' @param caption Optional character string specifying a caption to print above the table.
#'                Defaults to the \code{"caption"} attribute of the object, if any.
#' @param space Integer vector of length 1 or 2 specifying the number of blank lines to add
#'              before and after the table output. If length 1, the value is recycled.
#'
#' @return Invisibly returns a character vector of the markdown table lines.
#'         Also prints the markdown table to the console.
#'
#' @export
style_markdown <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (inherits(formatted, "tableR_list")) {
    for (tbl in formatted) {
      style_markdown(tbl, caption = attr(tbl, "caption"), space = attr(tbl, "space"))
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
  padding <- attr(formatted, "padding") %||% 0
  
  width <- width + padding * 2
  
  if (!is.null(row_names)) {
    header <- c("", header)
    align <- c("left", align)
    name_width <- max(1, max(nchar(row_names)))
    width <- c(name_width + padding * 2, width)
  }
  
  pad_cell <- function(cell, width, align) {
    cell <- as.character(cell)
    pad <- width - nchar(cell)
    switch(align,
           left = paste0(cell, strrep(" ", pad)),
           right = paste0(strrep(" ", pad), cell),
           center = paste0(strrep(" ", floor(pad / 2)), cell, strrep(" ", ceiling(pad / 2))),
           cell)
  }
  
  alignment_row <- function(align, width) {
    sapply(seq_along(align), function(i) {
      switch(align[i],
             left = paste0(":", strrep("-", width[i] - 1)),
             right = paste0(strrep("-", width[i] - 1), ":"),
             center = paste0(":", strrep("-", width[i] - 2), ":"),
             strrep("-", width[i]))
    })
  }
  
  header_line <- paste0("| ", paste(mapply(pad_cell, header, width, align), collapse = " | "), " |")
  divider <- paste0("| ", paste(alignment_row(align, width), collapse = " | "), " |")
  
  row_lines <- vapply(seq_len(nrow(rows_df)), function(i) {
    row <- as.character(rows_df[i, ])
    if (!is.null(row_names)) row <- c(row_names[i], row)
    paste0("| ", paste(mapply(pad_cell, row, width, align), collapse = " | "), " |")
  }, character(1))
  
  output <- c(rep("", space[1]))
  caption <- caption %||% attr(formatted, "caption")
  if (!is.null(caption) && nzchar(caption)) output <- c(output, caption, "")
  output <- c(output, header_line, divider, row_lines, rep("", space[2]))
  
  cat(paste(output, collapse = "\n"), "\n")
  invisible(output)
}

#' Style a 'tableR' Object as an APA-Style Table
#'
#' This function formats a `tableR` object as a plain text table following APA style guidelines.
#' It respects the column alignment specified in the `tableR` object and produces
#' horizontal lines that match the table width. Supports caption display and spacing.
#'
#' @param formatted A `tableR` object or `tableR_list` containing one or more tables to format.
#'   Must inherit from class `"tableR"`.
#' @param caption Optional character string to override the table caption. If `NULL`,
#'   the caption attribute from the `formatted` object is used if available.
#' @param space Numeric vector of length 1 or 2 specifying the number of blank lines to
#'   add before and after the table output. If length 1, used for both before and after.
#'
#' @return Invisibly returns a character vector of the formatted table lines.
#'   Also prints the table to the console.
#'
#' @export
style_apa <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (inherits(formatted, "tableR_list")) {
    for (tbl in formatted) {
      style_apa(tbl, caption = attr(tbl, "caption"), space = attr(tbl, "space"))
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
  padding <- attr(formatted, "padding") %||% 0
  
  width <- width + padding * 2
  
  if (!is.null(row_names)) {
    header <- c("", header)
    align <- c("left", align)
    name_width <- max(1, max(nchar(row_names)))
    width <- c(name_width + padding * 2, width)
  }
  
  pad_cell <- function(cell, width, align) {
    inner_width <- width - 2 * padding
    cell <- as.character(cell)
    pad <- inner_width - nchar(cell)
    padded <- switch(align,
                     left = paste0(cell, strrep(" ", pad)),
                     right = paste0(strrep(" ", pad), cell),
                     center = paste0(strrep(" ", floor(pad / 2)), cell, strrep(" ", ceiling(pad / 2))),
                     cell)
    paste0(strrep(" ", padding), padded, strrep(" ", padding))
  }
  
  horizontal_border <- strrep("-", sum(width) + (length(width) - 1))
  
  header_line <- paste(mapply(pad_cell, header, width, align), collapse = " ")
  
  row_lines <- vapply(seq_len(nrow(rows_df)), function(i) {
    row <- as.character(rows_df[i, ])
    if (!is.null(row_names)) row <- c(row_names[i], row)
    paste(mapply(pad_cell, row, width, align), collapse = " ")
  }, character(1))
  
  output <- c(rep("", space[1]))
  caption <- caption %||% attr(formatted, "caption")
  if (!is.null(caption) && nzchar(caption)) output <- c(output, caption, "")
  output <- c(output, horizontal_border, header_line, horizontal_border, row_lines, horizontal_border, rep("", space[2]))
  
  cat(paste(output, collapse = "\n"), "\n")
  invisible(output)
}

#' Render a TableR Object in Box Drawing Style
#'
#' Formats a `tableR` object as a text table with box-style borders using ASCII characters.
#' This style includes horizontal and vertical lines surrounding the table, column headers,
#' and rows, with proper padding and alignment for each column.
#'
#' @param formatted A `tableR` object (or `tableR_list`) to be styled as a box table.
#'   This object is typically created by `format_table()` and contains attributes for alignment,
#'   width, row names, and captions.
#' @param caption Optional character string to include as a caption above the table.
#'   If `NULL`, uses the `caption` attribute from the `formatted` object, if available.
#' @param space Integer vector of length 1 or 2 indicating the number of blank lines
#'   to add before and after the table output. If length 1, that number is used for both.
#'
#' @return Invisibly returns a character vector of the formatted table lines.
#'   Also prints the styled table directly to the console.
#'
#' @export
style_box <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (inherits(formatted, "tableR_list")) {
    for (tbl in formatted) {
      style_box(tbl, caption = attr(tbl, "caption"), space = attr(tbl, "space"))
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
  padding <- attr(formatted, "padding") %||% 0
  
  width <- width + padding * 2
  
  if (!is.null(row_names)) {
    header <- c("", header)
    align <- c("left", align)
    name_width <- max(1, max(nchar(row_names)))
    width <- c(name_width + padding * 2, width)
  }
  
  pad_cell <- function(cell, width, align) {
    inner_width <- width - 2 * padding
    cell <- as.character(cell)
    pad <- inner_width - nchar(cell)
    padded <- switch(align,
                     left = paste0(cell, strrep(" ", pad)),
                     right = paste0(strrep(" ", pad), cell),
                     center = paste0(strrep(" ", floor(pad / 2)), cell, strrep(" ", ceiling(pad / 2))),
                     cell)
    paste0(strrep(" ", padding), padded, strrep(" ", padding))
  }
  
  border_pieces <- vapply(width, function(w) paste0(rep("-", w), collapse = ""), character(1))
  horizontal_border <- paste0("+", paste(border_pieces, collapse = "+"), "+")
  
  header_line <- paste0("|", paste(mapply(pad_cell, header, width, align), collapse = "|"), "|")
  
  row_lines <- vapply(seq_len(nrow(rows_df)), function(i) {
    row <- as.character(rows_df[i, ])
    if (!is.null(row_names)) row <- c(row_names[i], row)
    paste0("|", paste(mapply(pad_cell, row, width, align), collapse = "|"), "|")
  }, character(1))
  
  output <- c(rep("", space[1]))
  caption <- caption %||% attr(formatted, "caption")
  if (!is.null(caption) && nzchar(caption)) output <- c(output, caption, "")
  output <- c(output, horizontal_border, header_line, horizontal_border, row_lines, horizontal_border, rep("", space[2]))
  
  cat(paste(output, collapse = "\n"), "\n")
  invisible(output)
}

#' Render a TableR Object as a LaTeX Table
#'
#' Converts a `tableR` object into LaTeX tabular syntax, respecting column alignments
#' and optionally including row names and a caption.
#'
#' @param formatted A `tableR` object (or `tableR_list`) typically created by `format_table()`.
#'   Must have attributes for alignment, row names, and optionally a caption.
#' @param caption Optional character string to override the caption attribute.
#' @param space Integer vector of length 1 or 2 specifying the number of blank lines
#'   to insert before and after the table output. If length 1, uses the same number before and after.
#'
#' @return Invisibly returns a character vector of LaTeX lines forming the table.
#'   Also prints the LaTeX table code to the console.
#'
#' @export
style_latex <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (inherits(formatted, "tableR_list")) {
    n <- length(formatted)
    captions <- if (!is.null(caption)) rep_len(caption, n) else
      vapply(formatted, function(x) attr(x, "caption") %||% "", character(1))
    outs <- vector("list", n)
    for (i in seq_len(n)) {
      outs[[i]] <- style_latex(formatted[[i]], caption = captions[[i]], space = space)
    }
    return(invisible(outs))
  }

  if (!inherits(formatted, "tableR")) stop("Input must be a 'tableR' object.")
  if (length(space) == 1) space <- rep(space, 2)

  header <- names(formatted)
  rows_df <- as.data.frame(formatted, stringsAsFactors = FALSE)
  row_names <- attr(formatted, "row_names")
  align <- attr(formatted, "align")

  if (!is.null(row_names)) {
    header <- c("", header)
    align <- c("left", align)
  }

  align_map <- c(left = "l", center = "c", right = "r")

  latex_align <- if (!is.null(align)) {
    sapply(align, function(a) {
      if (a %in% names(align_map)) align_map[[a]] else "l"
    })
  } else NULL

  col_align <- paste(latex_align, collapse = "")

  lines <- c("\\begin{tabular}{", col_align, "}")
  lines <- c(lines, "\\hline")
  lines <- c(lines, paste(c(header), collapse = " & "), "\\\\")
  lines <- c(lines, "\\hline")

  for (i in seq_len(nrow(rows_df))) {
    row <- as.character(rows_df[i, ])
    if (!is.null(row_names)) row <- c(row_names[i], row)
    lines <- c(lines, paste(row, collapse = " & "), "\\\\")
  }

  lines <- c(lines, "\\hline", "\\end{tabular}")

  output <- c()
  output <- c(output, rep("", space[1]))
  caption <- caption %||% attr(formatted, "caption")
  if (!is.null(caption) && nzchar(caption)) output <- c(output, paste0("\\caption{", caption, "}"), "")
  output <- c(output, lines, rep("", space[2]))

  cat(paste(output, collapse = "\n"), "\n")
  invisible(output)
}

#' Render a TableR Object as an HTML Table
#'
#' Converts a `tableR` object into an HTML `<table>` element, including `<thead>`, `<tbody>`,
#' optional row names, and an optional `<caption>`.
#'
#' @param formatted A `tableR` object (or `tableR_list`) typically created by `format_table()`.
#'   Should have attributes for row names and caption.
#' @param caption Optional character string to override the caption attribute.
#' @param space Integer vector of length 1 or 2 specifying the number of blank lines
#'   to insert before and after the table output. If length 1, uses the same number before and after.
#'
#' @return Invisibly returns a character vector of HTML lines forming the table.
#'   Also prints the HTML code to the console.
#'
#' @export
style_html <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (inherits(formatted, "tableR_list")) {
    n <- length(formatted)
    captions <- if (!is.null(caption)) rep_len(caption, n) else
      vapply(formatted, function(x) attr(x, "caption") %||% "", character(1))
    outs <- vector("list", n)
    for (i in seq_len(n)) {
      outs[[i]] <- style_html(formatted[[i]], caption = captions[[i]], space = space)
    }
    return(invisible(outs))
  }

  if (!inherits(formatted, "tableR")) stop("Input must be a 'tableR' object.")
  if (length(space) == 1) space <- rep(space, 2)

  header <- names(formatted)
  rows_df <- as.data.frame(formatted, stringsAsFactors = FALSE)
  row_names <- attr(formatted, "row_names")

  if (!is.null(row_names)) {
    header <- c("", header)
  }

  output <- c()
  output <- c(output, rep("", space[1]))
  caption <- caption %||% attr(formatted, "caption")
  if (!is.null(caption) && nzchar(caption)) {
    output <- c(output, paste0("<caption>", caption, "</caption>"))
  }
  output <- c(output, "<table>")
  output <- c(output, "  <thead>")
  output <- c(output, "    <tr>", paste0("      <th>", header, "</th>", collapse = ""), "    </tr>")
  output <- c(output, "  </thead>")
  output <- c(output, "  <tbody>")

  for (i in seq_len(nrow(rows_df))) {
    row <- as.character(rows_df[i, ])
    if (!is.null(row_names)) row <- c(row_names[i], row)
    output <- c(output, "    <tr>", paste0("      <td>", row, "</td>", collapse = ""), "    </tr>")
  }

  output <- c(output, "  </tbody>")
  output <- c(output, "</table>")
  output <- c(output, rep("", space[2]))

  cat(paste(output, collapse = "\n"), "\n")
  invisible(output)
}
