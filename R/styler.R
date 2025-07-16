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
    lapply(formatted, function(tbl) {
      style_console(tbl, caption = attr(tbl, "caption"), space = attr(formatted, "space"))
    })
    return(invisible(formatted))
  }
  
  if (!inherits(formatted, "tableR")) {
    stop("Input must be a 'tableR' object from format_table().")
  }
  
  if (length(space) == 1) {
    space <- rep(space, 2)
  }
  
  align <- attr(formatted, "align")
  width <- attr(formatted, "width")
  padding <- attr(formatted, "padding")
  row_names <- attr(formatted, "row_names")
  caption <- if (is.null(caption)) attr(formatted, "caption") else caption
  
  col_names <- names(formatted)
  data_rows <- as.data.frame(formatted, stringsAsFactors = FALSE)
  
  # Prepend row names if present
  if (!is.null(row_names)) {
    col_names <- c("", col_names)
    align <- c("left", align)
    width <- c(max(nchar(row_names)), width)
    data_rows <- cbind(row_names, data_rows, stringsAsFactors = FALSE)
  }
  
  pad_cell <- function(cell, width, align) {
    cell <- as.character(cell)
    pad <- width - nchar(cell)
    if (pad < 0) {
      return(substr(cell, 1, width))
    }
    switch(align,
           left   = paste0(cell, strrep(" ", pad)),
           right  = paste0(strrep(" ", pad), cell),
           center = {
             left_pad <- floor(pad / 2)
             right_pad <- ceiling(pad / 2)
             paste0(strrep(" ", left_pad), cell, strrep(" ", right_pad))
           },
           stop("Invalid alignment")
    )
  }
  
  header_line <- paste(mapply(pad_cell, col_names, width, align), collapse = strrep(" ", padding))
  
  row_lines <- apply(data_rows, 1, function(row) {
    paste(mapply(pad_cell, row, width, align), collapse = strrep(" ", padding))
  })
  
  output <- c()
  output <- c(output, rep("", space[1]))
  if (!is.null(caption)) {
    output <- c(output, caption, "")
  }
  output <- c(output, header_line, row_lines, rep("", space[2]))
  
  cat(paste(output, collapse = "\n"), "\n")
  
  invisible(formatted)
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
    lapply(formatted, function(tbl) {
      style_markdown(tbl, caption = attr(tbl, "caption"), space = attr(formatted, "space"))
    })
    return(invisible(formatted))
  }
  
  if (!inherits(formatted, "tableR")) {
    stop("Input must be a 'tableR' object from format_table().")
  }
  
  if (length(space) == 1) space <- rep(space, 2)
  
  align <- attr(formatted, "align")
  width <- attr(formatted, "width")
  row_names <- attr(formatted, "row_names")
  caption <- if (is.null(caption)) attr(formatted, "caption") else caption
  
  col_names <- names(formatted)
  data_rows <- as.data.frame(formatted, stringsAsFactors = FALSE)
  
  # Handle row names if present
  if (!is.null(row_names)) {
    row_names <- ifelse(is.na(row_names), "", row_names)
    max_width <- max(nchar(row_names), na.rm = TRUE)
    if (!is.finite(max_width) || max_width < 1) max_width <- 1L
    width <- c(max_width, width)
    align <- c("left", align)
    col_names <- c("", col_names)
    data_rows <- cbind(row_names, data_rows, stringsAsFactors = FALSE)
  }
  
  # Ensure width vector matches number of columns
  n_cols <- length(col_names)
  if (length(width) < n_cols) {
    width <- c(width, rep(1L, n_cols - length(width)))
  } else if (length(width) > n_cols) {
    width <- width[seq_len(n_cols)]
  }
  
  # Force minimum width of 1 for all columns
  width <- pmax(width, 1L)
  
  pad_cell <- function(cell, width, align) {
    cell <- as.character(cell)
    cell[is.na(cell)] <- ""
    pad <- width - nchar(cell)
    pad <- pmax(pad, 0)
    switch(align,
           left   = paste0(cell, strrep(" ", pad)),
           right  = paste0(strrep(" ", pad), cell),
           center = paste0(strrep(" ", floor(pad / 2)), cell, strrep(" ", ceiling(pad / 2))),
           cell)
  }
  
  align_marker <- function(a, w) {
    # Defensive: if width < 1, set to 1 to avoid strrep error
    if (w < 1) w <- 1
    if (a == "left")   return(paste0(":", strrep("-", w - 1)))
    if (a == "right")  return(paste0(strrep("-", w - 1), ":"))
    if (a == "center") return(paste0(":", strrep("-", max(w - 2, 0)), ":"))
    strrep("-", w)
  }
  
  header_line <- paste0("| ", paste(mapply(pad_cell, col_names, width, align), collapse = " | "), " |")
  align_line  <- paste0("| ", paste(mapply(align_marker, align, width), collapse = " | "), " |")
  
  data_lines <- apply(data_rows, 1, function(row) {
    row <- ifelse(is.na(row), "", row)
    paste0("| ", paste(mapply(pad_cell, row, width, align), collapse = " | "), " |")
  })
  
  md <- c()
  md <- c(md, rep("", space[1]))
  if (!is.null(caption) && nzchar(caption)) {
    md <- c(md, caption, "")
  }
  md <- c(md, header_line, align_line, data_lines, rep("", space[2]))
  
  cat(paste(md, collapse = "\n"), "\n")
  invisible(md)
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
    n <- length(formatted)
    captions <- if (!is.null(caption)) rep_len(caption, n) else
      vapply(formatted, function(x) attr(x, "caption") %||% "", character(1))
    outs <- vector("list", n)
    for (i in seq_len(n)) {
      outs[[i]] <- style_apa(formatted[[i]], caption = captions[[i]], space = space)
    }
    return(invisible(outs))
  }

  if (!inherits(formatted, "tableR")) stop("Input must be a 'tableR' object.")
  if (length(space) == 1) space <- rep(space, 2)

  align <- attr(formatted, "align")
  width <- attr(formatted, "width")
  row_names <- attr(formatted, "row_names")
  caption <- if (is.null(caption)) attr(formatted, "caption") else caption

  col_names <- names(formatted)
  data_rows <- as.data.frame(formatted, stringsAsFactors = FALSE)

  # Handle row names
  if (!is.null(row_names)) {
    row_names <- ifelse(is.na(row_names), "", row_names)
    max_width <- max(nchar(row_names), na.rm = TRUE)
    if (!is.finite(max_width) || max_width < 1) max_width <- 1L
    width <- c(max_width, width)
    align <- c("left", align)
    col_names <- c("", col_names)
    data_rows <- cbind(row_names, data_rows, stringsAsFactors = FALSE)
  }

  # Make sure width matches column count
  n_cols <- length(col_names)
  if (length(width) < n_cols) {
    width <- c(width, rep(1L, n_cols - length(width)))
  } else if (length(width) > n_cols) {
    width <- width[seq_len(n_cols)]
  }
  width <- pmax(width, 1L)  # Ensure minimum width 1

  pad_cell <- function(cell, width, align) {
    cell <- as.character(cell)
    cell[is.na(cell)] <- ""
    pad <- width - nchar(cell)
    pad <- pmax(pad, 0)
    switch(align,
      left   = paste0(cell, strrep(" ", pad)),
      right  = paste0(strrep(" ", pad), cell),
      center = paste0(strrep(" ", floor(pad / 2)), cell, strrep(" ", ceiling(pad / 2))),
      cell
    )
  }

  # Compose horizontal lines to exactly match table width including column separators
  total_width <- sum(width) + 3 * length(width) + 1 # width + 3 spaces per col + 1 for starting '+'
  horiz_line <- paste0("+", paste0(vapply(width, function(w) strrep("-", w + 2), character(1)), collapse = "+"), "+")

  header_line <- paste0("|", paste(mapply(function(name, w, a) {
    paste0(" ", pad_cell(name, w, a), " ")
  }, col_names, width, align), collapse = "|"), "|")

  data_lines <- apply(data_rows, 1, function(row) {
    paste0("|", paste(mapply(function(cell, w, a) {
      paste0(" ", pad_cell(cell, w, a), " ")
    }, row, width, align), collapse = "|"), "|")
  })

  md <- c()
  md <- c(md, rep("", space[1]))
  if (!is.null(caption) && nzchar(caption)) {
    md <- c(md, caption, "")
  }
  md <- c(md, horiz_line, header_line, horiz_line, data_lines, horiz_line, rep("", space[2]))

  cat(paste(md, collapse = "\n"), "\n")
  invisible(md)
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
    n <- length(formatted)
    captions <- if (!is.null(caption)) rep_len(caption, n) else
      vapply(formatted, function(x) attr(x, "caption") %||% "", character(1))
    outs <- vector("list", n)
    for (i in seq_len(n)) {
      outs[[i]] <- style_box(formatted[[i]], caption = captions[[i]], space = space)
    }
    return(invisible(outs))
  }

  if (!inherits(formatted, "tableR")) stop("Input must be a 'tableR' object.")
  if (length(space) == 1) space <- rep(space, 2)

  header <- names(formatted)
  rows_df <- as.data.frame(formatted, stringsAsFactors = FALSE)
  row_names <- attr(formatted, "row_names")
  align <- attr(formatted, "align")
  width <- attr(formatted, "width")

  # Handle row names
  if (!is.null(row_names)) {
    row_names <- ifelse(is.na(row_names), "", row_names)
    max_width <- max(nchar(row_names), na.rm = TRUE)
    if (!is.finite(max_width) || max_width < 1) max_width <- 1L
    width <- c(max_width, width)
    align <- c("left", align)
    header <- c("", header)
    rows_df <- cbind(row_names, rows_df, stringsAsFactors = FALSE)
  }

  # Make sure width matches columns count
  n_cols <- length(header)
  if (length(width) < n_cols) {
    width <- c(width, rep(1L, n_cols - length(width)))
  } else if (length(width) > n_cols) {
    width <- width[seq_len(n_cols)]
  }
  width <- pmax(width, 1L)  # ensure minimum width 1

  pad_cell <- function(cell, width, align) {
    cell <- as.character(cell)
    cell[is.na(cell)] <- ""
    pad <- width - nchar(cell)
    pad <- pmax(pad, 0)
    switch(align,
           left = paste0(cell, strrep(" ", pad)),
           right = paste0(strrep(" ", pad), cell),
           center = paste0(strrep(" ", floor(pad / 2)), cell, strrep(" ", ceiling(pad / 2))),
           cell)
  }

  # Build horizontal border line, width + 2 per col for padding and separators
  border_pieces <- vapply(width, function(w) strrep("-", w + 2), character(1))
  horizontal_border <- paste0("+", paste(border_pieces, collapse = "+"), "+")

  header_line <- paste0("|", paste(mapply(function(name, w, a) {
    paste0(" ", pad_cell(name, w, a), " ")
  }, header, width, align), collapse = "|"), "|")

  row_lines <- apply(rows_df, 1, function(row) {
    paste0("|", paste(mapply(function(cell, w, a) {
      paste0(" ", pad_cell(cell, w, a), " ")
    }, row, width, align), collapse = "|"), "|")
  })

  output <- c()
  output <- c(output, rep("", space[1]))
  caption <- caption %||% attr(formatted, "caption")
  if (!is.null(caption) && nzchar(caption)) output <- c(output, caption, "")
  output <- c(output,
              horizontal_border,
              header_line,
              horizontal_border,
              row_lines,
              horizontal_border,
              rep("", space[2]))

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
