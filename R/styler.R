# tableR
## Style Output

#' Render a Formatted Table to the Console with Styling
#'
#' Prints a table object created by [format_table()] to the console with aligned columns,
#' optional caption, and customizable vertical spacing. Useful for clean, readable console output.
#'
#' @param formatted An object of class `"formatted"` as returned by [format_table()].
#' @param caption Optional character string. A caption to display above the table.
#' @param space Integer of length 1 or 2. Number of blank lines before and after the table.
#' If a single value is provided, it is applied to both top and bottom.
#'
#' @return Invisibly returns the input `formatted` object. Used primarily for its side effect of printing to the console.
#'
#' @seealso [format_table()], [print.formatted()]
#'
#' @examples
#' df <- data.frame(a = c(1.2, 3.4), b = c("x", "y"))
#' styled <- format_table(df)
#' style_console(styled, caption = "My Table")
#'
#' @export
style_console <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (!inherits(formatted, "formatted")) {
    stop("Input must be a 'formatted' object. Use format_table(..., return_output = TRUE).")
  }
  
  # Normalize space argument
  if (length(space) == 1) {
    space <- rep(space, 2)
  } else if (length(space) != 2) {
    stop("space must be a single value or a vector of length 2")
  }
  
  header <- formatted$header
  rows <- formatted$rows
  row_names <- formatted$row_names
  align <- formatted$align
  width <- formatted$width
  
  # Prepend row names column if available
  if (!is.null(row_names)) {
    header <- c("", header)
    align <- c("left", align)
    name_width <- max(nchar(row_names), nchar(""))
    width <- c(name_width, width)
  }
  
  # Pad a single cell according to alignment and width
  pad_cell <- function(cell, width, align) {
    pad <- width - nchar(cell)
    if (pad < 0) {
      # truncate if cell is longer than width
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
           stop("Invalid alignment: must be 'left', 'right', or 'center'")
    )
  }
  
  # Format header line
  header_line <- paste(mapply(pad_cell, header, width, align), collapse = " ")
  
  # Format each data row
  row_lines <- vapply(seq_along(rows), function(i) {
    row <- unname(unlist(rows[[i]]))
    if (!is.null(row_names)) {
      row <- c(row_names[i], row)
    }
    paste(mapply(pad_cell, row, width, align), collapse = " ")
  }, character(1))
  
  # Compose output with optional caption and space
  output <- c()
  output <- c(output, rep("", space[1]))
  if (!is.null(caption)) {
    output <- c(output, caption, "")
  }
  output <- c(output, header_line, row_lines, rep("", space[2]))
  
  # Print output
  cat(paste(output, collapse = "\n"), "\n")
  
  # Return input invisibly for chaining
  invisible(formatted)
}


#' Render a Formatted Table as Markdown
#'
#' Prints a `"formatted"` table object to the console using Markdown table syntax,
#' with alignment indicators and optional caption. Useful for generating tables
#' for README files, RMarkdown documents, or other Markdown-based outputs.
#'
#' @param formatted An object of class `"formatted"` as returned by [format_table()].
#' @param caption Optional character string. A caption to print above the table.
#' @param space Integer of length 1 or 2. Number of blank lines to add before and after the table.
#' If a single value is provided, it is applied to both top and bottom spacing.
#'
#' @return Invisibly returns a character vector representing the Markdown-formatted table.
#'
#' @seealso [format_table()], [style_console()]
#'
#' @examples
#' df <- data.frame(a = c(1.23, 4.56), b = c("apple", "banana"))
#' tbl <- format_table(df)
#' style_markdown(tbl, caption = "Fruit Table")
#'
#' @export
style_markdown <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (length(space) == 1) space <- rep(space, 2)
  else if (length(space) != 2) stop("space must be a single value or a vector of length 2")
  
  header <- formatted$header
  rows <- formatted$rows
  row_names <- formatted$row_names
  align <- formatted$align
  width <- formatted$width

  if (!is.null(row_names)) {
    header <- c("", header)
    align <- c("left", align)
    name_width <- max(nchar(row_names), nchar(" "))
    width <- c(name_width, width)
  }

  pad_cell <- function(cell, width, align) {
    pad <- width - nchar(cell)
    switch(align,
           left = paste0(cell, strrep(" ", pad)),
           right = paste0(strrep(" ", pad), cell),
           center = paste0(strrep(" ", floor(pad/2)), cell, strrep(" ", ceiling(pad/2))),
           cell)
  }

  align_marker <- function(a, w) {
    if (a == "left")   return(paste0(":", strrep("-", w - 1)))
    if (a == "right")  return(paste0(strrep("-", w - 1), ":"))
    if (a == "center") return(paste0(":", strrep("-", w - 2), ":"))
    strrep("-", w)
  }

  header_line <- paste0("| ", paste(mapply(pad_cell, header, width, align), collapse = " | "), " |")
  align_line  <- paste0("| ", paste(mapply(align_marker, align, width), collapse = " | "), " |")
  data_lines <- vapply(seq_along(rows), function(i) {
    row <- unname(unlist(rows[[i]]))
    if (!is.null(row_names)) row <- c(row_names[i], row)
    paste0("| ", paste(mapply(pad_cell, row, width, align), collapse = " | "), " |")
  }, character(1))
  
  md <- c()
  md <- c(md, rep("", space[1]))
  if (!is.null(caption)) {
    md <- c(md, caption, "")
  }
  md <- c(md, header_line, align_line, data_lines, rep("", space[2]))
  
  cat(paste(md, collapse = "\n"), "\n")
  invisible(md)
}

#' Render a Formatted Table in APA Style
#'
#' Prints a `"formatted"` table using APA-style layout, including borders and centered headers.
#' Optionally adds a caption and customizable vertical spacing. Useful for formatting tables
#' for APA-style manuscripts or console display in APA format.
#'
#' @param formatted An object of class `"formatted"` as returned by [format_table()].
#' @param caption Optional character string. A caption to print above the table.
#' @param space Integer of length 1 or 2. Number of blank lines to include before and after the table.
#' If a single value is provided, it is applied to both top and bottom spacing.
#'
#' @return Invisibly returns a character vector containing the formatted APA-style table.
#'
#' @seealso [format_table()], [style_console()], [style_markdown()]
#'
#' @examples
#' df <- data.frame(a = c(1.23, 4.56), b = c("apple", "banana"))
#' tbl <- format_table(df)
#' style_apa(tbl, caption = "Table 1\nFruit Preferences")
#'
#' @export
style_apa <- function(formatted, caption = NULL, space = c(1, 1)) {
  if (length(space) == 1) space <- rep(space, 2)
  else if (length(space) != 2) stop("space must be a single value or a vector of length 2")

  header <- formatted$header
  rows <- formatted$rows
  row_names <- formatted$row_names
  align <- formatted$align
  width <- formatted$width

  if (!is.null(row_names)) {
    header <- c("", header)
    align <- c("left", align)
    name_width <- max(nchar(row_names), nchar(" "))
    width <- c(name_width, width)
  }

  pad_cell <- function(cell, width, align) {
    pad <- width - nchar(cell)
    switch(align,
           left = paste0(cell, strrep(" ", pad)),
           right = paste0(strrep(" ", pad), cell),
           center = paste0(strrep(" ", floor(pad/2)), cell, strrep(" ", ceiling(pad/2))),
           cell)
  }

  horizontal_line <- function(widths) {
    paste0("+", paste0(sapply(widths, function(w) strrep("-", w + 2)), collapse = "+"), "+")
  }

  header_line <- paste0("| ", paste(mapply(pad_cell, header, width, rep("center", length(align))), collapse = " | "), " |")
  data_lines <- vapply(seq_along(rows), function(i) {
    row <- unname(unlist(rows[[i]]))
    if (!is.null(row_names)) row <- c(row_names[i], row)
    paste0("| ", paste(mapply(pad_cell, row, width, align), collapse = " | "), " |")
  }, character(1))

  output <- c()
  if (!is.null(caption)) output <- c(output, caption, "")
  output <- c(output,
              horizontal_line(width),
              header_line,
              horizontal_line(width),
              data_lines,
              horizontal_line(width))

  output <- c(rep("", space[1]), output, rep("", space[2]))

  cat(paste(output, collapse = "\n"), "\n")
  invisible(output)
}
