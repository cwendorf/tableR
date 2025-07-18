# tableR
## Format Table

#' Format a vector, matrix, or data.frame into a formatted table object
#'
#' This function converts vectors, matrices, or data.frames into a formatted
#' table with specified column widths, alignments, padding, digit formatting,
#' spacing, and captions. If a list of tables is provided, it recursively
#' formats each element.
#'
#' @param x A vector, matrix, data.frame, or list of such objects to format.
#' @param width Numeric scalar or vector specifying minimal width of columns.
#' @param align Character scalar or vector specifying alignment for columns:
#'   "left", "right", or "center".
#' @param padding Numeric scalar specifying padding spaces (currently stored as attribute).
#' @param digits Integer specifying number of decimal digits for numeric formatting.
#' @param space Numeric vector of length 2 specifying empty lines before and after output.
#' @param caption Character vector or NULL specifying captions for tables.
#'
#' @return A formatted table object (class "tableR") or list of such objects.
#'
#' @export
format_table <- function(x, width = 8, align = "right", padding = 1,
                         digits = 2, space = c(1, 1), caption = NULL) {
  
  vec_to_df <- function(vec) {
    if (is.null(names(vec))) {
      names(vec) <- paste0("V", seq_along(vec))
    }
    df <- as.data.frame(as.list(vec), stringsAsFactors = FALSE)
    rownames(df) <- NULL
    df
  }
  
  mat_to_df <- function(mat) {
    df <- as.data.frame(mat, stringsAsFactors = FALSE)
    rownames(df) <- rownames(mat)
    df
  }
  
  to_table <- function(obj) {
    if (is.atomic(obj) && is.null(dim(obj))) {
      obj <- vec_to_df(obj)
    } else if (is.matrix(obj)) {
      obj <- mat_to_df(obj)
    } else if (!is.data.frame(obj)) {
      stop("Input must be a vector, matrix, or data.frame.")
    }
    
    rn <- rownames(obj)
    if (!is.null(rn)) {
      rn <- as.character(rn)
    }
    
    col_names <- names(obj)
    
    # Recycle or check widths, aligns, digits
    n_cols <- length(col_names)
    if (length(width) == 1) {
      width <- rep(width, n_cols)
    } else if (length(width) != n_cols) {
      stop("Length of width must be 1 or equal to number of columns.")
    }
    if (length(align) == 1) {
      align <- rep(align, n_cols)
    } else if (length(align) != n_cols) {
      stop("Length of align must be 1 or equal to number of columns.")
    }
    if (length(digits) == 1) {
      digits <- rep(digits, n_cols)
    } else if (length(digits) != n_cols) {
      stop("Length of digits must be 1 or equal to number of columns.")
    }
    
    obj[] <- lapply(seq_along(obj), function(i) {
      col <- obj[[i]]
      if (is.numeric(col)) {
        sprintf(paste0("%.", digits[i], "f"), col)
      } else {
        as.character(col)
      }
    })
    names(obj) <- col_names
    
    col_widths <- vapply(seq_along(obj), function(i) {
      max(nchar(c(col_names[i], obj[[i]])), na.rm = TRUE)
    }, integer(1))
    col_widths <- pmax(col_widths, width)
    
    structure(obj,
              class = c("tableR", "data.frame"),
              width = col_widths,
              align = align,
              padding = padding,
              space = space,
              caption = caption,
              row_names = rn)
  }
  
  if (is.list(x) && !inherits(x, "data.frame")) {
    nms <- names(x)
    out <- lapply(seq_along(x), function(i) {
      tbl <- to_table(x[[i]])
      if (!is.null(caption)) {
        if (length(caption) == length(x)) {
          attr(tbl, "caption") <- caption[[i]]
        } else {
          attr(tbl, "caption") <- caption[[1]]
        }
      } else if (is.null(attr(tbl, "caption"))) {
        if (!is.null(nms) && nms[i] != "") {
          attr(tbl, "caption") <- nms[i]
        } else {
          attr(tbl, "caption") <- paste0("Table ", i)
        }
      }
      tbl
    })
    class(out) <- "tableR_list"
    attr(out, "space") <- space
    return(out)
  }
  
  to_table(x)
}

#' Print a formatted table object to console
#'
#' @param x A formatted table object created by \code{format_table}.
#' @param ... Additional arguments (currently ignored).
#'
#' @noRd
print.tableR <- function(x, ...) {
  style_plain(x, caption = attr(x, "caption"), space = attr(x, "space"))
  invisible(x)
}

#' Print a list of formatted table objects to console
#'
#' @param x A list of formatted table objects created by \code{format_table}.
#' @param ... Additional arguments (currently ignored).
#'
#' @noRd
print.tableR_list <- function(x, ...) {
  style_plain(x, caption = attr(x, "caption"), space = attr(x, "space"))
  invisible(x)
}

#' knitr Print Method for `tableR` Objects
#'
#' This method allows `tableR` formatted tables to render cleanly in `knitr`-based documents
#' (such as R Markdown). It uses `style_plain()` to generate a plain-text table and outputs it
#' as-is within the document.
#'
#' @param x A `tableR` object created by \code{\link{format_table}}.
#' @param ... Additional arguments passed to methods (ignored here).
#'
#' @return An object of class \code{knitr_asis}, rendering the table as preformatted text.
#'
#' @export
knit_print.tableR <- function(x, ...) {
  out <- paste(style_plain(x), collapse = "\n")
  knitr::asis_output(out)
}

#' knitr Print Method for `tableR_list` Objects
#'
#' This method allows lists of `tableR` formatted tables to render cleanly in `knitr`-based documents
#' (such as R Markdown). It uses `style_plain()` to generate plain-text tables for each list element.
#'
#' @param x A `tableR_list` object created by \code{\link{format_table}}.
#' @param ... Additional arguments passed to methods (ignored here).
#'
#' @return An object of class \code{knitr_asis}, rendering the tables as preformatted text.
#'
#' @export
knit_print.tableR_list <- function(x, ...) {
  outputs <- lapply(x, function(tbl) {
    paste(capture.output(style_plain(tbl)), collapse = "\n")
  })
  out <- paste(outputs, collapse = "\n\n")
  knitr::asis_output(out)
}
