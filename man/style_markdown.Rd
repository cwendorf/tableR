% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/styler.R
\name{style_markdown}
\alias{style_markdown}
\title{Format a 'tableR' Object as a Markdown Table}
\usage{
style_markdown(formatted, caption = NULL, space = c(1, 1))
}
\arguments{
\item{formatted}{A \code{"tableR"} object (or \code{"tableR_list"}) created by \code{format_table()}.}

\item{caption}{Optional character string specifying a caption to print above the table.
Defaults to the \code{"caption"} attribute of the object, if any.}

\item{space}{Integer vector of length 1 or 2 specifying the number of blank lines to add
before and after the table output. If length 1, the value is recycled.}
}
\value{
Invisibly returns a character vector of the markdown table lines.
        Also prints the markdown table to the console.
}
\description{
Converts a formatted table (of class \code{"tableR"}) into a markdown-formatted table string.
Supports optional captions and spacing around the output.
}
