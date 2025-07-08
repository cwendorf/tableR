# tableR 
## Initial Examples

# Load the tableR source
source("http://raw.githubusercontent.com/cwendorf/tableR/main/source-tableR.R")

# -------------------------------------------------------------------------
# Section 1: Basic Table Formatting
# -------------------------------------------------------------------------

# Example 1.1: Simple Numeric Table
df_1.1 <- data.frame(
  A = c(1.234, 5.678),
  B = c(9.876, 3.210)
)
format_table(df_1.1)

# Example 1.2: Mixed Types with Row Names
df_1.2 <- data.frame(
  Score = c(98.456, 77.8),
  Passed = c(TRUE, FALSE)
)
rownames(df_1.2) <- c("Alice", "Bob")
format_table(df_1.2, digits = 1)

# -------------------------------------------------------------------------
# Section 2: Advanced Table Formatting
# -------------------------------------------------------------------------

# Example 2.1: Different Digits per Column
df_2.1 <- data.frame(
  A = c(12.3456, 7.89123),
  B = c(0.000123, 5000.1),
  C = c(1000.1, 3.14159)
)
format_table(df_2.1, digits = c(1, 4, 2))

# Example 2.2: Custom Alignment and Widths
df_2.2 <- data.frame(
  City = c("New York", "LA"),
  Pop = c(8419600, 3980400),
  Growth = c(0.01, -0.005)
)
rownames(df_2.2) <- c("2023", "2024")
format_table(df_2.2,
             digits = c(0, 0, 3),
             width = c(12, 10, 10),
             align = c("left", "right", "center"))

# Example 2.3: Center Alignment and Truncation
df_2.3 <- data.frame(
  Label = c("SuperLongLabel", "Short"),
  Value = c(3.14159, 2.71828)
)
format_table(df_2.3,
             digits = 2,
             width = c(12, 12),
             align = c("center", "right"))

# -------------------------------------------------------------------------
# Section 3: Non-Data Frame Inputs
# -------------------------------------------------------------------------

# Example 3.1: Numeric Matrix
mat_3.1 <- matrix(c(1.2345, 6.789, 0.00123, 456.7), nrow = 2)
format_table(mat_3.1, digits = 2)

# Example 3.2: Character Matrix
mat_3.2 <- matrix(c("apple", "banana", "kiwi", "melon"), nrow = 2)
format_table(mat_3.2)

# Example 3.3: Named Vector
vec_3.3 <- c(a = 1.234, b = 5.678)
format_table(vec_3.3, digits = 1)

# Example 3.4: Unnamed Vector
vec_3.4 <- c(10.1, 20.2)
format_table(vec_3.4)

# Example 3.5: Named List with Equal-Length Elements
lst_3.5 <- list(A = c(1.23, 4.56), B = c(7.89, 0.12))
format_table(lst_3.5, digits = 2)

# -------------------------------------------------------------------------
# Section 4: Table Styling and Printing
# -------------------------------------------------------------------------

# Example 4.1: Create Raw Data Frame
df_4.1 <- data.frame(
  M = c(12.3456, 7.89123),
  SD = c(0.000123, 5000.1),
  N = c(1000, 315),
  row.names = c("Group 1", "Group 2")
)

# Example 4.2: Print Without Formatting
df_4.1

# Example 4.3: Format and Unclass
df_4.1 |>
  format_table(digits = c(1, 4, 2)) |>
  unclass()

# Example 4.4: Default Formatted Print
df_4.1 |> format_table()
df_4.1 |> format_table(digits = c(1, 4, 0))

# Example 4.5: Console Styling
df_4.1 |>
  format_table(digits = c(1, 4, 0)) |>
  style_console(caption = "Table 1: Summary", space = c(1, 1))

# Example 4.6: Markdown Styling
df_4.1 |>
  format_table(digits = c(1, 4, 0)) |>
  style_markdown(caption = "### Table 1: Summary", space = c(1, 2))

# Example 4.7: APA Styling
df_4.1 |>
  format_table(digits = c(1, 4, 0)) |>
  style_apa("Table 1\nDescriptive Statistics", space = 3)

# -------------------------------------------------------------------------
# Section 5: Saving Formatted Tables
# -------------------------------------------------------------------------

# Example 5.1: View Working Directory for the Sake of Finding Files
getwd()

# Example 5.2: Save Console-Styled Table
df_4.1 |>
  format_table(digits = 2) |>
  style_console(caption = "My Table") |>
  save_output(file = "my_table.txt")

# Example 5.3: Save APA-Styled Table
df_4.1 |>
  format_table(digits = 2) |>
  style_apa(caption = "My Table") |>
  save_output(file = "my_apa_table.txt")

# Example 5.4: Save Markdown-Styled Table
df_4.1 |>
  format_table(digits = 2) |>
  style_markdown(caption = "My Table") |>
  save_output(file = "my_table.md")
