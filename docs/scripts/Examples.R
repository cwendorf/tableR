# tableR
## Examples

source("http://raw.githubusercontent.com/cwendorf/tableR/main/source-tableR.R")

# Example 1: Simple Numeric Table
df1 <- data.frame(
  A = c(1.234, 5.678),
  B = c(9.876, 3.210)
)
format_table(df1)


# Example 2: Mixed Types with Row Names
df2 <- data.frame(
  Score = c(98.456, 77.8),
  Passed = c(TRUE, FALSE)
)
rownames(df2) <- c("Alice", "Bob")
format_table(df2, digits = 1)


# Example 3: Different Digits per Column
df3 <- data.frame(
  A = c(12.3456, 7.89123),
  B = c(0.000123, 5000.1),
  C = c(1000.1, 3.14159)
)
format_table(df3, digits = c(1, 4, 2))


# Example 4: Custom Alignment and Widths
df4 <- data.frame(
  City = c("New York", "LA"),
  Pop = c(8419600, 3980400),
  Growth = c(0.01, -0.005)
)
rownames(df4) <- c("2023", "2024")
format_table(df4,
            digits = c(0, 0, 3),
            width = c(12, 10, 10),
            align = c("left", "right", "center"))

# Example 5: Center Alignment and Truncation
df5 <- data.frame(
  Label = c("SuperLongLabel", "Short"),
  Value = c(3.14159, 2.71828)
)
format_table(df5,
            digits = 2,
            width = c(12, 12),
            align = c("center", "right"))




# Ensure your format_table() function is defined before running these examples

# Example 1: Numeric Matrix
mat <- matrix(c(1.2345, 6.789, 0.00123, 456.7), nrow = 2)
format_table(mat, digits = 2)


# Example 2: Character Matrix
char_mat <- matrix(c("apple", "banana", "kiwi", "melon"), nrow = 2)
format_table(char_mat)


# Example 3: Named Vector
v <- c(a = 1.234, b = 5.678)
format_table(v, digits = 1)


# Example 4: Unnamed Vector
v2 <- c(10.1, 20.2)
format_table(v2)


# Example 5: Named List with Equal-Length Elements
lst <- list(A = c(1.23, 4.56), B = c(7.89, 0.12))
format_table(lst, digits = 2)



# Investigating the print methods

df <- data.frame(
  M = c(12.3456, 7.89123),
  SD = c(0.000123, 5000.1),
  N = c(1000, 315),
  row.names = c("Group 1", "Group 2")
)

# Without formatting
df

# Formatting without class
df |>
  format_table(digits = c(1, 4, 2)) |> unclass()

# Default print
df |>
  format_table()
df |>
  format_table(digits = c(1, 4, 0))

# Console printing with caption
df |>
  format_table(digits = c(1, 4, 0)) |>
  style_console(caption = "Table 1: Summary", space = c(1, 1))

df |>
  format_table(digits = c(1, 4, 0)) |>
  style_markdown(caption = "### Table 1: Summary", space = c(1,2))

df |>
  format_table(digits = c(1, 4, 0)) |>
  style_apa("Table 1\nDescriptive Statistics", space = 3)



## save

getwd()

df |> 
  format_table(digits = 2) |> 
  style_console(caption = "My Table") |> 
  save_output(file = "my_table.txt")

df |> 
  format_table(digits = 2) |> 
  style_apa(caption = "My Table") |> 
  save_output(file = "my_apa_table.txt")

df |> 
  format_table(digits = 2) |> 
  style_markdown(caption = "My Table") |> 
  save_output(file = "my_table.md")
