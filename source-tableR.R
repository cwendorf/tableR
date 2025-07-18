# Purpose: Source all R functions from a GitHub repo
# Updated: 2023-07-17

source.github <- function(username, repo, branch) {
  url <- paste("https://github.com/", username, "/", repo, "/archive/refs/heads/", branch, ".zip", sep = "")
  name <- paste(repo, "-", branch, sep = "")
  file <- paste(name, ".zip", sep = "")
  download.file(url = url, destfile = file)
  unzip(zipfile = file)
  folder <- paste(name, "/R/", sep = "")
  paths <- list.files(path = folder, full.names = TRUE)
  for (i in 1:length(paths)) source(paths[i])
  unlink(file)
  unlink(name, recursive = TRUE)
}

# Call for this Repo
source.github("cwendorf", "tableR", "main")
