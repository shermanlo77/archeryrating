source("archeryrating.R")

args <- commandArgs(TRUE)
html_path <- args[1]
if (is.na(html_path)) {
  html_path <- "example"
}

if (!file.exists(html_path)) {
  dir.create(html_path)
}
event_array <- c(
  4792,
  4795,
  4797,
  4803
)
archery_rating_html(event_array, event_array, html_path, "2019 World Cup")