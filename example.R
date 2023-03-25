source("archeryrating.R")

args <- commandArgs(TRUE)
html_path <- args[1]
n_thread <- args[2]
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

if (is.na(n_thread)) {
  archery_rating_html(
    event_array, event_array, html_path,
    "Hyundai Archery World Cup 2019"
  )
} else {
  archery_rating_html(
    event_array, event_array, html_path,
    "Hyundai Archery World Cup 2019",
    n_thread
  )
}
