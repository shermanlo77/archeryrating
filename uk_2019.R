source("archeryrating.R")
path <- "uk_2019"
if (!file.exists(path)) {
  dir.create(path)
}
event_array <- c(
  5434,
  5049,
  5701,
  5728,
  5677,
  5696,
  5814,
  5815,
  5905,
  5822,
  5416,
  6018,
  6033,
  6025,
  6055,
  6084
)
archery_rating_html(event_array, event_array, path, "UK Archery Rating 2019")
