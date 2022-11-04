source("archeryrating.R")
path <- "wa_2019"
if (!file.exists(path)) {
  dir.create(path)
}
recurve_event_array <- c(
  5316,
  5335,
  4791,
  4792,
  4795,
  4797,
  4799,
  4802,
  4803,
  4809,
  4810,
  5958,
  6113
)
compound_event_array <- c(
  5316,
  5335,
  4791,
  4792,
  4795,
  4797,
  4799,
  4802,
  4803,
  4809,
  5958,
  6113
)
archery_rating_html(
  recurve_event_array, compound_event_array, path, "WA Archery Rating 2019"
)
