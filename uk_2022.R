source("archeryrating.R")
path <- "uk_2022"
if (!file.exists(path)) {
  dir.create(path)
}
event_array <- c(
  10337,
  10571,
  10678
)
barebow_men_event_array <- c(
  10571,
  10678
)
barebow_women_event_array <- c(
  10571
)
archery_rating_html(event_array, event_array, path, "UK Archery Rating 2022",
  barebow_men_event_array = barebow_men_event_array,
  barebow_women_event_array = barebow_women_event_array
)
