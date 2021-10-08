source('archeryrating.R')
path = 'uk_2021'
if (!file.exists(path)) {
  dir.create(path)
}
eventArray = c(
  8355,
  8493,
  8494,
  8545,
  8621,
  8763,
  8764,
  8543,
  8925,
  8929,
  8638,
  9013,
  9072,
  9073,
  9268
)
archeryRatingHtml(eventArray, eventArray, path, "UK Archery Rating 2021")
