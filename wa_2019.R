source('archeryrating.R')
path = 'wa_2019'
if (!file.exists(path)) {
  dir.create(path)
}
recurveEventArray = c(
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
compoundEventArray = c(
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
archeryRatingHtml(recurveEventArray, compoundEventArray, path, "WA Archery Rating 2019")
