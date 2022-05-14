source('archeryrating.R')
path = 'uk_2022'
if (!file.exists(path)) {
  dir.create(path)
}
eventArray = c(
  10337,
  10571
)
barebowArray = c(
  10571
)
archeryRatingHtml(eventArray, eventArray, path, "UK Archery Rating 2022",
                  barebowEventArray=barebowArray)
