source('archeryrating.R')
path = 'uk_2022'
if (!file.exists(path)) {
  dir.create(path)
}
eventArray = c(
  10337,
  10571,
  10678
)
barebowMenEventArray = c(
  10571,
  10678
)
barebowWomenEventArray = c(
  10571
)
archeryRatingHtml(eventArray, eventArray, path, "UK Archery Rating 2022",
                  barebowMenEventArray=barebowMenEventArray,
                  barebowWomenEventArray=barebowWomenEventArray)
