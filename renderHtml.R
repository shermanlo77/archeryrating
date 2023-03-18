# Copyright (C) 2019-2020 Sherman Lo
# https://github.com/shermanlo77
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
#
# Contains functions and procedures for rendering HTML pages

library(tableHTML)

# Return html code for the start of the html file, includes <head> and starts
# <body> and main <div>
# Args:
#   title: the title of the html page
#   n_up_to_css: number of directories to go up to access the css directory
header <- function(title) {
  html <- paste0(
    "<!DOCTYPE html>\n",
    '<html lang="en">\n',
    "  <head>\n",
    "    <title>", title, " - Archery Rating</title>\n",
    '    <meta charset="UTF-8">\n',
    '    <meta name="viewport" content="width=device-width, initial-scale=1">\n',
    '    <link rel="stylesheet" href="https://www.w3schools.com/w3css/4/w3.css">\n',
    '    <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Raleway">\n',
    '    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">\n',
    '    <style>html,body,h1,h2,h3,h4,h5 {font-family: "Raleway", sans-serif}</style>\n',
    "  </head>\n",
    "\n",
    '  <body class="w3-light-grey">\n',
    '    <div class="w3-main">'
  )
  return(html)
}

# Return the final few lines of the html file
ender <- function() {
  html <- paste0(
    "    </div>\n",
    "  </body>\n",
    "</html>\n"
  )
  return(html)
}

# Return html code for the footer of the html page. Contains miscellaneous
# information such as license, github, time of rendering, versions
footer <- function(footer_notes = NULL) {
  html <- paste0(
    "      <br>\n",
    '      <div class="w3-container w3-dark-grey w3-padding-32">\n',
    '        <div class="w3-row">\n',
    "          GPL-3.0 License\n",
    "          <br>\n"
  )

  # add optional footer_notes
  if (!is.null(footer_notes)) {
    html <- paste0(
      html,
      "         ", footer_notes, "\n",
      "          <br>\n"
    )
  }

  html <- paste0(
    html,
    "          Rendered: ", Sys.time(), "\n",
    "          <br>\n",
    '          <a href="https://github.com/shermanlo77/archeryrating">Github: https://github.com/shermanlo77/archeryrating</a>', " @", system("git describe --tags --always", intern = TRUE), "\n",
    "          <br>\n",
    "          ", R.Version()$version.string, "\n",
    "          <br>\n",
    "          PlackettLuce version ", packageVersion("PlackettLuce"), "\n",
    "          <br>\n",
    "          Data from Ianseo may be cleaned and corrected manually\n",
    "       </div>\n",
    "      </div>\n"
  )
  return(html)
}

# Returns html code for a linked button
# Args:
#   text: text for the button
#   url: where to link to button
link_to_button <- function(text, url) {
  html <- paste0(
    '<form style="display: inline" action="', url,
    '" method="get"><button class="w3-button w3-blue">', text,
    " &#10140;</button></form>"
  )
  return(html)
}

# Returns html code for a linked button to a Ianseo page
# Args:
#   event_number: integer
link_to_ianseo <- function(event_number) {
  event_number <- as.character(event_number)
  html <- paste0(
    '<form style="display: inline" action="https://www.ianseo.net/Details.php" ',
    'method="get"><button class="w3-button w3-blue" type="submit" value="',
    event_number,
    '" name="toId">Ianseo &#10140;</button></form>'
  )
  return(html)
}

# Converts a table (or matrix) into a html table
table_to_html <- function(table) {
  # the table may include html code, so use escape=FALSE
  html <- tableHTML(table,
    escape = FALSE,
    class = "'w3-table w3-striped w3-bordered w3-border w3-hoverable w3-white'",
    rownames = FALSE
  )
  # tableHTML puts in additional unwanted borders, ids, styles, ...etc so remove
  # them
  html <- gsub('style="border-collapse:collapse;"', "", html)
  html <- gsub("border=1", "", html)
  for (i in 1:8) {
    html <- gsub(paste0(' id="tableHTML_column_', i, '"'), "", html)
  }
  for (i in 1:8) {
    html <- gsub(paste0(' id="tableHTML_header_', i, '"'), "", html)
  }
  return(html)
}

# Creates and saves a html file for the homepage of a season
# Links to all events and categories of the season
# Args:
#   category_bowtype_link: array of links to each category in order:
#     RM, RW, CM, CW
#   event_array_matrix: R table or matrix of events (and links) to display
#   location: where to save the html file
render_homepage <- function(title, category_bowtype_link, event_array_matrix,
                            location, footer_notes = NULL) {
  cat(paste0("Rendering ", location, "\n"))
  file_conn <- file(location)

  categories <- paste0(
    "        <h2>Categories</h2>",
    "        ", link_to_button("Men's Recurve", category_bowtype_link[1]), "<p>\n",
    "        ", link_to_button("Women's Recurve", category_bowtype_link[2]), "<p>\n",
    "        ", link_to_button("Men's Compound", category_bowtype_link[3]), "<p>\n",
    "        ", link_to_button("Women's Compound", category_bowtype_link[4]), "<p>\n"
  )
  if (length(category_bowtype_link) > 4) {
    categories <- paste0(
      categories,
      "        ", link_to_button("Men's Barebow", category_bowtype_link[5]), "<p>\n",
      "        ", link_to_button("Women's Barebow", category_bowtype_link[6]), "<p>\n"
    )
  }

  html <- paste0(
    header(title),
    "\n",
    '      <header class="w3-container" style="padding-top:22px">\n',
    "        <h1>", title, "</h1>\n",
    "        ", link_to_button("Home", "../index.html"), "<p>\n",
    "        ", link_to_button("Guide", "../guide.html"), "<p>\n",
    categories,
    "      </header>\n",
    '      <div class="w3-container">\n',
    "        <h2>Events</h2>\n",
    "        ", table_to_html(event_array_matrix), "\n\n",
    "      </div>\n",
    footer(footer_notes),
    ender()
  )
  writeLines(html, file_conn)
  close(file_conn)
}

# Creates and saves a html file for the ranks of a category
# Args:
#   category_bowtype: the name of they category (in full English)
#   table: R table or matrix of results (and links) to display
#   location: where to save the html file
render_rank <- function(category_bowtype, table, location,
                        footer_notes = NULL) {
  cat(paste0("Rendering ", location, "\n"))
  file_conn <- file(location)
  html <- paste0(
    header(category_bowtype),
    "\n",
    '      <header class="w3-container" style="padding-top:22px">\n',
    "        <h1>", category_bowtype, "</h1>\n",
    "        ", link_to_button("This season", "index.html"), "<p>\n",
    "      </header>\n",
    '      <div class="w3-container">\n',
    table_to_html(table), "\n\n",
    "      </div>\n",
    footer(footer_notes),
    ender()
  )
  writeLines(html, file_conn)
  close(file_conn)
}

# Creates and saves a html file for an event
# Args:
#   event: the name of the event (in full English)
#   event_number: integer
#   category_bowtype: the name of the category (in full English)
#   format: string the format (qualification, elimiation)
#   results: R table or matrix of results (and links) to display
#   location: where to save the html file
render_event <- function(event, event_number, category_bowtype, format, results,
                         location, footer_notes = NULL) {
  cat(paste0("Rendering ", location, "\n"))
  file_conn <- file(location)
  html <- paste0(
    header(event),
    "\n",
    '      <header class="w3-container" style="padding-top:22px">\n',
    "        <h1>", event, "</h1>\n",
    "        ", link_to_button("This season", "../index.html"), "<p>\n",
    "        ", link_to_ianseo(event_number), "\n",
    "      </header>\n",
    '      <div class="w3-container">\n',
    "        <h2>", paste0(category_bowtype, " - ", format), "</h2>\n",
    table_to_html(results), "\n\n",
    "      </div>\n",
    footer(footer_notes),
    ender()
  )
  writeLines(html, file_conn)
  close(file_conn)
}

# Creates and saves a html file for each individual
# Args:
#   archer: the name of the event (in full English)
#   country: string country code
#   category_bowtype: the name of their category (in full English)
#   archer_rank: int, rank of this archer
#   archer_points: int, number of points
#   event_table: R table or matrix of events attended (and links) to display
#   pairwise_table: R table or matrix of pairwise comparisons (and links) to
#     display
#   location: where to save the html file
render_individual <- function(archer, country, category_bowtype, archer_rank,
                              archer_points, event_table, pairwise_table,
                              location, footer_notes = NULL) {
  cat(paste0("Rendering ", location, "\n"))
  file_conn <- file(location)
  title <- paste(archer, "-", country)
  html <- paste0(
    header(title),
    "\n",
    '      <header class="w3-container" style="padding-top:22px">\n',
    "        <h1>", title, "</h1>\n",
    "        ", link_to_button("This season", "../index.html"), "\n",
    "      </header>\n",
    '      <div class="w3-container">\n',
    "        <h2>Rating Rank: ", archer_rank, "</h2>\n",
    "        <h2>Rating Points: ", archer_points, "</h2>\n",
    "      </div>\n",
    '      <div class="w3-container">\n',
    "        <h3>Events</h3>\n",
    table_to_html(event_table), "\n\n",
    "      </div>\n",
    '      <div class="w3-container">\n',
    "        <h3>Pairwise comparisons</h3>\n",
    table_to_html(pairwise_table), "\n\n",
    "      </div>\n",
    footer(footer_notes),
    ender()
  )
  writeLines(html, file_conn)
  close(file_conn)
}
