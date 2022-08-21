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

library(tableHTML);

#FUNCTION: HEADER
#Return html code for the start of the html file, includes <head> and starts <body> and main <div>
#PARAMETERS:
  #title: the title of the html page
  #nUpToCss: number of directories to go up to access the css directory
header = function(title, nUpToCss=0) {
  cssPrefix = paste0(paste0(replicate(nUpToCss, "../"), collapse=""), "css/");
  html = paste0(
      '<!DOCTYPE html>\n',
      '<html lang="en">\n',
      '  <head>\n',
      '    <title>', title, ' - Archery Rating</title>\n',
      '    <meta charset="UTF-8">\n',
      '    <meta name="viewport" content="width=device-width, initial-scale=1">\n',
      '    <link rel="stylesheet" href="', cssPrefix, 'w3.css">\n',
      '    <link rel="stylesheet" href="', cssPrefix, 'css.css">\n',
      '    <link rel="stylesheet" href="', cssPrefix, 'font-awesome.min.css">\n',
      '    <style>html,body,h1,h2,h3,h4,h5 {font-family: "Raleway", sans-serif}</style>\n',
      '  </head>\n',
      '\n',
      '  <body class="w3-light-grey">\n',
      '    <div class="w3-main">');
  return(html);
}

#FUNCTION: END
#Return the final few lines of the html file. Used in conjunction with
ender = function() {
  html = paste0(
      '    </div>\n',
      '  </body>\n',
      '</html>\n'
  );
  return(html);
}

#FUNCTION: FOOTER
#Return html code for the footer of the html page. Contains miscellaneous information such as
#license, github, time of rendering, versions
footer = function() {
  html = paste0(
    '      <br>\n',
    '      <div class="w3-container w3-dark-grey w3-padding-32">\n',
    '        <div class="w3-row">\n',
    '          GPL-3.0 License\n',
    '          <br>\n',
    '          <a href="https://github.com/shermanlo77/archeryrating">Github: https://github.com/shermanlo77/archeryrating</a>', ' @', system("git rev-parse HEAD", intern=TRUE), '\n',
    '          <br>\n',
    '          Rendered: ', Sys.time(), '\n',
    '          <br>\n',
    '          ', R.Version()$version.string, '\n',
    '          <br>\n',
    '          PlackettLuce version ', packageVersion("PlackettLuce"), '\n',
    '          <br>\n',
    '          Data from Ianseo may be cleaned and corrected manually. Data cleaning logs may be available in the repository or the homepage.\n',
    '       </div>\n',
    '      </div>\n');
  return(html);
}

#FUNCTION: LINK TO BUTTON
#Returns html code for a linked button
#PARAMETERS:
  #text: text for the button
  #url: where to link to button
linkToButton = function(text, url) {
  html = paste0('<form style="display: inline" action="', url,
                '" method="get"><button class="w3-button w3-blue">', text,
                ' &#10140;</button></form>');
  return(html);
}

#FUNCTION: LINK TO IANSEO
#Returns html code for a linked button to a Ianseo page
#PARAMETERS:
  #eventNumber: integer
linkToIanseo = function(eventNumber) {
  eventNumber = as.character(eventNumber);
  html = paste0('<form style="display: inline" action="https://www.ianseo.net/Details.php" ',
                'method="get"><button class="w3-button w3-blue" type="submit" value="', eventNumber,
                '" name="toId">Ianseo &#10140;</button></form>');
  return(html);
}

#FUNCTION: TABLE TO HTML
#Converts a table (or matrix) into a html table
tableToHtml = function(table) {
  #the table may include html code, so use escape=FALSE
  html = tableHTML(table, escape=FALSE,
                   class="'w3-table w3-striped w3-bordered w3-border w3-hoverable w3-white'",
                   rownames=FALSE);
  #tableHTML puts in additional unwanted borders, ids, styles, ...etc so remove them
  html = gsub('style="border-collapse:collapse;"' , '', html);
  html = gsub('border=1', '', html);
  for (i in 1:8) {
    html = gsub(paste0(' id="tableHTML_column_', i, '"'), '', html);
  }
  for (i in 1:8) {
    html = gsub(paste0(' id="tableHTML_header_', i, '"'), '', html);
  }
  return(html);
}

#PROCEDURE: RENDER HOMEPAGE
#Creates and saves a html file for the homepage of a season
#Links to all events and categories of the season
#PARAMETERS:
  #categoryBowtypeLink: array of links to each category in order: RM, RW, CM, CW
  #eventArrayMatrix: R table or matrix of events (and links) to display
  #location: where to save the html file
renderHomepage = function(title, categoryBowtypeLink, eventArrayMatrix, location) {
  cat(paste0("Rendering ", location, "\n"));
  fileConn = file(location);

  categories = paste0(
    '        <h2>Categories</h2>',
    '        ', linkToButton("Men's Recurve", categoryBowtypeLink[1]), '<p>\n',
    '        ', linkToButton("Women's Recurve", categoryBowtypeLink[2]), '<p>\n',
    '        ', linkToButton("Men's Compound", categoryBowtypeLink[3]), '<p>\n',
    '        ', linkToButton("Women's Compound", categoryBowtypeLink[4]), '<p>\n'
  )
  if (length(categoryBowtypeLink) > 4) {
    categories = paste0(
      categories,
      '        ', linkToButton("Men's Barebow", categoryBowtypeLink[5]), '<p>\n',
      '        ', linkToButton("Women's Barebow", categoryBowtypeLink[6]), '<p>\n')
  }

  html = paste0(
    header(title, 1),
    '\n',
    '      <header class="w3-container" style="padding-top:22px">\n',
    '        <h1>', title, '</h1>\n',
    '        ', linkToButton("Home", '../index.html'), '<p>\n',
    '        ', linkToButton("Guide", '../guide.html'), '<p>\n',
    categories,
    '      </header>\n',
    '      <div class="w3-container">\n',
    '        <h2>Events</h2>\n',
    '        ', tableToHtml(eventArrayMatrix), '\n\n',
    '      </div>\n',
    footer(),
    ender()
  );
  writeLines(html, fileConn);
  close(fileConn);
}

#PROCEDURE: RENDER RANK
#Creates and saves a html file for the ranks of a category
#PARAMETERS:
  #categoryBowtype: the name of they category (in full English)
  #table: R table or matrix of results (and links) to display
  #location: where to save the html file
renderRank = function(categoryBowtype, table, location) {
  cat(paste0("Rendering ", location, "\n"));
  fileConn = file(location);
  html = paste0(
    header(categoryBowtype, 1),
    '\n',
    '      <header class="w3-container" style="padding-top:22px">\n',
    '        <h1>', categoryBowtype, '</h1>\n',
    '        ', linkToButton("This season", "index.html"), '<p>\n',
    '      </header>\n',
    '      <div class="w3-container">\n',
    tableToHtml(table), '\n\n',
    '      </div>\n',
    footer(),
    ender()
  );
  writeLines(html, fileConn);
  close(fileConn);
}

#PROCEDURE: RENDER EVENT
#Creates and saves a html file for an event
#PARAMETERS:
  #event: the name of the event (in full English)
  #eventNumber: integer
  #categoryBowtype: the name of the category (in full English)
  #format: string the format (qualification, elimiation)
  #results: R table or matrix of results (and links) to display
  #location: where to save the html file
renderEvent = function(event, eventNumber, categoryBowtype, format, results, location) {
  cat(paste0("Rendering ", location, "\n"));
  fileConn = file(location);
  html = paste0(
    header(event, 2),
    '\n',
    '      <header class="w3-container" style="padding-top:22px">\n',
    '        <h1>', event, '</h1>\n',
    '        ', linkToButton("This season", "../index.html"), '<p>\n',
    '        ', linkToIanseo(eventNumber), '\n',
    '      </header>\n',
    '      <div class="w3-container">\n',
    '        <h2>', paste0(categoryBowtype, ' - ', format), '</h2>\n',
    tableToHtml(results), '\n\n',
    '      </div>\n',
    footer(),
    ender()
  );
  writeLines(html, fileConn);
  close(fileConn);
}

#PROCEDURE: RENDER INDIVIDUAL
#Creates and saves a html file for each individual
#PARAMETERS:
  #archer: the name of the event (in full English)
  #country: string country code
  #categoryBowtype: the name of their category (in full English)
  #archerRank: int, rank of this archer
  #archerPoints: int, number of points
  #eventTable: R table or matrix of events attended (and links) to display
  #pairwiseTable: R table or matrix of pairwise comparisons (and links) to display
  #location: where to save the html file
renderIndividual = function(archer, country, categoryBowtype, archerRank, archerPoints, eventTable,
                            pairwiseTable, location) {
  cat(paste0("Rendering ", location, "\n"));
  fileConn = file(location);
  title = paste(archer, '-', country);
  html = paste0(
    header(title, 2),
    '\n',
    '      <header class="w3-container" style="padding-top:22px">\n',
    '        <h1>', title, '</h1>\n',
    '        ', linkToButton("This season", "../index.html"), '\n',
    '      </header>\n',
    '      <div class="w3-container">\n',
    '        <h2>Rating Rank: ', archerRank,'</h2>\n',
    '        <h2>Rating Points: ', archerPoints, '</h2>\n',
    '      </div>\n',
    '      <div class="w3-container">\n',
    '        <h3>Events</h3>\n',
    tableToHtml(eventTable), '\n\n',
    '      </div>\n',
    '      <div class="w3-container">\n',
    '        <h3>Pairwise comparisons</h3>\n',
    tableToHtml(pairwiseTable), '\n\n',
    '      </div>\n',
    footer(),
    ender()
  );
  writeLines(html, fileConn);
  close(fileConn);
}
