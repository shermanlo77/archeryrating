# Copyright (C) 2019-2022 Sherman Lo
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

library(PlackettLuce)
library(readr)
library(stringi)
library(foreach)
library(doParallel)
source("cleannames.R")
source("renderHtml.R")

# Process the provided .csv files from IanseoParse and create a website
# containing the plackett luce rating scores. It creates categories:
#     men's recurve
#     women's recurve
#     men's compound
#     women's compound
#     optional: men's barebow
#     optional: women's barebow
# The plackett luce worth scores are translate to elo-like scores, see the
# function get_points()
# Note: this is computationally intensive, use of multiple threads is
# recommended and automatically used
#
# Args:
#   recurve_event_array: list of number codes (int) for each Ianseo recurve
#     event, must be in chronological order. They are also the name of
#     directories to be searched for .csv and .txt files
#   compound_event_array: list of number codes (int) for each Ianseo compound
#     event, must be in chronological order. They are also the name of
#     directories to be searched for .csv and .txt files
#   html_path: name of directory where the html files are saved
#   title: string, the name given to the collection of archery event, eg World
#     Cup, this will be, for example, put on the home page
#   footer_notes: optional string, notes to put in the footer for all html
#     files, for example, copyright notices or version number
#   barebow_men_event_array: optional, list of int for Ianseo men's barebow
#     events
#   barebow_women_event_array: optional, list of int for Ianseo women's barebow
#     events
archery_rating_html <- function(recurve_event_array,
                                compound_event_array,
                                html_path,
                                title,
                                footer_notes = NULL,
                                barebow_men_event_array = NULL,
                                barebow_women_event_array = NULL) {
  registerDoParallel(detectCores())

  # list of links for each category
  # eg "RM.html", "RW.html", "CM.html", "CW.html"
  category_bowtype_link <- c()
  # matrix of events as a html string (may also contain links)
  # to be displayed on the homepage
  #   dim 1: for each event
  #   dim 2: event name, format (elimination or qualification), list of
  #     categories with their links
  event_array_matrix <- matrix(nrow = 0, ncol = 3)
  colnames(event_array_matrix) <- c("Event", "Format", "Categories")

  # the homepage variables are category_bowtype_link and event_array_matrix,
  # they contain all of the information required to build the homepage
  # (index.html)
  homepage_vars <- list(
    category_bowtype_link = category_bowtype_link,
    event_array_matrix = event_array_matrix
  )

  for (bowtype in c("Recurve", "Compound", "Barebow")) {
    for (category in c("Men", "Women")) {

      # event_number_array is a vector of event numbers to iterate through
      # do not proceed if a barebow event is not provided
      if (bowtype == "Recurve") {
        event_number_array <- recurve_event_array
      } else if (bowtype == "Compound") {
        event_number_array <- compound_event_array
      } else if (category == "Men") {
        if (!is.null(barebow_men_event_array)) {
          event_number_array <- barebow_men_event_array
        } else {
          break
        }
      } else {
        if (!is.null(barebow_women_event_array)) {
          event_number_array <- barebow_women_event_array
        } else {
          break
        }
      }

      homepage_vars <- render_bowtype_category(
        homepage_vars, html_path, title, bowtype, category,
        event_number_array, footer_notes
      )
    }
  }

  render_homepage(
    title, homepage_vars$category_bowtype_link,
    homepage_vars$event_array_matrix,
    file.path(html_path, "index.html"),
    footer_notes
  )
}

# Render html pages for a category and bowtype
#
# Render html pages for a category and bowtype (eg Women's Compound), including
# the fitting of the Plackett-Luce model onto the data.
#
# Update the homepage_vars so that the homepage contain information and links to
# the results for this category.
#
# Args:
#   homepage_vars: list containing category_bowtype_link and event_array_matrix,
#     they shall be modified and returned
#   html_path: name of directory where the html files are saved
#   title: string, the name given to the collection of archery event
#   bowtype: string, full name of the bowtype, eg Recurve, Compound
#   category: string, full name of the category, eg Men, Women
#   event_number_array: vector of event numbers. In this implementation it is
#     assumed each event_number consist of both qualification and elimination
#   footer_notes: optional string, notes to put in the footer for all html files
#
# Returns:
#   homepage_vars with updated contents
render_bowtype_category <- function(homepage_vars, html_path, title, bowtype,
                                    category, event_number_array,
                                    footer_notes = NULL) {
  category_bowtype_link <- homepage_vars$category_bowtype_link
  event_array_matrix <- homepage_vars$event_array_matrix

  category_bowtype_code <- get_category_bowtype_code(bowtype, category)
  category_bowtype <- get_category_bowtype_string(bowtype, category)

  # create directory for this category
  if (!file.exists(file.path(html_path, category_bowtype_code))) {
    dir.create(file.path(html_path, category_bowtype_code))
  }

  # create directory for each event number
  for (event_number in event_number_array) {
    if (!file.exists(file.path(html_path, event_number))) {
      dir.create(file.path(html_path, event_number))
    }
  }

  # get rankings and events from Ianseo, reading .csv files done here
  ranks_and_events <- get_rankings_and_events(
    event_number_array, event_array_matrix, html_path,
    category, bowtype
  )

  # for each event, fit plackett luce
  plackett_luce_for_each_event <- fit_online_plackett_luce(ranks_and_events)


  ranks_and_events <- update_ranks_and_events(
    ranks_and_events, plackett_luce_for_each_event
  )

  # render html files for each event
  # get rating points too
  render_events(
    ranks_and_events,
    plackett_luce_for_each_event,
    bowtype, category,
    html_path, footer_notes
  )

  points <- get_points(
    plackett_luce_for_each_event[[length(plackett_luce_for_each_event)]]
  )

  quasi_error <- get_quasi_error(plackett_luce_for_each_event, names(points))

  # sort the points and construct a table showing all archers' points
  points <- sort(points, decreasing = TRUE)
  table_rank <- matrix(nrow = length(points), ncol = 5)
  table_rank[, 1] <- seq_len(length(points))
  table_rank[, 2] <- names(points)
  table_rank[, 3] <- ranks_and_events$country_array[names(points)]
  table_rank[, 4] <- round(points)
  table_rank[, 5] <- paste0("&plusmn;", quasi_error[names(points)])
  table_rank[, 2] <- convert_name_to_html(
    table_rank[, 2], paste0(category_bowtype_code, "/")
  )
  colnames(table_rank) <- c(
    "Rank", "Name", "Country",
    "Points", "Uncertainty"
  )
  render_rank(
    category_bowtype, table_rank,
    file.path(html_path, paste0(category_bowtype_code, ".html")),
    footer_notes
  )

  # link the homepage to the page containing all archers' points
  category_bowtype_link <- c(
    category_bowtype_link,
    paste0(category_bowtype_code, ".html")
  )

  # render a profile page for each archer
  render_archer_profiles(
    ranks_and_events, points, bowtype, category,
    html_path, footer_notes
  )

  homepage_vars <- list(
    category_bowtype_link = category_bowtype_link,
    event_array_matrix = ranks_and_events$event_array_matrix
  )
  return(homepage_vars)
}

# Extract (partial) ranking data and event information from .csv files
#
# Args:
#   event_number_array: vector of event number, used to read files from the
#     correct sub-directory
#   event_array_matrix: a homepage variable, matrix of events as a html string
#     to display on the homepage, this is modified and returned
#   html_path: name of directory where the html files are saved
#   category: string, full name of the category
#   bowtype: string, full name of the bowtype
# Returns: A list of the following variables:
#   event_array_matrix: matrix of events as a html string to display on the
#     homepage
#   rank_matrix: list for ranking data, length 2
#     [[1]] element 1 (rank matrix):
#       matrix of partial ranks, use 0 to denote not attended, each row is
#       for a WA 720 or a H2H match. For a WA 720, the row should contain
#       1, 2, 3, ..., number_of_competitors, and the rest zeros. For a H2H
#       match, it should contain 1, 2 and the rest zeros
#         dim1: for each partial rank (eg, for each WA 720 or each H2H
#           match)
#         dim2: for each archer (named using archers' names)
#     [[2]] element 2:
#       matrix for indicating the event of each row of the rank matrix in
#       element 1
#         dim1: for each row in the rank matrix
#         dim2: size 2, the first column contain the event number, the
#           second column is boolean, 0 or 1, 1 if is_qualification
#   country_array: vector of countries for each archer (names)
#     index names: string, archer's name
#     element: string rep of the event, country code
#   event_array: nested list of details for each event
#     dim1: [[for each WA 720 and H2H event]]
#     dim2:
#       [[1]] contains the name of event as a single string,
#       [[2]] results to display as a data frame
#       [[3]] rating points each archer has as a vector
#         index name: archer's name
#         contains: float, rating points
#   event_array_event_numbers:
#   event_number_is_qual_matrix: matrix, first column contains unique events and
#     the second column contains boolean, 1 if a qualification event, 0 if
#     elimination event, should look something like
#          [,1] [,2]
#     [1,] xxxx    1
#     [1,] xxxx    0
#     [1,] yyyy    1
#     [1,] yyyy    0
#     where each event number has a qualification event and elimination event
get_rankings_and_events <- function(event_number_array, event_array_matrix,
                                    html_path, category, bowtype) {
  rank_matrix <- list(matrix(nrow = 0, ncol = 0), c())
  country_array <- c()
  event_array <- list()
  # indicate the event number for each row in event_array[[]][[2]]
  #   dim1: for each event
  #   dim2:
  #     column 1 contains event number
  #     columns2 contains boolean as 0 or 1, 1 if is_qualification
  event_array_event_numbers <- matrix(nrow = 0, ncol = 2)

  category_bowtype_code <- get_category_bowtype_code(bowtype, category)

  # combine the rank matrices from each event
  # record the country of each archer
  # update the homepage link to events
  for (i_event in seq_len(length(event_number_array))) {
    event_number <- event_number_array[i_event]
    # get information (eg date and location) about the event number
    event <- read_file(file.path(as.character(event_number), "event.txt"))
    event <- paste0(gsub("[\n]", "<br>", event)) # use html new line

    # process qualification, then elimination
    for (is_qualification in c(TRUE, FALSE)) {
      if (is_qualification) {
        format <- "Qualification"
      } else {
        format <- "Elimination"
      }

      # event_summary is a list
      # [[1]]: partial rank matrix for this event, index are name of archers
      # [[2]]: data frame, summary of event results to be presented
      event_summary <- get_rank_row(
        event_number, is_qualification, bowtype, category
      )

      # get the rank matrix for this event
      # merge it/append it/concatenate it with the rank matrix for all
      # previous events
      rank <- event_summary[[1]]
      rank_matrix[[1]] <- combine_rank_matrix(rank_matrix[[1]], rank)
      rank_matrix[[2]] <- rbind(
        rank_matrix[[2]],
        t(matrix(
          rep(c(event_number, is_qualification), nrow(rank)),
          nrow = 2
        ))
      )

      # append points after event and gained at event to the results table
      table_results <- event_summary[[2]]

      # add the name of the event and the summary table to event_array
      event_array[[length(event_array) + 1]] <- list(
        event,
        table_results,
        NULL # to contain rating points later on
      )
      # add the event_number and is_qualification to
      # event_array_event_numbers
      event_array_event_numbers <- rbind(
        event_array_event_numbers,
        c(event_number, is_qualification)
      )

      # save the country for each archer to country_array
      for (i_archer in seq_len(nrow(table_results))) {
        archer <- table_results$Name[i_archer]
        if (length(country_array) == 0) {
          country_array <- c(
            country_array,
            toString(table_results[i_archer, "Country"])
          )
          names(country_array)[length(country_array)] <-
            table_results$Name[i_archer]
        } else if (!is.element(archer, names(country_array))) {
          country_array <- c(
            country_array,
            toString(table_results[i_archer, "Country"])
          )
          names(country_array)[length(country_array)] <-
            table_results$Name[i_archer]
        }
      }

      # link to this event page
      event_pointer <- ((event_array_matrix[, 1] == event) &
        (event_array_matrix[, 2] == format))
      link <- get_event_html(
        category_bowtype_code,
        matrix(c(event_number, is_qualification), ncol = 2), ""
      )
      button <- link_to_button(category_bowtype_code, link)
      # add button to event_array_matrix
      # if the event already exist (because it was done for a previous
      # category), put the button next to the button for the previous
      # category
      if (!any(event_pointer)) {
        event_array_matrix <- rbind(
          c(event, format, button),
          event_array_matrix
        )
      } else {
        event_array_matrix[which(event_pointer), 3] <- paste(
          event_array_matrix[which(event_pointer), 3],
          button
        )
      }
    }
  }
  # get each unique event (duplicate happens because there are multiple matches
  # in an event), should look this
  #      [,1] [,2]
  # [1,] xxxx    1
  # [1,] xxxx    0
  # [1,] yyyy    1
  # [1,] yyyy    0
  #
  # where each event number has a qualification event and elimination event
  event_number_is_qual_matrix <- matrix(unique(rank_matrix[[2]]), ncol = 2)

  ranks_and_events <- list(
    event_array_matrix = event_array_matrix,
    rank_matrix = rank_matrix,
    country_array = country_array,
    event_array = event_array,
    event_array_event_numbers = event_array_event_numbers,
    event_number_is_qual_matrix = event_number_is_qual_matrix
  )
  return(ranks_and_events)
}

# Fit Plackett Luce after each event number
#
# Fit Plackett Luce after each event number, ie fit for one event, then two,
# then three... etc, hence online as in online machine learning). This is done
# in parallel, use of foreach here.
#
# Args:
#   ranks_and_events: return value from get_rankings_and_events()
#
# Returns:
#   a list of PlackettLuce after each event
fit_online_plackett_luce <- function(ranks_and_events) {
  rank_matrix <- ranks_and_events$rank_matrix
  event_number_is_qual_matrix <- ranks_and_events$event_number_is_qual_matrix

  # for each event, fit plackett luce
  plackett_luce_for_each_event <- foreach(
    i_event = seq_len(nrow(event_number_is_qual_matrix))
  ) %dopar% {
    event_number <- event_number_is_qual_matrix[i_event, 1]
    is_qualification <- event_number_is_qual_matrix[i_event, 2]

    # get current event pointer
    rank_matrix_row_pointer <- (
      (rank_matrix[[2]][, 1] == event_number) &
        (rank_matrix[[2]][, 2] == is_qualification)
    )
    # then get all previous event pointers
    rank_matrix_row_pointer[seq_len(which(rank_matrix_row_pointer)[1])] <-
      TRUE
    # get rank matrix
    rank_matrix_at_this_event <- matrix(
      rank_matrix[[1]][rank_matrix_row_pointer, ],
      nrow = sum(rank_matrix_row_pointer)
    )
    # remove zero columns, this happens when the archer has not competed yet
    archer_pointer <- colSums(rank_matrix_at_this_event) > 0
    rank_matrix_at_this_event <- matrix(
      rank_matrix[[1]][rank_matrix_row_pointer, archer_pointer],
      nrow = sum(rank_matrix_row_pointer)
    )
    colnames(rank_matrix_at_this_event) <-
      colnames(rank_matrix[[1]])[archer_pointer]

    # fit plackett luce which is returned via foreach
    plackett_luce <- PlackettLuce(as.rankings(rank_matrix_at_this_event))
  }

  return(plackett_luce_for_each_event)
}

# Update ranks_and_events with rating points
#
# Update ranks_and_events with rating points such as points earned at each
# event, points lost, rank after each event
#
# Args:
#   ranks_and_events: return value of get_rankings_and_events()
#   plackett_luce_for_each_event: return value of fit_online_plackett_luce()
update_ranks_and_events <- function(ranks_and_events,
                                    plackett_luce_for_each_event) {
  event_array <- ranks_and_events$event_array
  event_number_is_qual_matrix <- ranks_and_events$event_number_is_qual_matrix

  # rating points for each archer, updates after each event
  points <- c()
  # would calculate rating points after each event, thus the iteration over
  # events
  for (i_event in seq_len(nrow(event_number_is_qual_matrix))) {
    is_qualification <- event_number_is_qual_matrix[i_event, 2]

    # get the points
    points_after <- get_points(plackett_luce_for_each_event[[i_event]])
    points_before <- rep(1440, length(points_after))
    names(points_before) <- names(points_after)
    points_before[names(points)] <- points
    points_diff <- points_after - points_before
    points <- points_after
    # save the points
    event_array[[i_event]][[3]] <- sort(points, decreasing = TRUE)

    # append points after event and gained at event to the results table
    table_event <- event_array[[i_event]][[2]]
    table_event <- cbind(
      table_event, round(points_after[table_event$Name]),
      round(points_diff[table_event$Name])
    )
    # label columns of the table
    if (is_qualification) {
      colnames(table_event)[5] <- "10+X"
      colnames(table_event)[6] <- "X"
    }
    colnames(table_event)[ncol(table_event) - 1] <- "Points after event"
    colnames(table_event)[ncol(table_event)] <- "Points earned at event"
    rownames(table_event) <- NULL
    event_array[[i_event]][[2]] <- table_event
  }

  ranks_and_events$event_array <- event_array
  return(ranks_and_events)
}


# Render a page for each event
#
# Args:
#   ranks_and_events: return value from update_ranks_and_events()
#   plackett_luce_for_each_event: return value from fit_online_plackett_luce()
#   bowtype: string, full name of the bowtype, eg Recurve, Compound
#   category: string, full name of the category, eg Men, Women
#   html_path: name of directory where the html files are saved
#   footer_notes: optional string, notes to put in the footer for all html files
render_events <- function(ranks_and_events,
                          plackett_luce_for_each_event,
                          bowtype, category,
                          html_path, footer_notes = NULL) {
  event_array <- ranks_and_events$event_array
  event_number_is_qual_matrix <- ranks_and_events$event_number_is_qual_matrix
  category_bowtype_code <- get_category_bowtype_code(bowtype, category)
  category_bowtype <- get_category_bowtype_string(bowtype, category)

  for (i_event in seq_len(nrow(event_number_is_qual_matrix))) {
    event <- event_array[[i_event]][[1]] # string describing the event
    event_number <- event_number_is_qual_matrix[i_event, 1]
    is_qualification <- event_number_is_qual_matrix[i_event, 2]
    if (is_qualification) {
      format <- "Qualification"
    } else {
      format <- "Elimination"
    }

    # construct the table
    if (is_qualification) {
      # IQ stands for individual qualification
      # to be consistent with Ianseo
      output_file <- paste0("IQ", category_bowtype_code, ".html")
    } else {
      # IF stands for individual finals
      # to be consistent with Ianseo
      output_file <- paste0("IF", category_bowtype_code, ".html")
    }
    output_file <- file.path(
      html_path, as.character(event_number), output_file
    )
    table_event <- event_array[[i_event]][[2]]
    table_event$Name <- convert_name_to_html(
      table_event$Name, paste0("../", category_bowtype_code, "/")
    )
    render_event(
      event, event_number, category_bowtype, format, table_event,
      output_file, footer_notes
    )
  }
}

# Return the quasi error of the rating points
#
# Args:
#   plackett_luce_for_each_event: return value of fit_online_plackett_luce()
#   points_name: name for each archer
get_quasi_error <- function(plackett_luce_for_each_event, points_name) {
  # for the final plackett_luce model, work out the quasi standard error
  quasi_error <- tryCatch(
    {
      quasi_error <- qvcalc(
        plackett_luce_for_each_event[[length(plackett_luce_for_each_event)]]
      )
      quasi_error <- round(
        quasi_error[[2]]$quasiSE * 400 * log10(exp(1))
      ) # convert to elo
    },
    error = function(cond) {
      message(cond)
      message("\nError in obtaining quasi error, uncertanities are omitted")
      return(rep(NA, length(points_name)))
    }
  )
  # na any uncertainity which are too large
  quasi_error[quasi_error > 1000] <- NA

  names(quasi_error) <- points_name

  return(quasi_error)
}

# Render all archers' profile page
#
# Args:
#   ranks_and_events: return value of update_ranks_and_events()
#   points: vector of rating points for each archer
#   bowtype: string, full name of the bowtype, eg Recurve, Compound
#   category: string, full name of the category, eg Men, Women
#   html_path: name of directory where the html files are saved
#   footer_notes: optional string, notes to put in the footer for all html files
render_archer_profiles <- function(ranks_and_events, points, bowtype, category,
                                   html_path, footer_notes = NULL) {
  rank_matrix <- ranks_and_events$rank_matrix
  country_array <- ranks_and_events$country_array
  event_array <- ranks_and_events$event_array
  event_array_event_numbers <- ranks_and_events$event_array_event_numbers
  event_number_array <- ranks_and_events$event_number_array
  category_bowtype_code <- get_category_bowtype_code(bowtype, category)
  category_bowtype <- get_category_bowtype_string(bowtype, category)

  # for each archer, construct a table
  for (i_archer in seq_len(length(points))) {
    archer <- names(points)[i_archer]
    country <- country_array[archer]
    ranks <- rank_matrix[[1]][, archer]
    archer_points <- points[archer]
    archer_rank <- which(names(points) == archer)

    # list all events (group all H2H)
    event_number_array <- matrix(
      unique(rank_matrix[[2]][which(ranks > 0, TRUE), ]),
      ncol = 2
    )

    # table of all events attended
    event_table <- matrix(nrow = nrow(event_number_array), ncol = 6)

    # for each event
    for (i_event in seq_len(nrow(event_number_array))) {
      # event_number_array columns: event_number, is_qualification
      event_number <- event_number_array[i_event, ]
      is_qualification <- event_number[2]
      event_number <- event_number[1]
      # get the pointers to the rows of event_array which this archer
      # attended
      event_array_pointer <- which(
        (event_array_event_numbers[, 1] == event_number) &
          (event_array_event_numbers[, 2] == is_qualification),
        TRUE
      )
      # list in event_array: event_name, summary table of the event, points
      # of everyone after event
      event_summary <- event_array[[event_array_pointer]]
      event_name <- event_summary[[1]]
      event_name <- substr(
        event_name, 1, regexpr("<br>", event_name)[1] - 1
      )
      points_after_event <- event_summary[[3]]
      event_summary <- event_summary[[2]]
      if (is_qualification) {
        event_format <- "Qualification"
      } else {
        event_format <- "Elimination"
      }
      # get the row (for the corresponding archer) from the event table
      individual_result <- event_summary[(event_summary$Name == archer), ]
      # CAUTION
      # individual_result$Rank[1], the index[1] is used as there may be
      # archers with the same name
      # take the first instance
      # ways to improve: output a warning, warn the user the data or names
      # need to be cleaned
      event_table[nrow(event_number_array) - i_event + 1, ] <- c(
        individual_result$Rank[1], event_name,
        event_format,
        individual_result$`Points after event`[1],
        individual_result$`Points earned at event`[1],
        which(names(points_after_event) == archer)
      )
    }

    # construct a table for each pairwise comparison
    # get pointers of the rows of the rank matrix where this archer has
    # attended
    rank_matrix_sub_pointer <- (rank_matrix[[1]][, archer] > 0)
    # get list of events this archer has attended
    #   dim 1: each event
    #   dim 2: event number, is_qualification
    archer_event_number <- matrix(
      rank_matrix[[2]][rank_matrix_sub_pointer, ],
      ncol = 2
    )
    # get columns of the rank matrix
    archer_rank_matrix <- matrix(
      rank_matrix[[1]][rank_matrix_sub_pointer, ],
      ncol = ncol(rank_matrix[[1]])
    )
    colnames(archer_rank_matrix) <- colnames(rank_matrix[[1]])
    # work out the number of pairs
    n_pair <- sum(archer_rank_matrix > 0) - nrow(archer_rank_matrix)
    pairwise_table <- matrix(nrow = n_pair, ncol = 7)
    i_pair <- 1
    # each event
    for (i_event in seq_len(nrow(archer_rank_matrix))) {
      event_number <- archer_event_number[i_event, 1]
      is_qualification <- archer_event_number[i_event, 2]
      # get the pointer for event_array for this corresponding event
      event_array_pointer <- which(
        (event_array_event_numbers[, 1] == event_number) &
          (event_array_event_numbers[, 2] == is_qualification),
        TRUE
      )
      event_name <- event_array[[event_array_pointer]][[1]]
      event_name <- substr(
        event_name, 1, regexpr("<br>", event_name)[1] - 1
      )
      event_name <- convert_event_to_html(
        event_name, category_bowtype_code,
        matrix(c(event_number, is_qualification), ncol = 2), "../"
      )
      # get the list of ranks for each archer
      event_rank <- archer_rank_matrix[
        i_event, archer_rank_matrix[i_event, ] > 0
      ]
      event_rank <- sort(event_rank, decreasing = TRUE)
      if (is_qualification) {
        event_format <- "Qualification"
      } else {
        event_format <- "Elimination"
      }
      # get list of opponents
      vs_name_array <- names(event_rank)
      # get the rank of this archer
      archer_rank_at_this_event <- event_rank[archer]
      # for each pair
      for (i_vs in seq_len(length(event_rank))) {
        vs_name <- vs_name_array[i_vs] # get name of opponent
        # if the opponent is not itself
        if (vs_name != archer) {
          # compare ranks and get the result
          vs_rank <- event_rank[vs_name]
          if (archer_rank_at_this_event < vs_rank) {
            result <- "WIN"
          } else if (archer_rank_at_this_event > vs_rank) {
            result <- "LOSE"
          } else {
            result <- "DRAW"
          }
          opponent_points <- points[vs_name]
          prob_win <- probability_to_html(get_probability_win(
            archer_points,
            opponent_points
          ))
          # put results in pairwise_table
          pairwise_table[n_pair - i_pair + 1, ] <- c(
            event_name, event_format, vs_name, country_array[vs_name],
            round(opponent_points), prob_win, result
          )
          i_pair <- i_pair + 1
        }
      }
    }

    # label columns of the table
    colnames(event_table) <- c(
      "Rank", "Event", "Format",
      "Points after event", "Points earned at event",
      "Rating rank after event"
    )
    colnames(pairwise_table) <- c(
      "Event", "Format", "VS", "Country",
      "Opponent's points now", "Estimated probability of win", "Result"
    )

    # add links to event_table
    event_table[, 2] <- convert_event_to_html(
      event_table[, 2], category_bowtype_code,
      matrix(apply(event_number_array, 2, rev), ncol = 2), "../"
    )
    pairwise_table[, 3] <- convert_name_to_html(pairwise_table[, 3], "")

    output_file <- name_to_html(archer)
    output_file <- file.path(
      html_path, category_bowtype_code,
      paste0(output_file, ".html")
    )
    render_individual(
      archer, country, category_bowtype, archer_rank, round(archer_points),
      event_table, pairwise_table, output_file, footer_notes
    )
  }
}

# Get the Ianseo string code given a bowtype and category
#
# Get the Ianseo string code given a bowtype and category
# eg Men's Recurve = RM
# eg Women's Recurve = RW
get_category_bowtype_code <- function(bowtype, category) {
  category_bowtype_code <- paste0(
    substr(bowtype, 1, 1),
    substr(category, 1, 1)
  )
  return(category_bowtype_code)
}

# Combine the bowtype and category strings into a single string
#
# Combine the bowtype and category strings into a single string
# eg Men's Recurve, Women's Recurve
get_category_bowtype_string <- function(bowtype, category) {
  return(paste0(category, "'s ", bowtype))
}

# Concatenate the two provided rank matrices
# Args:
#   rank1: rank matrix to concatenate
#   rank2: rank matrix to concatenate
# Returns:
#   concatenated rank matrix
combine_rank_matrix <- function(rank1, rank2) {
  names <- union(colnames(rank1), colnames(rank2))
  rank_matrix <- matrix(0,
    nrow = nrow(rank1) + nrow(rank2),
    ncol = length(names)
  )
  colnames(rank_matrix) <- names
  rank_matrix[seq_len(nrow(rank1)), colnames(rank1)] <- rank1
  rank_matrix[(nrow(rank1) + 1):nrow(rank_matrix), colnames(rank2)] <- rank2
  return(rank_matrix)
}

# Convert the coefficient of plackett_luce to elo-like points
# Args:
#   plackett_luce: fitted plackett_luce object
# Returns:
#   vector of points
get_points <- function(plackett_luce) {
  worth <- (coef(plackett_luce, log = FALSE, type = "worth"))
  n <- length(worth)
  k <- 1440 / 400 - sum(log10(worth)) / n
  elo <- 400 * (log10(worth) + k)

  return(elo)
}

# Return rows of a rank matrix from an event
#
# Args:
#   event_number: integer for Ianseo
#   is_qualification: boolean if this is a qualification round
#   bowtype: string, 'Recurve', 'Compound' or 'Barebow'
#   category: string, 'Men' or 'Women'
# Returns: a list with the following:
#   1. rows of a rank matrix
#   2. data from the event
get_rank_row <- function(event_number, is_qualification, bowtype, category) {

  # read the qualification
  file_qualification <- paste(as.character(event_number),
    "-Individual_Qualification-", bowtype,
    "_", category, ".csv",
    sep = ""
  )
  file_qualification <- file.path(
    as.character(event_number), file_qualification
  )
  qualification <- read.csv(file = file_qualification, header = TRUE, sep = ",")
  # format the data frame
  qualification$Name <- clean_names(as.character(qualification$Name))

  # for a round, eg WA 720
  if (is_qualification) {

    # order the data frame (just in case of any ties)
    qualification <- qualification[with(
      qualification,
      order(qualification$Score,
        qualification$Tens, qualification$Xs,
        decreasing = TRUE
      )
    ), ]

    # number of archers
    n <- nrow(qualification)

    # find any draws and assign equal rank
    n_draw <- 0
    qualification[1, 1] <- 1
    for (i in 2:n) {
      if (qualification[i, 4] == qualification[i - 1, 4]) {
        if (qualification[i, 5] == qualification[i - 1, 5]) {
          if (qualification[i, 6] == qualification[i - 1, 6]) {
            n_draw <- n_draw + 1
          }
        }
      }
      qualification[i, 1] <- i - n_draw
    }

    # a qualification produce a row in the rank matrix
    rank_matrix <- matrix(ncol = n, nrow = 1)
    colnames(rank_matrix) <- qualification$Name
    rank_matrix[1, ] <- qualification$Rank

    # return the qualification and rank_matrix
    return(list(rank_matrix, qualification))
  } else { # else it's brackets

    # get the csv file for the bracket
    file <- paste(as.character(event_number), "-Individual_Brackets-", bowtype,
      "_", category,
      ".csv",
      sep = ""
    )
    file <- file.path(as.character(event_number), file)
    bracket <- read.csv(file = file, header = TRUE, sep = ",")
    bracket <- bracket[, c(2, 4, 5, 7)] # remove unused columns
    bracket[bracket == 0] <- 2 # change is win/lose to rank
    # make archer names as string
    bracket[, 1] <- as.character(bracket[, 1])
    bracket[, 3] <- as.character(bracket[, 3])
    # keep a copy of the archers name in case Individual-Final is not available
    names_for_backup <- unique(c(bracket[, 1], bracket[, 3]))
    # clean the names and convert all names to lower case
    bracket[, 1] <- tolower(clean_names(bracket[, 1]))
    bracket[, 3] <- tolower(clean_names(bracket[, 3]))

    # get the csv file for the final h2h rank
    file <- paste(as.character(event_number), "-Individual_Final-", bowtype,
      "_", category,
      ".csv",
      sep = ""
    )
    file <- file.path(as.character(event_number), file)

    # try and read the final h2h rank, some Ianseo pages do not include it
    if (file.exists(file)) {
      final <- read.csv(file = file, header = TRUE, sep = ",")
      # format name column
      final$Name <- as.character(clean_names(as.character(final$Name)))
      # replace final country names with country names in qualification
      # ianseo omits full country names in brackets
      final$Country <- qualification$Country[
        match(final$Name, qualification$Name)
      ]
    } else {
      # if the final page does not exist, fill in all information with using
      # qualification
      # final only requires rank, name, country
      qualification <- qualification[, 1:3]
      # final position unknown, fill with na
      qualification[, 1] <- NA

      # only include qualification archers who are also in elimiation
      final <- qualification[
        is.element(tolower(qualification$Name), tolower(names_for_backup)),
      ]
    }

    # make rank matrix, one row for each match
    rank_matrix <- matrix(0, ncol = nrow(final), nrow = nrow(bracket))
    colnames(rank_matrix) <- final$Name

    # for each row, assign rank, use the name formatted in final$Name
    names_lower_case <- tolower(final$Name)
    for (i in seq_len(nrow(rank_matrix))) {
      # use the name in final$Name by checking if they are the same when all
      # converted to lower case
      rank_matrix[i, match(bracket[i, 1], names_lower_case)] <- bracket[i, 2]
      rank_matrix[i, match(bracket[i, 3], names_lower_case)] <- bracket[i, 4]
    }

    # return the final ranking and the rank matrix
    return(list(rank_matrix, final))
  }
}

# Convert name to a suitable html name
name_to_html <- function(name) {
  name <- gsub(" ", "-", tolower(stri_enc_toascii(name)), fixed = TRUE)
  return(name)
}

# Add a link to the name of archers in HTML format
# Args:
#   name: vector of names
#   prefix_to_link: path to where the individual archer's pages are stored
# Returns: vector of names in HTML format
convert_name_to_html <- function(name, prefix_to_link) {
  name <- paste0(
    '<a href="', prefix_to_link, name_to_html(name), '.html">', name,
    " &#10140;</a>"
  )
  return(name)
}

# For a given event and bow category, return <a> with url link for that event
# page
# Args:
#   event_name: vector of event names
#   category_bowtype_code
#   event_number_array: matrix of event numbers
#     dim 1: for each event in event_name
#     dim 2: event_number, is_qualification
#   prefix_to_link: path to where the event directory is
# Returns: vector of event names in HTML format
convert_event_to_html <- function(event_name, category_bowtype_code,
                                  event_number_array,
                                  prefix_to_link) {
  event_url <- get_event_html(
    category_bowtype_code, event_number_array,
    prefix_to_link
  )
  event_html <- paste0(
    '<a href="', event_url, '">', event_name,
    " &#10140;</a>"
  )
  return(event_html)
}

# For a given event and bow category, return url link for that event page
get_event_html <- function(category_bowtype_code, event_number_array,
                           prefix_to_link) {
  html_file <- event_number_array[, 2]
  event_number <- event_number_array[, 1]
  html_file[html_file == 1] <- "IQ"
  html_file[html_file == 0] <- "IF"
  url <- paste0(
    prefix_to_link, event_number, "/", html_file,
    category_bowtype_code, ".html"
  )
  return(url)
}

# Calculate the probability A beats B given their rating points
get_probability_win <- function(points_a, points_b) {
  return(1 / (1 + 10^((points_b - points_a) / 400)))
}

# Return a probability to a readable string form
probability_to_html <- function(prob) {
  if (prob < 0.01) {
    text <- "&lt;1&percnt;"
  } else if (prob > 0.99) {
    text <- "&gt;99&percnt;"
  } else {
    text <- paste0(round(prob * 100), "&percnt;")
  }
  return(text)
}
