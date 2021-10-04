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

library(PlackettLuce);
library(rmarkdown);
library(readr);
library(stringi);
library(foreach);
library(doParallel);

source('cleannames.R');
source('renderHtml.R');

#PROCEDURE: ARCHERY RATING HTML
#Process the provided .csv files from IanseoParse and create a website containing the plackett luce
  #rating scores. It creates categories: men's recurve, women's recurve, men's compound,
  #women's compound.
#the plackett luce worth scores are translate to elo-like scores, see function getPoints()
#Note: this is computationally intensive, use of multiple threads is recommended
#PARAMETERS:
  #recurveEventArray: number code for each Ianseo recurve event, must be in chronological order and
    #should be consistent with compoundEventArray
  #compoundEventArray: number code for each Ianseo compound event, must be in chronological order
    #and should be consistent with compoundEventArray
  #htmlPath: name of directory where the html files are saved
  #tile: title to put on the home page
archeryRatingHtml = function(recurveEventArray, compoundEventArray, htmlPath, title) {

  registerDoParallel(detectCores());

  #homepage variables
  categoryBowtypeLink = c(); #list of category and bowtypes, in full text format (eg Men's Recurve)
  #list of events
    #dim 1: for each event
    #dim 2: event name, format, list of categories and links (in categoryBowtypeCode format)
  eventArrayMatrix = matrix(nrow = 0, ncol = 3);
  colnames(eventArrayMatrix) = c('Event','Format','Categories');

  #list of plackettLuce and rank matrices
    #dim 1: for each category
    #dim 2: categoryBowtypeCode, rankMatrix, plackettLuce
  plackettLuceArray = list();

  for (bowtype in c('Recurve', 'Compound')) {

    for (category in c('Men', 'Women')){

      categoryBowtypeCode = paste0(substr(bowtype, 1, 1),substr(category,1,1));
      categoryBowtype = paste0(category,"'s ",bowtype);

      #create directory for this category
      if (!file.exists(file.path(htmlPath, categoryBowtypeCode))) {
        dir.create(file.path(htmlPath, categoryBowtypeCode))
      }

      #loop through these event numbers
      #in this implementation each event has a WA720 and bracket
      if (bowtype == 'Recurve'){
        eventNumberArray = recurveEventArray;
      } else {
        eventNumberArray = compoundEventArray;
      }

      #list for ranking data
        #element 1 (rank matrix):
          #matrix of ranks, use 0 to denote not attended
            #dim1: for each ranking (eg, each WA 720 and each H2H match)
            #dim2: for each archer (named using archers' names)
        #element 2 (pointer):
          #matrix for indicating the event of each row of the rank matrix
            #dim1: for each row in the rank matrix
            #dim2: event number, boolean: isQualification
      rankMatrix = list(matrix(nrow=0, ncol=0), c());
      countryArray = c(); #array of countries for each archer (names)
      #list of details for each event (WA720, bracket)
        #dim1: for each event
        #dim2: name of event, result summary, points
      eventArray = list();
      #indicate the event number for each row in eventArray[[]][[2]]
        #dim1: for each event
        #dim2: event number, boolean: isQualification
      eventArrayEventNumbers = matrix(nrow=0, ncol=2);

      #combine the rank matrices for each event
      #record the country of each archer
      #update the homepage link to events
      for (iEvent in 1:length(eventNumberArray)){
        eventNumber = eventNumberArray[iEvent];
        event = read_file(file.path(as.character(eventNumber), 'event.txt'));
        event = paste0( gsub('[\n]','<br>',event));

        #create directory for this event
        if (!file.exists(file.path(htmlPath, eventNumber))) {
          dir.create(file.path(htmlPath, eventNumber))
        }

        #for the qualification and the brackets
        for (isQualification in c(TRUE, FALSE)){

          if (isQualification){
            format = 'Qualification'
          } else {
            format = 'Elimination';
          }

          #eventSummary is a list
          #element 1: columns of rank matrix
          #element 2: result summary
          eventSummary = getRankRow(eventNumber, isQualification, bowtype, category);

          #get the rank matrix, append it and fit plackettLuce
          rank = eventSummary[[1]];
          rankMatrix[[1]] = combineRankMatrix(rankMatrix[[1]], rank);
          rankMatrix[[2]] = rbind(rankMatrix[[2]],
              t(matrix(rep(c(eventNumber, isQualification),nrow(rank)),nrow=2)));

          #append points after event and gained at event to the results table
          kable = eventSummary[[2]];

          #add the name of the event and the summary table to eventArray
          eventArray[[length(eventArray)+1]] = list(event, kable, NULL);
          #add the eventNumber and isQualification to eventArrayEventNumbers
          eventArrayEventNumbers = rbind(eventArrayEventNumbers, c(eventNumber, isQualification));

          #save the country for each archer
          for (iArcher in 1:nrow(kable)){
            archer = kable$Name[iArcher];
            if (length(countryArray) == 0) {
              countryArray = c(countryArray, toString(kable[iArcher, 'Country']));
              names(countryArray)[length(countryArray)] = kable$Name[iArcher];
            } else if (!is.element(archer,names(countryArray))){
              countryArray = c(countryArray, toString(kable[iArcher, 'Country']));
              names(countryArray)[length(countryArray)] = kable$Name[iArcher];
            }
          }

          #link to this event page
          eventPointer = (eventArrayMatrix[,1] == event) & (eventArrayMatrix[,2] == format);
          link = getEventHtml(categoryBowtypeCode,
                              matrix(c(eventNumber, isQualification),ncol=2), '');
          button = linkToButton(categoryBowtypeCode, link);
          if (!any(eventPointer)){
            eventArrayMatrix = rbind(c(event, format, button), eventArrayMatrix);
          } else {
            eventArrayMatrix[which(eventPointer),3] = paste(eventArrayMatrix[which(eventPointer),3],
                                                            button);
          }
        }
      }

      #get each unique event
      eventNumberArray = matrix(unique(rankMatrix[[2]]),ncol=2);

      #for each event, fit plackett luce
      plackettLuceForEachEvent = foreach (iEvent = 1:nrow(eventNumberArray)) %dopar% {

        eventNumber = eventNumberArray[iEvent,1];
        isQualification = eventNumberArray[iEvent,2];

        #get the pointer for this event and all previous events

        #get current event pointer
        rankMatrixRowPointer =
            (rankMatrix[[2]][,1] == eventNumber) & (rankMatrix[[2]][,2] == isQualification);
        #then get all previous event pointers
        rankMatrixRowPointer[1:which(rankMatrixRowPointer)[1]] = TRUE;
        #get rank matrix
        rankMatrixAtThisEvent = matrix(rankMatrix[[1]][rankMatrixRowPointer,],
            nrow = sum(rankMatrixRowPointer));
        #remove zero columns, this happens when the archer has not competed yet
        archerPointer = colSums(rankMatrixAtThisEvent)>0;
        rankMatrixAtThisEvent = matrix(rankMatrix[[1]][rankMatrixRowPointer,archerPointer],
            nrow = sum(rankMatrixRowPointer));
        colnames(rankMatrixAtThisEvent) = colnames(rankMatrix[[1]])[archerPointer];

        #fit plackett luce which is returned via foreach
        plackettLuce = PlackettLuce(as.rankings(rankMatrixAtThisEvent));
      }

      #rating points for each archer
      points = c();
      for (iEvent in 1:nrow(eventNumberArray)){

        event = eventArray[[iEvent]][[1]];
        eventNumber = eventNumberArray[iEvent,1];
        isQualification = eventNumberArray[iEvent,2];
        if (isQualification){
          format = 'Qualification'
        } else {
          format = 'Elimination';
        }

        #get the points
        pointsAfter = getPoints(plackettLuceForEachEvent[[iEvent]]);
        pointsBefore = rep(1440, length(pointsAfter));
        names(pointsBefore) = names(pointsAfter);
        pointsBefore[names(points)] = points;
        pointsDiff = pointsAfter - pointsBefore;
        points = pointsAfter;
        eventArray[[iEvent]][[3]] = sort(points, decreasing=TRUE); #save the points

        #append points after event and gained at event to the results table
        kable = eventArray[[iEvent]][[2]];
        kable = cbind(kable, round(pointsAfter[kable$Name]), round(pointsDiff[kable$Name]));
        #label columns of the table
        if (isQualification){
          colnames(kable)[5] = '10+X';
          colnames(kable)[6] = 'X';
        }
        colnames(kable)[ncol(kable)-1] = 'Points after event';
        colnames(kable)[ncol(kable)] = 'Points earned at event';
        rownames(kable) = NULL;
        eventArray[[iEvent]][[2]] = kable;

        #knit the table
        if (isQualification){
          outputFile = paste0('IQ',categoryBowtypeCode,'.html');
        } else {
          outputFile = paste0('IF',categoryBowtypeCode,'.html');
        }
        outputFile = file.path(htmlPath, as.character(eventNumber), outputFile);
        kable = eventArray[[iEvent]][[2]];
        kable$Name = convertNameToHtml(kable$Name, paste0('../',categoryBowtypeCode,'/'));
        renderEvent(event, categoryBowtype, format, kable, outputFile);
      }

      #save the rank matrix and plackettLuce model
      plackettLuceArray[[length(plackettLuceArray)+1]] = list(categoryBowtypeCode, rankMatrix,
          plackettLuceForEachEvent[[length(plackettLuceForEachEvent)]]);

      #for the final plackettLuce model, work out the quasi standard error
      quasiError = tryCatch({
        quasiError = qvcalc(plackettLuceForEachEvent[[length(plackettLuceForEachEvent)]]);
        quasiError = round(quasiError[[2]]$quasiSE * 400 * log10(exp(1))); #convert to elo
      },
      error=function(cond) {
        message(cond)
        message("\nError in obtaining quasi error, uncertanity are omitted")
        return(rep(NA, length(points)))
      })

      names(quasiError) = names(points);

      #sort the points and knit a table showing all archers' points
      points = sort(points, decreasing = TRUE);
      kable = matrix(nrow = length(points), ncol = 5);
      kable[,1] = 1:length(points);
      kable[,2] = names(points);
      kable[,3] = countryArray[names(points)];
      kable[,4] = round(points);
      kable[,5] = paste0('±',quasiError[names(points)]);
      kable[,2] = convertNameToHtml(kable[,2], paste0(categoryBowtypeCode,'/'));
      colnames(kable) = c('Rank','Name','Country','Points','Uncertainty');
      renderRank(categoryBowtype, kable,
                 file.path(htmlPath, paste0(categoryBowtypeCode,'.html')));

      #link the homepage to this
      categoryBowtypeLink = c(categoryBowtypeLink, paste0(categoryBowtypeCode, '.html'));

      #for each archer, knit a table
      for (iArcher in 1:length(points)) {
        archer = names(points)[iArcher];
        country = countryArray[archer];
        ranks = rankMatrix[[1]][,archer];
        archerPoints = round(points[archer]);
        archerRank = which(names(points)==archer);

        #list all events (group all H2H)
        eventNumberArray = matrix(unique(rankMatrix[[2]][which(ranks>0, TRUE),]),ncol=2);

        #table of all events attended
        kable = matrix(nrow = nrow(eventNumberArray), ncol = 6);

        #for each event
        for (iEvent in 1:nrow(eventNumberArray)){
          #eventNumberArray columns: eventNumber, isQualification
          eventNumber = eventNumberArray[iEvent,];
          isQualification = eventNumber[2];
          eventNumber = eventNumber[1];
          #get the pointers to the rows of eventArray which this archer attended
          eventArrayPointer = which(
            (eventArrayEventNumbers[,1]==eventNumber)&(eventArrayEventNumbers[,2]==isQualification),
            TRUE);
          #list in eventArray: eventName, summary table of the event, points of everyone after event
          eventSummary = eventArray[[eventArrayPointer]];
          eventName = eventSummary[[1]];
          eventName = substr(eventName,1,regexpr('<br>',eventName)[1]-1);
          pointsAfterEvent = eventSummary[[3]];
          eventSummary = eventSummary[[2]];
          if (isQualification){
            eventFormat = 'Qualification'
          } else {
            eventFormat = 'Elimination';
          }
          #get the row (for the corresponding archer) from the event table
          individualResult = eventSummary[(eventSummary$Name == archer),];
          #CAUTION
          #individualResult$Rank[1], the index[1] is used as there may be archers with the same name
            #take the first instance
          #ways to improve: output a warning, warn the user the data or names need to be cleaned
          kable[nrow(eventNumberArray)-iEvent+1,] = c(individualResult$Rank[1], eventName,
              eventFormat,
              individualResult$`Points after event`[1],
              individualResult$`Points earned at event`[1],
              which(names(pointsAfterEvent)==archer));
        }

        #knit a table for each pairwise comparison
        #get pointers of the rows of the rank matrix where this archer has attended
        rankMatrixSubPointer = (rankMatrix[[1]][,archer]>0);
        #get list of events this archer has attended
          #dim 1: each event
          #dim 2: event number, isQualification
        archerEventNumber = matrix(rankMatrix[[2]][rankMatrixSubPointer,],ncol=2);
        #get columns of the rank matrix
        archerRankMatrix = matrix(rankMatrix[[1]][rankMatrixSubPointer,],
            ncol=ncol(rankMatrix[[1]]));
        colnames(archerRankMatrix) = colnames(rankMatrix[[1]]);
        #work out the number of pairs
        nPair = sum(archerRankMatrix>0) - nrow(archerRankMatrix);
        kable2 = matrix(nrow = nPair, ncol = 6);
        iPair = 1;
        #each event
        for (iEvent in 1:nrow(archerRankMatrix)) {
          eventNumber = archerEventNumber[iEvent,1];
          isQualification = archerEventNumber[iEvent,2];
          #get the pointer for eventArray for this corresponding event
          eventArrayPointer = which(
            (eventArrayEventNumbers[,1]==eventNumber)&(eventArrayEventNumbers[,2]==isQualification),
            TRUE);
          eventName = eventArray[[eventArrayPointer]][[1]];
          eventName = substr(eventName,1,regexpr('<br>',eventName)[1]-1);
          eventName = convertEventToHtml(eventName, categoryBowtypeCode,
              matrix(c(eventNumber, isQualification),ncol=2), '../');
          #get the list of ranks for each archer
          eventRank = archerRankMatrix[iEvent,archerRankMatrix[iEvent,]>0];
          eventRank = sort(eventRank, decreasing=TRUE);
          if (isQualification){
            eventFormat = 'Qualification';
          } else {
            eventFormat = 'Elimination';
          }
          #get list of opponents
          vsNameArray = names(eventRank);
          #get the rank of this archer
          archerRankAtThisEvent = eventRank[archer];
          #for each pair
          for (iVs in 1:length(eventRank)){
            vsName = vsNameArray[iVs]; #get name of opponent
            #if the opponent is not itself
            if (vsName != archer){
              #compare ranks and get the result
              vsRank = eventRank[vsName];
              if (archerRankAtThisEvent<vsRank){
                result = 'WIN';
              } else if (archerRankAtThisEvent>vsRank){
                result = 'LOSE';
              } else {
                result = 'DRAW';
              }
              #put results in kable2
              kable2[nPair-iPair+1,] = c(eventName, eventFormat, vsName, countryArray[vsName],
                  round(points[vsName]), result);
              iPair = iPair + 1;
            }
          }
        }

        #lable columns of the table
        colnames(kable) = c('Rank','Event','Format','Points after event','Points earned at event',
            'Rating rank after event');
        colnames(kable2) = c('Event','Format','VS','Country',"Opponent's points now", 'Result');

        #add links to kable
        kable[,2] = convertEventToHtml(kable[,2], categoryBowtypeCode,
            matrix(apply(eventNumberArray,2,rev),ncol=2), '../');
        kable2[,3] = convertNameToHtml(kable2[,3],'');

        outputFile = nameToHtml(archer);
        outputFile = file.path(htmlPath, categoryBowtypeCode, paste0(outputFile,'.html'));
        renderIndividual(archer, country, categoryBowtype, archerRank, archerPoints, kable,
                         kable2, outputFile);
      }
    }
  }

  renderHomepage(title, categoryBowtypeLink, eventArrayMatrix, file.path(htmlPath, 'index.html'));
}

#FUNCTION: COMBINE RANK MATRIX
#Concatenate the two provided rank matrices
#PARAMETERS:
  #rank1: rank matrix to concatenate
  #rank2: rank matrix to concatenate
#RETURN:
  #concatenated rank matrix
combineRankMatrix = function(rank1, rank2){
  names = union(colnames(rank1), colnames(rank2));
  rankMatrix = matrix(0, nrow = nrow(rank1)+nrow(rank2), ncol = length(names));
  colnames(rankMatrix) = names;
  rankMatrix[1:nrow(rank1), colnames(rank1)] = rank1;
  rankMatrix[(nrow(rank1)+1):nrow(rankMatrix), colnames(rank2)] = rank2;
  return(rankMatrix)
}

#FUNCTION: GET POINTS
#Convert the coefficient of plackettLuce to elo-like points
#PARAMETERS:
  #plackettLuce: fitted plackettLuce object
#RETURN:
  #vector of points
getPoints = function(plackettLuce){

  worth = (coef(plackettLuce, log = FALSE, type = 'worth'));
  n = length(worth);
  k = 1440/400 - sum(log10(worth))/n;
  elo = 400 * (log10(worth) + k);

  return(elo);

}

#FUNCTION: GET RANK ROW
#Return rows of a rank matrix from an event
#
#PARAMETERS:
  #eventNumber: integer for Ianseo
  #isQualification: boolean if this is a qualification round
  #bowtype: string, 'Recurve' or 'Compound'
  #category: string, 'Men' or 'Women'
#RETURN a list with the following:
  #1. rows of a rank matrix
  #2. data from the event
getRankRow = function(eventNumber, isQualification, bowtype, category){

  #for a round, eg WA 720
  if (isQualification){

    #get the CSV file
    file = paste(as.character(eventNumber),"-Individual_Qualification-",bowtype,"_",category,
        ".csv", sep = "");
    file = file.path(as.character(eventNumber), file);
    qualification = read.csv(file=file, header=TRUE, sep=",");

    #format the data frame
    qualification$Name = cleanNames(as.character(qualification$Name));
    #order the data frame (just in case of any ties)
    qualification = qualification[with(qualification,
        order(qualification$Score,
        qualification$Tens, qualification$Xs, decreasing = TRUE)),];

    #number of archers
    n = nrow(qualification);

    #find any draws and assign equal rank
    nDraw = 0;
    qualification[1,1] = 1;
    for (i in 2:n){
      if (qualification[i,4] == qualification[i-1,4]){
        if (qualification[i,5] == qualification[i-1,5]){
          if (qualification[i,6] == qualification[i-1,6]){
            nDraw = nDraw+1;
          }
        }
      }
      qualification[i,1] = i-nDraw;
    }

    #a qualification produce a row in the rank matrix
    rankMatrix = matrix(ncol = n, nrow = 1);
    colnames(rankMatrix) = qualification$Name;
    rankMatrix[1,] = qualification$Rank;

    #return the qualification and rankMatrix
    return(list(rankMatrix, qualification));

  } else { #else it's brackets

    #get the csv file for the bracket
    file = paste(as.character(eventNumber),"-Individual_Brackets-",bowtype,"_",category,
        ".csv", sep = "");
    file = file.path(as.character(eventNumber), file);
    bracket = read.csv(file=file, header=TRUE, sep=",");
    bracket = bracket[,c(2,4,5,7)]; #remove unused columns
    bracket[bracket==0] = 2; #change is win/lose to rank
    #make archer names as string
    bracket[,1] = as.character(bracket[,1]);
    bracket[,3] = as.character(bracket[,3]);
    #clean the names and convert all names to lower case
    bracket[,1] = tolower(cleanNames(bracket[,1]));
    bracket[,3] = tolower(cleanNames(bracket[,3]));

    #get the csv file for the final h2h rank
    file = paste(as.character(eventNumber),"-Individual_Final-",bowtype,"_",category,
        ".csv", sep = "");
    file = file.path(as.character(eventNumber), file);
    final = read.csv(file=file, header=TRUE, sep=",");
    final$Name = as.character(cleanNames(as.character(final$Name))); #format name column

    #make rank matrix, one row for each match
    rankMatrix = matrix(0, ncol = nrow(final), nrow = nrow(bracket));
    colnames(rankMatrix) = final$Name;

    #for each row, assign rank, use the name formatted in final$Name
    namesLowerCase = tolower(final$Name);
    for (i in 1:nrow(rankMatrix)){
      #use the name in final$Name by checking if they are the same when all converted to lower case
      rankMatrix[i,match(bracket[i,1], namesLowerCase)] = bracket[i,2];
      rankMatrix[i,match(bracket[i,3], namesLowerCase)] = bracket[i,4];
    }

    #return the final ranking and the rank matrix
    return(list(rankMatrix, final));
  }

}

#FUNCTION: NAME TO HTML
#Convert name to a suitable html name
nameToHtml = function(name) {
  name = gsub(" ", "-", tolower(stri_enc_toascii(name)), fixed = TRUE);
  return(name);
}

#FUNCTION: CONVERT NAME TO HTML
#Add a link to the name of archers in HTML format
#PARAMETERS:
  #name: vector of names
  #prefixToLink: path to where the individual archer's pages are stored
#RETURN;
  #vector of names in HTML format
convertNameToHtml = function(name, prefixToLink) {
  name = paste0('<a href="', prefixToLink, nameToHtml(name), '.html">', name, '</a>');
  return(name);
}

#FUNCTION: CONVERT EVENT TO HTML
#For a given event and bow category, return <a> with url link for that event page
#PARAMETERS:
  #eventName: vector of event names
  #categoryBowtypeCode
  #eventNumberArray: matrix of event numbers
    #dim 1: for each event in eventName
    #dim 2: eventNumber, isQualification
  #prefixToLink: path to where the event directory is
#RETURN:
  #vector of event names in HTML format
convertEventToHtml = function(eventName, categoryBowtypeCode, eventNumberArray, prefixToLink) {
  eventUrl = getEventHtml(categoryBowtypeCode, eventNumberArray, prefixToLink)
  eventHtml = paste0('<a href="', eventUrl, '">', eventName, '</a>');
  return(eventHtml);
}

#FUNCTION: GET EVENT HTML
#For a given event and bow category, return url link for that event page
getEventHtml = function(categoryBowtypeCode, eventNumberArray, prefixToLink) {
  htmlFile = eventNumberArray[,2];
  eventNumber = eventNumberArray[,1];
  htmlFile[htmlFile==1] = 'IQ';
  htmlFile[htmlFile==0] = 'IF';
  url = paste0(prefixToLink, eventNumber, '/', htmlFile, categoryBowtypeCode, '.html');
  return(url);
}