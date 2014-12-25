#Statistics 133 Final Project
#Vincent Sheu, Winnie Wong, Rosie Abe, Lindy Chang, and JP Saunders
#Beating Vegas
#05.08.2012
#Statistics 133, Spring 2012 - Ibser, Kuang

## NOTES:
##  PACKAGES THAT ARE NECESSARY:
        library("XML")
        library("RCurl")
        library("RColorBrewer")

  ###################
  ###################
  #Importing of Data#
  ###################
  ###################
  ElevenTwelveDump = read.csv(url("http://home.comcast.net/~tlbeck/ncaabb11.csv"), header = TRUE, sep = ",")
  TenElevenDump = read.csv(url("http://home.comcast.net/~tlbeck/ncaabb10.csv"), header = TRUE, sep = ",")
  NineTenDump = read.csv(url("http://home.comcast.net/~tlbeck/ncaabb09.csv"), header = TRUE, sep = ",")
  EightNineDump = read.csv(url("http://home.comcast.net/~tlbeck/ncaabb08.csv"), header = TRUE, sep = ",")
  SevenEightDump = read.csv(url("http://home.comcast.net/~tlbeck/ncaabb07.csv"), header = TRUE, sep = ",")


# IndexNumberAdder takes Season Data and adds Game Index numbers so that games can be referred to later.
IndexAdder = function(SeasonData) {
  game.index = seq(1:nrow(SeasonData))
  date.index = apply(SeasonData,1,function(x) sum(1,-julian(strptime(SeasonData[1,1], format = "%m/%d/%Y")),julian(strptime(x[[1]], format = "%m/%d/%Y"))))
  return(data.frame(game.index,date.index,SeasonData))
}


# The input for the function "AfterTheNewYear" will be a season's worth of data imported from an external source.
# In each game, look for the first occurence of two digits, which should be the month.
# Subset the entire data set and choose only those played before May of each year.
# The basketball season runs from November to April, so this excludes games played in November and December.
# The output will be a seasons worth of data for games played on or after New Year's Day.
AfterTheNewYear = function(SeasonData) {
  reg = regexpr("[[:digit:]][[:digit:]]",SeasonData[,"date"])
  return(SeasonData[as.numeric(regmatches(SeasonData[,"date"],reg))<5,])
}

# Given Season Data, naRemove will remove any entries of games for which we are missing information.
naRemove = function(SeasonData) {
  return(SeasonData[complete.cases(SeasonData[c("hscore", "rscore", "line", "linesag", "linesage", "linesagp", "linemoore", "lineopen", "linedok", "linefox")]),])
}

# Given Season Data, CleanUp applies IndexAdder, AfterTheNewYear, and naRemove and returns cleaned up data for games after the new year of a given season.
CleanUp = function(SeasonData) {
  return(
    naRemove(
      AfterTheNewYear(
        IndexAdder(
          SeasonData[
            c("date", "home", "hscore", "road", "rscore", "line", "linesag", "linesage", "linesagp", "linemoore", "lineopen", "linedok", "linefox")
            ]
          )
        )
      )
    )
}

#AddResult takes season data and adds a column for the result of the game. The result is the Home Score - Road Score, and can be directly compared to the rest of the data.
AddResult = function(SeasonData) {
  hrscores = data.frame(SeasonData["hscore"], apply(SeasonData["rscore"],1,"-"))
  result = apply(hrscores, 1, sum)
  return(data.frame(SeasonData, result))
}

#Cleaning up the results.
ElevenTwelve = AddResult(CleanUp(ElevenTwelveDump))
TenEleven = AddResult(CleanUp(TenElevenDump))
NineTen  = AddResult(CleanUp(NineTenDump))
EightNine = AddResult(CleanUp(EightNineDump))
SevenEight = AddResult(CleanUp(SevenEightDump))


###################################################################################
###################################################################################
#Question Set 1: How accurate is both the Vegas opening line and final line?      #
#What % of games does it correctly predict the winner for?                        #
#How accurate are each of the prediction systems at predicting the correct winner?#
###################################################################################
###################################################################################
LineCorrectHelper = function(twodata) { #Helper function for CorrectPredictions. Returns TRUE if both pieces of data (inputed together) are positive or negative. If one of the two compared values is a 0, it should return FALSE, as no correct winner was predicted.
  return((twodata[[1]] > 0 & twodata[[2]] > 0)|(twodata[[1]] < 0 & twodata[[2]] < 0) )
}

WinnerPredicted = function(SeasonData) { #Takes in Season Data that has been cleaned up, and has a results column (e.g. "ElevenTwelves") and returns whether the line, opening line, and each prediction system correctly predicted the winner of the game.
  line.accurate = apply(SeasonData[,c("result", "line")], 1, LineCorrectHelper)
  lineopen.accurate = apply(SeasonData[,c("result", "lineopen")], 1, LineCorrectHelper)
  sag.accurate = apply(SeasonData[,c("result","linesag")], 1, LineCorrectHelper)
  sage.accurate = apply(SeasonData[,c("result", "linesage")], 1, LineCorrectHelper)
  sagp.accurate = apply(SeasonData[,c("result", "linesagp")], 1, LineCorrectHelper)
  moore.accurate = apply(SeasonData[,c("result", "linemoore")], 1, LineCorrectHelper)
  dok.accurate = apply(SeasonData[,c("result", "linedok")], 1, LineCorrectHelper)
  fox.accurate = apply(SeasonData[,c("result", "linefox")],1, LineCorrectHelper)
  return(data.frame(SeasonData[c("game.index", "date.index", "date", "home", "hscore", "road", "rscore", "result", "line", "lineopen")], line.accurate, lineopen.accurate, sag.accurate, sage.accurate, sagp.accurate, moore.accurate, dok.accurate, fox.accurate)
         )
}

#Each of the following data frames has information for every game after January 1 in a season, as well as whether each system's (including the line's/opening line's) predictions were correct.'
ElevenTwelveWinnerPredicted = WinnerPredicted(ElevenTwelve)
TenElevenWinnerPredicted = WinnerPredicted(TenEleven)
NineTenWinnerPredicted = WinnerPredicted(NineTen)
EightNineWinnerPredicted = WinnerPredicted(EightNine)
SevenEightWinnerPredicted = WinnerPredicted(SevenEight)

#Each of the following numerics contains data on how accurate each system was in predicting game winners over the course of a season.
#These will all be used in a summary data frame.
Line.Accuracy = c(mean(ElevenTwelveWinnerPredicted$line.accurate),
                 mean(TenElevenWinnerPredicted$line.accurate),
                 mean(NineTenWinnerPredicted$line.accurate),
                 mean(EightNineWinnerPredicted$line.accurate),
                 mean(SevenEightWinnerPredicted$line.accurate),
                 mean(c(ElevenTwelveWinnerPredicted$line.accurate,
                        TenElevenWinnerPredicted$line.accurate,
                        NineTenWinnerPredicted$line.accurate,
                        EightNineWinnerPredicted$line.accurate,
                        SevenEightWinnerPredicted$line.accurate
                        )
                      )
                 )

Opening.Line.Accuracy = c(mean(ElevenTwelveWinnerPredicted$lineopen.accurate),
                          mean(TenElevenWinnerPredicted$lineopen.accurate),
                          mean(NineTenWinnerPredicted$lineopen.accurate),
                          mean(EightNineWinnerPredicted$lineopen.accurate),
                          mean(SevenEightWinnerPredicted$lineopen.accurate),
                          mean(c(ElevenTwelveWinnerPredicted$lineopen.accurate,
                                 TenElevenWinnerPredicted$lineopen.accurate,
                                 NineTenWinnerPredicted$lineopen.accurate,
                                 EightNineWinnerPredicted$lineopen.accurate,
                                 SevenEightWinnerPredicted$lineopen.accurate
                                 )
                               )
                          )

Sagarin.Accuracy = c(mean(ElevenTwelveWinnerPredicted$sag.accurate),
                          mean(TenElevenWinnerPredicted$sag.accurate),
                          mean(NineTenWinnerPredicted$sag.accurate),
                          mean(EightNineWinnerPredicted$sag.accurate),
                          mean(SevenEightWinnerPredicted$sag.accurate),
                          mean(c(ElevenTwelveWinnerPredicted$sag.accurate,
                                 TenElevenWinnerPredicted$sag.accurate,
                                 NineTenWinnerPredicted$sag.accurate,
                                 EightNineWinnerPredicted$sag.accurate,
                                 SevenEightWinnerPredicted$sag.accurate
                                 )
                               )
                          )

Sagarin.ELO.Accuracy = c(mean(ElevenTwelveWinnerPredicted$sage.accurate),
                          mean(TenElevenWinnerPredicted$sage.accurate),
                          mean(NineTenWinnerPredicted$sage.accurate),
                          mean(EightNineWinnerPredicted$sage.accurate),
                          mean(SevenEightWinnerPredicted$sage.accurate),
                          mean(c(ElevenTwelveWinnerPredicted$sage.accurate,
                                 TenElevenWinnerPredicted$sage.accurate,
                                 NineTenWinnerPredicted$sage.accurate,
                                 EightNineWinnerPredicted$sage.accurate,
                                 SevenEightWinnerPredicted$sage.accurate
                                 )
                               )
                          )

Sagarin.Predictive.Accuracy = c(mean(ElevenTwelveWinnerPredicted$sagp.accurate),
                               mean(TenElevenWinnerPredicted$sagp.accurate),
                               mean(NineTenWinnerPredicted$sagp.accurate),
                               mean(EightNineWinnerPredicted$sagp.accurate),
                               mean(SevenEightWinnerPredicted$sagp.accurate),
                               mean(c(ElevenTwelveWinnerPredicted$sagp.accurate,
                                      TenElevenWinnerPredicted$sagp.accurate,
                                      NineTenWinnerPredicted$sagp.accurate,
                                      EightNineWinnerPredicted$sagp.accurate,
                                      SevenEightWinnerPredicted$sagp.accurate
                                 )
                               )
                          )

Sonny.Moore.Accuracy = c(mean(ElevenTwelveWinnerPredicted$moore.accurate),
                          mean(TenElevenWinnerPredicted$moore.accurate),
                          mean(NineTenWinnerPredicted$moore.accurate),
                          mean(EightNineWinnerPredicted$moore.accurate),
                          mean(SevenEightWinnerPredicted$moore.accurate),
                          mean(c(ElevenTwelveWinnerPredicted$moore.accurate,
                                 TenElevenWinnerPredicted$moore.accurate,
                                 NineTenWinnerPredicted$moore.accurate,
                                 EightNineWinnerPredicted$moore.accurate,
                                 SevenEightWinnerPredicted$moore.accurate
                                 )
                               )
                          )

Jon.Dokter.Accuracy = c(mean(ElevenTwelveWinnerPredicted$dok.accurate),
                          mean(TenElevenWinnerPredicted$dok.accurate),
                          mean(NineTenWinnerPredicted$dok.accurate),
                          mean(EightNineWinnerPredicted$dok.accurate),
                          mean(SevenEightWinnerPredicted$dok.accurate),
                          mean(c(ElevenTwelveWinnerPredicted$dok.accurate,
                                 TenElevenWinnerPredicted$dok.accurate,
                                 NineTenWinnerPredicted$dok.accurate,
                                 EightNineWinnerPredicted$dok.accurate,
                                 SevenEightWinnerPredicted$dok.accurate
                                 )
                               )
                          )

StatFox.Accuracy = c(mean(ElevenTwelveWinnerPredicted$fox.accurate),
                          mean(TenElevenWinnerPredicted$fox.accurate),
                          mean(NineTenWinnerPredicted$fox.accurate),
                          mean(EightNineWinnerPredicted$fox.accurate),
                          mean(SevenEightWinnerPredicted$fox.accurate),
                          mean(c(ElevenTwelveWinnerPredicted$fox.accurate,
                                 TenElevenWinnerPredicted$fox.accurate,
                                 NineTenWinnerPredicted$fox.accurate,
                                 EightNineWinnerPredicted$fox.accurate,
                                 SevenEightWinnerPredicted$fox.accurate
                                 )
                               )
                          )

#This is a data frame summarizing the accuracies of each system and line over each season, as well as overall.
AccuracySummary = data.frame(Line.Accuracy,
                             Opening.Line.Accuracy,
                             Sagarin.Accuracy,
                             Sagarin.ELO.Accuracy,
                             Sagarin.Predictive.Accuracy,
                             Sonny.Moore.Accuracy,
                             Jon.Dokter.Accuracy,
                             StatFox.Accuracy,
                             row.names = c("2011-2012",
                                           "2010-2011",
                                           "2009-2010",
                                           "2008-2009",
                                           "2007-2008",
                                           "Overall"
                                           )
                             )

#AccuracySummary is stored as a PDF file in the Images folder.
View(AccuracySummary)




#################################################################################################
#################################################################################################
#Question Set 2: How often do the prediction models for which we have data beat the spread?     #
#How often do they correctly predict outcomes of games in relation to the line and opening line?#
#################################################################################################
#################################################################################################
ResultsHelper = function(twodata) { #Helper function for AllLineResults. Takes a data.frame of two items. Compares the two. If the first is larger, returns TRUE or 1. If smaller, returns FALSE or 0. If tie, returns 0.5.
  if(twodata[[1]] == twodata[[2]])
    {return(0.5)
  } else {
    return(twodata[[1]]>twodata[[2]])
  }
}

AllLineResults = function(SeasonData) { #Takes in Season Data that has been cleaned up and has a results column (e.g. "ElevenTwelve"). Returns a data frame with games, whether the home team beat the line, and whether each prediction system predicted the home team beating the line.
  home.beat.line = apply(SeasonData[,c("result", "line")],1,ResultsHelper)
  sag.beat.line = apply(SeasonData[,c("linesag", "line")],1,ResultsHelper)
  sage.beat.line = apply(SeasonData[,c("linesage", "line")],1,ResultsHelper)
  sagp.beat.line = apply(SeasonData[,c("linesagp", "line")],1,ResultsHelper)
  moore.beat.line = apply(SeasonData[,c("linemoore", "line")],1,ResultsHelper)
  dok.beat.line = apply(SeasonData[,c("linedok", "line")],1,ResultsHelper)
  fox.beat.line = apply(SeasonData[,c("linefox", "line")],1,ResultsHelper)
  return(data.frame(SeasonData[c("game.index", "date.index", "date", "home", "hscore", "road", "rscore", "result", "line")], home.beat.line, sag.beat.line, sage.beat.line, sagp.beat.line, moore.beat.line, dok.beat.line, fox.beat.line))
}

AllOpeningLineResults = function(SeasonData) { #Takes in Season Data that has been cleaned up and has a results column (e.g. "ElevenTwelve". Returns a data frame with games, whether the home team beat the opening line, and whether each prediction system predicted the home team beating the opening line.
  home.beat.line = apply(SeasonData[,c("result", "lineopen")],1,ResultsHelper)
  sag.beat.line = apply(SeasonData[,c("linesag", "lineopen")],1,ResultsHelper)
  sage.beat.line = apply(SeasonData[,c("linesage", "lineopen")],1,ResultsHelper)
  sagp.beat.line = apply(SeasonData[,c("linesagp", "lineopen")],1,ResultsHelper)
  moore.beat.line = apply(SeasonData[,c("linemoore", "lineopen")],1,ResultsHelper)
  dok.beat.line = apply(SeasonData[,c("linedok", "lineopen")],1,ResultsHelper)
  fox.beat.line = apply(SeasonData[,c("linefox", "lineopen")],1,ResultsHelper)
  return(data.frame(SeasonData[c("game.index", "date.index", "date", "home", "hscore", "road", "rscore", "result", "lineopen")], home.beat.line, sag.beat.line, sage.beat.line, sagp.beat.line, moore.beat.line, dok.beat.line, fox.beat.line))
}


ElevenTwelveLinePredictions = AllLineResults(ElevenTwelve)
ElevenTwelveOpeningLinePredictions = AllOpeningLineResults(ElevenTwelve)
TenElevenLinePredictions = AllLineResults(TenEleven)
TenElevenOpeningLinePredictions = AllOpeningLineResults(TenEleven)
NineTenLinePredictions = AllLineResults(NineTen)
NineTenOpeningLinePredictions = AllOpeningLineResults(NineTen)
EightNineLinePredictions = AllLineResults(EightNine)
EightNineOpeningLinePredictions = AllOpeningLineResults(EightNine)
SevenEightLinePredictions = AllLineResults(SevenEight)
SevenEightOpeningLinePredictions = AllOpeningLineResults(SevenEight)

TheSameHelper = function(twodata) { #Helper function for CorrectPredictions. Returns whether two entries in the data frame are equivalent.
  return(twodata[[1]] == twodata[[2]])
}

CorrectPredictions = function(SeasonData) { #Takes in Season Data (e.g. "ElevenTwelveLinePredictions") and returns whether each prediction system correctly predicted the result. If the entry is TRUE, the prediction system got it right.
  sag.correct = apply(SeasonData[,c("home.beat.line","sag.beat.line")], 1, TheSameHelper)
  sage.correct = apply(SeasonData[,c("home.beat.line", "sage.beat.line")], 1, TheSameHelper)
  sagp.correct = apply(SeasonData[,c("home.beat.line", "sagp.beat.line")], 1, TheSameHelper)
  moore.correct = apply(SeasonData[,c("home.beat.line", "moore.beat.line")], 1, TheSameHelper)
  dok.correct = apply(SeasonData[,c("home.beat.line", "dok.beat.line")], 1, TheSameHelper)
  fox.correct = apply(SeasonData[,c("home.beat.line", "fox.beat.line")],1, TheSameHelper)
  return(data.frame(SeasonData[c("game.index", "date.index", "date", "home", "hscore", "road", "rscore", "result", "line", "home.beat.line")], sag.correct, sage.correct, sagp.correct, moore.correct, dok.correct, fox.correct))
}

CorrectOpeningPredictions = function(SeasonData) { #Takes in Season Data (e.g. "ElevenTwelveLinePredictions") and returns whether each prediction system correctly predicted the result. If the entry is TRUE, the prediction system got it right.
  sag.correct = apply(SeasonData[,c("home.beat.line","sag.beat.line")], 1, TheSameHelper)
  sage.correct = apply(SeasonData[,c("home.beat.line", "sage.beat.line")], 1, TheSameHelper)
  sagp.correct = apply(SeasonData[,c("home.beat.line", "sagp.beat.line")], 1, TheSameHelper)
  moore.correct = apply(SeasonData[,c("home.beat.line", "moore.beat.line")], 1, TheSameHelper)
  dok.correct = apply(SeasonData[,c("home.beat.line", "dok.beat.line")], 1, TheSameHelper)
  fox.correct = apply(SeasonData[,c("home.beat.line", "fox.beat.line")],1, TheSameHelper)
  return(data.frame(SeasonData[c("game.index", "date.index", "date", "home", "hscore", "road", "rscore", "result", "lineopen", "home.beat.line")], sag.correct, sage.correct, sagp.correct, moore.correct, dok.correct, fox.correct))
}


#####################################################################
#Data frames with how each system did on every game in our data sets#
#####################################################################
ElevenTwelveResults = CorrectPredictions(ElevenTwelveLinePredictions)
ElevenTwelveOpeningResults = CorrectOpeningPredictions(ElevenTwelveOpeningLinePredictions)
TenElevenResults = CorrectPredictions(TenElevenLinePredictions)
TenElevenOpeningResults = CorrectOpeningPredictions(TenElevenOpeningLinePredictions)
NineTenResults = CorrectPredictions(NineTenLinePredictions)
NineTenOpeningResults = CorrectOpeningPredictions(NineTenOpeningLinePredictions)
EightNineResults = CorrectPredictions(EightNineLinePredictions)
EightNineOpeningResults = CorrectOpeningPredictions(EightNineOpeningLinePredictions)
SevenEightResults = CorrectPredictions(SevenEightLinePredictions)
SevenEightOpeningResults = CorrectOpeningPredictions(SevenEightOpeningLinePredictions)


#############################################################
#Summary of results of systems against the line/opening line#
#############################################################
Sagarin = c(mean(ElevenTwelveOpeningResults$sag.correct),
            mean(ElevenTwelveResults$sag.correct),
            mean(TenElevenOpeningResults$sag.correct),
            mean(TenElevenResults$sag.correct),
            mean(NineTenOpeningResults$sag.correct),
            mean(NineTenResults$sag.correct),
            mean(EightNineOpeningResults$sag.correct),
            mean(EightNineResults$sag.correct),
            mean(SevenEightOpeningResults$sag.correct),
            mean(SevenEightResults$sag.correct),
            mean(c(ElevenTwelveOpeningResults$sag.correct,
                   ElevenTwelveResults$sag.correct,
                   TenElevenOpeningResults$sag.correct,
                   TenElevenResults$sag.correct,
                   NineTenOpeningResults$sag.correct,
                   NineTenResults$sag.correct,
                   EightNineOpeningResults$sag.correct,
                   EightNineResults$sag.correct,
                   SevenEightOpeningResults$sag.correct,
                   SevenEightResults$sag.correct
                   )
                 )
            )

Sagarin.ELO = c(mean(ElevenTwelveOpeningResults$sage.correct),
            mean(ElevenTwelveResults$sage.correct),
            mean(TenElevenOpeningResults$sage.correct),
            mean(TenElevenResults$sage.correct),
            mean(NineTenOpeningResults$sage.correct),
            mean(NineTenResults$sage.correct),
            mean(EightNineOpeningResults$sage.correct),
            mean(EightNineResults$sage.correct),
            mean(SevenEightOpeningResults$sage.correct),
            mean(SevenEightResults$sage.correct),
            mean(c(ElevenTwelveOpeningResults$sage.correct,
                   ElevenTwelveResults$sage.correct,
                   TenElevenOpeningResults$sage.correct,
                   TenElevenResults$sage.correct,
                   NineTenOpeningResults$sage.correct,
                   NineTenResults$sage.correct,
                   EightNineOpeningResults$sage.correct,
                   EightNineResults$sage.correct,
                   SevenEightOpeningResults$sage.correct,
                   SevenEightResults$sage.correct
                   )
                 )
            )

Sagarin.Predictive = c(mean(ElevenTwelveOpeningResults$sagp.correct),
            mean(ElevenTwelveResults$sagp.correct),
            mean(TenElevenOpeningResults$sagp.correct),
            mean(TenElevenResults$sagp.correct),
            mean(NineTenOpeningResults$sagp.correct),
            mean(NineTenResults$sagp.correct),
            mean(EightNineOpeningResults$sagp.correct),
            mean(EightNineResults$sagp.correct),
            mean(SevenEightOpeningResults$sagp.correct),
            mean(SevenEightResults$sagp.correct),
            mean(c(ElevenTwelveOpeningResults$sagp.correct,
                   ElevenTwelveResults$sagp.correct,
                   TenElevenOpeningResults$sagp.correct,
                   TenElevenResults$sagp.correct,
                   NineTenOpeningResults$sagp.correct,
                   NineTenResults$sagp.correct,
                   EightNineOpeningResults$sagp.correct,
                   EightNineResults$sagp.correct,
                   SevenEightOpeningResults$sagp.correct,
                   SevenEightResults$sagp.correct
                   )
                 )
                       )

Sonny.Moore = c(mean(ElevenTwelveOpeningResults$moore.correct),
            mean(ElevenTwelveResults$moore.correct),
            mean(TenElevenOpeningResults$moore.correct),
            mean(TenElevenResults$moore.correct),
            mean(NineTenOpeningResults$moore.correct),
            mean(NineTenResults$moore.correct),
            mean(EightNineOpeningResults$moore.correct),
            mean(EightNineResults$moore.correct),
            mean(SevenEightOpeningResults$moore.correct),
            mean(SevenEightResults$moore.correct),
            mean(c(ElevenTwelveOpeningResults$moore.correct,
                   ElevenTwelveResults$moore.correct,
                   TenElevenOpeningResults$moore.correct,
                   TenElevenResults$moore.correct,
                   NineTenOpeningResults$moore.correct,
                   NineTenResults$moore.correct,
                   EightNineOpeningResults$moore.correct,
                   EightNineResults$moore.correct,
                   SevenEightOpeningResults$moore.correct,
                   SevenEightResults$moore.correct
                   )
                 )
                )

Jon.Dokter = c(mean(ElevenTwelveOpeningResults$dok.correct),
            mean(ElevenTwelveResults$dok.correct),
            mean(TenElevenOpeningResults$dok.correct),
            mean(TenElevenResults$dok.correct),
            mean(NineTenOpeningResults$dok.correct),
            mean(NineTenResults$dok.correct),
            mean(EightNineOpeningResults$dok.correct),
            mean(EightNineResults$dok.correct),
            mean(SevenEightOpeningResults$dok.correct),
            mean(SevenEightResults$dok.correct),
            mean(c(ElevenTwelveOpeningResults$dok.correct,
                   ElevenTwelveResults$dok.correct,
                   TenElevenOpeningResults$dok.correct,
                   TenElevenResults$dok.correct,
                   NineTenOpeningResults$dok.correct,
                   NineTenResults$dok.correct,
                   EightNineOpeningResults$dok.correct,
                   EightNineResults$dok.correct,
                   SevenEightOpeningResults$dok.correct,
                   SevenEightResults$dok.correct
                   )
                 )
               )

StatFox = c(mean(ElevenTwelveOpeningResults$fox.correct),
            mean(ElevenTwelveResults$fox.correct),
            mean(TenElevenOpeningResults$fox.correct),
            mean(TenElevenResults$fox.correct),
            mean(NineTenOpeningResults$fox.correct),
            mean(NineTenResults$fox.correct),
            mean(EightNineOpeningResults$fox.correct),
            mean(EightNineResults$fox.correct),
            mean(SevenEightOpeningResults$fox.correct),
            mean(SevenEightResults$fox.correct),
            mean(c(ElevenTwelveOpeningResults$fox.correct,
                   ElevenTwelveResults$fox.correct,
                   TenElevenOpeningResults$fox.correct,
                   TenElevenResults$fox.correct,
                   NineTenOpeningResults$fox.correct,
                   NineTenResults$fox.correct,
                   EightNineOpeningResults$fox.correct,
                   EightNineResults$fox.correct,
                   SevenEightOpeningResults$fox.correct,
                   SevenEightResults$fox.correct
                   )
                )
            )

#PredictionResults is a data frame containing information on how each prediction system was against the line/opening line over the course of each season, as well as over all seasons.
PredictionResults = data.frame(Sagarin, 
                     Sagarin.ELO,
                     Sagarin.Predictive,
                     Sonny.Moore,
                     Jon.Dokter,
                     StatFox,
                     row.names = c("11-12 Against Opening Line",
                                   "11-12 Against Line",
                                   "10-11 Against Opening Line",
                                   "10-11 Against Line",
                                   "09-10 Against Opening Line",
                                   "09-10 Against Line",
                                   "08-09 Against Opening Line",
                                   "08-09 Against Line",
                                   "07-08 Against Opening Line",
                                   "07-08 Against Line",
                                   "Average over Five Seasons"
                                   )
                     )

#PredictionResults is saved as a pdf in the "Images" folder.
View(PredictionResults) 
  

########################
########################
#Tests for Significance#
########################
########################

#Each of the following numerics represents the p-value of a Mann-Whitney test determining whether there is a significant difference between the prediction system and coin-flipping.
p.sag = wilcox.test(as.numeric(c(ElevenTwelveOpeningResults$sag.correct,
                   ElevenTwelveResults$sag.correct,
                   TenElevenOpeningResults$sag.correct,
                   TenElevenResults$sag.correct,
                   NineTenOpeningResults$sag.correct,
                   NineTenResults$sag.correct,
                   EightNineOpeningResults$sag.correct,
                   EightNineResults$sag.correct,
                   SevenEightOpeningResults$sag.correct,
                   SevenEightResults$sag.correct
                   )),
             sample(c(0,1),
                    length(c(ElevenTwelveOpeningResults$sag.correct,
                             ElevenTwelveResults$sag.correct,
                             TenElevenOpeningResults$sag.correct,
                             TenElevenResults$sag.correct,
                             NineTenOpeningResults$sag.correct,
                             NineTenResults$sag.correct,
                             EightNineOpeningResults$sag.correct,
                             EightNineResults$sag.correct,
                             SevenEightOpeningResults$sag.correct,
                             SevenEightResults$sag.correct
                             )
                           ),
                    replace = TRUE
                    )
             )[[3]]

p.sage = wilcox.test(as.numeric(c(ElevenTwelveOpeningResults$sage.correct,
                   ElevenTwelveResults$sage.correct,
                   TenElevenOpeningResults$sage.correct,
                   TenElevenResults$sage.correct,
                   NineTenOpeningResults$sage.correct,
                   NineTenResults$sage.correct,
                   EightNineOpeningResults$sage.correct,
                   EightNineResults$sage.correct,
                   SevenEightOpeningResults$sage.correct,
                   SevenEightResults$sage.correct
                   )),
             sample(c(0,1),
                    length(c(ElevenTwelveOpeningResults$sage.correct,
                             ElevenTwelveResults$sage.correct,
                             TenElevenOpeningResults$sage.correct,
                             TenElevenResults$sage.correct,
                             NineTenOpeningResults$sage.correct,
                             NineTenResults$sage.correct,
                             EightNineOpeningResults$sage.correct,
                             EightNineResults$sage.correct,
                             SevenEightOpeningResults$sage.correct,
                             SevenEightResults$sage.correct
                             )
                           ),
                    replace = TRUE
                    )
             )[[3]]

p.sagp = wilcox.test(as.numeric(c(ElevenTwelveOpeningResults$sagp.correct,
                   ElevenTwelveResults$sagp.correct,
                   TenElevenOpeningResults$sagp.correct,
                   TenElevenResults$sagp.correct,
                   NineTenOpeningResults$sagp.correct,
                   NineTenResults$sagp.correct,
                   EightNineOpeningResults$sagp.correct,
                   EightNineResults$sagp.correct,
                   SevenEightOpeningResults$sagp.correct,
                   SevenEightResults$sagp.correct
                   )),
             sample(c(0,1),
                    length(c(ElevenTwelveOpeningResults$sagp.correct,
                             ElevenTwelveResults$sagp.correct,
                             TenElevenOpeningResults$sagp.correct,
                             TenElevenResults$sagp.correct,
                             NineTenOpeningResults$sagp.correct,
                             NineTenResults$sagp.correct,
                             EightNineOpeningResults$sagp.correct,
                             EightNineResults$sagp.correct,
                             SevenEightOpeningResults$sagp.correct,
                             SevenEightResults$sagp.correct
                             )
                           ),
                    replace = TRUE
                    )
             )[[3]]

p.moore = wilcox.test(as.numeric(c(ElevenTwelveOpeningResults$moore.correct,
                   ElevenTwelveResults$moore.correct,
                   TenElevenOpeningResults$moore.correct,
                   TenElevenResults$moore.correct,
                   NineTenOpeningResults$moore.correct,
                   NineTenResults$moore.correct,
                   EightNineOpeningResults$moore.correct,
                   EightNineResults$moore.correct,
                   SevenEightOpeningResults$moore.correct,
                   SevenEightResults$moore.correct
                   )),
             sample(c(0,1),
                    length(c(ElevenTwelveOpeningResults$moore.correct,
                             ElevenTwelveResults$moore.correct,
                             TenElevenOpeningResults$moore.correct,
                             TenElevenResults$moore.correct,
                             NineTenOpeningResults$moore.correct,
                             NineTenResults$moore.correct,
                             EightNineOpeningResults$moore.correct,
                             EightNineResults$moore.correct,
                             SevenEightOpeningResults$moore.correct,
                             SevenEightResults$moore.correct
                             )
                           ),
                    replace = TRUE
                    )
             )[[3]]

p.dok = wilcox.test(as.numeric(c(ElevenTwelveOpeningResults$dok.correct,
                   ElevenTwelveResults$dok.correct,
                   TenElevenOpeningResults$dok.correct,
                   TenElevenResults$dok.correct,
                   NineTenOpeningResults$dok.correct,
                   NineTenResults$dok.correct,
                   EightNineOpeningResults$dok.correct,
                   EightNineResults$dok.correct,
                   SevenEightOpeningResults$dok.correct,
                   SevenEightResults$dok.correct
                   )),
             sample(c(0,1),
                    length(c(ElevenTwelveOpeningResults$dok.correct,
                             ElevenTwelveResults$dok.correct,
                             TenElevenOpeningResults$dok.correct,
                             TenElevenResults$dok.correct,
                             NineTenOpeningResults$dok.correct,
                             NineTenResults$dok.correct,
                             EightNineOpeningResults$dok.correct,
                             EightNineResults$dok.correct,
                             SevenEightOpeningResults$dok.correct,
                             SevenEightResults$dok.correct
                             )
                           ),
                    replace = TRUE
                    )
             )[[3]]

p.fox = wilcox.test(as.numeric(c(ElevenTwelveOpeningResults$fox.correct,
                   ElevenTwelveResults$fox.correct,
                   TenElevenOpeningResults$fox.correct,
                   TenElevenResults$fox.correct,
                   NineTenOpeningResults$fox.correct,
                   NineTenResults$fox.correct,
                   EightNineOpeningResults$fox.correct,
                   EightNineResults$fox.correct,
                   SevenEightOpeningResults$fox.correct,
                   SevenEightResults$fox.correct
                   )),
             sample(c(0,1),
                    length(c(ElevenTwelveOpeningResults$fox.correct,
                             ElevenTwelveResults$fox.correct,
                             TenElevenOpeningResults$fox.correct,
                             TenElevenResults$fox.correct,
                             NineTenOpeningResults$fox.correct,
                             NineTenResults$fox.correct,
                             EightNineOpeningResults$fox.correct,
                             EightNineResults$fox.correct,
                             SevenEightOpeningResults$fox.correct,
                             SevenEightResults$fox.correct
                             )
                           ),
                    replace = TRUE
                    )
             )[[3]]

P.Values.For.Predictions.Against.Spread = data.frame(p.sag,
                                                     p.sage,
                                                     p.sagp, 
                                                     p.moore, 
                                                     p.dok, 
                                                     p.fox,
                                                     row.names = c("P Values for Test Against Coin Flipping"
                                                                   )
                                                     )

View(P.Values.For.Predictions.Against.Spread) #Saved as a PDF in the Images folder.


#####################################
#Are Accuracy Against the Spread and# 
#####################################
#From Accuracy Results and Prediction Results Against the Spread
acc = c(0.48657162, 0.4887161 ,0.49075205 ,0.49838115, 0.49434036, 0.4561874)
sacc = c(0.7264186 , 0.7135225, 0.7269713, 0.7254053, 0.7309322, 0.7229182)
lm(sacc ~ acc)
summary(lm(sacc ~ acc))

#Call:
#lm(formula = sacc ~ acc)

#Residuals:
#         1          2          3          4          5          6 
# 1.995e-03 -1.108e-02  2.200e-03  3.772e-07  5.863e-03  1.020e-03 

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  0.68398    0.09294   7.360  0.00182 **
#acc          0.08311    0.19122   0.435  0.68624   
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#Residual standard error: 0.006461 on 4 degrees of freedom
#Multiple R-squared: 0.0451,  Adjusted R-squared: -0.1936 
#F-statistic: 0.1889 on 1 and 4 DF,  p-value: 0.6862 

#This plot is saved in the Images folder
png("NoRelationship.png", width = 800, height = 600)
plot(acc, sacc, xlab = "Accuracy Against Spread", ylab = "Straight Up Accuracy", pch = 20, main = "No Relationship; R^2 = 0.0451", cex = 2)
abline(a = 0.68398, b = 0.08311, col = "RED")
dev.off()

################
################
#Visualizations#
################
################

####################################################################
#Plots of the accuracy all prediction systems as a function of time#
####################################################################
#Time is game index number.
#The first game of the new year is considered to be the first game of analysis (as it would be the first game for which we are trusting the system to predict correctly); therefore, the accuracy fluctuates wildly at first, before stabilizing.

AccuracyOverTime = function(SeasonData, Predictor, color = "BLACK") { #Plots accuracy of the predictor for all games to a point in time as a function of time. In this case, time will be game index number.
    for (i in 1:length(SeasonData$game.index)) {
    points(i + min(SeasonData$game.index) - 1,mean(SeasonData[Predictor][1:i,]), pch = ".", col = color)
  }
}

AccuracyTimePlot = function(LineData, OpeningLineData, title, ymin = 0, ymax = 1, legendsize = 1, filename, filewidth = 800, fileheight = 600) { #This function takes in data for the Line and Opening Line, a Title, and Minimum/Maximum for y, and spits out a graph of how each predictor does over time (game index number). Legend size can also be toggled.
png(filename, width = filewidth, height = fileheight)
plot(0,0, xlim = c(min(LineData$game.index), max(LineData$game.index)), ylim = (c(ymin, ymax)), main = title, ylab = "% of Correct Predictions", xlab = "Game Index Number")
AccuracyOverTime(OpeningLineData, "sag.correct", "pink")
AccuracyOverTime(LineData, "sag.correct", "red")
AccuracyOverTime(OpeningLineData, "sage.correct", "gold")
AccuracyOverTime(LineData, "sage.correct", "orange")
AccuracyOverTime(OpeningLineData, "sagp.correct", "green")
AccuracyOverTime(LineData, "sagp.correct", "green3")
AccuracyOverTime(OpeningLineData, "moore.correct", "lightblue")
AccuracyOverTime(LineData, "moore.correct", "blue")
AccuracyOverTime(OpeningLineData, "dok.correct", "magenta1")
AccuracyOverTime(LineData, "dok.correct", "magenta4")
AccuracyOverTime(OpeningLineData, "fox.correct", "purple")
AccuracyOverTime(LineData, "fox.correct", "purple4")
abline(h = 0.5)
legend("topright",
       c("Sagarin Against Opening", "Sagarin Against Line", "Sagarin ELO Against Opening", "Sagarin ELO Against Line", "Sagarin Predictive Against Opening", "Sagarin Predictive Against Line", "Moore Against Opening", "Moore Against Line", "Dokter Against Opening", "Dokter Against Line", "StatFox Against Opening", "StatFox Against Line", "50% Accuracy"), 
       pch = 19, cex = legendsize,
       col=c("pink", "red", "gold", "orange", "green", "green3", "lightblue", "blue", "magenta1", "magenta4", "purple", "purple4", "black"))
dev.off()
}

setwd("C:/Users/Vincent/Documents/Berkeley/S133FinalProject/Images")
#2011-2012
AccuracyTimePlot(ElevenTwelveResults, ElevenTwelveOpeningResults, "2011-2012: Accuracy at Different Points in Time", ymin = 0, ymax = 1, legendsize = 1, filename = "1112Accuracy.png", filewidth = 800, fileheight = 600)
#This is the zoomed in version of the previous graph for clarity, as all of the frequencies jumble up.
AccuracyTimePlot(ElevenTwelveResults, ElevenTwelveOpeningResults, "2011-2012: Accuracy at Different Points in Time (Zoomed)", ymin = 0.4, ymax = 0.6, legendsize = 1, filename = "1112AccuracyZoomed.png", filewidth = 800, fileheight = 600)

#2010-2011
AccuracyTimePlot(TenElevenResults, TenElevenOpeningResults, "2010-2011: Accuracy at Different Points in Time", ymin = 0, ymax = 1, legendsize = 1, filename = "1011Accuracy.png", filewidth = 800, fileheight = 600)
AccuracyTimePlot(TenElevenResults, TenElevenOpeningResults, "2010-2011: Accuracy at Different Points in Time (Zoomed)", ymin = 0.4, ymax = 0.6, legendsize = 0.95, filename = "1011AccuracyZoomed.png", filewidth = 800, fileheight = 600)

#2009-2010
AccuracyTimePlot(NineTenResults, NineTenOpeningResults, "2009-2010: Accuracy at Different Points in Time", ymin = 0, ymax = 1, legendsize = 1, filename = "0910Accuracy.png", filewidth = 800, fileheight = 600)
AccuracyTimePlot(NineTenResults, NineTenOpeningResults, "2009-2010: Accuracy at Different Points in Time (Zoomed)", ymin = 0.4, ymax = 0.6, legendsize = 0.775, filename = "0910AccuracyZoomed.png", filewidth = 800, fileheight = 600)

#2008-2009
AccuracyTimePlot(EightNineResults, EightNineOpeningResults, "2008-2009: Accuracy at Different Points in Time", ymin = 0, ymax = 1, legendsize = 1, filename = "0809Accuracy.png", filewidth = 800, fileheight = 600)
AccuracyTimePlot(EightNineResults, EightNineOpeningResults, "2008-2009: Accuracy at Different Points in Time (Zoomed)", ymin = 0.4, ymax = 0.6, legendsize = 0.9, filename = "0809AccuracyZoomed.png", filewidth = 800, fileheight = 600)

#2007-2008
AccuracyTimePlot(SevenEightResults, SevenEightOpeningResults, "2007-2008: Accuracy at Different Points in Time", ymin = 0, ymax = 1, legendsize = 1, filename = "0708Accuracy.png", filewidth = 800, fileheight = 600)
AccuracyTimePlot(SevenEightResults, SevenEightOpeningResults, "2007-2008: Accuracy at Different Points in Time (Zoomed)", ymin = 0.4, ymax = 0.6, legendsize = 1, filename = "0708AccuracyZoomed.png", filewidth = 800, fileheight = 600)


#############################################################
#Graphs of the accuracies of different systems over a season#
#############################################################
# Function to generate bar graphs to see the accuracy of each prediction system to A) Line or B) Opening Line
plotfxn = function(r1, r2, title, filename)
  # Examples for variables:
  # r1 = ElevenNineResults (line), r2 = ElevenNineOpeningResults (opening line)
  # title = 'Accuracy of Prediction Systems against Lines from year'
  # filename = "plot1.png"
{line = length(r1$sag.correct)
 
# Comparing predictions to line. Average = Correct predictions divided by total predictions.
 A = sum(r1$sag.correct)/line
 B = sum(r1$sage.correct)/line
 C = sum(r1$sagp.correct)/line
 D = sum(r1$moore.correct)/line
 E = sum(r1$dok.correct)/line
 F = sum(r1$fox.correct)/line
 # Comparing predictions to opening line. Average = Correct predictions divided by total predictions.
 oA = sum(r2$sag.correct)/line
 oB = sum(r2$sage.correct)/line
 oC = sum(r2$sagp.correct)/line
 oD = sum(r2$moore.correct)/line
 oE = sum(r2$dok.correct)/line
 oF = sum(r2$fox.correct)/line
# Putting in data frame format
 a = c(A, oA); b = c(B, oB); c = c(C, oC)
 d = c(D, oD); e = c(E, oE); f = c(F, oF)
# Making data frame  #first row = line, second row = opening
 alldata= data.frame(sag = a, sage=b, sagp=c, moore=d, dok=e, fox=f)
# Save plot as PNG
 png(filename)
 # Making bar graph - want adjacent and vertical bars
 bp = barplot(as.matrix(alldata), main=title,
              xlab="Prediction System",  ylim = c(0,0.6),
              ylab="Accuracy",horiz=FALSE,
              col=c("mistyrose","lightcyan"), beside = TRUE)
 #lightcyan = opening line
 # Making labels for the bars
 ndata = c(A, oA, B, oB, C, oC, D, oD, E, oE, F, oF)
 text(bp, 0, round(ndata, 3),cex=.55,pos=3)         
 # Making legend
 legend("topleft", c("Line","Opening"), cex = 0.7,col=c("mistyrose","lightcyan"), pch = 15)
 # Designating the 0.5 accuracy mark
 abline(a = 0.5, b=0, col = "blue")
 text(17, 0.51, "accuracy = 0.5", cex = .7)
 dev.off()
 }

# Commands for each season
plotfxn(SevenEightResults, SevenEightOpeningResults, 'Accuracy of Prediction Systems against Lines 2007-2008', 'accuracy0708.png')
plotfxn(EightNineResults, EightNineOpeningResults, 'Accuracy of Prediction Systems against Lines 2008-2009', 'accuracy0809.png')
plotfxn(NineTenResults, NineTenOpeningResults, 'Accuracy of Prediction Systems against Lines 2009-2010', 'accuracy0910.png')
plotfxn(TenElevenResults, TenElevenOpeningResults, 'Accuracy of Prediction Systems against Lines 2010-2011', 'accuracy1011.png')
plotfxn(ElevenTwelveResults, ElevenTwelveOpeningResults, 'Accuracy of Prediction Systems against Lines 2011-2012', 'accuracy1112.png')

###########################
###########################
#Google Earth Presentation#
###########################
###########################

#####################
## This R script has a few functions which all get used in the final function, LatLon.

## I have included ample comments which I think would be very helpful for anyone try-
## ing to edit any of these functions.


## NOTES:
##  PACKAGES THAT ARE NECESSARY:
## This R script has a few functions which all get used in the final function, LatLon.

## I have included ample comments which I think would be very helpful for anyone try-
## ing to edit any of these functions.



##   Initial Data Set
        Rankings_ncaabb11 <- read.csv("http://www.ocf.berkeley.edu/~vsheu/Rankings_ncaabb11.txt")
        View(Rankings_ncaabb11)
        Schools = Rankings_ncaabb11$School

##   I believe mostly all of the warnings that come from the functions are from the 
##   degrees symbol, and the weird A thing that appears when first reading the lati
##   tudes and longitudes into R (which doesn't seem to like these symbols).


        ####################################################################

## helper is a function that inputs the university's name into the geonames.org form. 

## INPUT: a character string (of a university)
## OUTPUT: raw latitude and longitude data from the geonames.org website

## NOTE: This still yields some errors, however the geonames website only accepts
## very specific input for each college/university.

## NOTE2: It is named helper because I didn't know what else to call it

helper = function(x) { 
  parsed = getForm("http://www.geonames.org/search.html", q = x, country = "") 
  tables = readHTMLTable(parsed)
  table = as.data.frame(tables$'NULL')
  lat = 0 
  lon = 0
  if (dim(table)[2] > 5) {
    whichschools = grep("^school", table[,4])
    schools = table[whichschools,]
    lat = as.character(schools[1,5])
    lon = as.character(schools[1,6])
  }
  else {
    lat = NA
    lon = NA
  }
  return(data.frame(x, lat, lon))
}

        ####################################################################

## get loops the helper function over all the names in a vector of school names.

## INPUT: a vector of character strings (of university names)
## OUTPUT: a data frame of latitudes and longitudes without any directional letterings
##         or symbols

get = function(x = Schools) {
  x = as.character(x)
  name = character(length(x))
  lats = character(length(x))
  lons = character(length(x))
  for (i in 1:length(x)) {
    latlon = helper(x[i])
    name[i] = as.character(latlon[1,1])
    lats[i] = as.character(latlon[1,2])
    lons[i] = as.character(latlon[1,3])
  }   
  a = data.frame(lats,lons)
  latlondf = apply(a, 2, function(x) gsub("Â", "", x))
  latlondf = apply(latlondf, 2, function(x) gsub("N ", "", x))
  latlondf = apply(latlondf, 2, function(x) gsub("W ", "", x))  
  latlondf = apply(latlondf, 2, function(x) gsub("°", "", x))
  latlondf = apply(latlondf, 2, function(x) gsub("'", "", x))
  b = data.frame(name,latlondf[,1],latlondf[,2])
  return(b)
}

         ####################################################################

## degminsec uses the get function and then expresses latitudes/longitudes as degrees, 
## minutes, and seconds as separated values.  It was made to be applied over columns 
## of a data frame (see below, LatLon)

## INPUT: values of latitude and longitude, from the output of get
## OUTPUT: degrees, minutes, and seconds 

degminsec = function(y) {
  deg = as.numeric(gsub(" [[:digit:]]+ [[:digit:]]+$", "", y))
  min = gsub("^[[:digit:]]+ ", "", y)
  min = as.numeric(gsub(" [[:digit:]]+$", "", min))
  sec = as.numeric(gsub("^[[:digit:]]+ [[:digit:]]+ ", "", y))
  return(data.frame(deg,min,sec))
}

        ####################################################################

## LatLon is the final function that uses the get function to retrieve latitudes and
## longitudes, it then converts these latitudes and longitudes into decimal format.

## INPUT: a vector of school names
## OUTPUT: a data frame with latitudes and longitudes expressed in decimal form. Longi-
##         tudes are negative because we are west of the prime meridian here in the US.
  
  LatLon = function(y = Schools) {
    latlondf = get(y)
    a = latlondf[,2:3]
    dms = apply(a, 2, degminsec)
    latdf = as.data.frame(dms[1])
    Latitude = latdf[1] + latdf[2]/60 + latdf[3]/3600
    londf = as.data.frame(dms[2])
    Longitude = londf[1] + londf[2]/60 + londf[3]/3600
    Longitude = Longitude * -1
    return(data.frame(latlondf[,1], Latitude, Longitude))
  }

          ######################################################

# Editing the list of school names to fit the geonames form

cleaned = gsub("St. ", "Saint ", Schools)
cleaned = gsub(" St.$", " State", cleaned)
cleaned = gsub("Cal ", "California ", cleaned)
cleaned = gsub("California-", "University of California ", cleaned)
uni = grep("University", cleaned)
cleaned[-uni] = paste(cleaned[-uni], "University")

        
# # Testing
# latlon = LatLon(cleaned)
# 
# # Errors
# NAs = apply(latlon, 1, function(x) NA %in% x)
# nas.df = data.frame(which(NAs == TRUE), cleaned[NAs])

        
# Manual Editing after checking the above Errors
cleaned[26] = "University of California, Berkeley"
cleaned[28] = "University of Nevada Las Vegas"
cleaned[39] = "Saint Mary's College of California"
cleaned[40] = "Brigham Young University"
cleaned[58] = "Virginia Technical University"
cleaned[64] = "Middle Tennessee State"
cleaned[67] = "Iona College New York"
cleaned[68] = "Saint Bonaventure"
cleaned[86] = "Louisiana State University"
cleaned[92] = "Missouri State University"
cleaned[98] = "Texas A and M University"
cleaned[111] = "University of California Santa Barbara"
cleaned[116] = "Wagner College"
cleaned[122] = "University of Texas Arlington University"
cleaned[125] = "University of Texas El Paso"
cleaned[130] = "University of Alabama Birmingham"
cleaned[142] = "Texas Christian University"
cleaned[151] = "Robert Morris College"
cleaned[159] = "South Carolina Upstate University"
cleaned[165] = "University of Southern California"
cleaned[166] = "California Polytechnic State University San Luis Obispo"
cleaned[171] = "California State University Fullerton"
cleaned[172] = "Saint Mary's University"
cleaned[181] = "Quinnipiac College"
cleaned[189] = "Louisiana Technical University"
cleaned[206] = "Wofford College"
cleaned[211] = "Tennessee Technical University"
cleaned[212] = "Indiana University Purdue University Indianapolis"
cleaned[219] = "Boston College"
cleaned[225] = "Central Connecticut University"
cleaned[231] = "High Point College"
cleaned[232] = "College of the Holy Cross"
cleaned[233] = "Saint Francis College New York"
cleaned[253] = "California State University Bakersfield"
cleaned[261] = "Winthrop College"
cleaned[265] = "Gardner Webb College"
cleaned[266] = "University of California Riverside"
cleaned[270] = "Marist College New York"
cleaned[278] = "Indiana University Purdue University at Fort Wayne"
cleaned[282] = "Bethune-Cookman College"
cleaned[285] = "Virginia Military Institute"
cleaned[295] = "College of William and Mary"
cleaned[302] = "New Jersey Institute of Technology"
cleaned[303] = "Texas A and M State University at Corpus Christi"
cleaned[304] = "North Carolina A and T"
cleaned[307] = "California State University Northridge"
cleaned[309] = "Saint Francis College Pennsylvania"
cleaned[310] = "Southern Illinois University Edwardsville"
cleaned[317] = "Canisius College"
cleaned[320] = "Florida A and M University"
cleaned[324] = "Houston Baptist College"
cleaned[330] = "Prairie View College"
cleaned[332] = "University of Tennessee at Martin"
cleaned[338] = "Alabama A and M University"

# Testing Again
latlon2 = LatLon(cleaned)
View(latlon2)

# Errors Again
NAs = apply(latlon2, 1, function(x) NA %in% x)
nas.df = data.frame(which(NAs == TRUE), cleaned[NAs])
View(nas.df)
        
#########################################################################
####################  Google Presentation ###############################

NineTenOpeningResults$home=paste(NineTenOpeningResults$home,"University")
NineTenOpeningResults$home = gsub(" St.", " State", NineTenOpeningResults$home)
NineTenOpeningResults$road=paste(NineTenOpeningResults$road,"University")
NineTenOpeningResults$road = gsub(" St.", " State", NineTenOpeningResults$road)

        ## Manual Editing NineTenOpeningResults schools names in order to merge latlon2 ##
NineTenOpeningResults$home = gsub("California University", "University of California Berkeley", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("California University", "University of California Berkeley", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("USC University", "University of Southern California", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("USC University", "University of Southern California", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("UC Davis University", "University of California Davis", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("UC Davis University", "University of California Davis", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("Troy State University", "Troy University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("Troy State University", "Troy University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("Canisius University", "Canisius College", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("Canisius University", "Canisius College", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("NC Charlotte University", "Charlotte University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("NC Charlotte University", "Charlotte University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("Cal Riverside University", "University of California Riverside", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("Cal Riverside University", "University of California Riverside", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("Middle Tenn State University", "Middle Tennessee State", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("Middle Tenn State University", "Middle Tennessee State", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("VA Commonwealth University", "Virginia Commonwealth University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("VA Commonwealth University", "Virginia Commonwealth University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("St. Joseph's PA University", "Saint Joseph's University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("St. Joseph's PA University", "Saint Joseph's University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("TCU University", "Texas Christian University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("TCU University", "Texas Christian University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("Loyola-Maryland University", "Loyola (MD) University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("Loyola-Maryland University", "Loyola (MD) University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("Virginia Tech University", "Virginia Technical University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("Virginia Tech University", "Virginia Technical University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("SE Missouri State University", "Southeast Missouri State University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("SE Missouri State University", "Southeast Missouri State University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("UC Irvine University", "University of California Irvine", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("UC Irvine University", "University of California Irvine", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("LSU University", "Louisiana State University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("LSU University", "Louisiana State University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("NC Greensboro University", "North Carolina-Greensboro University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("NC Greensboro University", "North Carolina-Greensboro University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("St. John's University", "Saint John's (NY) University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("St. John's University", "Saint John's (NY) University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("St. Peter's University", "Saint Peter's University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("St. Peter's University", "Saint Peter's University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("William & Mary University", "College of William and Mary", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("William & Mary University", "College of William and Mary", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("Charleston University", "College of Charleston University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("Charleston University", "College of Charleston University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("St. Bonaventure University", "Saint Bonaventure", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("St. Bonaventure University", "Saint Bonaventure", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("NC Wilmington University", "North Carolina-Wilmington University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("NC Wilmington University", "North Carolina-Wilmington University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("UTEP University", "University of Texas El Paso", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("UTEP University", "University of Texas El Paso", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("BYU University", "Brigham Young University", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("BYU University", "Brigham Young University", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("Cal State Fullerton University", "California State University Fullerton", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("Cal State Fullerton University", "California State University Fullerton", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("CS Northridge University", "California State University Northridge", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("CS Northridge University", "California State University Northridge", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("Cal Poly SLO University", "California Polytechnic State University San Luis Obispo", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("Cal Poly SLO University", "California Polytechnic State University San Luis Obispo", NineTenOpeningResults$road)
NineTenOpeningResults$home = gsub("UC Santa Barbara University", "University of California Santa Barbara", NineTenOpeningResults$home)
NineTenOpeningResults$road = gsub("UC Santa Barbara University", "University of California Santa Barbara", NineTenOpeningResults$road)

        
 ## Pick is a function that pick each school's beat line in both home team or road team,
 ## and calculate the beat spread for each school        
pick<-function(school)
           {for (i in 1:length(school)){
                 num1=grep(school[i],NineTenOpeningResults$home)
                 x=NineTenOpeningResults$home.beat.line[num1]
                 hometeam=length(which(x==1))
                   num2=grep(school[i],NineTenOpeningResults$road)
                   y=NineTenOpeningResults$home.beat.line[num2]
                   roadteam=length(which(y==0))
                   Beatspread[i]=(hometeam+roadteam)/(length(num1)+length(num2))
               }
              return (data.frame(school,Beatspread))
              }
#test
num1=grep("UCLA",NineTenOpeningResults$home)
                 x=NineTenOpeningResults$home.beat.line[num1]
                 hometeam=length(which(x==1))
                 num2=grep("UCLA",NineTenOpeningResults$road)
                 y=NineTenOpeningResults$home.beat.line[num2]
                 roadteam=length(which(y==0))
                 Beatspread=(hometeam+roadteam)/(length(num1)+length(num2))        
                 Beatspread        
        
school_beat=pick(latlon2$latlondf...1.)
latlon_beat=merge(latlon2,school_beat,by.x="latlondf...1.",by.y="school")

latlon_beatNAs = apply(latlon_beat, 1, function(x) NA %in% x)
latlon_beat2 = latlon_beat[latlon_beatNAs == FALSE,]
View(latlon_beat2)

addPlacemark = function(lat, lon, school, beat, parent){
  pm = newXMLNode("Placemark",
                  newXMLNode("Name", school),
                 attrs = c(id= school), parent = parent)

                 
  newXMLNode("description",
             paste(school,"\n Beat Spread: ", beat, sep = ""),
             parent = pm)
  newXMLNode("Point",
             newXMLNode("coordinates",
                        paste(lat, lon, 0, sep =",")),
             parent = pm)
}

kmlDoc = newXMLDoc()
kmlRoot = newXMLNode("kml", doc = kmlDoc)
DocN = newXMLNode("Document", parent = kmlRoot)
newXMLNode("name", "School", parent = DocN)
newXMLNode("description", "Beat Spread", parent = DocN)
newXMLNode("LookAt", newXMLNode("longitude","-121"),
           newXMLNode("latitude","43"),
           newXMLNode("altitude","4100000"),
           newXMLNode("tilt", "0"),
           newXMLNode("heading", "0"),
           newXMLNode("altitudeMode", "absolute"), parent = DocN)
newXMLNode("IconStyle", newXMLNode("scale","0.525"),newXMLNode("Icon","http://www.stat.berkeley.edu/users/nolan/data/KML/circles/yor5ball.png"), parent = YOR5)

Folder = newXMLNode("Folder", newXMLNode("name", "School Facts"), parent = DocN)

        for (i in 1:nrow(latlon_beat2)) {
               addPlacemark(
                 latlon_beat2[i,3], #Latitude
                 latlon_beat2[i,2], #Longitude
                 latlon_beat2[i,1], #School
                 latlon_beat2[i,4], #Beat Spread
                 Folder #Parent
               )
}


saveXML(kmlDoc, file = "Google.kml")

