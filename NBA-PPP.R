# Load data into dataframe `possessions`
setwd("/Users/andrewtan/Projects/Sports\ Analytics/basketball-analytics")
possessions <- read.csv("box_optical_poss.csv",
                        header = TRUE,
                        sep = ",")
possessions[is.na(possessions)] <- 0
possessions = possessions[as.character(possessions$possession.length) > "0:00", ]

# PPP Main Functions
getPPP = function(df=possessions) {
  n = nrow(df)
  points = sum(df$points)
  ppp = points / n
  return(ppp)
}
getTeamPPP = function(team_id, df=possessions) {
  team = df[df$team.global.id == team_id, ]
  ppp = getPPP(df=team)
  return(ppp)
}

## Transition PPP Helper Functions
getTransitionPPP = function(df=possessions, time="0:08") {
  transition = df[as.character(df$possession.length) <= time, ]
  ppp = getPPP(transition)
  return(ppp)
}
getTeamTransitionPPP = function(team_id, df=possessions, time="0:08") {
  team = df[df$team.global.id == team_id, ]
  ppp = getTransitionPPP(df=team)
  return(ppp)
}

## Half Court PPP Helper Functions
getHalfCourtPPP = function(df=possessions, time="0:08") {
  halfCourt = df[as.character(df$possession.length) > time, ]
  ppp = getPPP(halfCourt)
  return(ppp)
}
getTeamHalfCourtPPP = function(team_id, df=possessions, time="0:08") {
  team = df[df$team.global.id == team_id, ]
  ppp = getHalfCourtPPP(df=team)
  return(ppp)
}

## Assignment
# Get the transition and halfcourt PPP of:
# 1. Lakers
# 2. Opponents
# 3. NBA Average

getOpponentsDF = function(team_id, df=possessions) {
  team = possessions[possessions$team.global.id == team_id, ]
  gameCodes = unique(team$gamecode)
  gamesDF = possessions[any(possessions$gamecode %in% gameCodes), ]
  opponentsDF = gamesDF[gamesDF$team.global.id != team_id, ]
  return(opponentsDF)
}
# Get Lakers Opponents
lakersOpponentsDF = getOpponentsDF(13)

# Print Values
paste("Lakers Transition PPP: ", getTeamTransitionPPP(13))
paste("Lakers Half Court PPP: ", getTeamHalfCourtPPP(13))

paste("Opponent Transition PPP: ", getTransitionPPP(lakersOpponentsDF))
paste("Opponent Half Court PPP: ", getHalfCourtPPP(lakersOpponentsDF))

paste("NBA Transition PPP: ", getTransitionPPP(possessions))
paste("NBA Half Court PPP: ", getHalfCourtPPP(possessions))

