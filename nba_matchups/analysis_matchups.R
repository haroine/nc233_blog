require("readr")

## Uses FiveThirtyEight's data, available at:
## https://github.com/fivethirtyeight/data/tree/master/nba-elo
nbaGames <<- read_csv("nbaallelo.csv")
# After the NBA-ABA merger
nbaGames <<- nbaGames[(nbaGames$lg_id == "NBA") & (nbaGames$year_id >= "1977"),]

getGames <- function(franchise, season) {
  
  returnGames <- nbaGames[(nbaGames$year_id == season) & (nbaGames$fran_id == franchise),]
  
  return(returnGames)
}

seasonMeanElo <- function(franchise, season) {
 
  return(mean(getGames(franchise, season)$elo_i))
   
}

listYears <- as.character(c(1977:2015))
listTeams <- unique(nbaGames$team_id)
listFran <- unique(nbaGames$fran_id)

## Tests
# test <- getGames("Wizards","1977")
# seasonMeanElo("Pacers","2013")

matchUps <- data.frame(matrix(0, nrow = 30*29/2, ncol = length(listYears)+1))
colnames(matchUps) <- c("matchup",listYears)

fillMatchUpDF <- function(year) {
  
  currentRow <- 1
  idYear <- year - 1975
  
  for(i in 1:(length(listFran)-1)) {
    
    for(j in (i+1):length(listFran)) {
      
      meanEloMatchUp <- 0.5 * (seasonMeanElo(listFran[i], year) + seasonMeanElo(listFran[j], year) )
      matchUps[currentRow, 1] <<- paste(listFran[i],"-",listFran[j])
      matchUps[currentRow, idYear] <<- meanEloMatchUp
      
      currentRow <- currentRow + 1
    }
    
  }
  
}

# Fill dataframe with all mean Elos for matchups
for(year in 1977:2015) {
  fillMatchUpDF(year)
}

# write.csv(matchUps, "matchups_elo.csv", row.names=F)

matchUps$mean <- apply(matchUps[,2:40],1,mean, na.rm=T)

matchUp_ordered <- matchUps[order(matchUps$mean, decreasing = T),]
names_matchUp_ordered <- matchUp_ordered$matchup

# write.csv(names_matchUp_ordered, "matchups_order.csv")