## Functions to expedite data analysis process

## Imports a package which allows for easier regular expressions
library('stringr')

## Function which calculates team metrics by season
team_metrics_by_season <- function(seasonletter) {
    playoff_teams <- sort(tourneySeeds$Team[which(tourneySeeds$Season == seasonletter)])
    playoff_seeds <- tourneySeeds[which(tourneySeeds$Season == seasonletter), ]
    season <- regSeason[which(regSeason$Season == seasonletter), ]
    ##Each of these dataframes is labled "Var1" and "Freq" for TeamID and Statistic respectively
    #Wins (NOT A USABLEVAR, must scale)
    win_freq_table <- as.data.frame(table(season$Wteam))
    wins_by_team <- win_freq_table[win_freq_table$Var1 %in% playoff_teams, ]
    #Losses (NOT A USABLEVAR, must scale)
    loss_freq_table <- as.data.frame(table(season$Lteam), stringsAsFactors = FALSE)
    loss_by_team <- loss_freq_table[loss_freq_table$Var1 %in% playoff_teams, ]
    if(nrow(wins_by_team)!=nrow(loss_by_team)) {
      missing <- wins_by_team[!(wins_by_team$Var1 %in% season$Lteam),]
      diff <- nrow(wins_by_team)-nrow(loss_by_team)
      for(i in 1:diff) {
        add <- data.frame(Var1=missing[i,1],Freq=0)
        loss_by_team <- rbind(loss_by_team,add)
      }
    }
    loss_by_team <- loss_by_team[order(loss_by_team$Var1),]
    #Total Win Percentage
    gamesplayed <- as.vector(wins_by_team$Freq + loss_by_team$Freq)
    total_winpct <- round(wins_by_team$Freq / gamesplayed, digits = 3)
    total_winpct_by_team <- as.data.frame(cbind(as.vector(loss_by_team$Var1), total_winpct))
    colnames(total_winpct_by_team) <- c("Var1", "Freq")
    total_winpct_by_team$Freq <- as.numeric(as.character(total_winpct_by_team$Freq))
    #Num of wins in last 6 games
    wins_last_six_games_by_team <- data.frame()
    for(i in playoff_teams) {
        games <- season[which(season$Wteam == i | season$Lteam == i), ]
        numwins <- sum(tail(games$Wteam) == i)
        put <- c(i, numwins)
        wins_last_six_games_by_team <- rbind(wins_last_six_games_by_team, put)
    }
    colnames(wins_last_six_games_by_team) <- c("Var1", "Freq")
    #Seed
    pattern <- "[A-Z]([0-9][0-9])"
    team_seeds <- as.data.frame(str_match(playoff_seeds$Seed, pattern))
    seeds <- as.numeric(team_seeds$V2)
    playoff_seeds$Seed  <- seeds
    seed_col <- vector()
    for(i in playoff_teams) {
        val <- match(i, playoff_seeds$Team)
        seed_col <- c(seed_col, playoff_seeds$Seed[val])
    }
    team_seed <- data.frame("Var1" = playoff_teams, "Freq" =seed_col)
    #Away Win Percentage
    away_wins <- season[which(season$Wloc == "A"),]
    away_win_freq_table <- as.data.frame(table(away_wins$Wteam))
    away_wins_by_team <- away_win_freq_table[away_win_freq_table$Var1 %in% playoff_teams, ]
    away_loss <- season[which(season$Wloc == "H"),]
    away_loss_freq_table <- as.data.frame(table(away_loss$Lteam), stringsAsFactors = FALSE)
    away_loss_by_team <- away_loss_freq_table[away_loss_freq_table$Var1 %in% playoff_teams, ]
    if(nrow(away_wins_by_team)!=nrow(away_loss_by_team)) {
      missing <- away_wins_by_team[!(away_wins_by_team$Var1 %in% away_loss_by_team$Var1),]
      diff <- nrow(away_wins_by_team)-nrow(away_loss_by_team)
      for(i in 1:diff) {
        add <- data.frame(Var1=missing[i,1],Freq=0)
        away_loss_by_team <- rbind(away_loss_by_team,add)
      }
    }
    away_loss_by_team <- away_loss_by_team[order(away_loss_by_team$Var1),]
    awaygamesplayed <- as.vector(away_wins_by_team$Freq + away_loss_by_team$Freq)
    away_winpct <- round(away_wins_by_team$Freq / awaygamesplayed, digits = 3)
    away_winpct_by_team <- as.data.frame(cbind(as.vector(away_loss_by_team$Var1), away_winpct))
    colnames(away_winpct_by_team) <- c("Var1", "Freq")
    away_winpct_by_team$Freq <- as.numeric(as.character(away_winpct_by_team$Freq))
    #Close games
    season$margin <- season$Wscore - season$Lscore
    closegame <- season[which(season$margin <= 2),]
    close_win_freq_table <- as.data.frame(table(closegame$Wteam),stringsAsFactors = FALSE)
    close_wins_by_team <- close_win_freq_table[close_win_freq_table$Var1 %in% playoff_teams, ]
    if(nrow(close_wins_by_team)<length(playoff_teams)) {
      missing <- playoff_teams[!(playoff_teams %in% close_wins_by_team$Var1)]
      diff <- length(missing)
      for(i in 1:diff) {
        add <- data.frame(Var1=missing[i],Freq=0)
        close_wins_by_team <- rbind(close_wins_by_team,add)
      }
    }
    close_wins_by_team <- close_wins_by_team[order(close_wins_by_team$Var1),]
    close_wins_by_team$Freq <- as.numeric(as.character(close_wins_by_team$Freq))
    close_loss_freq_table <- as.data.frame(table(closegame$Lteam),stringsAsFactors = FALSE)
    close_loss_by_team <- close_loss_freq_table[close_loss_freq_table$Var1 %in% playoff_teams, ]
    if(nrow(close_loss_by_team)<length(playoff_teams)) {
      missing <- playoff_teams[!(playoff_teams %in% close_loss_by_team$Var1)]
      diff <- length(missing)
      for(i in 1:diff) {
        add <- data.frame(Var1=missing[i],Freq=0)
        close_loss_by_team <- rbind(close_loss_by_team,add)
      }
    }
    close_loss_by_team <- close_loss_by_team[order(close_loss_by_team$Var1),]
    close_loss_by_team$Freq <- as.numeric(as.character(close_loss_by_team$Freq))
    #Not close games
    biggame <- season[which(season$margin >=7),]
    big_win_freq_table <- as.data.frame(table(biggame$Wteam),stringsAsFactors = FALSE)
    big_wins_by_team <- big_win_freq_table[big_win_freq_table$Var1 %in% playoff_teams, ]
    if(nrow(big_wins_by_team)<length(playoff_teams)) {
      missing <- playoff_teams[!(playoff_teams %in% big_wins_by_team$Var1)]
      diff <- length(missing)
      for(i in 1:diff) {
        add <- data.frame(Var1=missing[i],Freq=0)
        big_win_by_team <- rbind(big_win_by_team,add)
      }
    }
    big_wins_by_team <- big_wins_by_team[order(big_wins_by_team$Var1),]
    big_wins_by_team$Freq <- as.numeric(as.character(big_wins_by_team$Freq))
    big_loss_freq_table <- as.data.frame(table(biggame$Lteam),stringsAsFactors = FALSE)
    big_loss_by_team <- big_loss_freq_table[big_loss_freq_table$Var1 %in% playoff_teams, ]
    if(nrow(big_loss_by_team)<length(playoff_teams)) {
      missing <- playoff_teams[!(playoff_teams %in% big_loss_by_team$Var1)]
      diff <- length(missing)
      for(i in 1:diff) {
        add <- data.frame(Var1=missing[i],Freq=0)
        big_loss_by_team <- rbind(big_loss_by_team,add)
      }
    }
    big_loss_by_team <- big_loss_by_team[order(big_loss_by_team$Var1),]
    big_loss_by_team$Freq <- as.numeric(as.character(big_loss_by_team$Freq))
    # Rankings
    ratings_final <- data.frame()
      playoff_teams <- sort(tourneySeeds$Team[which(tourneySeeds$Season == seasonletter)])
      ratings_season <- ratings[which(ratings$season == seasonletter),]
      ratings_season <- ratings_season[ratings_season$team %in% playoff_teams,]
      ratings_season_final <- data.frame(season=seasonletter,team=playoff_teams)
      for(j in 4:length(ratings_season)){
        ratings_season_team_final <- data.frame()
        for(k in 1:length(playoff_teams)){
          ratings_season_team <- ratings_season[which(ratings_season$team == playoff_teams[k]), ]
          ratings_season_team <- ratings_season_team[order(ratings_season_team$rating_day_num),]
          ranks <- ratings_season_team[complete.cases(ratings_season_team[,j]),j]
          if(sum(ranks) == 0){
            rank_final <- data.frame(team = playoff_teams[k], orank = NA)
          } else {
            rank_final <- data.frame(team = playoff_teams[k], orank = ranks[length(ranks)])
          }
          ratings_season_team_final <- rbind(ratings_season_team_final,rank_final)
        }
        naming <- colnames(ratings_season_final)
        ratings_season_final <- cbind(ratings_season_final,ratings_season_team_final[,2])
        colnames(ratings_season_final) <- c(naming,colnames(ratings_season)[j])
      }  
    # Other Stats
    season$Wmargin <- season$Wscore - season$Lscore
    season$Lmargin <- -1*season$Wmargin
    season_other_win <- season[,c(3,9:21,36)]
    colnames(season_other_win) <- gsub("W","A_",colnames(season_other_win))
    season_other_lose <- season[,c(5,22:34,37)]
    colnames(season_other_lose) <- colnames(season_other_win)
    season_other <- rbind(season_other_win,season_other_lose)
    season_other_tot <- aggregate(season_other[,2:15],by=list(TEAMID=season_other$A_team),
                                      FUN=sum)
    season_other_tot$A_fgp <- season_other_tot$A_fgm/season_other_tot$A_fga
    season_other_tot$A_fgp3 <- season_other_tot$A_fgm3/season_other_tot$A_fga3
    season_other_tot <- season_other_tot[season_other_tot$TEAMID %in% playoff_teams,]
      
    team_metrics <- data.frame()
    team_metrics <- cbind(total_winpct_by_team, wins_last_six_games_by_team$Freq,
    team_seed$Freq, away_winpct_by_team$Freq, close_wins_by_team$Freq, close_loss_by_team$Freq,
    big_wins_by_team$Freq,big_loss_by_team$Freq,ratings_season_final[,-(1:2)],season_other_tot[,-1])
    
    headers <- colnames(ratings_season_final[,-(1:2)])
    for(i in 1:length(headers)) {
      headers[i] <- paste("A",headers[i],sep="_")
    }
    
    colnames(team_metrics) <- c("TEAMID", "A_TWPCT", "A_WST6", "A_SEED", "A_AWAYPCT", 
                                "A_CloseWin","A_CloseLoss","A_BigWin","A_BigLoss",headers,
                                colnames(season_other_tot)[-1])
    return(team_metrics)
}


## Function which creates the Train data set for each season, to create the full
## Train data set loop this function through seasons A-M
train_frame_model <- function(seasonletter) {
    teamMetrics <- team_metrics_by_season(seasonletter)
    season_matches <- tourneyRes[which(tourneyRes$Season == seasonletter), ]
    team <- vector()
    result <- vector()
    for(i in c(1:nrow(season_matches))) {
        row <- season_matches[i, ]
        if(row$Wteam < row$Lteam) {
            vector <- paste(seasonletter,"_",row$Wteam,"_", row$Lteam, sep ="")
            team <- c(team, vector)
            result <- c(result, 1)
        } else {
            oth <- paste(seasonletter, "_", row$Lteam, "_", row$Wteam, sep ="")
            team <- c(team, oth)
            result <- c(result, 0)
        }
    }
    model_data_frame <- data.frame("Matchup" = team, "Win" = result)
    teamMetrics_away <- teamMetrics
    headersB <- gsub("A_","B_",colnames(teamMetrics))
    colnames(teamMetrics_away) <- headersB
    pattern <- "[0-9]{4}_([0-9]{4})_([0-9]{4})"
    teamIDs <- as.data.frame(str_match(model_data_frame$Matchup, pattern))
    teamIDs <- teamIDs[ , c(2,3)]
    colnames(teamIDs) <- c("HomeID", "AwayID")
    model_data_frame <- cbind(model_data_frame, teamIDs)
    home_frame <- data.frame()
    for(i in model_data_frame$HomeID) {
        home_frame <- rbind(home_frame, teamMetrics[match(i, teamMetrics$TEAMID), ])
    }
    #Removing teamID column
    home_frame <- home_frame[ , -1]
    
    away_frame <- data.frame()
    for(i in model_data_frame$AwayID) {
        away_frame <- rbind(away_frame, teamMetrics_away[match(i, teamMetrics_away$TEAMID), ])
    }
    away_frame <- away_frame[ , -1]
    
    model_data_frame <- cbind(model_data_frame, home_frame, away_frame)
    
    return(model_data_frame)
}


## Creates the "Test" data set per season
test_frame_model <- function(season) {
    model_data_frame <- submissionFile(season)
    teamMetrics <- team_metrics_by_season(season)
    teamMetrics_away <- teamMetrics
    headersB <- gsub("A_","B_",colnames(teamMetrics))
    colnames(teamMetrics_away) <- headersB
    pattern <- "[0-9]{4}_([0-9]{4})_([0-9]{4})"
    teamIDs <- as.data.frame(str_match(model_data_frame$Id, pattern))
    teamIDs <- teamIDs[ , c(2,3)]
    colnames(teamIDs) <- c("HomeID", "AwayID")
    model_data_frame <- cbind(model_data_frame, teamIDs)
    home_frame <- data.frame()
    for(i in model_data_frame$HomeID) {
        home_frame <- rbind(home_frame, teamMetrics[match(i, teamMetrics$TEAMID), ])
    }
    #Removing teamID column
    home_frame <- home_frame[ , -1]
    
    away_frame <- data.frame()
    for(i in model_data_frame$AwayID) {
        away_frame <- rbind(away_frame, teamMetrics_away[match(i, teamMetrics_away$TEAMID), ])
    }
    away_frame <- away_frame[ , -1]
    
    model_data_frame <- cbind(model_data_frame, home_frame, away_frame)
    
    return(model_data_frame)
}


## Creates the submission file, every possible first round combination for each
## season
submissionFile <- function(season) {
    playoffTeams <- sort(tourneySeeds$Team[which(tourneySeeds$Season == season)])
    numTeams <- length(playoffTeams)
    matrix <- matrix(nrow =numTeams, ncol = numTeams)
    for(i in c(1:numTeams)) {
        for(j in c(1:numTeams)) {
            matrix[i,j] <- paste(season,"_",playoffTeams[i],"_", playoffTeams[j], sep ="")
        }
    }
    keep <- upper.tri(matrix, diag = F)
    idcol <- vector()
    for(i in c(1:numTeams)) {
        for(j in c(1:numTeams)) {
            if(keep[i,j] == T) {
                idcol <- c(idcol, matrix[i,j])
            }
        }
    }
    form <- data.frame("Id" = idcol, "Pred" = NA)
    return(form)
}

## Compare to Actuals
evaluation <- function(prediction_model,response,test_seasons) {
  compare <- data.frame()
  for (i in test_seasons) {
    compare <- rbind(compare,train_frame_model(i))
  }
  compare_clean <- compare[,colnames(train_data_frame_clean)]
  comparex <- compare_clean[,-(1:4)]
  compare$Prediction <- drop(predict(prediction_model,newdata=comparex,type=response))
  if(response=="response") {
    compare$Prediction <- (compare$Prediction>.5)*1
  } else {
    }
  compare$daynum <- tourneyRes[which(tourneyRes$Season %in% test_seasons), 2]
  compare$correct <- (compare$Prediction == compare$Win)*1

  eval_totgames <- nrow(compare)
  eval_correct <- sum(compare$correct)
  eval_correctper <- eval_correct/eval_totgames
  eval_totchamp <- length(which(compare$daynum == 154))
  eval_correctchamp <- sum(compare$correct[which(compare$daynum == 154)])
  eval_champper <- eval_correctchamp/eval_totchamp
  print(eval_correctper)
  print(eval_champper)
}

# Log-Loss function
logloss <- function(n,yhat,y) {
  calc <- vector()
  for(i in 1:n) {
    if(yhat[i]==0) {
      yhat[i] <- yhat[i]+1e-5
    }
    if(yhat[i]==1) {
      yhat[i] <- yhat[i]-1e-5
    }
    calc[i] <- ( y[i] * log(yhat[i]) ) + ( (1-y[i]) * log(1-yhat[i]) )
  }
  return(-sum(calc)/n)
}

SeasonElo <- function(seasonletter){
  library(PlayerRatings)
  library(data.table)
  seasonDataDt <- regSeason[which(regSeason$Season == seasonletter), c(2,3,5,7)]
  resultVector <- rep(1, nrow(seasonDataDt))
  advantageVector <- as.numeric(seasonDataDt$Wloc == "H")
  seasonDataDf <- data.frame(yearDay = seasonDataDt$Daynum,
                             tid1 = seasonDataDt$Wteam, 
                             tid2 = seasonDataDt$Lteam, 
                             result = resultVector)
  EloRatings <- elo(x = seasonDataDf, gamma = advantageVector)
  EloRatingsDt <- as.data.table(EloRatings$ratings)
  return(EloRatingsDt)
}
