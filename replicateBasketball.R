# Functions for replicating advanced metrics from bootstrapped seasons.
library(plyr)
library(parallel)

## load("PCAfiles.RData")


sum_col_fun <- function(col){
  return(function(df){ sum(as.numeric(df[,col]), na.rm=TRUE) })
}

aggr_pct_fun <- function(pct, tot){
  return(function(df){
    sum(as.numeric(df[,pct])*as.numeric(df[,tot]), na.rm=TRUE)/
      sum(as.numeric(df[,tot]), na.rm=TRUE)
  }
  )
}

wt_aggr_fun <- function(cols, wts){
  return(function(df)
    sum(as.matrix(df[cols]) %*% wts)
  )
}

aggr_funs <- c(MP = sum_col_fun("MP"),
               FG = sum_col_fun("FG"),
               FGA = sum_col_fun("FGA"),
               FG. = aggr_pct_fun("FG.", "FGA"),
               X3PA = sum_col_fun("X3PA"),
               X3P. = aggr_pct_fun("X3P.", "X3PA"),
               FGA = sum_col_fun("FGA"),
               FG. = aggr_pct_fun("FG.", "FGA"),
               FTA = sum_col_fun("FTA"),
               FT. = aggr_pct_fun("FT.", "FTA"),
               PF = sum_col_fun("PF"),
               PTS = sum_col_fun("PTS"),
               ORB = sum_col_fun("ORB"),
               DRB = sum_col_fun("DRB"),
               TRB = sum_col_fun("TRB"),
               AST = sum_col_fun("AST"),
               STL = sum_col_fun("STL"),
               BLK = sum_col_fun("BLK"),
               TOV = sum_col_fun("TOV"))


## input player data frame, team data frame, and opponents
addAdvancedStats <- function(pdf, tdf, oppdf, otherYears) {

    leagueStats <- colSums(tdf[, 2:ncol(tdf)])
    
    tdf <- tdf[match(pdf$Tm, tdf$Tm), ]
    oppdf <- oppdf[match(pdf$Tm, oppdf$Opp), ]

    ## reconstruct total atempts from percentages
    pdf$FT <- round(pdf[["FT."]] * pdf[["FTA"]])
    pdf$FT[pdf$FTA==0] <- 0
    pdf$X3P <- round(pdf[["X3P."]] * pdf[["X3PA"]])
    pdf$X3P[pdf$X3PA==0] <- 0
    
    pdf$TSA <- pdf$FGA + 0.44 * pdf$FTA
    pdf$TSper <- pdf$PTS / (2*pdf$TSA)
    pdf$eFGper <- with(pdf, (FG + 0.5 * X3P. * X3PA) / FGA)
    pdf$ORBper <- with(pdf, 100 * (ORB * (tdf$MP / 5)) / (MP * (tdf$ORB + oppdf$DRB)))
    pdf$DRBper <- with(pdf, 100 * (DRB * (tdf$MP / 5)) / (MP * (tdf$DRB + oppdf$ORB)))
    pdf$TRBper <- with(pdf, 100 * (TRB * (tdf$MP / 5)) / (MP * (tdf$TRB + oppdf$TRB)))
    pdf$ASTper <- with(pdf, 100 * AST / (((MP / (tdf$MP / 5)) * tdf$FG) - FG))

    pdf$BLKper <- with(pdf, 100 * (BLK * (tdf$MP / 5)) / (MP * (oppdf$FGA - oppdf$X3PA)))
    pdf$USGper <- with(pdf, 100 * ((FGA + 0.44 * FTA + TOV) * (tdf$MP / 5)) / (MP * (tdf$FGA + 0.44 * tdf$FTA + tdf$TOV)))

    pdf$TOVper <- with(pdf, 100 * TOV / (FGA + 0.44 * FTA + TOV))
    pdf[["STLper"]] <- calcSTL(pdf, tdf, oppdf, leagueStats)
    pdf$X3PAr <- pdf$X3PA/pdf$FGA
    
    pdf$STL36min <- pdf$STL/pdf$MP*36
    pdf$ORB36min <- pdf$ORB/pdf$MP*36
    pdf$DRB36min <- pdf$DRB/pdf$MP*36
    pdf$TRB36min <- pdf$TRB/pdf$MP*36
    pdf$AST36min <- pdf$AST/pdf$MP*36
    pdf$BLK36min <- pdf$BLK/pdf$MP*36
    pdf$TOV36min <- pdf$TOV/pdf$MP*36
    pdf$PF36min <- pdf$PF/pdf$MP*36
    pdf$PTS36min <- pdf$PTS/pdf$MP*36
    pdf$X3PA36min <- pdf$X3PA/pdf$MP*36
    pdf$FGA36min <- pdf$FGA/pdf$MP*36
    pdf$FTA36min <- pdf$FTA/pdf$MP*36
    pdf$PF36min <- pdf$PF/pdf$MP*36
    
    pdf$ORtg <- calcORtg(pdf, tdf, oppdf)
    pdf$DRtg <- calcDRtg(pdf, tdf, oppdf)
    pdf$OWS <- calcOWS(pdf, tdf, oppdf, leagueStats)
    pdf$DWS <- calcDWS(pdf, tdf, oppdf, leagueStats)
    pdf$WS <- pdf$OWS + pdf$DWS
    pdf[["WS/48"]] <- pdf$WS / pdf$MP * 48
    pdf$BPM <- calcBPM(pdf, tdf, oppdf, leagueStats)
    pdf$OBPM <- calcOBPM(pdf, tdf, oppdf, leagueStats)
    pdf$DBPM <- calcDBPM(pdf, tdf, oppdf, leagueStats)
    pdf$VORP <- (pdf$BPM + 2.0) * pdf$MP / (tdf$MP / 5) * tdf$GP /82.

    pdf$PER <- calcPER2(pdf, tdf, oppdf, leagueStats)


    pdf[["3P%SS"]] <- calc3PSS(pdf, otherYears)


    pdf$MPG <- pdf$MP / pdf$GP

    pdf
}

# Game logs
bootstrap_season <- function(games_df, otherYears=NULL, identity=FALSE){
    
    team_stats <- c("MP", "FG", "FGA", "FTA", "AST", "TOV", "TRB", "ORB", "DRB", "PTS", "X3PA", "STL", "PF", "BLK")

    yearsList <- lapply(unique(games_df$Year), function(yr) {

        x <- games_df[games_df$Year==yr, ]
        
        resampledDates <- list()
        gamesPlayed <- numeric(length(unique(x$Tm)))
        names(gamesPlayed) <- unique(x$Tm)
        for(Tm in unique(x$Tm)) {
            if(identity) {
                resampledDates[[Tm]] <- unique(x$Date[x$Tm==Tm])
            } else {
                resampledDates[[Tm]] <- sample(unique(x$Date[x$Tm==Tm]), replace=TRUE)
            }
            gamesPlayed[Tm] <- length(unique(x$Date[x$Tm==Tm]))
        }

        player_totals <- ddply(x, .(Tm), function(y) {

            Tm <- y$Tm[1]
            bootSeason <- do.call(rbind, lapply(resampledDates[[Tm]], function(d) y[y$Date==d, ]))
            player_season_team <- ddply(bootSeason, .(Name), function(z) {
                resvec <- sapply(aggr_funs, function(fun) fun(z))
                resvec["GP"] <- sum(!is.na(z$MP) & z$MP>0)

                ## Compute team shooting percentage w/o player
                wo <- bootSeason[bootSeason$Name != z$Name[1], ]
                resvec["TmTSwo"] <- sum(as.numeric(wo$PTS), na.rm=TRUE) /
                    (2 * sum(as.numeric(wo$FGA), na.rm=TRUE) +
                     0.88 * sum(as.numeric(wo$FTA), na.rm=TRUE))
                
                
                resvec
            })


                
            player_season_team
        })

        team_totals <- ddply(x, .(Tm), function(y) {
            Tm <- y$Tm[1]
            bootSeason <- do.call(rbind, lapply(resampledDates[[Tm]], function(d) y[y$Date==d, ]))
            
            ts <- bootSeason[, team_stats]
            ts <- apply(ts, 2, as.numeric)
            team_season <- colSums(ts, na.rm=TRUE)
            team_season["FT"] <- round(sum(as.numeric(y[, "FTA"]) *
                                           as.numeric(y[, "FT."]),
                                           na.rm=TRUE))
            team_season["X3P"] <- round(sum(as.numeric(y[, "X3PA"]) *
                                           as.numeric(y[, "X3P."]),
                                           na.rm=TRUE))

            team_season
        })
        team_totals$GP <- gamesPlayed[team_totals$Tm]

        opponent_totals <- ddply(x, .(Opp), function(y) {
            Tm <- y$Opp[1]
            bootSeason <- do.call(rbind, lapply(resampledDates[[Tm]], function(d) y[y$Date==d, ]))
            os <- apply(bootSeason[, team_stats], 2, as.numeric)
            opponent_season <- colSums(os, na.rm=TRUE)
            opponent_season["FT"] <- round(sum(as.numeric(y[, "FTA"]) *
                                           as.numeric(y[, "FT."]),
                                           na.rm=TRUE))
            opponent_season["X3P"] <- round(sum(as.numeric(y[, "X3PA"]) *
                                           as.numeric(y[, "X3P."]),
                                           na.rm=TRUE))
            opponent_season
        })
        
        list(player=player_totals, team=team_totals, opponent=opponent_totals)
    })
    
    player_season <- addAdvancedStats(yearsList[[1]]$player, yearsList[[1]]$team,
                     yearsList[[1]]$opponent, otherYears)
    
    ## Align column names
    colnames(player_season) <- sapply(colnames(player_season),
                                      function(x) gsub("[.]|(per)", "%", x))
    colnames(player_season) <- sapply(colnames(player_season),
                                      function(x) gsub("^X", "", x))
    player_season

}

calcORtg <- function(pdf, tdf, oppdf) {

    qAST <- with(pdf, ((MP / (tdf$MP / 5)) * (1.14 * ((tdf$AST - AST) / tdf$FG))) + ((((tdf$AST / tdf$MP) * MP * 5 - AST) / ((tdf$FG / tdf$MP) * MP * 5 - FG)) * (1 - (MP / (tdf$MP / 5)))))
    FG_Part <- with(pdf, FG * (1 - 0.5 * ((PTS - FT) / (2 * FGA)) * qAST))
    AST_Part <- with(pdf, 0.5 * (((tdf$PTS - tdf$FT) - (PTS - FT)) / (2 * (tdf$FGA - FGA))) * AST)
    FT_Part <- with(pdf, (1-(1-(FT/FTA))^2)*0.4*FTA)
    Team_Scoring_Poss <- tdf$FG + (1 - (1 - (tdf$FT / tdf$FTA))^2) * tdf$FTA * 0.4

    Team_ORBper <- tdf$ORB / (tdf$ORB + (oppdf$TRB - oppdf$ORB))
    Team_PlayPer <- Team_Scoring_Poss / (tdf$FGA + tdf$FTA * 0.4 + tdf$TOV)
    Team_ORB_Weight <- ((1 - Team_ORBper) * Team_PlayPer) / ((1 - Team_ORBper) * Team_PlayPer + Team_ORBper * (1 - Team_PlayPer))
    ORB_Part <- with(pdf, ORB * Team_ORB_Weight * Team_PlayPer)

    ScPoss <- (FG_Part + AST_Part + FT_Part) * (1 - (tdf$ORB / Team_Scoring_Poss) * Team_ORB_Weight * Team_PlayPer) + ORB_Part

    FGxPoss <- with(pdf, (FGA - FG) * (1 - 1.07 * Team_ORBper))
    FTxPoss <- with(pdf, ((1 - (FT / FTA))^2) * 0.4 * FTA)
    TotPoss <- with(pdf, ScPoss + FGxPoss + FTxPoss + TOV)

    PProd_FG_Part <- with(pdf, 2 * (FG + 0.5 * X3P) * (1 - 0.5 * ((PTS - FT) / (2 * FGA)) * qAST))
    PProd_AST_Part <- with(pdf, 2 * ((tdf$FG - FG + 0.5 * (tdf$X3P - X3P)) / (tdf$FG - FG)) * 0.5 * (((tdf$PTS - tdf$FT) - (PTS - FT)) / (2 * (tdf$FGA - FGA))) * AST)
    PProd_ORB_Part <- with(pdf, ORB * Team_ORB_Weight * Team_PlayPer * (tdf$PTS / (tdf$FG + (1 - (1 - (tdf$FT / tdf$FTA))^2) * 0.4 * tdf$FTA)))

    PProd <- with(pdf, (PProd_FG_Part + PProd_AST_Part + FT) * (1 - (tdf$ORB / Team_Scoring_Poss) * Team_ORB_Weight * Team_PlayPer) + PProd_ORB_Part)

    ORtg <- 100 * (PProd / TotPoss)

    ORtg
}

calcOWS <- function(pdf, tdf, oppdf, leagueStats) {

    qAST <- with(pdf, ((MP / (tdf$MP / 5)) * (1.14 * ((tdf$AST - AST) / tdf$FG))) + ((((tdf$AST / tdf$MP) * MP * 5 - AST) / ((tdf$FG / tdf$MP) * MP * 5 - FG)) * (1 - (MP / (tdf$MP / 5)))))
    FG_Part <- with(pdf, FG * (1 - 0.5 * ((PTS - FT) / (2 * FGA)) * qAST))
    AST_Part <- with(pdf, 0.5 * (((tdf$PTS - tdf$FT) - (PTS - FT)) / (2 * (tdf$FGA - FGA))) * AST)
    FT_Part <- with(pdf, (1-(1-(FT/FTA))^2)*0.4*FTA)
    Team_Scoring_Poss <- tdf$FG + (1 - (1 - (tdf$FT / tdf$FTA))^2) * tdf$FTA * 0.4

    Team_ORBper <- tdf$ORB / (tdf$ORB + (oppdf$TRB - oppdf$ORB))
    Team_PlayPer <- Team_Scoring_Poss / (tdf$FGA + tdf$FTA * 0.4 + tdf$TOV)
    Team_ORB_Weight <- ((1 - Team_ORBper) * Team_PlayPer) / ((1 - Team_ORBper) * Team_PlayPer + Team_ORBper * (1 - Team_PlayPer))
    ORB_Part <- with(pdf, ORB * Team_ORB_Weight * Team_PlayPer)

    ScPoss <- (FG_Part + AST_Part + FT_Part) * (1 - (tdf$ORB / Team_Scoring_Poss) * Team_ORB_Weight * Team_PlayPer) + ORB_Part

    FGxPoss <- with(pdf, (FGA - FG) * (1 - 1.07 * Team_ORBper))
    FTxPoss <- with(pdf, ((1 - (FT / FTA))^2) * 0.4 * FTA)
    TotPoss <- with(pdf, ScPoss + FGxPoss + FTxPoss + TOV)

    PProd_FG_Part <- with(pdf, 2 * (FG + 0.5 * X3P) * (1 - 0.5 * ((PTS - FT) / (2 * FGA)) * qAST))
    PProd_AST_Part <- with(pdf, 2 * ((tdf$FG - FG + 0.5 * (tdf$X3P - X3P)) / (tdf$FG - FG)) * 0.5 * (((tdf$PTS - tdf$FT) - (PTS - FT)) / (2 * (tdf$FGA - FGA))) * AST)
    PProd_ORB_Part <- with(pdf, ORB * Team_ORB_Weight * Team_PlayPer * (tdf$PTS / (tdf$FG + (1 - (1 - (tdf$FT / tdf$FTA))^2) * 0.4 * tdf$FTA)))

    PProd <- with(pdf, (PProd_FG_Part + PProd_AST_Part + FT) * (1 - (tdf$ORB / Team_Scoring_Poss) * Team_ORB_Weight * Team_PlayPer) + PProd_ORB_Part)

    ORtg <- 100 * (PProd / TotPoss)

    Team_Possessions <- 0.5 * ((tdf$FGA + 0.4 * tdf$FTA - 1.07 * (tdf$ORB / (tdf$ORB + tdf$DRB)) * (tdf$FGA - tdf$FG) + tdf$TOV) + (oppdf$FGA + 0.4 * oppdf$FTA - 1.07 * (oppdf$ORB / (oppdf$ORB + tdf$DRB)) * (oppdf$FGA - oppdf$FG) + oppdf$TOV))

    Opp_Possessions <- 0.5 * ((oppdf$FGA + 0.4 * oppdf$FTA - 1.07 * (oppdf$ORB / (oppdf$ORB + oppdf$DRB)) * (oppdf$FGA - oppdf$FG) + oppdf$TOV) + (tdf$FGA + 0.4 * tdf$FTA - 1.07 * (tdf$ORB / (tdf$ORB + oppdf$DRB)) * (tdf$FGA - tdf$FG) + tdf$TOV))

    League_Possessions <- (leagueStats["FGA"] + 0.4 * leagueStats["FTA"] - 1.07 *
                           (leagueStats["ORB"] / (leagueStats["ORB"] + leagueStats["DRB"])) * (leagueStats["FGA"] - leagueStats["FG"]) + leagueStats["TOV"])
    
    Marg_Off <- PProd - 0.92 * (leagueStats["PTS"]/League_Possessions) * (TotPoss)
    Team_Pace <- 48 * ((Team_Possessions + Opp_Possessions) / (2 * (tdf$MP / 5)))
    League_Pace <- 48 * (League_Possessions / (leagueStats["MP"] / 5))
    MPPW <- 0.32 * (leagueStats["PTS"] / leagueStats["GP"]) * (Team_Pace / (League_Pace))
    OWS <- Marg_Off / MPPW

    OWS
}

calcDRtg <- function(pdf, tdf, oppdf) {

    DORper <- oppdf$ORB / (oppdf$ORB + tdf$DRB)
    DFGper <- oppdf$FG / oppdf$FGA
    FMwt <- (DFGper * (1 - DORper)) / (DFGper * (1 - DORper) + (1 - DFGper) * DORper)
    Stops1 <- with(pdf, STL + BLK * FMwt * (1 - 1.07 * DORper) + DRB * (1 - FMwt))

    Stops2 <- with(pdf, (((oppdf$FGA - oppdf$FG - tdf$BLK) / tdf$MP) * FMwt * (1 - 1.07 * DORper) + ((oppdf$TOV - tdf$STL) / tdf$MP)) * MP + (PF / tdf$PF) * 0.4 * oppdf$FTA * (1 - (oppdf$FT / oppdf$FTA))^2)

    Stops <- Stops1 + Stops2

    Team_Possessions <- 0.5 * ((tdf$FGA + 0.4 * tdf$FTA - 1.07 * (tdf$ORB / (tdf$ORB + tdf$DRB)) * (tdf$FGA - tdf$FG) + tdf$TOV) + (oppdf$FGA + 0.4 * oppdf$FTA - 1.07 * (oppdf$ORB / (oppdf$ORB + tdf$DRB)) * (oppdf$FGA - oppdf$FG) + oppdf$TOV))
        
    StopPer = (Stops * oppdf$MP) / (Team_Possessions * pdf$MP)

    Team_Defensive_Rating <- 100 * (oppdf$PTS / Team_Possessions)
    D_Pts_per_ScPoss <- oppdf$PTS / (oppdf$FG + (1 - (1 - (oppdf$FT / oppdf$FTA))^2) * oppdf$FTA*0.4)
    
    DRtg <- Team_Defensive_Rating + 0.2 * (100 * D_Pts_per_ScPoss * (1 - StopPer) - Team_Defensive_Rating)

    
}

calcDWS <- function(pdf, tdf, oppdf, leagueStats) {
    DRtg <- calcDRtg(pdf, tdf, oppdf)

    Team_Possessions <- 0.5 * ((tdf$FGA + 0.4 * tdf$FTA - 1.07 * (tdf$ORB / (tdf$ORB + tdf$DRB)) * (tdf$FGA - tdf$FG) + tdf$TOV) + (oppdf$FGA + 0.4 * oppdf$FTA - 1.07 * (oppdf$ORB / (oppdf$ORB + tdf$DRB)) * (oppdf$FGA - oppdf$FG) + oppdf$TOV))

    Opp_Possessions <- 0.5 * ((oppdf$FGA + 0.4 * oppdf$FTA - 1.07 * (oppdf$ORB / (oppdf$ORB + oppdf$DRB)) * (oppdf$FGA - oppdf$FG) + oppdf$TOV) + (tdf$FGA + 0.4 * tdf$FTA - 1.07 * (tdf$ORB / (tdf$ORB + oppdf$DRB)) * (tdf$FGA - tdf$FG) + tdf$TOV))
    
    Team_Pace <- 48 * ((Team_Possessions + Opp_Possessions) / (2 * (tdf$MP / 5)))    
    League_Possessions <- (leagueStats["FGA"] + 0.4 * leagueStats["FTA"] - 1.07 *
                           (leagueStats["ORB"] / (leagueStats["ORB"] + leagueStats["DRB"])) * (leagueStats["FGA"] - leagueStats["FG"]) + leagueStats["TOV"])
    Marg_Def <- pdf$MP / tdf$MP * (Opp_Possessions) * (1.08 * (leagueStats["PTS"] / League_Possessions) - (DRtg / 100))

    League_Pace <- 48 * (League_Possessions / (leagueStats["MP"] / 5))
    
    MPPW <- 0.32 * (leagueStats["PTS"] / leagueStats["GP"]) * (Team_Pace / (League_Pace))
    DWS <- Marg_Def / MPPW

    DWS
}

calcBPM <- function(pdf, tdf, oppdf, leagueStats) {

    ## Coefficients for Linear Model
    a <- 0.123391
    b <- 0.119597
    c <- -0.151287
    d <- 1.255644
    e <- 0.531838
    f <- -0.305868
    g <- 0.921292
    h <- 0.711217
    i <- 0.017022	
    j <- 0.297639
    k <- 0.213485
    l <- 0.725930

    ReMPG <- pdf$MP / (pdf$GP + 4) 

    Team_Possessions <- 0.5 * ((tdf$FGA + 0.4 * tdf$FTA - 1.07 * (tdf$ORB / (tdf$ORB + tdf$DRB)) * (tdf$FGA - tdf$FG) + tdf$TOV) + (oppdf$FGA + 0.4 * oppdf$FTA - 1.07 * (oppdf$ORB / (oppdf$ORB + tdf$DRB)) * (oppdf$FGA - oppdf$FG) + oppdf$TOV))
    
    Opp_Possessions <- 0.5 * ((oppdf$FGA + 0.4 * oppdf$FTA - 1.07 * (oppdf$ORB / (oppdf$ORB + oppdf$DRB)) * (oppdf$FGA - oppdf$FG) + oppdf$TOV) + (tdf$FGA + 0.4 * tdf$FTA - 1.07 * (tdf$ORB / (tdf$ORB + oppdf$DRB)) * (tdf$FGA - tdf$FG) + tdf$TOV))

    STLper <- with(pdf, 100 * (STL * (tdf$MP / 5)) / (MP * Opp_Possessions))

    ## This needs to be w/out player

    Team_ORtg <- 100 * tdf$PTS / Team_Possessions
    Team_DRtg <- 100 * oppdf$PTS / Team_Possessions

    RebVers <- with(pdf, b*ORBper + c*DRBper + l*sqrt(ASTper*TRBper)) - 6.28
    DefPart <- with(pdf, d*STLper + e*BLKper - 2.93)
    ValShot <- with(pdf, 2*TSper - k - 2*TmTSwo + i*ASTper +
                          j*(X3PAr - leagueStats["X3PA"] / leagueStats["FGA"]))
    OffPart <- with(pdf, h * ValShot * USGper*(1 - TOVper/100) -
                         g * TOVper/100*USGper + f * ASTper  + 6.11)

    rawBPM <- RebVers + DefPart + OffPart + a*ReMPG - 3.02

    
    perMin <- 5*(pdf$MP/tdf$MP)
    rawContrib <- rawBPM * perMin
    pdf$TmContrib <- 0
    for(Tm in unique(pdf$Tm)) {
        pdf$TmContrib[pdf$Tm == Tm] <- sum(rawContrib[pdf$Tm == Tm], na.rm=TRUE)
    }
    
    rawBPM
}

calcOBPM <- function(pdf, tdf, oppdf, leagueStats) {

    ## Coefficients for Linear Model
    a <- 0.064448
    b <- 0.211125
    c <- -0.107545
    d <- 0.346513
    e <- -0.052476
    f <- -0.041787
    g <- 0.932965
    h <- 0.687359
    i <- 0.007952	
    j <- 0.374706
    k <- -0.181891
    l <- 0.239862

    ReMPG <- pdf$MP / (pdf$GP + 4)
    
    Opp_Possessions <- 0.5 * ((oppdf$FGA + 0.4 * oppdf$FTA - 1.07 * (oppdf$ORB / (oppdf$ORB + oppdf$DRB)) * (oppdf$FGA - oppdf$FG) + oppdf$TOV) + (tdf$FGA + 0.4 * tdf$FTA - 1.07 * (tdf$ORB / (tdf$ORB + oppdf$DRB)) * (tdf$FGA - tdf$FG) + tdf$TOV))
    STLper <- with(pdf, 100 *(STL * (tdf$MP / 5)) / (MP * Opp_Possessions))
    
    rawOBPM <- with(pdf, a * ReMPG + b * ORBper + c * DRBper + d * STLper +
        e * BLKper + f * ASTper - g*USGper*TOVper/100 + 
        h*USGper*(1-TOVper/100)*(2*(TSper - TmTSwo) + i*ASTper + j*(X3PAr - leagueStats["X3PA"]/leagueStats["FGA"]) - k) + l*sqrt(ASTper*TRBper))

    OBPM <- rawOBPM - 3*1.58

    OBPM
}

calcDBPM <- function(pdf, tdf, oppdf, leagueStats) {
    calcBPM(pdf, tdf, oppdf, leagueStats) - calcOBPM(pdf, tdf, oppdf, leagueStats)
}

calcPER <- function(pdf, tdf, oppdf, leagueStats) {

    
    factor <- (2 / 3) - (0.5 * (leagueStats["AST"] / leagueStats["FG"])) / (2 * (leagueStats["FG"] / leagueStats["FT"]))
    VOP <- leagueStats["PTS"] / (leagueStats["FGA"] - leagueStats["ORB"] + leagueStats["TOV"] + 0.44 * leagueStats["FTA"])
    DRBper <- (leagueStats["TRB"] - leagueStats["ORB"]) / leagueStats["TRB"]

    
    uPER <- with(pdf,
    (1 / MP) *
    (X3P - (PF * leagueStats["FT"] / leagueStats["PF"])
        + (FT / 2 * (2 - tdf$AST / (3 * tdf$FG)))
        + (FG * (2 - factor * tdf$AST / tdf$FG))
        + (2/3 * AST)
        + VOP * (DRBper * (2 * ORB + BLK - 0.2464 * (FTA - FT) - (FGA - FG) - TRB)
            + (.44 * leagueStats["FTA"] * PF)/leagueStats["PF"]
            - TOV - ORB + STL + TRB -0.1936 * (FTA-FT))
    ))
    
    Team_Possessions <- 0.5 * ((tdf$FGA + 0.4 * tdf$FTA - 1.07 * (tdf$ORB / (tdf$ORB + tdf$DRB)) * (tdf$FGA - tdf$FG) + tdf$TOV) + (oppdf$FGA + 0.4 * oppdf$FTA - 1.07 * (oppdf$ORB / (oppdf$ORB + tdf$DRB)) * (oppdf$FGA - oppdf$FG) + oppdf$TOV))

    Opp_Possessions <- 0.5 * ((oppdf$FGA + 0.4 * oppdf$FTA - 1.07 * (oppdf$ORB / (oppdf$ORB + oppdf$DRB)) * (oppdf$FGA - oppdf$FG) + oppdf$TOV) + (tdf$FGA + 0.4 * tdf$FTA - 1.07 * (tdf$ORB / (tdf$ORB + oppdf$DRB)) * (tdf$FGA - tdf$FG) + tdf$TOV))

    League_Possessions <- (leagueStats["FGA"] + 0.4 * leagueStats["FTA"] - 1.07 *
                           (leagueStats["ORB"] / (leagueStats["ORB"] + leagueStats["DRB"])) * (leagueStats["FGA"] - leagueStats["FG"]) + leagueStats["TOV"])
    
    Team_Pace <- 48 * ((Team_Possessions + Opp_Possessions) / (2 * (tdf$MP / 5)))
    League_Pace <- 48 * (League_Possessions / (leagueStats["MP"] / 5))

    pace_adjustment = League_Pace / Team_Pace

    aPER <- (pace_adjustment) * uPER
    lg_aPER <- sum(pdf$MP / sum(pdf$MP) * uPER, na.rm=TRUE)
    
    PER <- aPER * (15 / lg_aPER)

    PER
}

calcPER2 <- function(pdf, tdf, oppdf, leagueStats) {
    PER <- with(pdf, (FG * 85.910 + STL * 53.897 + X3P * 51.757
                + FT * 46.845 + BLK * 39.190 + ORB * 39.190
                + AST * 34.677 + DRB * 14.707
                - PF * 17.174
                - (FTA - FT) * 20.091
                - (FGA - FG) * 39.190
                - TOV * 53.897)/MP)

    PER
}

calc3PSS <- function(pdf, otherYears) {
    if(!is.null(otherYears)) {

        shrunkenThrees <- sapply(1:nrow(pdf), function(i) {

            nm <- pdf$Name[i]
            shrunkenThree <- NA
            if(nm %in% otherYears$Name) {

                tabName <- otherYears[otherYears$Name == nm, ]
                seasons <- unique(tabName$Season)
                total3PA <- round(tabName[["3PA36min"]] * tabName$MP / 36)
                total3PM <- round(tabName[["3P%"]] * total3PA)
                total3PA <- aggregate(total3PA, by=list(tabName$Season),
                                      function(x) sum(x, na.rm=TRUE))[, 2]
                total3PM <- aggregate(total3PM, by=list(tabName$Season),
                                      function(x) sum(x, na.rm=TRUE))[, 2]

                ## plug in resampled value for 2014
                seasonIdx <- match("2014", seasons)
                indices <- which(!is.na(total3PA) & !is.na(total3PM) & total3PA > 0)

                if(seasonIdx %in% indices & length(indices) > 1) {
                    total3PA[seasonIdx] <- pdf$X3PA[i]
                    total3PM[seasonIdx] <- pdf$X3P[i]
                    shrunkenThree <- tryCatch({
                        shrink <- gbp(total3PM[indices], total3PA[indices],
                                      model="binomial")
                        shrunkenThree <- shrink$post.mean[seasonIdx]
                    }, error = function(e) { shrunkenThree <- NA},
                    finally = { shrunkenThree })

                }
            }

            shrunkenThree

        })
        
    } else {
        shrunkenThrees <- NULL
    }
    
    shrunkenThrees
}

calcSTL <- function(pdf, tdf, oppdf, leagueStats) {
    Opp_Possessions <- 0.5 * ((oppdf$FGA + 0.4 * oppdf$FTA - 1.07 * (oppdf$ORB / (oppdf$ORB + oppdf$DRB)) * (oppdf$FGA - oppdf$FG) + oppdf$TOV) + (tdf$FGA + 0.4 * tdf$FTA - 1.07 * (tdf$ORB / (tdf$ORB + oppdf$DRB)) * (tdf$FGA - tdf$FG) + tdf$TOV))
    STLper <- with(pdf, 100 *(STL * (tdf$MP / 5)) / (MP * Opp_Possessions))

    STLper
}


