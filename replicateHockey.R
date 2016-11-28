# Functions for replicating advanced metrics from bootstrapped seasons.
library(plyr)
library(parallel)
library(Rgbp)

updateColnames <- function(mat) {
    nmap <- list(PM="+/-",
                 G_EV="EV",
                 G_PP="PP",
                 G_SH="SH",
                 FOper="FO%",
                 Take="TK",
                 Give="GV",
                 ShotPer="S%",
                 oiGoalsFor="TGF",
                 oiGoalsAgainst="TGA",
                 CorsiFor="CF",
                 CorsiAgainst="CA",
                 CFper="CF%",
                 CFperRel="CF% rel",
                 FenwickFor="FF",
                 FenwickAgainst="FA",
                 FFper="FF%",
                 FFperRel="FF% rel",
                 oiShPer="oiSH%",
                 oiSvPer="oiSV%"
                 )

    for(i in 1:ncol(mat)) {
        nm <- colnames(mat)[i]
        if(nm %in% names(nmap))
            colnames(mat)[i] <- nmap[[nm]]
    }

    mat
}

sum_col_fun <- function(col){
  return(function(df){ sum(as.numeric(df[,col]), na.rm=TRUE) })
}

aggr_pct_fun <- function(pct, tot){
  return(function(df){
    sum(as.numeric(df[, pct])*as.numeric(df[, tot]), na.rm=TRUE) /
      sum(as.numeric(df[, tot]), na.rm=TRUE)
  }
  )
}

aggr_div_fun <- function(numer, tot){
  return(function(df){
    sum(as.numeric(df[, numer]), na.rm=TRUE) /
    sum(as.numeric(df[, tot]), na.rm=TRUE)
  }
  )
}

aggr_frac_fun <- function(outcome1, outcome2){
  return(function(df){
      numer <- sum(as.numeric(df[, outcome1]), na.rm=TRUE)
      numer / (sum(as.numeric(df[, outcome2]), na.rm=TRUE) + numer)
  }
  )
}



wt_aggr_fun <- function(cols, wts){
  return(function(df)
    sum(as.matrix(df[cols]) %*% wts)
  )
}

aggr_funs <- c(GP = function(x) nrow(x),
               TOI = sum_col_fun("TOI"),
               G = sum_col_fun("G"),
               A = sum_col_fun("A"),
               PTS = sum_col_fun("PTS"),
               PM = sum_col_fun("+/-"),
               PIM = sum_col_fun("PIM"),
               G_EV = sum_col_fun("Goals (EV)"),
               G_PP = sum_col_fun("Goals (PP)"),
               G_SH = sum_col_fun("Goals (SH)"),
               GW = sum_col_fun("GW"),
               A_EV = sum_col_fun("Assists (EV)"),
               A_PP = sum_col_fun("Assists (PP)"),
               A_SH = sum_col_fun("Assists (SH)"),
               S = sum_col_fun("S"),
               ShotPer = aggr_div_fun("G", "S"),
               TSA = sum_col_fun("TSA"),
               SHFT = sum_col_fun("SHFT"),
               HIT = sum_col_fun("HIT"),
               BLK = sum_col_fun("BLK"),
               FOwin = sum_col_fun("FOwin"),
               FOloss = sum_col_fun("FOloss"),
               FOper = aggr_frac_fun("FOwin", "FOloss"),
               SFor = sum_col_fun("SFor"),
               MFor = sum_col_fun("MFor"),
               BFor = sum_col_fun("BFor"),
               SAgainst = sum_col_fun("SAgainst"),
               MAgainst = sum_col_fun("MAgainst"),
               BAgainst = sum_col_fun("BAgainst"),
               SForOff = sum_col_fun("SForOff"),
               MForOff = sum_col_fun("MForOff"),
               BForOff = sum_col_fun("MForOff"),
               SAgainstOff = sum_col_fun("SAgainstOff"),
               MAgainstOff = sum_col_fun("MAgainstOff"),
               BAgainstOff = sum_col_fun("BAgainstOff"),
               Give = sum_col_fun("Give"),
               Take = sum_col_fun("Take"),
               Toff = sum_col_fun("TOff"),
               oiGoalsFor = sum_col_fun("oiGoalsFor"),
               oiGoalsAgainst = sum_col_fun("oiGoalsAgainst"),
               offGoalsFor = sum_col_fun("offGoalsFor"),
               offGoalsAgainst = sum_col_fun("offGoalsAgainst"))

addPlayerMetrics <- function(x) {
    newMetricNames <- c("CorsiFor", "CorsiAgainst", "FenwickFor",
                        "FenwickAgainst",
                        "CorsiForOff", "CorsiAgainstOff", "FenwickForOff",
                        "FenwickAgainstOff",
                        "CFper",
                        "CFperRel",
                        "C60",
                        "CRel60",
                        "FFper",
                        "FFperRel"
                        )
    newMetrics <- rep(0, length(newMetricNames))
    names(newMetrics) <- newMetricNames

    newMetrics["CorsiFor"] <- x["SFor"] + x["MFor"] + x["BFor"] + x["oiGoalsFor"]
    newMetrics["CorsiAgainst"] <- x["SAgainst"] + x["MAgainst"] + x["BAgainst"] + x["oiGoalsAgainst"]
    newMetrics["FenwickFor"] <- x["SFor"] + x["MFor"] + x["oiGoalsFor"]
    newMetrics["FenwickAgainst"] <- x["SAgainst"] + x["MAgainst"] + x["oiGoalsAgainst"]
    newMetrics["CorsiForOff"] <- x["SForOff"] + x["MForOff"] + x["BForOff"] + x["offGoalsFor"]
    newMetrics["CorsiAgainstOff"] <- x["SAgainstOff"] +
        x["MAgainstOff"] + x["BAgainstOff"] + x["offGoalsAgainst"]
    newMetrics["FenwickForOff"] <- x["SForOff"] + x["MForOff"] + x["offGoalsFor"]
    newMetrics["FenwickAgainstOff"] <- x["SAgainstOff"] + x["MAgainstOff"] + x["offGoalsAgainst"]

    newMetrics["CFper"] <- newMetrics["CorsiFor"] /
        (newMetrics["CorsiFor"] + newMetrics["CorsiAgainst"])
    newMetrics["CFperRel"] <- newMetrics["CFper"] -  newMetrics["CorsiForOff"] /
        (newMetrics["CorsiForOff"] + newMetrics["CorsiAgainstOff"])
    newMetrics["C60"] <- (newMetrics["CorsiFor"] - newMetrics["CorsiAgainst"]) * 60 / x["TOI"]
    newMetrics["CRel60"] <- (newMetrics["CorsiFor"] / x["TOI"] -  newMetrics["CorsiForOff"] / x["Toff"]) * 60

    newMetrics["FFper"] <- newMetrics["FenwickFor"] /
        (newMetrics["FenwickFor"] + newMetrics["FenwickAgainst"])
    newMetrics["FFperRel"] <- newMetrics["FFper"] -  newMetrics["FenwickForOff"] /
        (newMetrics["FenwickForOff"] + newMetrics["FenwickAgainstOff"])

    
    c(x, newMetrics)
}


# Game logs
bootstrap_season <- function(games_df, teamStats, identity=FALSE,
                             excludePlayoffs=TRUE){

    teams <- unique(c(games_df$Home, games_df$Away))
    all_player_season <- ldply(as.list(teams), function(team) {
        print(sprintf("Bootstrapping %s", team))
        team_df <- games_df[(games_df$Home==team & games_df[["Home/Away"]]=="") |
                            (games_df$Away==team & games_df[["Home/Away"]]=="@"),
                            ]
        if(identity) {
            resampledDates <- unique(team_df$Date)
        } else { 
            resampledDates <- sample(unique(team_df$Date), replace=TRUE)
        }

        if(excludePlayoffs) {
            team_df <- team_df[team_df$Playoffs==FALSE, ]
        }
        
        player_season <- ddply(team_df, .(Name, Year), function(x){

            dates <- match(resampledDates, x$Date)
            dates <- dates[!is.na(dates)]
            xsamp <- x[dates, ]
            resvec <- sapply(aggr_funs,
                             function(fun) fun(xsamp))

            resvec <- addPlayerMetrics(resvec)

            resvec <- data.frame(matrix(resvec, nrow=1, ncol=length(resvec),
                                        dimnames=list(NULL, names(resvec))))
            resvec$Pos <- unique(x$Pos)[1]
            resvec
        })

        ## For Offensive Point Shares (OPS)
        teamGoals <- sum(player_season$G)
        teamAssists <- sum(player_season$A)

        player_season$oiShPer <- player_season$oiGoalsFor / (player_season$SFor + player_season$oiGoalsFor)
        player_season$oiSvPer <- 1 - player_season$oiGoalsAgainst / (player_season$SAgainst +  player_season$oiGoalsAgainst)
        player_season$PDO <- player_season$oiShPer + player_season$oiSvPer

        
        player_season$GC <- (player_season$G + 1/2*player_season$A) *
            teamGoals / (teamGoals + 1/2*teamAssists)
        player_season$marginalGoals <- player_season$GC -
            7/12*player_season$TOI * sum(player_season$GC) /
            sum(player_season$TOI)

        ## FOR Defensive Point Shares (DPS)
        teamTimeOnIce <- player_season$TOI / sum(player_season$TOI)

        ## hard-coding in league shots against per minute = 0.535
        propTeamMargGA <- (7 - 2 * (teamStats$SA[teamStats$Tm==team] / teamStats$SA[teamStats$Tm=="AVG"])) / 7

        ## position adjustment
        posAdj <- ifelse(player_season$Pos=="D", 10/7, 5/7)

        ## team marginal goals against 
        teamMargGA <- (1 + (7 / 12)) * teamStats$GF[teamStats$Tm=="AVG"] - (teamStats$GA[teamStats$Tm==team])

        pmPos <- sapply(player_season$Pos, function(x)
            sum(player_season$PM[player_season$Pos==x & !is.na(player_season$Pos)]))
        toiPos <- sapply(player_season$Pos, function(x)
            sum(player_season$TOI[player_season$Pos==x & !is.na(player_season$Pos)]))
        
        ## plus minus adjustment 
        pmAdj <- 1/7 * posAdj * (player_season$PM - pmPos * player_season$TOI/toiPos)
        ## marginal goals gainst
        margGA <- teamTimeOnIce * propTeamMargGA * posAdj * teamMargGA + pmAdj

        ## Marginal goals per point
        margGPP <- teamStats$GF[teamStats$Tm=="AVG"] / teamStats$PTS[teamStats$Tm=="AVG"]
        player_season$DPS <- margGA / margGPP

        ## Offensive point shares
        margGoals <- player_season$GC - (7/12) * player_season$GP * sum(player_season$GC) / sum(player_season$GP)
        player_season$OPS <- margGoals / margGPP

        player_season$PS <- player_season$DPS + player_season$OPS

        player_season$ATOI <- player_season$TOI / player_season$GP
            
        player_season
    })

    all_player_season <- updateColnames(all_player_season)
    all_player_season
}

