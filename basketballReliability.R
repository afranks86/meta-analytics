rm(list=ls())

library(sbgcop)
library(xtable)
library(plyr)
library(ggplot2)
library(Rgbp)
source("replicateBasketball.R")
source("util.R")

dat.dir <- 'data'
fig.dir <- 'Figs'

load(file.path(dat.dir, "NameIdMap.RData"))
load(file.path(dat.dir, "bballTotals.Rdata"))


## normalize columns (per 36 min by default)
colsToNormalize <- c("TOV - BadPass", "TOV - LostBall", "TOV - Other", "PF - Shooting", "PF - Blocking", "PF - Offensive", "PF - Take", "PGA", "SfDrawn", "And1", "Blkd")

bballTotals[, colsToNormalize] <- apply(bballTotals[, colsToNormalize], 2, function(x) x / bballTotals$MP* 36)

zero_pct <- apply(bballTotals, 2, function(x){ mean(x==0, na.rm=TRUE)})

# Remove all player-seasons in a stat corresponding to players who have _ever_ had a zero in
# that stat.
zeros_removed <- ddply(bballTotals, .(Name), function(x){
  x[, apply(x, 2, function(y) any(!is.na(y) & y==0))] <- NA
  x
})

######## Bootstrap replicate basketball data #############
load(file.path(dat.dir, "bballGamelogs2015.Rdata"))

## Verify correct reconstruction of season totals data
## from gamelog data 

reconstructed <- bootstrap_season(gamelogs15, identity=TRUE)
totals1415 <- zeros_removed[zeros_removed$Season==2014, ]
totals1415 <- addThreeBetweenPlayer(totals1415)

rnms <- intersect(paste(reconstructed$Name, reconstructed$Tm),
                  paste(totals1415$Name, totals1415$Tm))

idx1 <- match(rnms, paste(reconstructed$Name, reconstructed$Tm))
idx2 <- match(rnms, paste(totals1415$Name, totals1415$Tm))
intersection <- setdiff(intersect(colnames(reconstructed),
                                  colnames(totals1415)),
                        c("Tm", "Name"))
for(metric in intersection) {
    plot(reconstructed[idx1, metric],
         totals1415[idx2, metric], main=metric)
    abline(a=0, b=1)
    
    browser()
}


## At least 250 minutes, all players
minMP <- 250
minPlayers <- bballTotals$Name[bballTotals$MP > minMP]
totals <- bballTotals[bballTotals$Name %in% minPlayers, ]
totals <- addThreeWithinPlayer(totals)

## compare similar players and remove players who shoot few threes
totals[is.na(totals[["3P%SS"]]), "3P%"] <- NA
nathrees <- totals$Name[is.na(totals[["3P%"]])]

## bootstrap replicate totals (20 times)
nboot <- 20
gdfSubset <- gamelogs15[gamelogs15$Name %in% minPlayers, ]
repsList <- mclapply(1:nboot, function(x) {
    print(sprintf("boostrapping %i of %i", x, nboot))
    bootstrap_season(gdfSubset, totals)
})

reps <- do.call(rbind, repsList)
reps <- reps[reps$MP > minMP*3/4, ]
reps[reps$Name %in% nathrees, c("3P%", "3P%SS")] <- NA
save(reps, file=file.path(dat.dir, "basketballReps.Rdata"))

reps[["3P%"]][reps[["3PA"]] < 10] <- NA

## Compute discrimination and stability scores for all metrics
varList <- getVariances(totals, reps, exclude=c("3PA"))

disc <- varList$discrimination
stab <- varList$stability

##################
## Plot metric reliabilities
##################

## Manual jitter for legibility
stab["OBPM"] <- stab["OBPM"] - 0.01
stab["USG%"] <- stab["USG%"] - 0.01
stab["DRB%"] <- stab["DRB%"] - 0.01
disc["ORB%"] <- disc["ORB%"] + 0.01
stab["BLK%"] <- stab["BLK%"] + 0.01
stab["TOV%"] <- stab["TOV%"] - 0.01
stab["PTS36min"] <- stab["PTS36min"] + 0.01

pdf(file.path(fig.dir, "reliability.pdf"))

names(stab) <- gsub("36min", "", names(disc))
plot(disc, stab, cex=0,
     xlab="Discrimination", ylab="Stability",
     xlim=c(0.3, 1.0), ylim=c(0.3, 1.0),
     main="Metric Reliabilities", cex.axis=1.2, cex.main=1.5, cex.lab=1.2)
colors <- rep("black", length(stab))
colors[names(stab)=="3P%SS"] <- "red"
names(stab)[names(stab)=="3P%SS"] <- "3P% EB"
text(disc, stab, labels=names(stab), cex=0, col=colors)

dev.off()

## LBJ Three point shrinkage plot from paper
totalsLBJ <- totals[totals$Name == "LeBron James", ]
total3PA <- round(totalsLBJ[["3PA36min"]] * totalsLBJ$MP / 36)
total3PM <- round(totalsLBJ[["3P%"]] * total3PA)
indices <- which(!is.na(total3PA) & !is.na(total3PM) & total3PA > 0)

pdf(file.path(fig.dir, "lbj_threes.pdf"))
plot.gbpCust(gbp(total3PM[indices], total3PA[indices],
                 model="binomial"), seasons=totalsLBJ$Season,
             sort=FALSE)
dev.off()


