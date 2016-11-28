rm(list=ls())
library(sbgcop)
library(parallel)
source("replicateHockey.R")
source("util.R")

dat.dir <- 'data/'
fig.dir <- 'Figs/'


#######    Bootstrap replicate by game (2015)  ###############

load(sprintf("%shockeyGamelogs2015.Rdata", dat.dir))
load(sprintf("%shockeyTeamStats.Rdata", dat.dir))


reps <- mclapply(1:20, function(x) bootstrap_season(gamelogs15, teamStats), mc.cores=2)
allreps <- do.call(rbind, reps)
save(allreps, reps, file=sprintf("%shockeyReps.Rdata", dat.dir))



###############     Load season totals data      ###############

load(sprintf("%shockeyTotals.RData", dat.dir))

TOI <- totalsDat[, "TOI"]

## normalize some columns by 100 (percentages)
colsToNormalizeBy100 <- c("S%", "CF%", "CF% rel", "FF% rel", "FF%", "oiSH%", "oiSV%", "PDO", "oZS%", "dZS%", "FO%")
totalsDat[, colsToNormalizeBy100] <- apply(totalsDat[, colsToNormalizeBy100], 2, function(x) x / 100)
totalsDat <- totalsDat[, which(!apply(totalsDat,2,function(x) all(is.na(x))))]

colsToRemove <- c("FOwin", "FOloss")
totalsDat <- totalsDat[, setdiff(colnames(totalsDat), colsToRemove)]

## leave some columns as is
colsToNotNormalize <- c("S%", "CF%", "CF% rel", "FF% rel", "FF%", "oiSH%", "oiSV%", "PDO", "oZS%", "dZS%", "TOI", "FO%", "PGA", "PGF", "+/-", "OPS", "DPS", "PS", "ATOI", "Season", "Name")


colsToNormalize <- setdiff(colnames(totalsDat), colsToNotNormalize)

## normalize other columns by minutes played
totalsDat[, colsToNormalize] <- apply(totalsDat[, colsToNormalize], 2, function(x) {
    x * 60 / totalsDat[, "TOI"]
})

## same normalization for bootstrap replicates
allreps[, colsToNormalize] <- apply(allreps[, colsToNormalize], 2, function(x) {
    x * 60 / allreps[, "TOI"]
})

minMinutes <- 500
varList <- getVariances(totalsDat[totalsDat$TOI > minMinutes, ],
                        allreps[allreps$TOI > minMinutes, ],
                        seasons="2014-15",
                        exclude=c("CF", "CA", "FF", "FA"))

discriminationScores <- varList$discrimination
stabilityScores <- varList$stability

## manual jitter for legibility
stabilityScores["BLK"] <-
     stabilityScores["BLK"] + 0.02
stabilityScores["GC"] <-
    stabilityScores["GC"] + 0.02
discriminationScores["DPS"] <-
    discriminationScores["DPS"] + 0.02

discriminationScores["CF%"] <-
     discriminationScores["CF%"] + 0.02


pdf(sprintf"%shockeyReliability.pdf", fig.dir))
plot(discriminationScores, stabilityScores, cex=0,
     xlab="Discrimination", ylab="Stability", main="Metric Reliabilities (NHL)",
     xlim=c(0, 1.0), ylim=c(0, 1.0), cex.axis=1.2, cex.main=1.5, cex.lab=1.2)
text(discriminationScores, stabilityScores, labels=names(stabilityScores), cex=0)
dev.off()

save(discriminationScores, stabilityScores, file=sprintf("%shockeyReliabilityScores.Rdata", dat.dir))

