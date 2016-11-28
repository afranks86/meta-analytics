rm(list=ls())
library(sbgcop)
library(parallel)
source("replicateHockey.R")
source("util.R")

dat.dir <- '.'
fig.dir <- '~/course/xyresearch/XYHoops/meta-analytics/paper/Figs/'

######################################################
#######     Replicate gamelogs (2015)  ###############
######################################################

## load("hockeyGamelogs2015.Rdata")
## load("hockeyTeamStats.Rdata")

## bootstrap replication by game
reps <- mclapply(1:20, function(x) bootstrap_season(gamelogs15, teamStats), mc.cores=2)
allreps <- do.call(rbind, reps)
save(allreps, reps, file="hockeyReps.Rdata")


######################################################
###############     Season totals      ###############
######################################################

load("hockeyTotals.RData")

mat <- mat[, unique(colnames(mat))]
mat[, "ATOI"] <- sapply(mat[, "ATOI"], minsec2dec)

rnms <- paste(mat[,"Name"],mat[,"Season"])
numericMat <- apply(mat[, 7:ncol(mat)], 2, function(col) as.numeric(col))
nas <- apply(numericMat, 1, function(row) all(is.na(row)))
rownames(numericMat) <- rnms
numericMat <- as.data.frame(numericMat)
numericMat$Name <- mat[, "Name"]
numericMat$Season <- mat[, "Season"]

nonCareer <- !grepl("(seasons?)|(Career)|(\\\302)",rnms)
numericMat <- numericMat[which(!nas&nonCareer),]
rnms <- rnms[which(!nas&nonCareer)]
tab <- numericMat

# Names in all caps for consistency with allreps
tab$Name <- toupper(tab$Name)

tab[["C60"]] <- (tab$CF - tab$CA) * 60 / tab$TOI
tab[["C60"]] <- (tab$CF - tab$CA) * 60 / tab$TOI

colsToNormalizeBy100 <- c("S%", "CF%", "CF% rel", "FF% rel", "FF%", "oiSH%", "oiSV%", "PDO", "oZS%", "dZS%", "FO%")
tab[, colsToNormalizeBy100] <- apply(tab[, colsToNormalizeBy100], 2, function(x) x / 100)


colsToRemove <- c("FOwin", "FOloss")

tab <- tab[, which(!apply(tab,2,function(x) all(is.na(x))))]

TOI <- tab[, "TOI"]
tab <- tab[, setdiff(colnames(tab), colsToRemove)]

colsToNotNormalize <- c("S%", "CF%", "CF% rel", "FF% rel", "FF%", "oiSH%", "oiSV%", "PDO", "oZS%", "dZS%", "TOI", "FO%", "PGA", "PGF", "+/-", "OPS", "DPS", "PS", "ATOI", "Season", "Name")


colsToNormalize <- setdiff(colnames(tab), colsToNotNormalize)

tab[, colsToNormalize] <- apply(tab[, colsToNormalize], 2, function(x) {
    x * 60 / tab[, "TOI"]
})

allreps[, colsToNormalize] <- apply(allreps[, colsToNormalize], 2, function(x) {
    x * 60 / allreps[, "TOI"]
})

zero_pct <- apply(tab, 2, function(x){ mean(x==0, na.rm=TRUE)})

# Remove all player-seasons in a stat corresponding to players who have _ever_ had a zero in
# that stat.
zeros_removed <- ddply(tab, .(Name), function(x){
  x[, apply(x, 2, function(y) any(!is.na(y) & y==0))] <- NA
  x
})

minMinutes <- 500
varList <- getVariances(zeros_removed[zeros_removed$TOI > minMinutes, ],
                        allreps[allreps$TOI > minMinutes, ], seasons="2014-15",
                        exclude=c("CF", "CA", "FF", "FA"))

discriminationScores <- varList$discrimination
stabilityScores <- varList$stability

stabilityScores["BLK"] <-
     stabilityScores["BLK"] + 0.02
stabilityScores["GC"] <-
    stabilityScores["GC"] + 0.02
discriminationScores["DPS"] <-
    discriminationScores["DPS"] + 0.02

discriminationScores["CF%"] <-
     discriminationScores["CF%"] + 0.02


pdf("paper/Figs/hockeyReliability.pdf")
plot(discriminationScores, stabilityScores, cex=0,
     xlab="Discrimination", ylab="Stability", main="Metric Reliabilities (NHL)",
     xlim=c(0, 1.0), ylim=c(0, 1.0), cex.axis=1.2, cex.main=1.5, cex.lab=1.2)
text(discriminationScores, stabilityScores, labels=names(stabilityScores), cex=0)
dev.off()
save(discriminationScores, stabilityScores, file="hockeyReliabilityScores.Rdata")


###############################################################################

totals2015 <- zeros_removed[zeros_removed$Season=="2014-15", ]
truth <- bootstrap_season(gamelogs15, teamStats, identity=TRUE)

commonCols <- intersect(colnames(truth), colnames(totals2015))
commonCols <- commonCols[-match("Name", commonCols)]

commonNames <- intersect(toupper(totals2015$Name), truth$Name)
commonNames <- setdiff(commonNames, unique(toupper(totals2015$Name[duplicated(totals2015$Name)])))
commonNames <- setdiff(commonNames, unique(truth$Name[duplicated(truth$Name)]))

totals2015 <- totals2015[toupper(totals2015$Name) %in% commonNames, ]
truth <- truth[truth$Name %in% commonNames, ]
rownames(totals2015) <- toupper(totals2015$Name)
rownames(truth) <- truth$Name

orig.mar <- par("mar")
par(mfrow=c(6, 3), mar=c(2, 2, 2, 1))
for(col in commonCols[1:18]) {
    plot(totals2015[commonNames, col], truth[commonNames, col], main=col,
         axes=FALSE)

    abline(a=0, b=1, col="blue")
}
par(mfrow=c(6, 3))
for(col in commonCols[19:36]) {
    plot(totals2015[commonNames, col], truth[commonNames, col], main=col,
         axes=FALSE)

    abline(a=0, b=1, col="blue")
}

