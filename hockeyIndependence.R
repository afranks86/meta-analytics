rm(list=ls())
library(sbgcop)
library(plyr)
library(lme4)
source("util.R")

fig.dir <- 'Figs/'
dat.dir <- 'data/'

###############     Load season totals data      ###############
load(sprintf("%shockeyTotals.Rdata", dat.dir))

## normalize some columns by 100 (percentages)
colsToNormalizeBy100 <- c("S%", "CF%", "CF% rel", "FF% rel", "FF%", "oiSH%", "oiSV%", "PDO", "oZS%", "dZS%", "FO%")
totalsDat[, colsToNormalizeBy100] <- apply(totalsDat[, colsToNormalizeBy100], 2, function(x) x / 100)

colsToRemove <- c("FOwin", "FOloss")

totalsDat <- totalsDat[, which(!apply(totalsDat,2,function(x) all(is.na(x))))]

TOI <- totalsDat[, "TOI"]
totalsDat <- totalsDat[, setdiff(colnames(totalsDat), colsToRemove)]

## leave some columns as is
colsToNotNormalize <- c("S%", "CF%", "CF% rel", "FF% rel", "FF%", "oiSH%", "oiSV%", "PDO", "oZS%", "dZS%", "TOI", "FO%", "+/-", "OPS", "DPS", "PS", "ATOI", "Season", "Name")


colsToNormalize <- setdiff(colnames(totalsDat), colsToNotNormalize)

## normalize other columns by minutes played
totalsDat[, colsToNormalize] <- apply(totalsDat[, colsToNormalize], 2, function(x) {
    x * 60 / totalsDat[, "TOI"]
})

############# gaussian copula estimation #################
res <- sbgcop.mcmc(totalsDat[, setdiff(colnames(totalsDat), c("Name", "Season"))])
C <- apply(res$C.psamp, c(1,2), mean)
imputedTab <- res$Y.pmean
save(res, C, res, file=sprintf("%shockey-sbg.RData", dat.dir))

## Generate R-squared matrix
RsquaredMatrix <- matrix(1, nrow=ncol(C), ncol=ncol(C))
rownames(RsquaredMatrix) <- colnames(C)
for(metric in colnames(C)) {
    print(metric)

    count <- ncol(C)
    removedSet <- c(metric)
    rsq <- condVar(C, metric, setdiff(colnames(C), c(removedSet)))
    RsquaredMatrix[metric, count] <- rsq
    count <- count - 1
    
    while(count > 1) {

        idx <- which.max(sapply(setdiff(colnames(C), removedSet), function(x) condVar(C, metric, setdiff(colnames(C), c(removedSet, x)))))
        nextOut <- setdiff(colnames(C), removedSet)[idx]
        removedSet <- c(removedSet, nextOut)
        rsq <- condVar(C, metric, setdiff(colnames(C), removedSet))
        RsquaredMatrix[metric, count] <- rsq
        count <- count - 1
    }
}

## R-squared figure
par(mar=c(5.1, 5.1, 4.1, 2.1))
png(sprintf("%shockey-Rsquared.png", fig.dir),
    width=400, height=400)
plot(RsquaredMatrix["PTS", ], type="l", ylim=c(0,1), lwd=3,
     ylab="R-Squared", xlab="Number of Included Metrics",
     main="Hockey Metrics",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
lines(c(1, RsquaredMatrix["TK", ]), ylim=c(0,1), lwd=3, col="red")
lines(c(1, RsquaredMatrix["CF% rel", ]), ylim=c(0,1), lwd=3, col="dark green")
lines(c(1, RsquaredMatrix["oiSH%", ]), ylim=c(0,1), lwd=3, col="blue")
lines(c(1, RsquaredMatrix["dZS%", ]), ylim=c(0,1), lwd=3, col="orange")
legend("bottomleft", legend=c("PTS", "TK", "CF% rel", "OiSH%", "dZS%"),
       col=c("black", "red", "dark green", "blue", "orange"), lwd=3, cex=1.2)
dev.off()

## Dendrogram of metric dependencies
pdf(sprintf("%shockeyDendro.pdf", fig.dir), width=10)
plot(hclust(dist(abs(C))), main="", xlab="", axes=F, sub="", ylab="Dependencies between NHL Metrics", cex=1.2, cex.lab=1.5, lwd=2)
dev.off()

## Eigen-analysis of correlation matrix
eig <- eigen(C)
vals <- eig$values
vecs <- eig$vectors
head(cumsum(vals)/sum(vals),n=20)

pdf(sprintf("%svarExp-nhl.pdf", fig.dir))
plot(cumsum(vals)/sum(vals), type="l", ylim=c(0,1), lwd=3,
     ylab="Independence", xlab="Number of Included Metrics",
     main="All NHL Metrics",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
grid()
dev.off()

## Principle component analysis metrics
imputedTab <- as.data.frame(imputedTab)
normTab <- apply(imputedTab[TOI > 200,], 2, function(col) {
    n <- length(col)
    F <- ecdf(col)
    qnorm(n/(n+1)*F(col))
})
rownames(normTab) <- rownames(imputedTab[TOI > 200,])

## first and third pcs in paper
for(i in 1:3) {
    vi <- vecs[, i]
    names(vi) <- colnames(normTab)
    superstat <- as.matrix(normTab) %*% vi
    names(superstat) <- rownames(normTab)
    print(tail(sort(superstat), n=10))
    print(head(sort(superstat), n=10))
    print(head(sort(abs(vi), decreasing=TRUE), n=10))
    
}


