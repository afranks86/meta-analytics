rm(list=ls())

dat.dir <- 'data'
fig.dir <- 'Figs'

library(sbgcop)
library(xtable)
source("util.R")

load(file.path(dat.dir, "NameIdMap.RData"))
load(file.path(dat.dir, "bballTotalsInd.Rdata"))

res <- sbgcop.mcmc(bballTotals)
C <- apply(res$C.psamp, c(1,2), mean)
imputedTab <- res$Y.pmean
save(res, C, imputedTab,
     file=file.path(dat.dir, "bball-sbg.RData"))

## create dendrogram
pdf(file.path(fig.dir, "sbballDendro.pdf"), width=14)
plot(hclust(dist(abs(C))), main="", xlab="", axes=F, sub="", ylab="Dependencies between NBA Metrics", cex=0.75, cex.lab=1.5)
dev.off()

## eigendecomposition of latent correlation matrix
eig <- eigen(C)
vals <- eig$values
vecs <- eig$vectors
head(cumsum(vals)/sum(vals), n=20)

## variance explained figure
pdf(file.path(fig.dir, "sbballVarExplained.pdf"), width=4, height=4)
plot(cumsum(vals)/sum(vals),ylim=c(0,1),pch=19,ylab="Variance Explained",xlab="Number of Components", main="All NBA Metrics", type="l", lwd=3, cex.lab=1.2)
grid()
dev.off()


normTab <- apply(imputedTab[imputedTab[,"MP"] >= 410, ], 2, function(col) {
    n <- length(col)
    F <- ecdf(col)
    qnorm(n/(n+1)*F(col))
})
rownames(normTab) <- rownames(imputedTab[imputedTab[,"MP"] >= 410,])

for(i in 1:3) {
    vi <- vecs[, i]
    names(vi) <- colnames(normTab)
    superstat <- as.matrix(normTab) %*% vi
    names(superstat) <- rownames(normTab)
    print(tail(sort(superstat), n=15))
    print(head(sort(superstat), n=15))
    print(head(sort(abs(vi), decreasing=TRUE), n=10))
    
}

v1 <- vecs[, 1]
v2 <- vecs[, 2]
v3 <- vecs[, 3]
v4 <- vecs[, 4]
v5 <- vecs[, 5]
names(v1) <- colnames(normTab)
superstat1 <- as.matrix(normTab) %*% v1
names(superstat1) <- rownames(normTab)
names(v2) <- colnames(normTab)
superstat2 <- -1 *as.matrix(normTab) %*% v2
names(v3) <- colnames(normTab)
superstat3 <- -1 *as.matrix(normTab) %*% v3
names(v4) <- colnames(normTab)
superstat4 <- -1 *as.matrix(normTab) %*% v4
names(v5) <- colnames(normTab)
superstat5 <- -1 *as.matrix(normTab) %*% v5
rnms <- rownames(normTab)
rnms <- sapply(rnms, function(nm) paste(unlist(strsplit(nm, " - "))[1:2], collapse=" - "))
names(superstat1) <- names(superstat2) <- names(superstat3) <-
    names(superstat4) <- names(superstat5) <- rnms

RsquaredMatrix <- MostSimilar <- matrix(1, nrow=ncol(C), ncol=ncol(C))
rownames(RsquaredMatrix) <- rownames(MostSimilar) <- colnames(C)
for(metric in colnames(C)) {
    print(metric)
    nextOut <- metric
    count <- ncol(C)
    removedSet <- c(metric)
    rsq <- condVar(C, metric, setdiff(colnames(C), c(removedSet)))
    RsquaredMatrix[metric, count] <- rsq
    MostSimilar[metric, count] <- nextOut
    count <- count - 1
    
    while(count > 1) {

        remaining <- setdiff(colnames(C), removedSet)
        idx <- which.max(sapply(remaining, function(x)
            condVar(C, metric, setdiff(colnames(C), c(removedSet, x)))))
        nextOut <- setdiff(colnames(C), removedSet)[idx]
        removedSet <- c(removedSet, nextOut)
        rsq <- condVar(C, metric, setdiff(colnames(C), removedSet))
        RsquaredMatrix[metric, count] <- rsq
        MostSimilar[metric, count] <- nextOut
        count <- count - 1
    }
    MostSimilar[metric, 1] <- setdiff(remaining, nextOut)
}

png(file.path(fig.dir, "Independence-1.png",
              width=400, height=400)
par(mar=c(5.1, 5.1, 4.1, 2.1))
plot(RsquaredMatrix["VORP", ], type="l", ylim=c(0,1), lwd=3,
     ylab="Independence", xlab="Number of Included Metrics",
     main="Overall Skill Metrics",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
lines(RsquaredMatrix["WS", ], ylim=c(0,1), lwd=3, col="red")
lines(RsquaredMatrix["PER", ], ylim=c(0,1), lwd=3, col="dark green")
lines(RsquaredMatrix["BPM", ], ylim=c(0,1), lwd=3, col="blue")
lines(RsquaredMatrix["PTS", ], ylim=c(0,1), lwd=3, col="orange")
legend("bottomleft", legend=c("VORP", "WS", "PER", "BPM", "PTS"),
       col=c("black", "red", "dark green", "blue", "orange"), lwd=3, cex=1.2)
dev.off()
oevals <- eigen(C[c("VORP", "WS", "PER", "BPM", "PTS"), c("VORP", "WS", "PER", "BPM", "PTS")])$values
print(cumsum(oevals)/sum(oevals))

png(file.path(fig.dir, "Independence-2.png")
  , width=400, height=400)
plot(RsquaredMatrix["DBPM", ], type="l", ylim=c(0,1), lwd=3,
     ylab="Independence", xlab="Number of Included Metrics",
     main="Defensive Metrics",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
lines(RsquaredMatrix["STL", ], ylim=c(0,1), lwd=3, col="red")
lines(RsquaredMatrix["BLK", ], ylim=c(0,1), lwd=3, col="dark green")
lines(RsquaredMatrix["DWS", ], ylim=c(0,1), lwd=3, col="blue")
lines(RsquaredMatrix["DRtg", ], ylim=c(0,1), lwd=3, col="orange")

legend("bottomleft", legend=c("DBPM", "STL", "BLK", "DWS", "DRtg"),
       col=c("black", "red", "dark green", "blue", "orange"), lwd=3, cex=1.2)
dev.off()
devals <- eigen(C[c("STL", "BLK", "DWS", "DBPM", "DRtg"),
                 c("STL", "BLK", "DWS", "DBPM", "DRtg")])$values
print(cumsum(devals)/sum(devals))


pdf(file.path(fig.dir, "bballDefVarExplained.pdf"),
    width=4, height=4)
plot(cumsum(devals)/sum(devals),ylim=c(0,1),pch=19,ylab="Variance Explained",xlab="Number of Components", main="Defensive NBA Metrics", type="l", lwd=3, cex.lab=1.2)
grid()
dev.off()

pdf(file.path(fig.dir, "bballOmniVarExplained.pdf"),
    width=4, height=4)
plot(cumsum(oevals)/sum(oevals),ylim=c(0,1),pch=19,ylab="Variance Explained",xlab="Number of Components", main="Omnibus NBA Metrics", type="l", lwd=3, cex.lab=1.2)
grid()
dev.off()


## Circle plots 
plotNames <- c("LeBron James", "Stephen Curry", "DeAndre Jordan", "Kirk Hinrich")
toplot <- paste0(plotNames, " - 2014")
starDat <- cbind(ecdf(superstat1)(superstat1[toplot]),
                 ecdf(superstat2)(superstat2[toplot]),
                 ecdf(superstat3)(superstat3[toplot]))
pdf(file.path(fig.dir, "starsPlot.pdf"), width=14)
stars(starDat, labels=plotNames, nrow=1, ncol=4, draw.segments=TRUE, scale=FALSE, col.segments=c("#10b1c3", "#c31212", "black"), cex=1.5, flip.labels=FALSE)
dev.off()
