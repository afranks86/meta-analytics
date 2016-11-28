## Get bootstrap, within player variance and total variance
## seasons: which season(s) to use to calculate SV
## minSeason: minimun number of seasons required to count toward WV
getVariances <- function(totals, reps, seasons="2014", exclude=c(), minSeason=4) {

    exclude <- c(exclude, "Name", "Tm")
    commonMetrics <- setdiff(intersect(colnames(totals), colnames(reps)),
                             exclude)

    ## Total variance
    TV <- apply(totals, 2, function(w) var(w, na.rm=TRUE))[commonMetrics]

    ## Single season variance for calculating discrimination
    SV <- apply(totals[totals$Season %in% seasons, ], 2,
                function(w) var(w, na.rm=TRUE))[commonMetrics]
    
    bootstrapVars <- ddply(reps, .(Name), function(x) {
        if(nrow(x) < 10) {
            vec <- c()
        } else{ 
            vec <- apply(x, 2, function(w) var(w, na.rm=TRUE))
            vec["Name"] <- x$Name[1]
        }
        vec
    })

    ## Average bootstrap vars within a season
    BV <- colMeans(data.matrix(bootstrapVars[, commonMetrics]), na.rm=TRUE)

    withinPlayerVar <- ddply(totals, .(Name), function(x) {
        if(nrow(x) >= minSeason) {
            vec <- apply(x, 2, function(w) var(w, na.rm=TRUE))
        } else{
            vec <- rep(NA, ncol(x))
            names(vec) <- colnames(x)
        }
        vec["Name"] <- x$Name[1]
        vec
    })
    WV <- colMeans(data.matrix(withinPlayerVar[, commonMetrics]), na.rm=TRUE)
    
    intersectNames <- intersect(withinPlayerVar$Name, bootstrapVars$Name)
    commonCols <- intersect(colnames(withinPlayerVar), colnames(bootstrapVars))
    commonCols <- setdiff(commonCols, c("Name", "Year", "Tm"))
    diffVars <-
        data.matrix(withinPlayerVar[withinPlayerVar$Name %in% intersectNames, commonCols]) -
        data.matrix(bootstrapVars[bootstrapVars$Name %in% intersectNames, commonCols])
    DV <- colMeans(diffVars, na.rm=TRUE)[commonMetrics]
    
    discriminationScores <- 1 - BV / SV
    
    stabilityScores <- 1 - (WV - BV) / (TV - BV)

    list(TV=TV, BV=BV, WV=BV, SV=SV,
         discrimination=discriminationScores,
         stability=stabilityScores)
    
}

## Return conditional variance
condVar <- function(cov, varCols, condCols=NULL) {
    if(is.null(condCols)) {
        condCols <- setdiff(colnames(cov), varCols)
    }
    allCols <- union(varCols, condCols)
    cov <- cov[allCols, allCols]
    cov[varCols, varCols] - cov[varCols, condCols] %*% solve(cov[condCols, condCols]) %*% cov[condCols, varCols]
}

## For basketball
addThreeWithinPlayer <- function(tab) {

    tab <- ddply(tab, .(Name), function(tabName) {
        total3PA <- round(tabName[["3PA36min"]] * tabName$MP / 36)
        total3PM <- round(tabName[["3P%"]] * total3PA)
        indices <- which(!is.na(total3PA) & !is.na(total3PM) & total3PM > 1)
        if(length(indices) > 2) {
            shrink <- gbp(total3PM[indices], total3PA[indices], model="binomial")
            tabName[["3P%SS"]] <- tabName[["3P%"]]
            tabName[["3P%SS"]][indices] <- shrink$post.mean
            tabName[["3P%SS"]][-indices] <- NA
            
        }
        
        tabName
    })

    tab
    
    
}

## For basketball
addThreeBetweenPlayer <- function(tab) {

    tab <- ddply(tab, .(Season), function(tabSeason) {

        total3PA <- round(tabSeason[["3PA36min"]] * tabSeason$MP / 36)
        total3PM <- round(tabSeason[["3P%"]] * total3PA)
        indices <- which(!is.na(total3PA) & !is.na(total3PM) & total3PA > 0)
        shrink <- gbp(total3PM[indices], total3PA[indices], model="binomial")
        tabSeason[["3P%SP"]] <- rep(NA, nrow(tabSeason))
        tabSeason[["3P%SP"]][indices] <- shrink$post.mean

        tabSeason
    })

    tab
    
}

## min:sec -> min
minsec2dec <- function(x) {

    splt <- unlist(strsplit(x, ":"))
    timeInMin <- as.numeric(splt[1]) + as.numeric(splt[2])/60
    timeInMin

}

plot.gbpCust <- function (x, playerNm="LeBron James", seasons=NULL, sort = TRUE, ...) 
{
    y <- x$sample.mean
    se <- x$se
    if (any(is.na(x$prior.mean))) {
        pr.m <- x$prior.mean.hat
    }
    else {
        pr.m <- x$prior.mean
    }
    po.m <- x$post.mean
    po.sd <- x$post.sd
    po.low <- x$post.intv.low
    po.upp <- x$post.intv.upp
    if (sort == TRUE) {
        temp.data <- as.data.frame(cbind(y, se, pr.m, po.m, po.sd, 
            po.low, po.upp))
        if (x$model == "gr") {
            temp.data <- temp.data[order(temp.data$se, decreasing = TRUE), 
                ]
        }
        else {
            temp.data <- temp.data[order(temp.data$se), ]
        }
        y <- temp.data$y
        se <- temp.data$se
        pr.m <- temp.data[, 3]
        po.m <- temp.data[, 4]
        po.sd <- temp.data[, 5]
        po.low <- temp.data[, 6]
        po.upp <- temp.data[, 7]
    }
    index <- 1:length(se)
    ylim.low <- ifelse(min(po.low, y) >= 0, 0.8 * min(po.low, 
        y), 1.2 * min(po.low, y))
    ylim.upp <- ifelse(max(po.upp, y) >= 0, 1.2 * max(po.upp, 
        y), 0.8 * max(po.upp, y))
    par(fig = c(0.25, 1, 0.5, 1), xaxs = "r", yaxs = "r", mai = c(0.5, 
        0.5, 0.5, 0.3), las = 1, ps = 13)
    if (x$model != "gr") {
        se <- sqrt(y * (1 - y)/se)
    }
    sqrtV <- se
    sdlens <- sqrtV/max(sqrtV)
    postlens <- po.sd/max(sqrtV)
    xmin <- min(c(y, po.m, pr.m))
    xmax <- max(c(y, po.m, pr.m))
    sunflowerplot(rep(4, length(y)) ~ y, ylim = c(-1, 5), xlim = c(xmin - 
        abs(xmin) * 0.1, xmax + abs(xmax) * 0.1), yaxt = "n", 
        col.lab = "white", main = "LeBron James three point percentage ", pch = 1, 
        cex = 1)
    if (length(unique(pr.m)) == 1) {
        abline(v = pr.m, col = 4)
    }
    else {
        points(pr.m, rep(0, length(pr.m)), col = 4, pch = "-", 
            cex = 2)
    }
    sunflowerplot(rep(0, length(y)) ~ po.m, add = TRUE, col = "red", 
        cex = 1, pch = 16)
    abline(h = 4)
    abline(h = 0)
    sapply(1:length(y), function(i) {
        lines(c(y[i], po.m[i]), c(4, 0))
        lines(c(y[i], y[i] + sdlens[i] * sd(y) * 0.4), c(4, 4 + 
            sdlens[i]), col = "darkviolet")
        lines(c(po.m[i] - postlens[i] * sd(y) * 0.4, po.m[i]), 
            c(0 - postlens[i], 0), col = "darkgreen")
        xcord <- ((4 * po.m[i]/(y[i] - po.m[i]) - 4 * po.m/(y - 
            po.m))/(4/(y[i] - po.m[i]) - 4/(y - po.m)))
        ycord <- 4/(y - po.m) * xcord - 4/(y - po.m) * po.m
        coords <- subset(cbind(xcord, ycord), ycord > 0 & ycord < 
            4)
        points(coords, pch = 0)
    })
    par(fig = c(0, 1, 0, 0.5), xaxs = "r", yaxs = "r",
        mai = c(0.8, 0.9, 0.5, 0.3), las = 1, ps = 13, new = TRUE)
    if(!is.null(seasons)) {
        xl <- "Season"
    } else {
        if (sort == TRUE) {
            if (x$model == "gr") {
                xl <- c("Groups sorted by the descending order of se")
            } else {
                xl <- c("Groups sorted by the ascending order of n")
            }
        } else {
            xl <- c("Groups in the order of data input")
        }
    }
    plot(index, po.m, ylim = c(ylim.low, ylim.upp), xlab = xl, ylab="3P%", main = paste(100 * x$confidence.lvl, "% Interval"), 
         col = 2, pch = 19, xaxt="n")
    if(is.null(seasons))
        seasons <- index
    axis(1, at=index, labels=seasons, las=2)
    sapply(1:length(y), function(j) {
        lines(rep(index[j], 2), c(po.low[j], po.upp[j]), lwd = 0.5)
    })
    points(index, po.low, cex = 1.5, pch = "-")
    points(index, po.upp, cex = 1.5, pch = "-")
    points(index, y)
    if (length(unique(pr.m)) == 1) {
        abline(h = pr.m, col = 4)
    }
    else {
        points(index, pr.m, col = 4, pch = "-", cex = 2)
    }
    if (x$model == "gr") {
        se.or.n <- "Standard error"
    }
    else {
        se.or.n <- "Number of attempts"
    }
    if (x$model == "br" & length(x$weight) != 1) {
        par(fig = c(0, 0.35, 0.5, 1), xaxs = "r", yaxs = "r", 
            mai = c(0.4, 0.4, 0.5, 0), las = 1, ps = 9, oma = c(0, 
                0, 0, 0), new = TRUE)
        plot(1, type = "n", axes = F, xlab = "", ylab = "")
        legend("topleft", pch = c(19, 1, NA, NA, NA), col = c(2, 
            1, 4, "darkviolet", "darkgreen"), lwd = c(NA, 
            NA, 2, 2), c("Posterior mean \nof random effect", 
            "Sample mean", "Posterior mean of\nexpected random effect", 
            se.or.n, "Posterior sd \nof random effect"), 
            seg.len = 0.5, bty = "n", xpd = TRUE)
    }
    else {
        par(fig = c(0, 0.35, 0.5, 1), xaxs = "r", yaxs = "r", 
            mai = c(0.4, 0.1, 0.5, 0), las = 1, ps = 13, oma = c(0, 
                0, 0, 0), new = TRUE)
        plot(1, type = "n", axes = F, xlab = "", ylab = "")
        legend("topleft", pch = c(19, 1, NA, NA, NA), col = c(2, 
            1, 4, "darkviolet", "darkgreen"), lwd = c(NA, 
            NA, 2, 2, 2), c("Shrunken estimate", "Empiricial percentage", 
            "Career average", se.or.n, "Posterior sd"), 
            seg.len = 0.5, bty = "n", xpd = TRUE)
 

    }
}
