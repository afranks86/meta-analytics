## Get bootstrap, within player variance and total variance
getVariances <- function(totals, reps, seasons="2014", exclude=c()) {
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
        vec <- apply(x, 2, function(w) var(w, na.rm=TRUE))
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
