################################################################################
# Original R code for the analysis in:
#
# Vanoli J, et al. Confounding mechanisms and adjustment strategies in air 
#   pollution epidemiology: a case-study assessment with the UK Biobank cohort. 
#   Under review. 
# http://...
#
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/UKB-confounding
################################################################################

################################################################################
# TABLES
################################################################################

################################################################################
# TABLE OF DESCRIPTIVE STATS

# CATEGORICAL VARIABLES
tabdcat <- lapply(dvarcat, function(x) {
  
  # SELECT THE DATA
  dd <- subset(bdbasevar, eid %in% fulldata$eid)
  
  # EXTRACT STATS AND MISSING
  stat <- table(dd[[x]])
  nmis <- sum(is.na(dd[[x]]))
  text <- paste0(funformat(c(stat,nmis), digits=0, big.mark=","), " (", 
    funformat(c(stat,nmis)/nrow(dd)*100),"%)")
  lev <- levels(dd[[x]])
  
  # PRODUCE TABLE
  tab <- cbind(c(lev,"Missing (%)"), text)
  dimnames(tab) <- list(rep(x, length(lev)+1), NULL)
  tab
}) |> Reduce(rbind, x=_) |> as.data.table(keep.rownames=TRUE)

# EXPOSURE 
tabdexp <- lapply(dvarcat, function(x) {
  
  # SELECT THE DATA
  dd <- subset(bdbasevar, eid %in% fulldata$eid)
  
  # EXTRACT STATS AND MISSING
  mean <- tapply(fulldata$pm25_07, fulldata[[x]], mean)
  sd <- tapply(fulldata$pm25_07, fulldata[[x]], sd)
  nmis <- sum(is.na(dd[[x]]))
  text <- paste0(funformat(mean, digits=2, big.mark=","), " (", 
    funformat(sd, digits=2, big.mark=","),")")
  lev <- levels(fulldata[[x]])
  
  # PRODUCE TABLE
  tab <- cbind(lev, text)
  dimnames(tab) <- list(rep(x, length(lev)), NULL)
  tab
}) |> Reduce(rbind, x=_) |> as.data.table(keep.rownames=TRUE)

# RATES
tabdrate <- lapply(dvarcat, function(x) {
  
  # EXTRACT STATS AND MISSING
  tab <- as.matrix(frate(fulldata, x)[c(1,4)])
  tab[,2] <- funformat(as.numeric(tab[,2]), digits=2, big.mark=",")
  lev <- levels(fulldata[[x]])
  
  # PRODUCE TABLE
  dimnames(tab) <- list(rep(x, length(lev)),NULL)
  tab
}) |> Reduce(rbind, x=_) |> as.data.table(keep.rownames=TRUE)

# MERGE
tabdescr <- merge.data.table(tabdcat, tabdexp, all.x=T, by=c("rn","V1"), sort=FALSE)
tabdescr <- merge.data.table(tabdescr, tabdrate, all.x=T, by=c("rn","V1"), sort=FALSE)
names(tabdescr) <- c("variable", "levels", "count(%)", "exposure average(SD)",
  "death rate")

# SAVE
write.csv(tabdescr, file="tables/tabdescr.csv")

################################################################################
# TABLE OF EFFECT ESTIMATES FOR THE MAIN AND SENSITIVITY ANALYSES

# MAIN ANALYSIS
tabmodres <- apply(modres, 1, frange) |> as.matrix()
tabmodres <- cbind(paste("Model", seq(nrow(modres))), tabmodres)
write.csv(tabmodres, file="tables/tabmodres.csv")

# SENSITIVITY ANALYSIS (TWO SEPARATE TABLES)
tabmodsens <- apply(modsens, 1, frange) |> as.matrix()
write.csv(tabmodsens[1:9,], file="tables/tabmodsens1.csv")
write.csv(tabmodsens[10:15,], file="tables/tabmodsens2.csv")
