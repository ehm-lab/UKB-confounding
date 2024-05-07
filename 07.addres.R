################################################################################
# CONFOUNDING ISSUES IN AIR POLLUTION EPIDEMIOLOGY
################################################################################

################################################################################
# ADDITIONAL RESULTS
################################################################################

################################################################################
# DESCRIPTIVE STATS


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


# CATEGORICAL VARIABLES
tabdexp <- lapply(dvarcat, function(x) {
  
  # EXTRACT STATS AND MISSING
  mean <- aggregate(fulldata$pm25_07, by=list(fulldata[[x]]), FUN=mean)
  sd <- aggregate(fulldata$pm25_07, by=list(fulldata[[x]]), FUN=sd)
  text <- paste0(funformat(mean[,2], digits=2, big.mark=","), " (", 
                 funformat(sd[,2], digits=2, big.mark=","),")")
  lev <- levels(fulldata[[x]])
  
  # PRODUCE TABLE
  tab <- cbind(lev, text)
  dimnames(tab) <- list(rep(x, length(lev)), NULL)
  tab
}) |> Reduce(rbind, x=_) |> as.data.table(keep.rownames=TRUE)


# CATEGORICAL VARIABLES
tabdrate <- lapply(dvarcat, function(x) {
  
  # EXTRACT STATS AND MISSING
  tab<-as.matrix(frate(fulldata, x)[c(1,4)])
  tab[,2] <- funformat(as.numeric(tab[,2]), digits=2, big.mark=",")
  lev <- levels(fulldata[[x]])
  
  # PRODUCE TABLE
  dimnames(tab) <- list(rep(x, length(lev)),NULL)
  tab
  
}) |> Reduce(rbind, x=_) |> as.data.table(keep.rownames=TRUE)


#MERGE
tabs<-merge.data.table(tabdcat,tabdexp,
                       all.x = T, by=c("rn","V1"), sort=FALSE)
tabs<-merge.data.table(tabs,tabdrate,
                       all.x = T, by=c("rn","V1"), sort=FALSE)
names(tabs)<-c("variable","levels","count(%)",
               "exposure average(SD)","death rate")

# SAVE
write.csv(tabs, file="tables/tabs.csv")


# TOTAL SUBJECT IN THE ORIGINAL COHORT AND IN THE FINAL DATASET
nrow(bdcohortinfo)
length(unique(fulldata$eid))
length(unique(fulldata$eid)) / nrow(bdcohortinfo) * 100

# LENGTH OF FOLLOW-UP: AVERAGE AND TOTAL
fulldata[, .(fu=(max(dexit)-min(dstartfu))/365.25), by=eid][,
  .(meanfu=mean(fu), totfu=sum(fu))]

# NUMBER OF ALL-CAUSE AND NON-ACCIDENTAL DEATHS 
sum(fulldata[, list(death=any(!is.na(icd10))), by=eid]$death)
sum(fulldata$event)
