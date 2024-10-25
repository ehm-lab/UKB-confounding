################################################################################
# CONFOUNDING ISSUES IN AIR POLLUTION EPIDEMIOLOGY
################################################################################

################################################################################
# ADDITIONAL RESULTS
################################################################################

################################################################################
# DESCRIPTIVE STATS

# TOTAL SUBJECT IN THE ORIGINAL COHORT AND IN THE FINAL DATASET
nrow(bdcohortinfo)
length(unique(fulldata$eid))
length(unique(fulldata$eid)) / nrow(bdcohortinfo) * 100

# LENGTH OF FOLLOW-UP: AVERAGE AND TOTAL
fulldata[, .(fu=(max(dexit)-min(dstartfu))/365.25), by=eid][,
  .(meanfu=mean(fu), totfu=sum(fu))]

# NUMBER OF NON-ACCIDENTAL DEATHS 
sum(fulldata$event)
