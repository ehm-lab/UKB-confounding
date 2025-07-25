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
