################################################################################
# R code for reproducing the analysis in:
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
# LOAD THE PACKAGES AND SET THE PARAMETERS
################################################################################

# LOAD THE PACKAGES
library(zen4R)
library(data.table) ; library(dplyr)
library(survival) ; library(Epi)
library(dlnm) ; library(splines) ; library(coxme)
library(ggplot2); library(patchwork) ; library(scales) ; library(ggrepel)
library(corrplot)
library(sf)

# CREATE FOLDERS (IF NEEDED)
for(fold in c("tables","figures","temp","data"))
  if(!fold %in% list.files()) dir.create(fold)

# SELECT MORTALITY OUTCOMES (EXCLUDE ACCIDENTAL)
icdcode <- LETTERS[seq(which(LETTERS=="R"))]

# RANGE INCRESE FOR HR COMPUTATION
pminc <- 10

# LAG WINDOW
lag <- 7

# SETS FOR CONFOUNDER MODELS
confarea <- c("tdi", "urbrur", "greenspace")
confses <- c("ethnic", "educ", "income", "employ")
confother <- c("smkstatus", "smkpackyear", "alcoholintake", "wthratio", "ipaq",
  "livealone")
# NO health AND illness
confall <- c(confarea, confses, confother)

# FUNCTION TO DEFINE THE MODEL FORMULA (WITH DEFAULT)
funfmod <- function(response="Surv(dstartfu, dexit, event)",
  strata=c("asscentre", "sex", "birthyear"), conf=confall, lag=7) {
  f <- paste0(response, " ~ ", "strata(", paste0(strata, collapse=", "), ") + ",
    paste0(conf, collapse=" + "), " + pm25_0", lag)
  formula(f, env=parent.frame())
}

# FUNCTION TO FORMAT ESTIMATES
funformat <- function(x, digits=1, big.mark="") 
  formatC(x, digits=digits, format="f", big.mark=big.mark)

# FUNCTION TO FORMAT ESTIMATES WITH RANGES
frange <- function(est, digits=2, big.mark="", sep="-") {
  paste0(funformat(est[1], digits=digits, big.mark=big.mark), " (",
    funformat(est[2], digits=digits, big.mark=big.mark), sep,
    funformat(est[3], digits=digits, big.mark=big.mark), ")")
}

# FUNCTION TO EXTRACT DISTRIBUTIONAL STATS
fdstat <- function(x, per=perlin, digits=2, big.mark="", sep=" to ") 
  c(mean(x, na.rm=T),quantile(x, per, na.rm=T)) |> 
  frange(digits=digits, big.mark=big.mark, sep=sep)

# FUNCTION TO DEFINE DEATH RATES BY SOME VARIABLE
frate <- function(data, var, mult=10^5) 
  summarise(data, cases=sum(event), py=sum(dexit-dstartfu)/365.25, 
    rate=cases/py*10^5, .by=var) |> arrange(get(var))

# AGE GROUPING
agebreaks <- c(0, 9:16*5, 100)
agelabs <- c("<45", paste0(9:15*5, "-", 9:15*5+4), "80+")

# LISTS OF VARIABLES FOR DESCRIPTIVE STATS
dvarcat <- c("sex","tdicat","urbrur","greenspacecat","ethnic","educ",
  "income","employ","smkstatus","smkpackyearcat","alcoholintake","wthratiocat",
  "ipaq","livealone")
