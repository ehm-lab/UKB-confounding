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
# RUN MODELS WITH DIFFERENT CONFOUNDING CONTROL
################################################################################

# OBJECTS TO STORE THE RESULTS
reslist <- list()

# FIT THE MAIN MODEL
fmod <- funfmod()
mod <- coxph(fmod, data=fulldata, ties="efron")

# STORE THE RESULT (HR FOR A 10-UNIT INCREASE IN PM25)
reslist$main <- ci.exp(mod, subset="pm25_07", ctr.mat=matrix(10))

################################################################################
# TEMPORAL CONFOUNDING

# WITH SPLINES OF AGE
fmod <- funfmod(strata=c("asscentre", "sex"), 
  conf=c(confall, "ns(agestartfu, knots=equalknots(agestartfu, df=4))"))
reslist$splage <- update(mod, fmod) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

# WITHOUT CONTROL FOR AGE
fmod <- funfmod(strata=c("asscentre", "sex"))
reslist$noage <- update(mod, fmod) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

# WITH AGE AS TIME AXIS AND STRATIFIED BY CALENDAR TIME
fmod <- funfmod(response="Surv(agestartfu, ageexit, event)",
  strata=c("asscentre", "sex", "year"))
reslist$ageaxis1 <- update(mod, fmod) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

# WITH AGE AS TIME AXIS AND SPLINES OF CALENDAR TIME
fmod <- funfmod(response="Surv(agestartfu, ageexit, event)",
  strata=c("asscentre", "sex"), 
  conf=c(confall, "ns(dstartfu, knots=equalknots(dstartfu, df=4))"))
reslist$ageaxis2 <- update(mod, fmod) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

# WITH AGE AS TIME AXIS AND WITHOUT CONTROL FOR CALENDAR TIME
fmod <- funfmod(response="Surv(agestartfu, ageexit, event)",
  strata=c("asscentre", "sex"))
reslist$ageaxis3 <- update(mod, fmod) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

################################################################################
# SPATIAL CONFOUNDING

# WITHOUT ASSESSMENT CENTRE
fmod <- funfmod(strata=c("sex", "birthyear"))
reslist$noasscen <- update(mod, fmod) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

################################################################################
# CONFOUNDING FROM AREA-LEVEL VARIABLES 

# WITHOUT SPATIAL (AREA-LEVEL) PREDICTORS
for(var in confarea) {
  cat(var, "")
  fmod <- funfmod(conf=setdiff(confall, var))
  reslist[[paste0("no", var)]] <- update(mod, fmod) |> 
    ci.exp(subset="pm25_07", ctr.mat=matrix(10))
}

################################################################################
# CONFOUNDING FROM INDIVIDUAL-LEVEL VARIABLES

# WITHOUT INDIVIDUAL SES VARS
fmod <- funfmod(conf=setdiff(confall, confses))
reslist$noses <- update(mod, fmod) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

# WITHOUT LIFESTYLE INDIVIDUAL VARS
fmod <- funfmod(conf=setdiff(confall, confother))
reslist$noother <- update(mod, fmod) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

################################################################################
# PUT TOGETHER AND SAVE

# ASSEMBLE
modres <- Reduce(rbind, reslist)
rownames(modres) <- names(reslist)

# SAVE
saveRDS(modres, file="temp/modres.RDS")
