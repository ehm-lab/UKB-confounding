################################################################################
# CONFOUNDING ISSUES IN AIR POLLUTION EPIDEMIOLOGY
################################################################################

################################################################################
# RUN SENSITIVITY MODEL FOR ASSESSMENT CENTRE INCLUSION
################################################################################

# OBJECTS TO STORE THE RESULTS
resens <- list()

# FIT MODEL WITH STRATA
fmod <- funfmod()
resens$strata <- coxph(fmod, data=fulldata, ties="efron") |> 
  ci.exp(mod, subset="pm25_07", ctr.mat=matrix(10))

# FIT MODEL WITH WITH DUMMY VARIABLE
fmod <- funfmod(strata=c("sex","birthyear"), 
                conf=c(confall, "asscentre"))
resens$dummy <- update(mod, fmod) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

#FIT MODEL WITH RANDOM EFFECT (coxme)
fmod <- funfmod(strata=c("sex","birthyear"), 
                conf=c(confall, "(1| asscentre)"))
resens$coxme <- coxme(fmod, data=fulldata, ties="efron") |>  
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

#FIT MODEL WITH FRAILTY
fmod <- funfmod(strata=c("sex","birthyear"), 
                conf=c(confall, "frailty(asscentre)"))
resens$frailty <- update(mod, fmod) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

#FIT MODEL WITH CLUSTER (DO NOT USE "cluster(asscentre)" in formula)
fmod <- funfmod(strata=c("sex","birthyear"), 
                conf=confall)
resens$coxme <- update(mod, formula=fmod, cluster=asscentre) |>  
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))


################################################################################
# PUT TOGETHER AND SAVE

# ASSEMBLE
modres <- Reduce(rbind, resens)
rownames(modres) <- names(resens)

# SAVE
saveRDS(modres, file="temp/modresens.RDS")
