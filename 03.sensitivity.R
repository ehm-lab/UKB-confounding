################################################################################
# CONFOUNDING ISSUES IN AIR POLLUTION EPIDEMIOLOGY
################################################################################

################################################################################
# RUN SENSITIVITY ANALYSIS
################################################################################

# OBJECTS TO STORE THE RESULTS
senslist <- list()

# FIT THE MAIN MODEL
fmod <- funfmod()
senslist$main <- coxph(fmod, data=fulldata, ties="efron") |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

################################################################################
# EFFECT OF ASSESSMENT CENTRE

# FIT MODEL WITH WITH DUMMY VARIABLE
fmod <- funfmod(strata=c("sex","birthyear"), conf=c(confall, "asscentre"))
senslist$dummy <- update(mod, fmod) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

# FIT MODEL WITH RANDOM EFFECT (coxme)
fmod <- funfmod(strata=c("sex","birthyear"),  conf=c(confall, "(1| asscentre)"))
senslist$coxme <- coxme(fmod, data=fulldata, ties="efron") |>  
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

# FIT MODEL WITH FRAILTY
fmod <- funfmod(strata=c("sex","birthyear"), 
  conf=c(confall, "frailty(asscentre)"))
senslist$frailty <- update(mod, fmod) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

# FIT MODEL WITH FRAILTY WITH GAUSSIAN RANDOM EFFECTS
fmod <- funfmod(strata=c("sex","birthyear"), 
  conf=c(confall, "frailty(asscentre, distribution='gaussian')"))
senslist$frailty2 <- update(mod, fmod) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

# FIT MODEL WITH CLUSTER (DO NOT USE "cluster(asscentre)" in formula)
fmod <- funfmod(strata=c("sex","birthyear"), conf=confall)
senslist$cluster <- update(mod, formula=fmod, cluster=asscentre) |>  
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

################################################################################
# EXCLUSION OF LONDON AND/OR GLASGOW

# FIT MODEL WITHOUT LONDON 
fmod <- funfmod(strata=c("sex","birthyear"), conf=c(confall))
senslist$nolnd <- update(mod, data=subset(fulldata, !(asscentre %in% 
    c("Hounslow","Croydon","Barts")))) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

# FIT MODEL WITHOUT GLASGOW
fmod <-  funfmod(strata=c("sex","birthyear"), conf=c(confall))
senslist$noglg <- update(mod, data=subset(fulldata, 
  !(asscentre %in% c("Glasgow")))) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

# FIT MODEL WITHOUT LONDON AND GLASGOW
fmod <-  funfmod(strata=c("sex","birthyear"), conf=c(confall))
senslist$nolndglg <- update(mod, data=subset(fulldata, 
  !(asscentre %in% c("Hounslow","Croydon","Barts","Glasgow")))) |> 
  ci.exp(subset="pm25_07", ctr.mat=matrix(10))

################################################################################
# SINGLE LIFESTYLE CONFOUNDERS

# EXCLUDE EACH LYFESTYLE PREDICTOR IN TURN
for(var in confother) {
  cat(var, "")
  fmod <- funfmod(conf=setdiff(confall, var))
  senslist[[paste0("no", var)]] <- update(mod, fmod) |> 
    ci.exp(subset="pm25_07", ctr.mat=matrix(10))
}

################################################################################
# PUT TOGETHER AND SAVE

# ASSEMBLE
modsens <- Reduce(rbind, senslist)
rownames(modsens) <- names(senslist)

# SAVE
saveRDS(modsens, file="temp/modsens.RDS")
