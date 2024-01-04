################################################################################
# CONFOUNDING ISSUES IN AIR POLLUTION EPIDEMIOLOGY
################################################################################

################################################################################
# PREPARE THE DATA
################################################################################

# LOAD COHORT DATASET, BASELINE VARS, OUTCOME DATASET, IMPUTED VARS
bdcohortinfo <- readRDS(paste0(maindir, "bdcohortinfo.RDS")) |> as.data.table()

# LOAD BASELINE VARS (SECOND IMPUTED DATASET)
bdbasevar <- readRDS(paste0(maindir, "bdbasevarmi.RDS")) |> 
  complete(action="all") |> _[[2]] |> as.data.table()

# LOAD OUTCOME DATASET (EXCLUDING ACCIDENTAL DEATHS)
outdeath <- readRDS(paste0(maindir, "outdeath.RDS")) |> as.data.table() |>
  subset(substr(icd10,1,1) %in% icdcode)

# LOAD THE PM DATA, DEFINE THE LAGS
pmdata <- fread(paste0(pmdir, "ukbenv_annual_2003-2021_rebadged.csv"))
names(pmdata)[1] <- "eid"
pmdata[, paste0("pm25_",0,lag):=rowMeans(Reduce(cbind, shift(pm25, 0:lag))), 
  by=eid]

# TRANSFORM BASELINE VARIABLES IN UNORDERED FACTORS (FOR REGRESSION MODEL)
ordvar <- names(bdbasevar)[sapply(bdbasevar, is.ordered)]
bdbasevar[, (ordvar):=lapply(.SD, factor, ordered=F), .SDcols=ordvar]

# MERGE THE DATA ACROSS SOURCES: COHORT, OUTCOME, BASELINE VARIABLES
fulldata <- merge(bdcohortinfo, outdeath, all.x=T) |> 
  merge(bdbasevar[, c("eid","asscentre","sex")])

# CREATE AN (APPROXIMATE) MONTH OF BIRTH
#fulldata[, birthmonth:=round(as.numeric(dob)/30)]
fulldata[, birthyear:=year(dob)]

# DEFINE EXIT TIME AND EVENT (RESET IF EVENT AFTER END OF FOLLOW-UP)
fulldata[, dexit:=fifelse(!is.na(dod), pmin(devent,dendfu), dendfu)]
fulldata[devent>dexit, `:=`(devent=NA, icd10=NA)]
fulldata[, event:=(!is.na(icd10))+0]

# SPLIT THE DATA BY CALENDAR YEAR
cut <- year(range(fulldata$dstartfu)[1]):year(range(fulldata$dendfu)[2]) |>
  paste0("-01-01") |> as.Date() |> as.numeric()
fulldata[, `:=`(dstartfu=as.numeric(dstartfu), dexit=as.numeric(dexit))]
fulldata <- survSplit(Surv(dstartfu, dexit, event) ~., fulldata, cut=cut) |> 
  as.data.table()

# ASSIGN THE YEAR (AS THE ENTER TIME AFTER SPLITTING MINUS 1)
fulldata[, year:= year(as.Date(dstartfu, origin=as.Date("1970-01-01")))-1]

# CREATE AGE AT ENTER AND EXIT TIMES (AS DAYS)
fulldata[, `:=`(agestartfu=(dstartfu-as.numeric(dob))/365.25,
  ageexit=(dexit-as.numeric(dob))/365.25)]

# MERGE WITH IMPUTED BASELINE VARS
fulldata <- subset(fulldata, select=-c(sex,asscentre)) |> 
  merge(bdbasevar, by="eid") |> 
  setkey(eid, year)

# MERGE WITH PM DATA REMOVING SUBJECTS/PERIODS WITH (PARTIALLY) MISSING EXPOSURE
setkey(fulldata, eid, year)
setkey(pmdata, eid, year)
fulldata <- merge(fulldata, na.omit(pmdata), by=c("eid","year"))
