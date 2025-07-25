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
# GRAPHS
################################################################################

################################################################################
# TEMPORAL TRENDS

# AXIS LABEL FOR PM
pmlab <- expression(paste(PM[2.5]," (",mu,"g/",m^3,")"))

# PLOT MORTALITY RATES AND PM ACROSS YEARS
plotyear <- pmdata |> 
  subset(eid %in% fulldata$eid & year >= 2006) |> 
  summarize(pm25 = mean(pm25, na.rm=T), .by=year) |>
  ggplot(aes(x=year, y=pm25)) +
  geom_line() +
  geom_point(size=6, shape=18) +
  geom_point(aes(x=year,y=rate/100), size=5, col="red", data=frate(fulldata, "year")) +
  geom_line(aes(x=year,y=rate/100), col="red", data=frate(fulldata, "year")) +
  theme(axis.title=element_text(size=15), axis.text.x = element_text(size=10)) +
  scale_y_continuous(limits=c(0,15), 
    sec.axis = sec_axis(~ . * 100, name="Mortality rate (x 100,000)")) +
  scale_x_continuous(breaks=2006:2021, minor_breaks=NULL) +
  labs(title="By calendar time", y=pmlab, x="Calendar year") +
  theme_bw()

# PLOT MORTALITY RATES AND PM ACROSS AGE
plotage <- pmdata |> 
  subset(eid %in% fulldata$eid) |> 
  summarize(pm25 = mean(pm25, na.rm=T), .by=agegr) |>
  ggplot(aes(x=agegr, y=pm25, group=1)) +
  geom_line() +
  geom_point(size=6, shape=18) +
  geom_point(aes(x=agegr,y=rate/100), size=5, col="red", data=frate(fulldata, "agegr")) +
  geom_line(aes(x=agegr,y=rate/100), col="red", data=frate(fulldata, "agegr")) +
  theme(axis.title=element_text(size=15), axis.text.x = element_text(size=10)) +
  scale_y_continuous(limits=c(0,35), 
    sec.axis = sec_axis(~ . * 100, name="Mortality rate (x 100,000)")) +
  #scale_x_discrete(minor_breaks=NULL) +
  labs(title="By age", y=pmlab, x="Age group") +
  theme_bw()

# SAVE AS SINGLE PLOT
ggsave(file="figures/plottrend.pdf", plotyear + plotage, width=15, height=5)

################################################################################
# ASSESSMENT CENTRE

# PLOT MORTALITY RATES VS PM ACROSS CENTRE
plotasscentre <- summarise(fulldata, pm25=mean(pm25), .by=asscentre) |>
  merge(frate(fulldata, "asscentre")) |>
  ggplot(aes(x=rate, y=pm25)) +
  geom_point(aes(size=py), show.legend=F) +
  geom_smooth(method="lm", aes(weight=py), se=T) +
  geom_text_repel(aes(label=asscentre), size=3) +
  coord_cartesian(xlim=c(450,1000), ylim=c(5,15)) +
  labs(y=pmlab, x="Mortality rate (x 100,000)") +
  scale_x_continuous(breaks=5:10*100, minor_breaks=NULL) +
  theme_bw()

# SAVE PLOT
ggsave(file="figures/plotasscentre.pdf", width=10, height=6)

################################################################################
# CONTEXTUAL AND INDIVIDUAL-LEVEL VARIABLES

# LIST OF VARIABLES
varlist <- c("tdicat","urbrur","greenspacecat",
  "educ", "income", "employ",
  "smkpackyearcat", "alcoholintake", "ipaq")
varlabs <- c("Area-level deprivation", "Urban-rural", "Greenspace", 
  "Education", "Income (£)", "Employment", 
  "Smoking", "Alcohol intake", "Physical activity")

# LIST OF EXPOSURE PLOTS
plotexpvarlist <- lapply(seq(varlist), function(i) {
  subset(fulldata, select=c("eid", varlist[i])) |> unique() |>
    merge(pmdata) |> subset(select=c("eid","year","pm25",varlist[i])) |> 
    na.omit() |>
    ggplot(aes(x=get(varlist[i]), y=pm25)) +
    geom_boxplot(fill="lightskyblue", alpha=0.8, outlier.shape=NA) +
    scale_x_discrete(labels = NULL) +
    labs(title=varlabs[i], y=pmlab, x="") +
    theme_bw()
})

# LIST OF MORTALITY RATE PLOTS
plotratevarlist <- lapply(seq(varlist), function(i) {
  frate(fulldata, varlist[i]) |>
    ggplot(aes(x=get(varlist[i]), y=rate))+
    geom_bar(stat = "identity", col=1, fill=grey(0.7), alpha=0.3) +
    #ylim(0, 1300) +
    labs(y="Mortality rate (x 100,000)", x="") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    #scale_x_discrete(guide = guide_axis(angle = 35)) +
    theme_bw() 
})

# COMBINED PLOTS
plotareavar <- (Reduce('+', plotexpvarlist[1:3]) + plot_layout(widths=c(5,5,3))) /
  (Reduce('+', plotratevarlist[1:3]) + plot_layout(widths=c(5,5,3)))
plotsesvar <- (Reduce('+', plotexpvarlist[4:6]) + plot_layout(widths=c(4,5,3))) /
  (Reduce('+', plotratevarlist[4:6]) + plot_layout(widths=c(4,5,3)))
plotothervar <- (Reduce('+', plotexpvarlist[7:9]) + plot_layout(widths=c(5,6,3))) /
  (Reduce('+', plotratevarlist[7:9]) + plot_layout(widths=c(5,6,3)))

# SAVE
ggsave(file="figures/plotareavar.pdf", plotareavar, width=10, height=6)
ggsave(file="figures/plotsesvar.png", plotsesvar, width=10, height=6)
ggsave(file="figures/plotothervar.pdf", plotothervar, width=10, height=6)

################################################################################
# CORRELATION MATRIX PLOT

# GENERATE THE PLOT
corvar <- bdbasevar[, lapply(.SD, as.numeric)] |> cor()
dimnames(corvar) <- list(names(bdbasevar), names(bdbasevar))
fcol <- colorRampPalette(c(muted("blue"), "white", muted("red")))
corrplot(corvar, tl.cex=0.5, col=fcol(200), tl.col="black")

# SAVE
dev.print(png, file="figures/plotcorvar.png", width=480, height=480)

