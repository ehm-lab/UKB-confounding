################################################################################
# CONFOUNDING ISSUES IN AIR POLLUTION EPIDEMIOLOGY
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
  geom_point(size=5) +
  geom_point(aes(x=year,y=rate/100), size=5, col="red", data=frate(fulldata, "year")) +
  geom_line(aes(x=year,y=rate/100), col="red", data=frate(fulldata, "year")) +
  theme(axis.title=element_text(size=15), axis.text.x = element_text(size=10)) +
  scale_y_continuous(limits=c(0,15), 
    sec.axis = sec_axis(~ . * 100, name="Mortality rate (x 100,000)")) +
  scale_x_continuous(breaks=2006:2021, minor_breaks=NULL) +
  labs(title="By calendar time", y=pmlab, x="Calendar year") +
  theme_bw()

#PLOT MORTALITY RATES AND PM ACROSS AGE
plotage <- pmdata |> 
  subset(eid %in% fulldata$eid) |> 
  summarize(pm25 = mean(pm25, na.rm=T), .by=agegr) |>
  ggplot(aes(x=agegr, y=pm25, group=1)) +
  geom_line() +
  geom_point(size=5) +
  geom_point(aes(x=agegr,y=rate/100), size=5, col="red", data=frate(fulldata, "agegr")) +
  geom_line(aes(x=agegr,y=rate/100), col="red", data=frate(fulldata, "agegr")) +
  theme(axis.title=element_text(size=15), axis.text.x = element_text(size=10)) +
  scale_y_continuous(limits=c(0,35), 
    sec.axis = sec_axis(~ . * 100, name="Mortality rate (x 100,000)")) +
  #scale_x_discrete(minor_breaks=NULL) +
  labs(title="By age", y=pmlab, x="Age group") +
  theme_bw()

# SAVE AS SINGLE PLOT
# ggsave(file="figures/plottrend.png", plotyear + plotage, width=5000,
#   height=1500, units="px")
ggsave(file="figures/plottrend.pdf", plotyear + plotage, width=15, height=5)

################################################################################
# SPATIAL DISTRIBUTION

# PLOT MORTALITY RATES VS PM ACROSS CENTRE
plotasscentre <- summarise(fulldata, pm25=mean(pm25), .by=asscentre) |>
  merge(frate(fulldata, "asscentre")) |>
  ggplot(aes(x=rate, y=pm25)) +
  geom_point(aes(size=py), show.legend=F) +
  geom_smooth(method="lm", aes(weight=py), se=T) +
  geom_text_repel(aes(label=asscentre), size=3) +
  coord_cartesian(xlim=c(450,1000), ylim=c(5,15)) +
  labs(y=pmlab, x="Mortality rate (x 100,000)") +
  theme_bw()

#SAVE PLOT
#ggsave(file="figures/plotasscentre.png", width=3500, height=2000, units="px")
ggsave(file="figures/plotasscentre.pdf", width=10, height=6)


#####################################################################
#PLOT MORTALITY RATES VS PM FOR INDIVIDUAL- and AREA- LEVEL COVARIATES
#####################################################################

pmlist<-list()
mrlist <- list()

for (i in 1:length(confall))  {
  
  var <- confall[i]
  
  pmplot <- pmdata |>  
    merge(bdbasevar[,.(eid, var= eval(parse(text = var)))],  by="eid") |>
    subset(eid %in% fulldata$eid, 
           select=c("eid", "var", "pm25")) |>
    na.omit() |>
    ggplot(aes(x=var, y=pm25)) +
    geom_boxplot(fill="lightskyblue", alpha=0.8, outlier.alpha=0.8,
                 outlier.size=0.5, shape=19) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))+
    labs(y="", x="") +
    theme_bw() 
  
  mrplot <- fulldata |> 
    _[,.(eid,icd10,var=eval(parse(text = var)))] |>
    unique(by="eid") |>
    group_by(var) |>
    summarize(cases=sum(!is.na(icd10)), 
              pop = n(),
              rate = (cases/pop)*1000) |>
    ggplot(aes(x=var,y=rate)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(guide = guide_axis(n.dodge=2))+
    theme_bw()+
    ylim(0, 140) +
    labs(y="", x=confall[i]) 
  
  
  pmlist[[i]]<-pmplot
  mrlist[[i]]<-mrplot
}


###################################################
#CREATE PLOTS FOR AREA-LEVEL CONFS
#Arrange plots
plot<-wrap_plots(c(pmlist[which(confall %in% confarea)],
                        mrlist[which(confall %in% confarea)]) , ncol=3) + 
  plot_annotation(title = 'Particulate and mortality trends by confounders',
                  theme = theme(plot.title = element_text(size = 20)))

#SAVE PLOT
ggsave(file = "C:/Users/lsh2004062/Downloads/trendarea.png",
       plot, width = 4000, height =2500, units = "px")


###################################################
#CREATE PLOTS FOR INDIVIDUAL SES CONFS
#Arrange plots
plot<-wrap_plots(c(pmlist[which(confall %in% confses)],
                   mrlist[which(confall %in% confses)]) , ncol=4) + 
  plot_annotation(title = 'Particulate and mortality trends by confounders',
                  theme = theme(plot.title = element_text(size = 20)))

#SAVE PLOT
ggsave(file = "C:/Users/lsh2004062/Downloads/trendses.png",
       plot, width = 5000, height =2500, units = "px")

###################################################
#CREATE PLOTS FOR INDIVIDUAL "OTHER" CONFS (2 PLOTS SEPARATE)
#Arrange plots
plot<-wrap_plots(c(pmlist[which(confall %in% confother)][1:3],
                   mrlist[which(confall %in% confother)][1:3]) , ncol=3) + 
  plot_annotation(title = 'Particulate and mortality trends by confounders',
                  theme = theme(plot.title = element_text(size = 20)))

#SAVE PLOT
ggsave(file = "C:/Users/lsh2004062/Downloads/trendother1.png",
       plot, width = 4000, height =2500, units = "px")



plot<-wrap_plots(c(pmlist[which(confall %in% confother)][4:6],
                   mrlist[which(confall %in% confother)][4:6]) , ncol=3) + 
  plot_annotation(title = 'Particulate and mortality trends by confounders',
                  theme = theme(plot.title = element_text(size = 20)))

#SAVE PLOT
ggsave(file = "C:/Users/lsh2004062/Downloads/trendother2.png",
       plot, width = 4000, height =2500, units = "px")
