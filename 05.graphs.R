################################################################################
# ANALYSIS OF CONFOUNDING IN THE UKB COHORT
################################################################################

################################################################################
# CONFOUNDING ISSUES IN AIR POLLUTION EPIDEMIOLOGY
################################################################################

################################################################################
#EXPOSURE GRAPHS
################################################################################


#COMPUTE MORTALITY RATES ACROSS 

#YEARS
mryr <- fulldata |> 
  group_by(year) |>
  summarize(cases=sum(!is.na(icd10) & event==1), 
            pop = n(),
            rate = (cases/pop)*1000)

#AGE
mrage <- fulldata |> 
  mutate(age = (year - year(dob))) |>
  group_by(age) |>
  summarize(cases=sum(!is.na(icd10) & event==1), 
            pop = n(),
            rate = (cases/pop)*1000)

#CENTRE
mrcentre <- fulldata |> 
  unique(by = "eid") |>
  group_by(asscentre) |>
  summarize(cases=sum(!is.na(icd10)), 
            pop = n(),
            rate = (cases/pop)*1000,
            age = median(agebase))



####################################################
#PLOT PM2.5 ACROSS YEARS
####################################################
# ylab <- expression(paste("Annual exposure to ",PM[2.5]," (",mu,"g/",m^3,")"))
# 
# pmdata |> 
#   subset(eid %in% fulldata$eid, select=c("eid", "year", "pm25")) |> 
#   na.omit() |>
#   filter(year >= 2006) |>
#   ggplot(aes(x=factor(year), y=pm25)) +
#   geom_boxplot(fill="lightskyblue", alpha=0.8, outlier.alpha=0.8,
#                outlier.size=0.5, shape=19) +
#   labs(y=ylab, x="Year") +
#   theme_bw() 

#SAVE PLOT
# ggsave(file = "C:/Users/lsh2004062/Downloads/pmtrend.png",
#        width = 3500, height =2000, units = "px")


####################################################
#PLOT TEMPORAL TRENDS BY AGE AND CALENDAR
####################################################

#PLOT MORTALITY RATES AND PM ACROSS YEARS
ylab <- expression(paste("Mean of annual exposure to ",PM[2.5]," (",mu,"g/",m^3,")"))

trendyrs <- pmdata |> 
  subset(eid %in% fulldata$eid, select=c("eid", "year", "pm25")) |> 
  na.omit() |>
  group_by(year) |>
  summarize(pm25 = mean(pm25)) |>
  filter(year >= 2006) |>
  ggplot(aes(x=year, y=pm25)) +
  geom_line(linewidth=2) +
  geom_point(aes(x=year,y=rate),
             size=4, col="red",data=mryr) +
  geom_line(aes(x=year,y=rate),
            linewidth=1.5, col="red",data=mryr) +
  theme(axis.title=element_text(size=15),
        axis.text.x = element_text(size=10)) +
  scale_y_continuous(limits=c(0,15),
                     sec.axis = sec_axis(~ . * 1, 
             name = "mortality rates (x1000)")) +
  scale_x_continuous(breaks=2006:2021) +
  labs(title="By calendar time", y=ylab, x="Calendar year") +
  theme_bw()

#PLOT MORTALITY RATES AND PM ACROSS AGE
trendage<- pmdata |> 
  subset(eid %in% fulldata$eid, select=c("eid", "year", "pm25")) |> 
  merge(bdcohortinfo[,.(eid, yob=year(dob))], by="eid", all.y=F) |>
  _[,.(eid, year, pm25, age = year-yob)] |>
  na.omit() |>
  group_by(age) |>
  summarize(pm25 = mean(pm25)) |>
  ggplot(aes(x=age, y=pm25)) +
  geom_line(linewidth=2) +
  geom_point(aes(x=age,y=rate),
             size=4, col="red",
             data=subset(mrage,cases>1)) +
  geom_line(aes(x=age,y=rate),
            linewidth=1.5, col="red",
            data=subset(mrage,cases>1)) +
  theme(axis.title=element_text(size=15),
        axis.text.x = element_text(size=10)) +
  scale_y_continuous( limits=c(0,40),
                      sec.axis = sec_axis(~ . * 1, 
                      name = "mortality rates (x1000)")) +
  scale_x_continuous(breaks=seq(min(mrage$age),
                                max(mrage$age),by=2)) +
  labs(title="By age", y=ylab, x="Age (in years)") +
  theme_bw() 

#Arrange plots
trendplot<-wrap_plots(list(trendyrs,trendage) , ncol=2) + 
  plot_annotation(title = 'Particulate and mortality trends',
                  theme = theme(plot.title = element_text(size = 20)))


#SAVE PLOT
ggsave(file = "C:/Users/lsh2004062/Downloads/temptrend.png",
       trendplot, width = 5000, height =1500, units = "px")


#PLOT MORTALITY RATES VS PM ACROSS CENTRE
fulldata |> 
  na.omit() |>
  group_by(asscentre) |>
  summarize(pm25 = mean(pm25)) |>
  merge(mrcentre, by="asscentre") |>
  ggplot(aes(x=rate,y=pm25, colour=age)) +
  geom_point(aes(size = pop)) +
  geom_smooth(method = "lm", aes(weight=pop), se = T) +
  geom_text_repel(aes(label = asscentre),size = 3) +
  scale_color_gradient(low="blue", high="red") +
  labs(title="Death rates Vs PM2.5") +
  theme(plot.title = element_text(face="bold")) +
  coord_cartesian(xlim =c(50,90), ylim = c(5,15))+
  labs(y=bquote("average PM"[2.5]), 
       x="mortality rate (x 1000 people)")

#SAVE PLOT
ggsave(file = "C:/Users/lsh2004062/Downloads/centre.png",
       width = 3500, height =2000, units = "px")



#####################################################################
#PLOT MORTALITY RATES VS PM FOR AREA-LEVEL COVARIATES
#####################################################################

##################
#PM DISTRIBUTION
# ##################
# 
# #DEPRIVATION
# pmplot1 <- pmdata |>  
#   merge(bdbasevar[,.(eid, tdi)],  by="eid") |>
#   mutate(deprivation = cut(tdi, 5, 
#           label = c("1th","2nd","3rd","4th","5th"))) |>
#   subset(eid %in% fulldata$eid, select=c("eid",   "deprivation",  "pm25")) |>
#   na.omit() |>
#   ggplot(aes(x=deprivation, y=pm25)) +
#   geom_boxplot(fill="lightskyblue", alpha=0.8, outlier.alpha=0.8,
#                outlier.size=0.5, shape=19) +
#   labs(y=ylab, x="") +
#   theme_bw() 
# 
# 
# #GREENSPACE
# pmplot2 <- pmdata |>  
#   merge(bdbasevar[,.(eid, greenspace)],  by="eid") |>
#   mutate(greenspace= cut(greenspace, 5, 
#           label = c("1th","2nd","3rd","4th","5th"))) |>
#   subset(eid %in% fulldata$eid, 
#          select=c("eid","greenspace","pm25")) |>
#   na.omit() |>
#   ggplot(aes(x=greenspace, y=pm25)) +
#   geom_boxplot(fill="lightskyblue", alpha=0.8, outlier.alpha=0.8,
#                outlier.size=0.5, shape=19) +
#   labs(y="", x="") +
#   theme_bw() 
# 
# #URBAN/RURAL
# pmplot3 <- pmdata |>  
#   merge(bdbasevar[,.(eid, urbrur)],  by="eid") |>
#   subset(eid %in% fulldata$eid, 
#          select=c("eid", "urbrur", "pm25")) |>
#   na.omit() |>
#   ggplot(aes(x=urbrur, y=pm25)) +
#   geom_boxplot(fill="lightskyblue", alpha=0.8, outlier.alpha=0.8,
#                outlier.size=0.5, shape=19) +
#   labs(y="", x="") +
#   theme_bw() 
# 
# 
# 
# 
# ##################
# #MORTALITY RATES
# ##################
# 
# #DEPRIVATION
# mrplot1 <- fulldata |> 
#   unique(by="eid") |>
#   group_by(deprivation) |>
#   summarize(cases=sum(!is.na(icd10)), 
#             pop = n(),
#             rate = (cases/pop)*1000) |>
#   ggplot(aes(x=deprivation,y=rate))+
#   geom_bar(stat = "identity") +
#   ylim(0, 140) +
#   labs(y="rate (x1000 persons)", x="deprivation") +
#   theme_bw()
# 
# #GREENSPACE
# mrplot2 <- fulldata |> 
#   unique(by="eid") |>
#   group_by(greenspace) |>
#   summarize(cases=sum(!is.na(icd10)), 
#             pop = n(),
#             rate = (cases/pop)*1000) |>
#   ggplot(aes(x=greenspace,y=rate))+
#   geom_bar(stat = "identity") +
#   ylim(0, 140) +
#   labs(y="", x="Greenspace percentage (%)") +
#   theme_bw()
# 
# #URBAN/RURAL
# mrplot3 <- fulldata |> 
#   unique(by="eid") |>
#   group_by(urbrur) |>
#   summarize(cases=sum(!is.na(icd10)), 
#             pop = n(),
#             rate = (cases/pop)*1000) |>
#   ggplot(aes(x=urbrur,y=rate)) +
#   geom_bar(stat = "identity") +
#   ylim(0, 140) +
#   labs(y="", x="urban/rural classification") +
#   theme_bw()

#Arrange plots
trendplot<-wrap_plots(list(pmplot1,pmplot2, pmplot3,
                           mrplot1,mrplot2,mrplot3) , ncol=3) + 
           plot_annotation(title = 'Particulate and mortality trends by area-level confounders',
                  theme = theme(plot.title = element_text(size = 20)))

#SAVE PLOT
ggsave(file = "C:/Users/lsh2004062/Downloads/trendarea.png",
       trendplot, width = 4000, height =2500, units = "px")


#####################################################################
#PLOT MORTALITY RATES VS PM FOR INDIVIDUAL-LEVEL COVARIATES
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
#CREATE PLOTS FOR INDIVIDUAL SES CONFS
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
