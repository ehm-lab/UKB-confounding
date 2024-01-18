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
ylab <- expression(paste("Annual exposure to ",PM[2.5]," (",mu,"g/",m^3,")"))

pmdata |> 
  subset(eid %in% fulldata$eid, select=c("eid", "year", "pm25")) |> 
  na.omit() |>
  filter(year >= 2006) |>
  ggplot(aes(x=factor(year), y=pm25)) +
  geom_boxplot(fill="lightskyblue", alpha=0.8, outlier.alpha=0.8,
               outlier.size=0.5, shape=19) +
  labs(y=ylab, x="Year") +
  theme_bw() 

#SAVE PLOT
ggsave(file = "C:/Users/lsh2004062/Downloads/pmtrend.png",
       width = 3500, height =2000, units = "px")



####################################################
#PLOT MORTALITY RATE AND PM ACROSS YEARS
####################################################
ylab <- expression(paste("Mean of annual exposure to ",PM[2.5]," (",mu,"g/",m^3,")"))

pmdata |> 
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
  theme(axis.title=element_text(size=5)) +
  scale_y_continuous( ylab, 
    sec.axis = sec_axis(~ . * 1.20, 
        name = "mortality rates (x1000)")) +
  scale_x_continuous(breaks=2006:2021) +
  labs( x="Year") +
  theme_bw() 

#SAVE PLOT
ggsave(file = "C:/Users/lsh2004062/Downloads/mrpmtrend.png",
       width = 3500, height =2000, units = "px")




####################################################
#PLOT MORTALITY RATES VS PM ACROSS CENTRE
####################################################


#INCLUDING ALL CENTRES

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


#EXCLUDING LONDON AND GLASGOW
fulldata |> 
  na.omit() |>
  group_by(asscentre) |>
  summarize(pm25 = mean(pm25)) |>
  merge(mrcentre, by="asscentre") |>
  filter(!(asscentre %in% c("Barts",
                            "Croydon",
                            "Hounslow",
                            "Glasgow"))) |>
  ggplot(aes(x=rate,y=pm25, colour=age)) +
  geom_point(aes(size = pop)) +
  geom_smooth(method = "lm", aes(weight=pop), se = T) +
  geom_text_repel(aes(label = asscentre),size = 3) +
  scale_color_gradient(low="blue", high="red") +
  labs(title="Death rates Vs PM2.5 excluding London and Glasgow") +
  theme(plot.title = element_text(face="bold")) +
  coord_cartesian(xlim =c(50,90), ylim = c(5,15))+
  labs(y=bquote("average PM"[2.5]), 
       x="mortality rate (x 1000 people)")

            
            

            
