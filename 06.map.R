################################################################################
# CONFOUNDING ISSUES IN AIR POLLUTION EPIDEMIOLOGY
################################################################################

################################################################################
# MAP OF ASSESSMENT CENTRES
################################################################################

# LOAD COUNTRIES SHAPEFILE
file.copy(paste0("V:/VolumeQ/AGteam/ONS/geography/shapefiles", 
  "/COUNTRY_UK/Countries_December_2022_GB_BUC.zip"), getwd())
unzip(zipfile="Countries_December_2022_GB_BUC.zip", exdir=getwd())
countryshp <- st_read("CTRY_DEC_2022_GB_BUC.shp")[2]
file.remove(list.files()[grep("CTRY_DEC_2022", list.files(), fixed=T)])
file.remove(list.files()[grep("Countries_December_2022", list.files(), fixed=T)])

# LOAD ASSESSMENT CENTRE DATA
asscentreloc <- read.csv(paste0("V:/VolumeQ/AGteam/UKBiobank/data/original/",
  "asscentre.csv"))

# MAP
mapasscentre <- ggplot(data=countryshp) + 
  geom_sf(fill=grey(0.9), col=1) +
  geom_point(aes(x=gridx*100, y=gridy*100), data=asscentreloc) +
  geom_label_repel(aes(x=gridx*100, y=gridy*100, label=short_name), 
    max.overlaps=20, size=3, data=asscentreloc) +
  ylim(NA, 1040000) + labs(x="", y="") +
  theme_bw() 

# SAVE AS SINGLE PLOT
ggsave(file="figures/mapasscentre.png", mapasscentre, width=10, height=15)
