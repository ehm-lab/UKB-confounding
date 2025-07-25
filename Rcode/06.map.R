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
# MAP OF ASSESSMENT CENTRES
################################################################################

# DOWNLOAD ASSESSMENT CENTRE INFO AND UK SHAPEFILES (IF NEEDED)
files <- c("asscentre.csv", "Countries_December_2022_GB_BUC.zip")
for(x in files) if(! x %in% list.files("data"))
  download_zenodo("10.5281/zenodo.13983169", path="data", files=x)

# LOAD COUNTRIES SHAPEFILE
unzip(zipfile="data/Countries_December_2022_GB_BUC.zip", exdir=getwd())
countryshp <- st_read("CTRY_DEC_2022_GB_BUC.shp")[2]
file.remove(list.files()[grep("CTRY_DEC_2022", list.files(), fixed=T)])

# LOAD ASSESSMENT CENTRE DATA
asscentreloc <- read.csv("data/asscentre.csv")

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
