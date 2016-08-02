library(ggmap)
#library(plyr)
library(dplyr)
library(ggplot2)
library(countrycode)
library(rgdal)
#library(raster)
library(ggthemes)

# Set up graphical params
MaxWidth = 19.05*0.95

# Load data
setwd("~/Google Drive/Synced/2016_TopSen/analysis/")

source("analyzeGroup.R")

finalData <- read.csv("finalData.csv", stringsAsFactors=FALSE, encoding="UTF-8", fileEncoding="UTF-8")

countryResults <- analyzeGroup(finalData,"country",20,"Proportion Female Across All Editors from Each Country","Number of Countries","Female Representation by Country","Country","Gender Proportion","Gender Breakdown for High-Editor Countries",15)

geoData <- countryResults$results %>%
  dplyr::select(country, percentFemale) %>%
  rename(country.iso2c = country)

geoData$country.name <- countrycode(geoData$country.iso2c, "iso2c", "country.name")
geoData$country.region <- countrycode(geoData$country.iso2c, "iso2c", "region")

world <- map_data('world')
world$country.iso2c <- countrycode(world$region, "country.name", "iso2c")

choropleth <- world %>% left_join(geoData, c("country.iso2c" = "country.iso2c")) %>% filter(country.iso2c != "AQ")
choropleth$female_d <- cut(choropleth$percentFemale, breaks = c(0.0, 0.025, 0.05, 0.075, 0.100, 0.125, 0.150, 0.175, 0.200, 0.225))
p <- ggplot(choropleth, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = female_d), colour = alpha("#333333", 1/2), size = 0.2) +
  coord_proj("+proj=wintri +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  scale_fill_brewer(palette = "YlGn", na.value = "#dddddd", name="Proportion Female") +
  theme_map() + 
  theme(plot.title = element_text(size=11)) +
  ggtitle("Proportion of Editorships in Country That Are Female")

ggsave(filename = "figures/worldMap.pdf", plot = p, width = MaxWidth, height = MaxWidth / 1.544803, units = "cm", dpi = 300)

parliamentCorr <- geoData %>%
  dplyr::select(country.iso2c, percentFemale) %>%
  rename(iso2c = country.iso2c) %>%
  join(WDI(indicator = "SG.GEN.PARL.ZS", start=2011, end=2011))

cor(parliamentCorr$percentFemale, 
    parliamentCorr$SG.GEN.PARL.ZS,
    use="complete.obs",
    method="spearman")

ggplot(parliamentCorr,  aes(x = percentFemale, y = SG.GEN.PARL.ZS, label = country)) + 
  geom_label() + 
  labs(x = "Percent Female Editorships", 
       y = "Percent Parliamentarians Who Are Female",
       title = "Female editorships vs Female parliamentarians for each country")

