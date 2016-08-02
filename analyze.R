# library(ggmap)
# library(plyr)
# library(dplyr)
library(stringr)
# library(tidyr)
library(ggplot2)
library(countrycode)
library(choroplethr)
library(choroplethrMaps)

# Load data
setwd("~/Google Drive/Research/Projects/Current/2016_TopSen/analysis/")
finalData <- read.csv("finalData.csv", stringsAsFactors=FALSE, encoding="UTF-8", fileEncoding="UTF-8")

# For controlling colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Set up graphical params
MaxWidth = 19.05*0.95
GoldenRatio = 1/2*(1+sqrt(5))

# Source Shilad's function
source("analyzeGroup.R")
source("analyzeOverall.R")


######################
# Summary statistics #
######################

percentFemale <- sum(finalData$probF)/nrow(finalData)
percentMale <- sum(finalData$probM)/nrow(finalData)
percentUnknown <- sum(finalData$probNA)/nrow(finalData)
percentEditor <- sum(finalData$title=="editor")/nrow(finalData)
percentManaging <- sum(finalData$title=="managing")/nrow(finalData)
percentOther <- sum(finalData$title=="other")/nrow(finalData)
numJournals <- length(unique(finalData$journal))
numPublishers <- length(unique(finalData$publisher))
percentJournalPure <- nrow(unique(subset(finalData,field=="PURE",select="journal")))/numJournals
percentJournalApplied <- nrow(unique(subset(finalData,field=="APPLIED",select="journal")))/numJournals
percentJournalBoth <- nrow(unique(subset(finalData,field=="BOTH",select="journal")))/numJournals
percentEditorPure <- nrow(subset(finalData,field=="PURE"))/nrow(finalData)
percentEditorApplied <- nrow(subset(finalData,field=="APPLIED"))/nrow(finalData)
percentEditorBoth <- nrow(subset(finalData,field=="BOTH"))/nrow(finalData)
countryData <- subset(finalData,country!="")
countryDataAmount <- nrow(countryData)
numCountries <- length(unique(countryData$country))
topCountries <- head(with(countryData,table(country)[order(-table(country))]),n=10)/countryDataAmount*100

#####################
# Gender by journal #
#####################
finalData$journal <- toupper(finalData$journal)
journalResults <- analyzeGroup(finalData,"journal",minCount = 10,"Proportion of Journal Editorships Held by Women","Number of Journals","Representation of Women by Journal","Journal","Gender Proportion","Gender Breakdown for Journals with Highest Representation of Women", maxRows=10, myBinwidth = 0.01,labelWidth=25, labelAngle=50)
ggsave(filename = "figures/fig4.pdf", plot = journalResults[[4]], width = MaxWidth, height = MaxWidth/GoldenRatio, units = "cm", dpi = 600)
ggsave(filename = "figures/fig5.pdf", plot = journalResults[[5]], width = MaxWidth, height = MaxWidth/GoldenRatio, units = "cm", dpi = 600)
temp <- journalResults[[2]]
noFemaleJournals <- subset(temp,percentFemale==0,select="journal")
noFemaleJournalsByField <- unique(subset(finalData, journal %in% noFemaleJournals$journal,select=c("journal","field")))
table(noFemaleJournalsByField$field)
topFemaleJournals <- temp[1:10,"journal"]
topJournalsByField <- unique(subset(finalData, journal %in% topFemaleJournals,select=c("journal","field")))
table(topJournalsByField$field)

#######################
# Gender by publisher #
#######################
results <- analyzeGroup(finalData,"publisher",minCount = 100,"Proportion of Publisher Editorships Held by Women","Number of Publishers","Representation of Women by Publisher","Publisher","Gender Proportion","Gender Breakdown for Publishers with Over 100 Editorships", maxRows=20, myBinwidth = 0.01,labelWidth=22, labelAngle=50)
ggsave(filename = "figures/fig6.pdf", plot = results[[4]], width = MaxWidth, height = MaxWidth/GoldenRatio, units = "cm", dpi = 600)
ggsave(filename = "figures/fig7.pdf", plot = results[[5]], width = MaxWidth, height = MaxWidth/GoldenRatio, units = "cm", dpi = 600)
results <- analyzeGroup(finalData,"publisher",minCount = 1,"Proportion of Publisher Editorships Held by Women","Number of Publishers","Representation of Women by Publisher","Publisher","Gender Proportion","Gender Breakdown for Publishers with Higest Representation of Women", maxRows=10, myBinwidth = 0.01,labelWidth=16, labelAngle=50)
# temp <- results[[2]]
# fewFemale <- subset(temp,percentFemale<=0.005,select=c("publisher","count"))
# fewFemaleJournalPub <- unique(subset(finalData, publisher %in% fewFemale$publisher,select=c("journal","publisher")))
# # table(noFemaleJournalsByField$field)
# topFemale <- temp[1:10,"publisher"]

######################
# Gender by subfield #
######################
results <- analyzeGroup(finalData,"field",minCount = 1,"Proportion Female of Publisher Editorships","Number of Publishers","Female Representation by Publisher","Publisher","Gender Proportion","Gender Breakdown for Publishers with Over 100 Editorships", maxRows=-1, myBinwidth = 0.01,labelWidth=22, labelAngle=50)

###################
# Gender by title #
###################
results <- analyzeGroup(finalData,"title",minCount = 1,"Proportion Female of Publisher Editorships","Number of Publishers","Female Representation by Publisher","Publisher","Gender Proportion","Gender Breakdown for Publishers with Over 100 Editorships", maxRows=-1, myBinwidth = 0.01,labelWidth=22, labelAngle=50)

#####################
# Gender by country #
#####################

# Remove editorships with no country
subData <- subset(finalData,!country=="")
results <- analyzeGroup(subData,"country",minCount = 1,"Proportion of Country Editorships Held by Women","Number of Countries","Representation of Women by Country","Country","Gender Proportion","Gender Breakdown for Countries with Over 200 Editorships", maxRows=30, myBinwidth = 0.01,labelWidth=22, labelAngle=50)
ggsave(filename = "figures/fig8.pdf", plot = results[[4]], width = MaxWidth, height = MaxWidth/GoldenRatio, units = "cm", dpi = 600)
data("country.map")
countrylist <- unique(country.map[,c("iso_a2","region")])
countrylist$region <- toupper(countrylist$region)
subData <- merge(subData,countrylist,by.x="country",by.y="iso_a2",all.x=TRUE,all.y=FALSE)
results <- analyzeGroup(subData,"region",minCount = 200,"Proportion of Country Editorships Held by Women","Number of Countries","Representation of Women by Country","Country","Gender Proportion","Gender Breakdown for Countries with Over 200 Editorships", maxRows=30, myBinwidth = 0.01,labelWidth=22, labelAngle=50)
ggsave(filename = "figures/fig10.pdf", plot = results[[5]], width = MaxWidth, height = MaxWidth/GoldenRatio, units = "cm", dpi = 600)
results <- analyzeGroup(finalData,"country",minCount = 1,"Proportion of Country Editorships Held by Women","Number of Countries","Representation of Women by Country","Country","Gender Proportion","Gender Breakdown for Countries with Over 200 Editorships", maxRows=30, myBinwidth = 0.01,labelWidth=22, labelAngle=50)

###########################
# Gender by impact factor #
###########################
finalData$journal <- toupper(finalData$journal)
journalResults <- analyzeGroup(finalData,"journal",minCount = 1,"Proportion Female of Journal Editorships","Number of Journals","Female Representation by Journal","Journal","Gender Proportion","Gender Breakdown for Journals with Higest Female Representation", maxRows=-1, myBinwidth = 0.01,labelWidth=25, labelAngle=50)
temp1 <- journalResults[[2]]
temp2 <- unique(subset(finalData,select=c("journal","X5YrImpactFactor")))
impactData <- merge(temp1,temp2)
impactData <- subset(impactData,!is.na(X5YrImpactFactor))
p <- ggplot(impactData, aes(percentFemale, X5YrImpactFactor))
p <- p + geom_point()
cor.test(impactData$X5YrImpactFactor,impactData$percentFemale,method="spearman")
temp <- impactData[-grep("SIAM",impactData$journal),]
cor.test(temp$X5YrImpactFactor,temp$percentFemale,method="spearman")
mybreaks <- quantile(impactData$X5YrImpactFactor,probs=seq(0,1,by=0.2),include.lowest=TRUE)
quintiles <- cut(impactData$X5YrImpactFactor,breaks=mybreaks,labels=c("Q1","Q2","Q3","Q4","Q5"))
quintiles[is.na(quintiles)] <- "Q1"
impactData$quintile <- quintiles
p <- ggplot(subset(impactData,!is.na(X5YrImpactFactor)), aes(x=quintile, y=percentFemale)) + geom_boxplot()
model <- lm(percentFemale ~ X5YrImpactFactor, data= impactData)
