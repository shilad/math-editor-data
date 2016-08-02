require(plyr)
require(dplyr)
require(ggplot2)
require(tidyr)


analyzeOverall <- function (data, barXlabel, barYlabel, barTitle) {

  # Some printing and graphical parameters
  # For controlling colors
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }  
  

  # Create data
  overallData <- dplyr::summarize(data,
                                  count = n(),
                                  percentFemale = sum(probF)/n(),
                                  percentMale = sum(probM)/n(),
                                  percentNA = sum(probNA)/n(),
                                  rawPercentFemale = sum(gender=="female",na.rm=TRUE)/n(),
                                  rawPercentMale = sum(gender=="male",na.rm=TRUE)/n(),
                                  rawPercentNA = sum(is.na(gender))/n()
  )
  overallData <- as.data.frame(overallData)
  overallData <- gather(overallData,"Gender","Value",2:4)
  
  
  # Create histogram
  p1 <- ggplot(overallData, aes(x=Gender, y=Value, fill=Gender)) + geom_bar(stat="identity")
  p1 <- p1 + scale_fill_manual(labels=c("Female", "Male", "Undetermined"),values=gg_color_hue(3)[c(2,3,1)])
  p1 <- p1 + scale_x_discrete(labels=c("Female","Male","Undetermined"))
  p1 <- p1 + ggtitle(barTitle)
  p1 <- p1 + xlab(barXlabel) + ylab(barYlabel)
  p1 <- p1 + ylim(0,1)
  p1 <- p1 + theme(legend.position=c(0.5,0.93), legend.justification=c(0.5,0.5), legend.direction="horizontal", legend.box="horizontal")
  
  # Return
  return(p1)
  
}