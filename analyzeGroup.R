require(plyr)
require(dplyr)
require(ggplot2)
require(tidyr)


analyzeGroup <- function (data, colName, minCount, histXlabel, histYlabel, histTitle, barXlabel, barYlabel, barTitle, maxRows = -1, myBinwidth, labelWidth, labelAngle) {
  printf <- function(...) cat(sprintf(...))
  
  # Some printing and graphical parameters
  # For controlling colors
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }  
  
  printf("============================= RESULTS FOR %s =============================\n", colName)
  
  # Create data
  byGroup <- group_by_(data, colName) %>%
    dplyr::summarize(
      count = n(),
      percentFemale = sum(probF)/n(),
      percentMale = sum(probM)/n(),
      percentNA = sum(probNA)/n(),
      rawPercentFemale = sum(gender=="female",na.rm=TRUE)/n(),
      rawPercentMale = sum(gender=="male",na.rm=TRUE)/n(),
      rawPercentNA = sum(is.na(gender))/n()
    )
  byGroup <- arrange(byGroup,percentFemale)
  byGroup <- as.data.frame(byGroup)
  meanDiff <- mean(abs(byGroup$percentFemale - byGroup$rawPercentFemale))
  
  printf("\nMean absolute difference between corrected and raw female proportions is %f\n\n", meanDiff)
  
  results <- byGroup %>% 
    filter(count >= minCount) %>% 
    arrange(desc(percentFemale)) %>% 
    select_(colName, "count", "percentFemale", "percentMale", "percentNA")
  
  printf("Corrected estimates of gender by %s (min count = %d)\n\n", colName, minCount)
  print(results)
  
  significance <- data.frame(journal=character(), 
                             count=integer(), 
                             percentFemale=double(), 
                             p_value=character(), 
                             stringsAsFactors = FALSE)
  overall <- c(sum(data$probF), sum(data$probM, data$probNA))
  for (i in 1:nrow(results)) {
    row <- results[i,]
    forGroup <- c(row$percentFemale, row$percentMale + row$percentNA) * row$count
    m <- matrix(as.integer(round(t(cbind(overall, forGroup)))), nrow=2, ncol=2)
    m[1,] <- m[1,] - m[2,]
    x <- chisq.test(m)
    pv <- p.adjust(x$p.value, "holm", n=nrow(results))
    if (pv < 0.001) {
      p <- "***"
    } else if (pv < 0.01) {
      p <- "**"
    } else if (pv < 0.05) {
      p <- "*"
    } else if (pv < 0.10) {
      p <- "+"
    } else {
      p <- ""
    }
    p <- pv
    newRow <-  data.frame(journal=row[,colName], 
                          count=row$count, 
                          percentFemale=row$percentFemale, 
                          p_value=p, 
                          stringsAsFactors = FALSE)
    significance <- rbind(significance, newRow)
  }
  
  
  printf("\n\nAnalysis of significance above, p-values are in comparison to overall gender distribution:\n\n")
  printf("+ for p < 0.1;   * for p < 0.05;   ** for p < 0.01;   *** for p < 0.001\n\n")
  print(significance)
  
  # Create histogram
  p1 <- ggplot(byGroup, aes(x=percentFemale)) + geom_histogram(fill=gg_color_hue(3)[c(2)], binwidth = myBinwidth)
  p1 <- p1 + geom_vline(xintercept=median(byGroup$percentFemale), linetype = "longdash")
  p1 <- p1 + xlab(histXlabel) + ylab(histYlabel)
  p1 <- p1 + theme(axis.text.x = element_text(size=9), axis.text.y = element_text(size=9), axis.title.x = element_text(size=11), axis.title.y = element_text(size=11), plot.title = element_text(size=11))
  p1 <- p1 + ggtitle(histTitle)
  
  # Create bar chart
  barData <- results
  if (maxRows > 0 && nrow(barData) > maxRows) barData <- barData[1:maxRows,]
  barData[,colName] <- as.factor(barData[,colName])
  barData[,colName] <- factor(barData[,colName], levels = barData[,colName])
  barData <- gather(barData,"Gender","Value",3:5)
  barData <- ddply(barData, colName, transform, label_y=cumsum(Value))
  barData$x <- barData[,colName]
  p2 <- ggplot(barData, aes(x=x, y=Value, fill=Gender)) + geom_bar(stat="identity")
  p2 <- p2 + geom_text(data=subset(barData,Gender=="percentNA"),aes(y=label_y+0.03,label=count))
  p2 <- p2 + geom_text(data=subset(barData,Gender=="percentFemale"),size=3,aes(label=paste(format(round(Value*100,1), nsmall=1), "%")))
  p2 <- p2 + scale_x_discrete(labels = function(x) str_wrap(x,width = labelWidth))
  p2 <- p2 + theme(axis.text.x = element_text(angle=labelAngle, hjust=1, vjust=1))
  p2 <- p2 + ggtitle(barTitle)
  p2 <- p2 + xlab(barXlabel) + ylab(barYlabel)
  p2 <- p2 + theme(axis.text.x = element_text(size=9), axis.text.y = element_text(size=9), axis.title.x = element_text(size=11), axis.title.y = element_text(size=11), plot.title = element_text(size=11))
  p2 <- p2 + scale_fill_manual(labels=c("Women", "Men", "Undetermined"),values=gg_color_hue(3)[c(2,3,1)])
  p2 <- p2 + theme(legend.position=c(0.5,0.5), legend.justification=c(0.5,0), legend.direction="horizontal", legend.box="horizontal")
  
  
  # Return
  return(list(byGroup=byGroup,results=results,significance=significance,histogram=p1,barchart=p2))
  
}