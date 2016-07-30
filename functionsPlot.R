#create plot(line) for given Hz
createPlotHertz <- function(hertz, sampleSize = nrow(sample)){
  columnName <- paste("Hz",hertz,sep="")
  sampleTemp <- sample[seq(0,nrow(sample),nrow(sample)/sampleSize),]
  plot <- ggplot(data = sampleTemp,
                 aes(x = timeMs, y = sampleTemp[columnName])) + 
          geom_line() + 
          ggtitle(paste(hertz,"Hz - Sample size:", sampleSize)) + 
          scale_y_continuous(name="Value", labels = comma) +
          scale_x_continuous(name = "Elapsed Ms" , breaks = seq(0,600000,60000))
  return(plot)
}

#create plot(line) for given ms
createPlotMs <- function(milliSecond,sampleSize = ncol(sample)-2){
  sampleTemp <- as.data.frame(t(sample[toString(milliSecond),2:ncol(sample)]))
  sampleTemp$hertz <- as.numeric(substr(rownames(sampleTemp),3,nchar(rownames(sampleTemp))))
  names(sampleTemp) <- c("value","hertz")
  plot <- ggplot(data = sampleTemp[seq(0,nrow(sampleTemp),nrow(sampleTemp)/sampleSize),],
                 aes(x = hertz, y = value)) +
          geom_bar(stat = "identity") + 
          ggtitle(paste(milliSecond, "ms" ))  
  
  return(plot)
}

#create plot(bar) Average for given timeframe and Hz range
createPlotHzAvgBar <- function(milliSecondFrom = 1,milliSecondTo = 60000, hertzFrom = 12, hertzTo =24000 ){
  #save parameter's column index 
  hzLower<-  grep(paste("^Hz",hertzFrom,"$",sep=""), colnames(sample))-1
  hzUpper<-  grep(paste("^Hz",hertzTo,"$",sep=""), colnames(sample))-1
  
  tempDf <- sample %>%
              filter(timeMs > milliSecondFrom & timeMs < milliSecondTo) %>%
              select(-timeMs) %>% 
              select(hzLower:hzUpper)
  
  tempDf <- as.data.frame(colSums(tempDf))
  tempDf$hertz <- as.numeric(substr(rownames(tempDf),3,nchar(rownames(tempDf))))
  rownames(tempDf) <-NULL
  names(tempDf) <- c("value","hertz")

  title <- paste("Ms: ",as.numeric(milliSecondFrom)," - ",milliSecondTo,"   ","Hz: ",hertzFrom," - ",hertzTo,sep="" )
  
  plot <- ggplot(data = tempDf,
                 aes(x = hertz, y = value)) +
          geom_bar(stat = "identity") + 
          ggtitle(title) + 
          scale_y_continuous(name="Value", labels = comma) +
          scale_x_continuous(name = "Hz" , breaks = seq(0,max(tempDf$hertz),2000))
  
  return(plot)
}

#create plot(line) Average for given timeframe and Hz range
createPlotHzAvgLine <- function(milliSecondFrom = 1,milliSecondTo = 60000, hertzFrom = 12, hertzTo =24000 ){
  #save parameter's column index 
  hzLower<-  grep(paste("^Hz",hertzFrom,"$",sep=""), colnames(sample))-1
  hzUpper<-  grep(paste("^Hz",hertzTo,"$",sep=""), colnames(sample))-1
  
  tempDf <<- sample %>%
              filter(timeMs > milliSecondFrom & timeMs < milliSecondTo) %>%
              select(-timeMs) %>% 
              select(hzLower:hzUpper)
  
  tempDf <<- as.data.frame(colSums(tempDf))
  tempDf$hertz <<- as.numeric(substr(rownames(tempDf),3,nchar(rownames(tempDf))))
  rownames(tempDf) <<-NULL
  names(tempDf) <<- c("value","hertz")

  plotTitle <- paste("Ms: ",milliSecondFrom," - ",milliSecondTo,"\n","Hz: ",hertzFrom," - ",hertzTo,sep="" )
  
  plot <- ggplot(data = tempDf,
                 aes(x = hertz, y = value)) +
          geom_line() + 
          ggtitle(plotTitle) + 
          scale_y_continuous(name="Value", labels = comma) +
          scale_x_continuous(name = "Hz" , breaks = seq(0,max(tempDf$hertz),1000))
  
  return(plot)
}

#create multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}