## 1 feature as type
MyBoxViolinPlot_1_f <- function(vector2,   sampleType2,  colours2,   path2,   fileName2,  title2,  xLab2,  yLab2,    height2=4,   width2=4,   Ymin2=0, Ymax2=3) { 
  vector2[vector2>Ymax2] <- Ymax2
  vector2[vector2<Ymin2] <- Ymin2
  DataFrame_Local  <- data.frame(   sampleType=sampleType2,   yAxis=vector2    ) 

myTempFunction_1 <- function() {
  FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType ) ) +  
    geom_violin(  colour = NA  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.00001, position=position_dodge(width=0.9)    ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) +
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet3",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
}
tryCatch(
    myTempFunction_1(),
    error = function(err){"myTempFunction_1:00001"}
)

}  





## 2 features as type
MyBoxViolinPlot_2_f <- function(vector2,   sampleType2,  sampleType3,  colours2,   path2,   fileName2,  title2,  xLab2,  yLab2,    height2=4,   width2=4,   Ymin2=0, Ymax2=3) { 
  vector2[vector2>Ymax2] <- Ymax2
  vector2[vector2<Ymin2] <- Ymin2
  DataFrame_Local  <- data.frame(   sampleType=sampleType2,   yAxis=vector2,  sampleTypeB=sampleType3   ) 

myTempFunction_5 <- function() {
  FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(  colour = NA  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.00001, position=position_dodge(width=0.9)    ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) +
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
}
tryCatch(
    myTempFunction_5(),
    error = function(err){"myTempFunction_5:00005"}
)
    
myTempFunction_6 <- function() {
  FigureTemp4b <- ggplot( DataFrame_Local, aes(x=sampleTypeB, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(  colour = NA  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.001   ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) +
    facet_grid(.~sampleType)+
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4b,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2)
}
tryCatch(
    myTempFunction_6(),
    error = function(err){"myTempFunction_6:00006"}
)
    

 
}  





## 3 features as type
MyBoxViolinPlot_3_f <- function(vector2,   sampleType2,  sampleType3, sampleType4,  path2,   fileName2,  title2,  xLab2,  yLab2,    height2=4,   width2=4,   Ymin2=0, Ymax2=3) { 
  vector2[vector2>Ymax2] <- Ymax2
  vector2[vector2<Ymin2] <- Ymin2
  DataFrame_Local  <- data.frame(   sampleType=sampleType2,   yAxis=vector2,  sampleTypeB=sampleType3 , sampleTypeC=sampleType4   ) 


myTempFunction_1 <- function() { 
  FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(  colour = NA  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.001, position=position_dodge(width=0.9)   ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
}
tryCatch(
    myTempFunction_1(),
    error = function(err){"myTempFunction_1:00001"}
)
  
myTempFunction_1 <- function() { 
  FigureTemp4b <- ggplot( DataFrame_Local, aes(x=sampleTypeC, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(  colour = NA  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.001, position=position_dodge(width=0.9)     ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    facet_grid(.~sampleType)+
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4b,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2)
}
tryCatch(
    myTempFunction_1(),
    error = function(err){"myTempFunction_1:00001"}
)
 

}  





