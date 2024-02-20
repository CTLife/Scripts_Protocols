
##############################################################################################################################################################################################
library(stringr)
library(ggplot2)


outDir_g     <-   "9.colocalization_peak-leve"
 
if( ! file.exists(outDir_g)   ) { dir.create(outDir_g,   recursive = TRUE) }
##############################################################################################################################################################################################





##############################################################################################################################################################################################
suppressPackageStartupMessages( library(ROTS) )
suppressPackageStartupMessages( library(GenomicRanges) )
suppressPackageStartupMessages( library(rtracklayer) )
suppressPackageStartupMessages( library(Rsubread) )
suppressPackageStartupMessages( library(Rsamtools) )
suppressPackageStartupMessages( library(ade4) )
suppressPackageStartupMessages( library(made4) )
suppressPackageStartupMessages( library(edgeR) )
suppressPackageStartupMessages( library(EDASeq) )
suppressPackageStartupMessages( library(RUVSeq) )
suppressPackageStartupMessages( library(RColorBrewer) )
suppressPackageStartupMessages( library(ggplot2) )
suppressPackageStartupMessages( library(stringr) )
suppressPackageStartupMessages( library(ggplot2)  )
suppressPackageStartupMessages( library(randomcoloR) )
suppressPackageStartupMessages( library(RColorBrewer) )
suppressPackageStartupMessages( library(stringr) )
suppressPackageStartupMessages( library(dendextend) )
suppressPackageStartupMessages( library(factoextra) ) 
suppressPackageStartupMessages( library(MASS) )
suppressPackageStartupMessages( library(ggplot2)  )
suppressPackageStartupMessages( library(scales) )
suppressPackageStartupMessages( library(corrplot) )
suppressPackageStartupMessages( library(gplots) )
suppressPackageStartupMessages( library(Hmisc) )


MyTheme_1_g <- function(textSize1=14, hjust1=NULL, vjust1=NULL,  angle1=NULL) {    # "hjust=1, vjust=1, angle=30" for some boxplots.
  ggplot2::theme(  
    line  = element_line(colour="black",  linewidth=1.0,   linetype=1,      lineend=NULL),                                                                                        
    rect  = element_rect(colour="black",  linewidth=1.0,   linetype=1,      fill="transparent" ),                                                                                 
    text  = element_text(family="serif",  face="plain",  colour="black",  size=textSize1, hjust=0.5, vjust=0.5,   angle=0, lineheight=1.0,  margin = NULL, debug = NULL),     
    title = element_text(family="serif",  face="plain",  colour="black",  size=textSize1, hjust=0.5, vjust=0.5,   angle=0, lineheight=1.0,  margin = NULL, debug = NULL),    
    ## aspect.ratio = 1,       
    axis.title    = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=0,       lineheight=1.0,  margin = NULL, debug = NULL),        
    axis.title.x  = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=0,       lineheight=1.0,  margin = NULL, debug = NULL),        
    axis.title.y  = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=90,      lineheight=1.0,  margin = NULL, debug = NULL),        
    axis.text     = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=0,       lineheight=1.0,  margin = NULL, debug = NULL),                                                          
    axis.text.x   = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=hjust1, vjust=vjust1, angle=angle1,  lineheight=1.0,  margin = NULL, debug = NULL),        
    axis.text.y   = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=0,       lineheight=1.0,  margin = NULL, debug = NULL),      
    axis.ticks        = element_line(colour="black", linewidth=0.5, linetype=1, lineend=NULL),          ## tick marks along axes (element_line; inherits from line). 
    axis.ticks.x      = element_line(colour="black", linewidth=0.5, linetype=1, lineend=NULL),          ## x axis tick marks (element_line; inherits from axis.ticks)
    axis.ticks.y      = element_line(colour="black", linewidth=0.5, linetype=1, lineend=NULL),          ## y axis tick marks (element_line; inherits from axis.ticks)
    axis.ticks.length = grid::unit(2.0,   "mm",   data=NULL),                                      ## length of tick marks (unit), ‘"mm"’ Millimetres.  10 mm = 1 cm. 
    axis.line         = element_line(colour="transparent", linewidth=0.3, linetype=1, lineend=NULL),    ## lines along axes (element_line; inherits from line). 
    axis.line.x       = element_line(colour="transparent", linewidth=0.3, linetype=1, lineend=NULL),    ## line along x axis (element_line; inherits from axis.line)
    axis.line.y       = element_line(colour="transparent", linewidth=0.3, linetype=1, lineend=NULL),	   ## line along y axis (element_line; inherits from axis.line)    
    legend.background    = element_rect(colour="transparent", linewidth=1, linetype=1, fill="transparent" ), 	## background of legend (element_rect; inherits from rect)
    legend.spacing       = grid::unit(1, "mm", data=NULL), 	                                                ## extra space added around legend (unit). 
    legend.key           = element_rect(colour="transparent", linewidth=2, linetype=1, fill="transparent" ), 	## background underneath legend keys. 
    legend.key.size      = grid::unit(6,   "mm", data=NULL) , 	                                                ## size of legend keys   (unit; inherits from legend.key.size)
    legend.key.height    = grid::unit(6.5, "mm", data=NULL) , 	                                                ## key background height (unit; inherits from legend.key.size)
    legend.key.width     = grid::unit(8,   "mm", data=NULL) ,                                                   ## key background width  (unit; inherits from legend.key.size)
    legend.text          = element_text(family="serif", face=NULL, colour="black", size=textSize1, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	##legend item labels. 
    legend.text.align    = 0, 	                    ## alignment of legend labels (number from 0 (left) to 1 (right))
    legend.title         = element_blank(),   	    ## title of legend (element_text; inherits from title)
    legend.title.align   = 0, 	                    ## alignment of legend title (number from 0 (left) to 1 (right))
    legend.position      = "right", 	            ## the position of legends. ("left", "right", "bottom", "top", or two-element numeric vector)
    legend.direction     = "vertical",        	    ## layout of items in legends  ("horizontal" or "vertical")    
    legend.justification = "center",      	    ## anchor point for positioning legend inside plot ("center" or two-element numeric vector)   
    legend.box           = NULL, 	            ## arrangement of multiple legends ("horizontal" or "vertical")  
    legend.box.just      = NULL, 	            ## justification of each legend within the overall bounding box, when there are multiple legends ("top", "bottom", "left", or "right")       
    panel.background   = element_rect(colour="transparent", linewidt=0.0, linetype=1, fill="transparent" ),     ## background of plotting area, drawn underneath plot (element_rect; inherits from rect)
    panel.border       = element_rect(colour="black", linewidt=0.5, linetype=1, fill=NA ), 	                    ## border around plotting area, drawn on top of plot so that it covers tick marks and grid lines. This should be used with fill=NA (element_rect; inherits from rect)                                     
    panel.spacing      = grid::unit(1, "mm", data=NULL) , 	                                            ## margin around facet panels (unit)   
    panel.spacing.x    = grid::unit(1, "mm", data=NULL) ,
    panel.spacing.y    = grid::unit(1, "mm", data=NULL) ,
    panel.grid         = element_blank(), 	                                                            ## grid lines (element_line; inherits from line)   
    panel.grid.major   = element_line(colour="transparent", linewidth=NULL, linetype=NULL, lineend=NULL) , 	    ## major grid lines (element_line; inherits from panel.grid)  
    panel.grid.minor   = element_line(colour="transparent", linewidth=NULL, linetype=NULL, lineend=NULL) ,       ## minor grid lines (element_line; inherits from panel.grid)   
    panel.grid.major.x = element_line(colour="transparent", linewidth=NULL, linetype=NULL, lineend=NULL) , 	    ## vertical major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.major.y = element_line(colour="transparent", linewidth=NULL, linetype=NULL, lineend=NULL) ,       ## horizontal major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.minor.x = element_line(colour="transparent", linewidth=NULL, linetype=NULL, lineend=NULL) ,       ## vertical minor grid lines (element_line; inherits from panel.grid.minor)
    panel.grid.minor.y = element_line(colour="transparent", linewidth=NULL, linetype=NULL, lineend=NULL) ,       ## horizontal minor grid lines (element_line; inherits from panel.grid.minor)    
    plot.background  = element_rect(colour="transparent", linewidth=NULL, linetype=NULL, fill="transparent" ),                                            ## background of the entire plot (element_rect; inherits from rect)   
    plot.title       = element_text(family="serif", face=NULL, colour="black", size=textSize1, hjust=0.5, vjust=0.5,   angle=NULL, lineheight=NULL),     ## plot title (text appearance) (element_text; inherits from title)   
    plot.margin      = grid::unit(c(5, 5, 5, 5), "mm", data=NULL), 	                                                                                ## margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)    
    strip.background = element_rect(colour=NULL,    linewidth=NULL, linetype=NULL, fill=NULL ), 	                                                      ## background of facet labels (element_rect; inherits from rect)   
    strip.text       = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	      ## facet labels (element_text; inherits from text)
    strip.text.x     = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	      ## facet labels along horizontal direction (element_text; inherits from strip.text)
    strip.text.y     = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL)   	      ## facet labels along vertical direction (element_text; inherits from strip.text) 
  ) 
} 


MySaveGgplot2_1_g <- function(ggplot2Figure1,  path1, fileName1,  height1, width1) {
  SVG1 <- paste(path1,  "/",  "SVG",  sep = "",  collapse = NULL)
  PDF1 <- paste(path1,  "/",  "PDF",  sep = "",  collapse = NULL)
  #EPS1 <- paste(path1,  "/",  "EPS",  sep = "",  collapse = NULL)
  if( ! file.exists(SVG1) ) { dir.create(SVG1) }
  if( ! file.exists(PDF1) ) { dir.create(PDF1) }
  #if( ! file.exists(EPS1) ) { dir.create(EPS1) }
  ggplot2::ggsave(filename=paste(SVG1, "/", fileName1, ".svg", sep=""),  plot = last_plot(), device = "svg",   path = NULL, scale = 1, width = width1, height = height1, units = "in", dpi = 3000, limitsize = FALSE)
  ggplot2::ggsave(filename=paste(PDF1, "/", fileName1, ".pdf", sep=""),  plot = last_plot(), device = "pdf",   path = NULL, scale = 1, width = width1, height = height1, units = "in", dpi = 3000, limitsize = FALSE)
  #ggplot2::ggsave(filename=paste(EPS1, "/", fileName1, ".eps", sep=""),  plot = last_plot(), device =cairo_ps, path = NULL, scale = 1, width = width1, height = height1, units = "in", dpi = 3000, limitsize = FALSE)
}


## df contains two columns, the first column (cond_col=1) is sample type, the second column (val_col=2) is value. (must be).
whisk_1_g <- function(df, cond_col=1, val_col=2) {  
  require(reshape2)
  condname <- names(df)[cond_col]  ## save the name of the first column.
  names(df)[cond_col] <- "cond" 
  names(df)[val_col]  <- "value"
  b   <- boxplot(value~cond, data=df, plot=FALSE)   
  df2 <- cbind(as.data.frame(b$stats), c("min","lq","m","uq","max"))
  names(df2) <- c(levels(df$cond), "pos")
  df2 <- reshape2::melt(df2, id="pos", variable.name="cond")
  df2 <- reshape2::dcast(df2, cond~pos)   
  names(df2)[1] <- condname 
  print(df2)
  df2
}


MyBoxViolinPlot_1_f <- function(vector2,   sampleType2,  path2,   fileName2,  title2,  xLab2,  yLab2,    height2=4,   width2=4,   Ymin2=0, Ymax2=3) { 
  vector2[vector2>Ymax2] <- Ymax2
  vector2[vector2<Ymin2] <- Ymin2
  DataFrame_Local  <- data.frame(   sampleType=sampleType2,   yAxis=vector2    ) 
  
  FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis ) ) +  
    geom_violin(  colour = "red", fill="red"  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.00001, position=position_dodge(width=0.9)    ) +   
    stat_summary( position=position_dodge(width=0.9), fun=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  
  myTempFunction_1 <- function() {
    FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType ) ) +  
      geom_violin(  colour = NA  ) + 
      geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.00001, position=position_dodge(width=0.9)    ) +   
      stat_summary( position=position_dodge(width=0.9), fun=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
      xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
    MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet2",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
  }
  tryCatch(
    myTempFunction_1(),
    error = function(err){"myTempFunction_1:00001"}
  )
  
  myTempFunction_1 <- function() {
    FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis ) ) +  
      geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.00001, position=position_dodge(width=0.9)    ) +   
      stat_summary( position=position_dodge(width=0.9), fun=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
      xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
    MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_BoxPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
  }
  tryCatch(
    myTempFunction_1(),
    error = function(err){"myTempFunction_1:00001"}
  )
  
}  


MyBoxViolinPlot_1_f_each <- function(vector2,   sampleType2,  path2,   fileName2,  title2,  xLab2,  yLab2,    height2=4,   width2=4,   Ymin2=0, Ymax2=3) { 
  vector2[vector2>Ymax2] <- Ymax2
  vector2[vector2<Ymin2] <- Ymin2
  DataFrame_Local  <- data.frame(   sampleType=sampleType2,   yAxis=vector2    ) 
  
  FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis ) ) +  
    geom_violin(  colour = "red", fill="red"  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0   ) +   
    stat_summary(  fun=mean,  color="yellow4",  geom="point", shape=19,  show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  myTempFunction_1 <- function() {
    FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis ) ) +  
      geom_boxplot( outlier.shape=NA, outlier.size=0    ) +   
      stat_summary(   fun=mean,  color="yellow4",  geom="point", shape=19,   show.legend = FALSE) + 
      xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
    MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_BoxPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
  }
  tryCatch(
    myTempFunction_1(),
    error = function(err){"myTempFunction_1:00001"}
  )
  
}  


## 3 features as type
MyBoxViolinPlot_3_f <- function(vector2,   sampleType3, sampleType4,  path2,   fileName2,  title2,  xLab2,  yLab2,    height2=4,   width2=4,   Ymin2=0, Ymax2=3) { 
  vector2[vector2>Ymax2] <- Ymax2
  vector2[vector2<Ymin2] <- Ymin2
  DataFrame_Local  <- data.frame(   yAxis=vector2,  sampleTypeB=sampleType3 , sampleTypeC=sampleType4   ) 
  
  
  myTempFunction_1 <- function() { 
    FigureTemp2a <- ggplot( DataFrame_Local, aes(x=sampleTypeC, y=yAxis, fill=sampleTypeB) ) +  
      geom_boxplot( outlier.shape=NA, outlier.size=0, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
      stat_summary( position=position_dodge(width=0.75), fun=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
      xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
    MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp2a,  path1=path2, fileName1=paste(fileName2, "_boxPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  }
  tryCatch(
    myTempFunction_1(),
    error = function(err){"myTempFunction_1:00001"}
  )
  
  
  
  myTempFunction_1 <- function() { 
    FigureTemp4b <- ggplot( DataFrame_Local, aes(x=sampleTypeC, y=yAxis , fill=sampleTypeB) ) +  
      geom_violin(  colour = NA  ) + 
      geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.001, position=position_dodge(width=0.9)     ) +   
      stat_summary( position=position_dodge(width=0.9), fun=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
      xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
    MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4b,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  }
  tryCatch(
    myTempFunction_1(),
    error = function(err){"myTempFunction_1:00001"}
  )
}  


## 3 features as type
MyBoxViolinPlot_3A_f <- function(vector2,   sampleType3, sampleType4, level_order=NA, path2,   fileName2,  title2,  xLab2,  yLab2,    height2=4,   width2=4,   Ymin2=0, Ymax2=3) { 
  vector2[vector2>Ymax2] <- Ymax2
  vector2[vector2<Ymin2] <- Ymin2
  DataFrame_Local  <- data.frame(   yAxis=vector2,  sampleTypeB=sampleType3 , sampleTypeC=sampleType4   ) 
  
  
  myTempFunction_1 <- function() { 
    FigureTemp2a <- ggplot( DataFrame_Local, aes(x=factor(sampleTypeC, level=level_order ), y=yAxis, fill=sampleTypeB) ) +  
      geom_boxplot( outlier.shape=NA, outlier.size=0, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
      stat_summary( position=position_dodge(width=0.75), fun=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
      xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
    MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp2a,  path1=path2, fileName1=paste(fileName2, "_boxPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  }
  tryCatch(
    myTempFunction_1(),
    error = function(err){"myTempFunction_1:00001"}
  )
  
  
  
  myTempFunction_1 <- function() { 
    FigureTemp4b <- ggplot( DataFrame_Local, aes(x=factor(sampleTypeC, level=level_order ), y=yAxis , fill=sampleTypeB) ) +  
      geom_violin(  colour = NA  ) + 
      geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.001, position=position_dodge(width=0.9)     ) +   
      stat_summary( position=position_dodge(width=0.9), fun=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
      xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
    MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4b,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  }
  tryCatch(
    myTempFunction_1(),
    error = function(err){"myTempFunction_1:00001"}
  )
}  


## compare  probability  density
MyDensity_1_f <- function(vector2, sampleType2,  colours2,  path2,   fileName2,  title2,  xLab2, height2=4,  width2=4,  xMin2=0,  xMax2=1.5,   yMin2=0,  yMax2=10) {
  vector2[vector2>xMax2] <- xMax2
  vector2[vector2<xMin2] <- xMin2
  dataframeB  <- data.frame( xAxis = vector2,  sampleType=sampleType2 )
  
  FigureTemp1 <- { ggplot(data=dataframeB, aes(x=xAxis, fill=sampleType, colour=sampleType) )   +  xlab(xLab2) + ylab("Probability density") +  ggtitle(title2)  +  
      geom_density(mapping = NULL, data = NULL, stat = "density", position = "identity", na.rm = FALSE, alpha=0.3  ) +
      scale_colour_manual( values=colours2   ) + scale_fill_manual( values = colours2) +  ylim(yMin2, yMax2) + 
      scale_x_continuous(limits=c(xMin2, xMax2)  ) +  
      #geom_hline(yintercept=-0.01, lty=1, col="white", size=0.6) +
      MyTheme_1_g( hjust1=NULL, vjust1=NULL,  angle1=NULL,   textSize=14 )    +  guides( colour = guide_legend(override.aes = list(size=0, shape=1)) ) }
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "-density-limitY",      sep="",  collapse=NULL),  height1=height2,  width1=width2)
  
  FigureTemp2 <- { ggplot(data=dataframeB, aes(x=xAxis, fill=sampleType, colour=sampleType) )   +  xlab(xLab2) + ylab("Probability density") +  ggtitle(title2)  +  
      geom_density(mapping = NULL, data = NULL, stat = "density", position = "identity", na.rm = FALSE, alpha=0.3  ) +
      scale_colour_manual( values=colours2   ) + scale_fill_manual( values = colours2) +  
      scale_x_continuous(limits=c(xMin2, xMax2)  ) +  
      #geom_hline(yintercept=-0.01, lty=1, col="white", size=0.6) +
      MyTheme_1_g( hjust1=NULL, vjust1=NULL,  angle1=NULL,   textSize=14 )    +  guides( colour = guide_legend(override.aes = list(size=0, shape=1)) ) }
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp2,  path1=path2, fileName1=paste(fileName2, "-density",             sep="",  collapse=NULL),  height1=height2,  width1=width2)
  
  FigureTemp4 <- { ggplot(data=dataframeB, aes(x=xAxis, fill=sampleType, colour=sampleType) )   +  xlab(xLab2) + ylab("Probability density") +  ggtitle(title2)  +  
      geom_density(mapping = NULL, data = NULL, stat = "density", position = "identity", na.rm = FALSE, alpha=0.0  ) +
      scale_colour_manual( values=colours2   ) + scale_fill_manual( values = colours2) +  
      scale_x_continuous(limits=c(xMin2, xMax2)  ) +  
      #geom_hline(yintercept=-0.01, lty=1, col="white", size=0.6) +
      MyTheme_1_g( hjust1=NULL, vjust1=NULL,  angle1=NULL,   textSize=14 )   +  guides( colour = guide_legend(override.aes = list(size=1.5, shape=1)) ) }
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4,  path1=path2, fileName1=paste(fileName2, "-density2",            sep="",  collapse=NULL),  height1=height2,  width1=width2)
  
  FigureTemp5 <- { ggplot(data=dataframeB, aes(x=xAxis, fill=sampleType, colour=sampleType) )   +  xlab(xLab2) + ylab("Probability density") +  ggtitle(title2)  +  
      geom_line(stat="density", alpha=1.0 ) +
      scale_colour_manual( values=colours2   ) + scale_fill_manual( values = colours2) +  ylim(yMin2, yMax2) + 
      scale_x_continuous(limits=c(xMin2, xMax2)  ) +  
      #geom_hline(yintercept=-0.01, lty=1, col="white", size=0.6) +
      MyTheme_1_g( hjust1=NULL, vjust1=NULL,  angle1=NULL,   textSize=14 )    +  guides( colour = guide_legend(override.aes = list(size=1.5, shape=1)) ) }
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp5,  path1=path2, fileName1=paste(fileName2, "-density3-limitY",     sep="",  collapse=NULL),  height1=height2,  width1=width2)
  
  FigureTemp6 <- { ggplot(data=dataframeB, aes(x=xAxis, fill=sampleType, colour=sampleType) )   +  xlab(xLab2) + ylab("Probability density") +  ggtitle(title2)  +  
      geom_line(stat="density", alpha=1.0 ) +
      scale_colour_manual( values=colours2   ) + scale_fill_manual( values = colours2) +  
      scale_x_continuous(limits=c(xMin2, xMax2)  ) +  
      #geom_hline(yintercept=-0.01, lty=1, col="white", size=0.6) +
      MyTheme_1_g( hjust1=NULL, vjust1=NULL,  angle1=NULL,   textSize=14 )   +  guides( colour = guide_legend(override.aes = list(size=1.5, shape=1)) ) }
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp6,  path1=path2, fileName1=paste(fileName2, "-density3",            sep="",  collapse=NULL),  height1=height2,  width1=width2)
}


##############################################################################################################################################################################################





##############################################################################################################################################################################################
 
rawDF1 <- read.table(  "1_BA9/1.2/1_BA9.nominals.txt",    header=F,   sep=" ", comment.char = "", quote ="" )    
rawDF2 <- read.table(  "2_BA24/1.4/2_BA24.nominals.txt",  header=F,   sep=" ", comment.char = "", quote ="" )    
rawDF3 <- read.table(  "3_C/1.3/3_C.nominals.txt" ,       header=F,   sep=" ", comment.char = "", quote ="" )    
rawDF4 <- read.table(  "4_H/1.9/4_H.nominals.txt" ,       header=F,   sep=" ", comment.char = "", quote ="" )    
rawDF5 <- read.table(  "5_T/1.2/5_T.nominals.txt" ,       header=F,   sep=" ", comment.char = "", quote ="" )    
dim(rawDF1)
dim(rawDF2)
dim(rawDF3)
dim(rawDF4)
dim(rawDF5)


finalDF1 <- read.table(  "8_full_info_m6A-QTLs/Final.1_BA9.full_info.txt",   header=TRUE,   sep="\t", comment.char = "", quote ="" )    
finalDF2 <- read.table(  "8_full_info_m6A-QTLs/Final.2_BA24.full_info.txt",  header=TRUE,   sep="\t", comment.char = "", quote ="" )    
finalDF3 <- read.table(  "8_full_info_m6A-QTLs/Final.3_C.full_info.txt" ,    header=TRUE,   sep="\t", comment.char = "", quote ="" )    
finalDF4 <- read.table(  "8_full_info_m6A-QTLs/Final.4_H.full_info.txt" ,    header=TRUE,   sep="\t", comment.char = "", quote ="" )    
finalDF5 <- read.table(  "8_full_info_m6A-QTLs/Final.5_T.full_info.txt" ,    header=TRUE,   sep="\t", comment.char = "", quote ="" )    
dim(finalDF1)
dim(finalDF2)
dim(finalDF3)
dim(finalDF4)
dim(finalDF5)





rawDF1[1:5, ]

raw_genes_1 <- as.vector( rawDF1[ , 1] )
raw_SNPs_1  <- as.vector( rawDF1[ , 8] )
raw_p_1     <- as.vector( rawDF1[ , 12] )
raw_d_1     <- as.vector( rawDF1[ , 7] )

raw_genes_2 <- as.vector( rawDF2[ , 1] )
raw_SNPs_2  <- as.vector( rawDF2[ , 8] )
raw_p_2     <- as.vector( rawDF2[ , 12] )
raw_d_2     <- as.vector( rawDF2[ , 7] )

raw_genes_3 <- as.vector( rawDF3[ , 1] )
raw_SNPs_3  <- as.vector( rawDF3[ , 8] )
raw_p_3     <- as.vector( rawDF3[ , 12] )
raw_d_3     <- as.vector( rawDF3[ , 7] )

raw_genes_4 <- as.vector( rawDF4[ , 1] )
raw_SNPs_4  <- as.vector( rawDF4[ , 8] )
raw_p_4     <- as.vector( rawDF4[ , 12] )
raw_d_4     <- as.vector( rawDF4[ , 7] )

raw_genes_5 <- as.vector( rawDF5[ , 1] )
raw_SNPs_5  <- as.vector( rawDF5[ , 8] )
raw_p_5     <- as.vector( rawDF5[ , 12] )
raw_d_5     <- as.vector( rawDF5[ , 7] )






finalDF1[1:5, ]

final_genes_1     <- as.vector( finalDF1$phe_id )
final_SNPs_1      <- as.vector( finalDF1$var_id )
final_nom_pval_1  <- as.vector( finalDF1$nom_pval )

final_genes_2     <- as.vector( finalDF2$phe_id )
final_SNPs_2      <- as.vector( finalDF2$var_id )
final_nom_pval_2  <- as.vector( finalDF2$nom_pval )

final_genes_3     <- as.vector( finalDF3$phe_id )
final_SNPs_3      <- as.vector( finalDF3$var_id )
final_nom_pval_3  <- as.vector( finalDF3$nom_pval )

final_genes_4     <- as.vector( finalDF4$phe_id )
final_SNPs_4      <- as.vector( finalDF4$var_id )
final_nom_pval_4  <- as.vector( finalDF4$nom_pval )

final_genes_5     <- as.vector( finalDF5$phe_id )
final_SNPs_5      <- as.vector( finalDF5$var_id )
final_nom_pval_5  <- as.vector( finalDF5$nom_pval )



pairsQTL_1 <- paste(final_genes_1, final_SNPs_1, sep="...")
pairsQTL_2 <- paste(final_genes_2, final_SNPs_2, sep="...")
pairsQTL_3 <- paste(final_genes_3, final_SNPs_3, sep="...")
pairsQTL_4 <- paste(final_genes_4, final_SNPs_4, sep="...")
pairsQTL_5 <- paste(final_genes_5, final_SNPs_5, sep="...")
length(pairsQTL_1)
length(pairsQTL_2)
length(pairsQTL_3)
length(pairsQTL_4)
length(pairsQTL_5)




pairsQTL_1 <- paste(final_genes_1, final_SNPs_1, sep="...")
pairsQTL_2 <- paste(final_genes_2, final_SNPs_2, sep="...")
pairsQTL_3 <- paste(final_genes_3, final_SNPs_3, sep="...")
pairsQTL_4 <- paste(final_genes_4, final_SNPs_4, sep="...")
pairsQTL_5 <- paste(final_genes_5, final_SNPs_5, sep="...")
length(pairsQTL_1)
length(pairsQTL_2)
length(pairsQTL_3)
length(pairsQTL_4)
length(pairsQTL_5)


rawPairs_1 <- paste(raw_genes_1, raw_SNPs_1, sep="...")
rawPairs_2 <- paste(raw_genes_2, raw_SNPs_2, sep="...")
rawPairs_3 <- paste(raw_genes_3, raw_SNPs_3, sep="...")
rawPairs_4 <- paste(raw_genes_4, raw_SNPs_4, sep="...")
rawPairs_5 <- paste(raw_genes_5, raw_SNPs_5, sep="...")
length(rawPairs_1)
length(rawPairs_2)
length(rawPairs_3)
length(rawPairs_4)
length(rawPairs_5)








####################################################################################
raw_names <-  intersect_all(  rawPairs_1,   rawPairs_2   )
length( raw_names ) 

name_1_2 <- raw_names[ runif(1000000, min=0, max=length(raw_names)) ]
final_pvalues_X <- c()
final_pvalues_Y <- c()
index_X <- match( name_1_2,  rawPairs_1)
index_Y <- match( name_1_2,  rawPairs_2)
final_pvalues_X <- raw_p_1[index_X]
final_pvalues_Y <- raw_p_2[index_Y]
length(final_pvalues_X)
length(final_pvalues_Y)

df_1 = data.frame( x= -log10(final_pvalues_X), y= -log10(final_pvalues_Y) )
p1 <- ggplot(df_1, aes(x = x, y = y)) + geom_point(shape=16, color="blue", alpha=0.1, size=0.1) + xlim(c(0,6)) + ylim(c(0,6)) + geom_abline(linewidth=0.2, linetype="dashed", color="black") + 
  ggtitle("BA9 vs BA24") +     xlab("-log10(p-value) in BA9") + ylab("-log10(p-value) in BA24")                                                                     
png( "1-vs-2.png" ,   width = 3, height = 3, units = "in", res = 1200, pointsize = 4 )
ggarrange( p1,    labels = "",  ncol = 1, nrow = 1)
dev.off()


 


####################################################################################
raw_names <-  intersect_all(  rawPairs_1,   rawPairs_3   )
length( raw_names ) 

name_1_2 <- raw_names[ runif(1000000, min=0, max=length(raw_names)) ]
final_pvalues_X <- c()
final_pvalues_Y <- c()
index_X <- match( name_1_2,  rawPairs_1)
index_Y <- match( name_1_2,  rawPairs_3)
final_pvalues_X <- raw_p_1[index_X]
final_pvalues_Y <- raw_p_3[index_Y]
length(final_pvalues_X)
length(final_pvalues_Y)

df_1 = data.frame( x= -log10(final_pvalues_X), y= -log10(final_pvalues_Y) )
p1 <- ggplot(df_1, aes(x = x, y = y)) + geom_point(shape=16, color="blue", alpha=0.1, size=0.1) + xlim(c(0,6)) + ylim(c(0,6)) + geom_abline(linewidth=0.2, linetype="dashed", color="black") + 
  ggtitle("BA9 vs C") +     xlab("-log10(p-value) in BA9") + ylab("-log10(p-value) in C")                                                                     
png( "1-vs-3.png" ,   width = 3, height = 3, units = "in", res = 1200, pointsize = 4 )
ggarrange( p1,    labels = "",  ncol = 1, nrow = 1)
dev.off()






####################################################################################
raw_names <-  intersect_all(  rawPairs_1,   rawPairs_4   )
length( raw_names ) 

name_1_2 <- raw_names[ runif(1000000, min=0, max=length(raw_names)) ]
final_pvalues_X <- c()
final_pvalues_Y <- c()
index_X <- match( name_1_2,  rawPairs_1)
index_Y <- match( name_1_2,  rawPairs_4)
final_pvalues_X <- raw_p_1[index_X]
final_pvalues_Y <- raw_p_4[index_Y]
length(final_pvalues_X)
length(final_pvalues_Y)

df_1 = data.frame( x= -log10(final_pvalues_X), y= -log10(final_pvalues_Y) )
p1 <- ggplot(df_1, aes(x = x, y = y)) + geom_point(shape=16, color="blue", alpha=0.1, size=0.1) + xlim(c(0,6)) + ylim(c(0,6)) + geom_abline(linewidth=0.2, linetype="dashed", color="black") + 
  ggtitle("BA9 vs H") +     xlab("-log10(p-value) in BA9") + ylab("-log10(p-value) in H")                                                                     
png( "1-vs-4.png" ,   width = 3, height = 3, units = "in", res = 1200, pointsize = 4 )
ggarrange( p1,    labels = "",  ncol = 1, nrow = 1)
dev.off()







####################################################################################
raw_names <-  intersect_all(  rawPairs_1,   rawPairs_5   )
length( raw_names ) 

name_1_2 <- raw_names[ runif(1000000, min=0, max=length(raw_names)) ]
final_pvalues_X <- c()
final_pvalues_Y <- c()
index_X <- match( name_1_2,  rawPairs_1)
index_Y <- match( name_1_2,  rawPairs_5)
final_pvalues_X <- raw_p_1[index_X]
final_pvalues_Y <- raw_p_5[index_Y]
length(final_pvalues_X)
length(final_pvalues_Y)

df_1 = data.frame( x= -log10(final_pvalues_X), y= -log10(final_pvalues_Y) )
p1 <- ggplot(df_1, aes(x = x, y = y)) + geom_point(shape=16, color="blue", alpha=0.1, size=0.1) + xlim(c(0,6)) + ylim(c(0,6)) + geom_abline(linewidth=0.2, linetype="dashed", color="black") + 
  ggtitle("BA9 vs T") +     xlab("-log10(p-value) in BA9") + ylab("-log10(p-value) in T")                                                                     
png( "1-vs-5.png" ,   width = 3, height = 3, units = "in", res = 1200, pointsize = 4 )
ggarrange( p1,    labels = "",  ncol = 1, nrow = 1)
dev.off()












####################################################################################
raw_names <-  intersect_all(  rawPairs_2,   rawPairs_3   )
length( raw_names ) 

name_1_2 <- raw_names[ runif(1000000, min=0, max=length(raw_names)) ]
final_pvalues_X <- c()
final_pvalues_Y <- c()
index_X <- match( name_1_2,  rawPairs_2)
index_Y <- match( name_1_2,  rawPairs_3)
final_pvalues_X <- raw_p_2[index_X]
final_pvalues_Y <- raw_p_3[index_Y]
length(final_pvalues_X)
length(final_pvalues_Y)

df_1 = data.frame( x= -log10(final_pvalues_X), y= -log10(final_pvalues_Y) )
p1 <- ggplot(df_1, aes(x = x, y = y)) + geom_point(shape=16, color="blue", alpha=0.1, size=0.1) + xlim(c(0,6)) + ylim(c(0,6)) + geom_abline(linewidth=0.2, linetype="dashed", color="black") + 
  ggtitle("BA24 vs C") +     xlab("-log10(p-value) in BA24") + ylab("-log10(p-value) in C")                                                                     
png( "2-vs-3.png" ,   width = 3, height = 3, units = "in", res = 1200, pointsize = 4 )
ggarrange( p1,    labels = "",  ncol = 1, nrow = 1)
dev.off()






####################################################################################
raw_names <-  intersect_all(  rawPairs_2,   rawPairs_4   )
length( raw_names ) 

name_1_2 <- raw_names[ runif(1000000, min=0, max=length(raw_names)) ]
final_pvalues_X <- c()
final_pvalues_Y <- c()
index_X <- match( name_1_2,  rawPairs_2)
index_Y <- match( name_1_2,  rawPairs_4)
final_pvalues_X <- raw_p_2[index_X]
final_pvalues_Y <- raw_p_4[index_Y]
length(final_pvalues_X)
length(final_pvalues_Y)

df_1 = data.frame( x= -log10(final_pvalues_X), y= -log10(final_pvalues_Y) )
p1 <- ggplot(df_1, aes(x = x, y = y)) + geom_point(shape=16, color="blue", alpha=0.1, size=0.1) + xlim(c(0,6)) + ylim(c(0,6)) + geom_abline(linewidth=0.2, linetype="dashed", color="black") + 
  ggtitle("BA24 vs H") +     xlab("-log10(p-value) in BA24") + ylab("-log10(p-value) in H")                                                                     
png( "2-vs-4.png" ,   width = 3, height = 3, units = "in", res = 1200, pointsize = 4 )
ggarrange( p1,    labels = "",  ncol = 1, nrow = 1)
dev.off()







####################################################################################
raw_names <-  intersect_all(  rawPairs_2,   rawPairs_5   )
length( raw_names ) 

name_1_2 <- raw_names[ runif(1000000, min=0, max=length(raw_names)) ]
final_pvalues_X <- c()
final_pvalues_Y <- c()
index_X <- match( name_1_2,  rawPairs_2)
index_Y <- match( name_1_2,  rawPairs_5)
final_pvalues_X <- raw_p_2[index_X]
final_pvalues_Y <- raw_p_5[index_Y]
length(final_pvalues_X)
length(final_pvalues_Y)

df_1 = data.frame( x= -log10(final_pvalues_X), y= -log10(final_pvalues_Y) )
p1 <- ggplot(df_1, aes(x = x, y = y)) + geom_point(shape=16, color="blue", alpha=0.1, size=0.1) + xlim(c(0,6)) + ylim(c(0,6)) + geom_abline(linewidth=0.2, linetype="dashed", color="black") + 
  ggtitle("BA24 vs T") +     xlab("-log10(p-value) in BA24") + ylab("-log10(p-value) in T")                                                                     
png( "2-vs-5.png" ,   width = 3, height = 3, units = "in", res = 1200, pointsize = 4 )
ggarrange( p1,    labels = "",  ncol = 1, nrow = 1)
dev.off()













####################################################################################
raw_names <-  intersect_all(  rawPairs_3,   rawPairs_4   )
length( raw_names ) 

name_1_2 <- raw_names[ runif(1000000, min=0, max=length(raw_names)) ]
final_pvalues_X <- c()
final_pvalues_Y <- c()
index_X <- match( name_1_2,  rawPairs_3)
index_Y <- match( name_1_2,  rawPairs_4)
final_pvalues_X <- raw_p_3[index_X]
final_pvalues_Y <- raw_p_4[index_Y]
length(final_pvalues_X)
length(final_pvalues_Y)

df_1 = data.frame( x= -log10(final_pvalues_X), y= -log10(final_pvalues_Y) )
p1 <- ggplot(df_1, aes(x = x, y = y)) + geom_point(shape=16, color="blue", alpha=0.1, size=0.1) + xlim(c(0,6)) + ylim(c(0,6)) + geom_abline(linewidth=0.2, linetype="dashed", color="black") + 
  ggtitle("C vs H") +     xlab("-log10(p-value) in C") + ylab("-log10(p-value) in H")                                                                     
png( "3-vs-4.png" ,   width = 3, height = 3, units = "in", res = 1200, pointsize = 4 )
ggarrange( p1,    labels = "",  ncol = 1, nrow = 1)
dev.off()







####################################################################################
raw_names <-  intersect_all(  rawPairs_3,   rawPairs_5   )
length( raw_names ) 

name_1_2 <- raw_names[ runif(1000000, min=0, max=length(raw_names)) ]
final_pvalues_X <- c()
final_pvalues_Y <- c()
index_X <- match( name_1_2,  rawPairs_3)
index_Y <- match( name_1_2,  rawPairs_5)
final_pvalues_X <- raw_p_3[index_X]
final_pvalues_Y <- raw_p_5[index_Y]
length(final_pvalues_X)
length(final_pvalues_Y)

df_1 = data.frame( x= -log10(final_pvalues_X), y= -log10(final_pvalues_Y) )
p1 <- ggplot(df_1, aes(x = x, y = y)) + geom_point(shape=16, color="blue", alpha=0.1, size=0.1) + xlim(c(0,6)) + ylim(c(0,6)) + geom_abline(linewidth=0.2, linetype="dashed", color="black") + 
  ggtitle("C vs T") +     xlab("-log10(p-value) in C") + ylab("-log10(p-value) in T")                                                                     
png( "3-vs-5.png" ,   width = 3, height = 3, units = "in", res = 1200, pointsize = 4 )
ggarrange( p1,    labels = "",  ncol = 1, nrow = 1)
dev.off()







####################################################################################
raw_names <-  intersect_all(  rawPairs_4,   rawPairs_5   )
length( raw_names ) 

name_1_2 <- raw_names[ runif(1000000, min=0, max=length(raw_names)) ]
final_pvalues_X <- c()
final_pvalues_Y <- c()
index_X <- match( name_1_2,  rawPairs_4)
index_Y <- match( name_1_2,  rawPairs_5)
final_pvalues_X <- raw_p_4[index_X]
final_pvalues_Y <- raw_p_5[index_Y]
length(final_pvalues_X)
length(final_pvalues_Y)

df_1 = data.frame( x= -log10(final_pvalues_X), y= -log10(final_pvalues_Y) )
p1 <- ggplot(df_1, aes(x = x, y = y)) + geom_point(shape=16, color="blue", alpha=0.1, size=0.1) + xlim(c(0,6)) + ylim(c(0,6)) + geom_abline(linewidth=0.2, linetype="dashed", color="black") + 
  ggtitle("H vs T") +     xlab("-log10(p-value) in H") + ylab("-log10(p-value) in T")                                                                     
png( "4-vs-5.png" ,   width = 3, height = 3, units = "in", res = 1200, pointsize = 4 )
ggarrange( p1,    labels = "",  ncol = 1, nrow = 1)
dev.off()











intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}



raw_genes_common  <- intersect_all(  raw_genes_1,   raw_genes_2 ,  raw_genes_3,   raw_genes_4 ,   raw_genes_5    )
length( raw_genes_common ) 


raw_SNPs_common  <- intersect_all(  raw_SNPs_1,   raw_SNPs_2 ,  raw_SNPs_3,   raw_SNPs_4 ,   raw_SNPs_5    )
length( raw_SNPs_common ) 



######################################
raw_names <-  intersect_all(  rawPairs_2,   rawPairs_3 ,  rawPairs_4   )
length( raw_names ) 






QTLs_common  <- intersect_all(  pairsQTL_1,   pairsQTL_2       )
length( QTLs_common ) 




max( raw_p_1 )
max( raw_p_2 )
max( raw_p_3 )
max( raw_p_4 )
max( raw_p_5 )


##############################################################
final_nom_pval_1_temp  <- final_nom_pval_1
final_nom_pval_1_temp[ which.min( final_nom_pval_1_temp ) ] = 1
final_nom_pval_1_temp[ which.min( final_nom_pval_1_temp ) ] = 1
final_nom_pval_1_temp[ which.min( final_nom_pval_1_temp ) ] = 1
final_nom_pval_1_temp[ which.min( final_nom_pval_1_temp ) ] = 1
final_nom_pval_1_temp[ which.min( final_nom_pval_1_temp ) ] = 1
final_nom_pval_1_temp[ which.min( final_nom_pval_1_temp ) ] = 1
final_nom_pval_1_temp[ which.min( final_nom_pval_1_temp ) ] = 1
final_nom_pval_1_temp[ which.min( final_nom_pval_1_temp ) ] = 1


##index_1_1 <- which.min( final_nom_pval_1_temp )    
index_1_1 <- which(final_genes_1 == "chr17:46260811-46261188...377...+")
index_1_1 <- index_1_1[1]
final_genes_1[ index_1_1 ]

SNP_1_1       <- final_SNPs_2[ index_1_1 ]
phenotype_1_1 <- final_genes_2[ index_1_1 ]

phenotype_1_1 %in%  raw_genes_common
SNP_1_1 %in%  raw_SNPs_common




genes_one <- phenotype_1_1



raw_index_1  <-    str_detect( raw_genes_1,   genes_one)
length(raw_index_1)
length(raw_index_1[raw_index_1])
raw_genes_1A <- raw_genes_1[ raw_index_1 ]
raw_SNPs_1A  <- raw_SNPs_1[ raw_index_1 ]
raw_p_1A     <- raw_p_1[ raw_index_1 ]
raw_d_1A     <- raw_d_1[ raw_index_1 ]
length(raw_genes_1A)
length(raw_SNPs_1A)
length(raw_p_1A)
length(raw_d_1A)




raw_index_2  <-    str_detect( raw_genes_2,   genes_one)
length(raw_index_2)
length(raw_index_2[raw_index_2])
raw_genes_2A <- raw_genes_2[ raw_index_2 ]
raw_SNPs_2A  <- raw_SNPs_2[ raw_index_2 ]
raw_p_2A     <- raw_p_2[ raw_index_2 ]
raw_d_2A     <- raw_d_2[ raw_index_2 ]
length(raw_genes_2A)
length(raw_SNPs_2A)
length(raw_p_2A)
length(raw_d_2A)






raw_index_3  <-    str_detect( raw_genes_3,   genes_one)
length(raw_index_3)
length(raw_index_3[raw_index_3])
raw_genes_3A <- raw_genes_3[ raw_index_3 ]
raw_SNPs_3A  <- raw_SNPs_3[ raw_index_3 ]
raw_p_3A     <- raw_p_3[ raw_index_3 ]
raw_d_3A     <- raw_d_3[ raw_index_3 ]
length(raw_genes_3A)
length(raw_SNPs_3A)
length(raw_p_3A)
length(raw_d_3A)







raw_index_4  <-    str_detect( raw_genes_4,   genes_one)
length(raw_index_4)
length(raw_index_4[raw_index_4])
raw_genes_4A <- raw_genes_4[ raw_index_4 ]
raw_SNPs_4A  <- raw_SNPs_4[ raw_index_4 ]
raw_p_4A     <- raw_p_4[ raw_index_4 ]
raw_d_4A     <- raw_d_4[ raw_index_4 ]
length(raw_genes_4A)
length(raw_SNPs_4A)
length(raw_p_4A)
length(raw_d_4A)





raw_index_5  <-    str_detect( raw_genes_5,   genes_one)
length(raw_index_5)
length(raw_index_5[raw_index_5])
raw_genes_5A <- raw_genes_5[ raw_index_5 ]
raw_SNPs_5A  <- raw_SNPs_5[ raw_index_5 ]
raw_p_5A     <- raw_p_5[ raw_index_5 ]
raw_d_5A     <- raw_d_5[ raw_index_5 ]
length(raw_genes_5A)
length(raw_SNPs_5A)
length(raw_p_5A)
length(raw_d_5A)


##########################################################





 
 


final_SNPs <- c( raw_SNPs_1A, raw_SNPs_2A, raw_SNPs_3A, raw_SNPs_4A, raw_SNPs_5A  )
length( final_SNPs )

final_SNPs <- unique( final_SNPs )
length( final_SNPs )



final_pvalues_1 <- c()
final_pvalues_2 <- c()
final_pvalues_3 <- c()
final_pvalues_4 <- c()
final_pvalues_5 <- c()

for( i in   c(1:length(final_SNPs)) ){
  final_pvalues_1[i] = 1
  for( j in   c(1:length(raw_SNPs_1A)) ){
    if(final_SNPs[i] == raw_SNPs_1A[j]){final_pvalues_1[i] = raw_p_1A[j] }
  }
}


for( i in   c(1:length(final_SNPs)) ){
  final_pvalues_2[i] = 1
  for( j in   c(1:length(raw_SNPs_2A)) ){
    if(final_SNPs[i] == raw_SNPs_2A[j]){final_pvalues_2[i] = raw_p_2A[j] }
  }
}


for( i in   c(1:length(final_SNPs)) ){
  final_pvalues_3[i] = 1
  for( j in   c(1:length(raw_SNPs_3A)) ){
    if(final_SNPs[i] == raw_SNPs_3A[j]){final_pvalues_3[i] = raw_p_3A[j] }
  }
}



for( i in   c(1:length(final_SNPs)) ){
  final_pvalues_4[i] = 1
  for( j in   c(1:length(raw_SNPs_4A)) ){
    if(final_SNPs[i] == raw_SNPs_4A[j]){final_pvalues_4[i] = raw_p_4A[j] }
  }
}



for( i in   c(1:length(final_SNPs)) ){
  final_pvalues_5[i] = 1
  for( j in   c(1:length(raw_SNPs_5A)) ){
    if(final_SNPs[i] == raw_SNPs_5A[j]){final_pvalues_5[i] = raw_p_5A[j] }
  }
}











final_distance <- c()

for( i in   c(1:length(final_SNPs)) ){
  final_distance[i] = 1000000
  for( j in   c(1:length(raw_SNPs_1A)) ){
    if(final_SNPs[i] == raw_SNPs_1A[j]){final_distance[i] = raw_d_1A[j] }
  }
}


for( i in   c(1:length(final_SNPs)) ){
  for( j in   c(1:length(raw_SNPs_2A)) ){
    if(final_SNPs[i] == raw_SNPs_2A[j]){final_distance[i] = raw_d_2A[j] }
  }
}


for( i in   c(1:length(final_SNPs)) ){
  for( j in   c(1:length(raw_SNPs_3A)) ){
    if(final_SNPs[i] == raw_SNPs_3A[j]){final_distance[i] = raw_d_3A[j] }
  }
}



for( i in   c(1:length(final_SNPs)) ){
  for( j in   c(1:length(raw_SNPs_4A)) ){
    if(final_SNPs[i] == raw_SNPs_4A[j]){final_distance[i] = raw_d_4A[j] }
  }
}



for( i in   c(1:length(final_SNPs)) ){
  for( j in   c(1:length(raw_SNPs_5A)) ){
    if(final_SNPs[i] == raw_SNPs_5A[j]){final_distance[i] = raw_d_5A[j] }
  }
}


final_distance[final_distance==1000000]











matrix_LD <- read.table(  "LD/sepChr/chr19.LD" ,    header=F,   sep="\t", comment.char = "", quote ="" )    
dim(matrix_LD)
matrix_LD[1:10 , ]






pos1  <- matrix_LD[,2]
pos2  <- matrix_LD[,3]
LDcol <- matrix_LD[,5]



 
refSNP <- SNP_1_1
refSNP 

refSNP <- str_replace_all(string=refSNP, pattern="^chr\\d+:", replacement="")
refSNP <- str_replace_all(string=refSNP, pattern=":\\S+$", replacement="")
refSNP <- str_replace_all(string=refSNP, pattern=":\\S+$", replacement="")
refSNP




final_SNPs_pos <- str_replace_all(string=final_SNPs, pattern="^chr\\d+:", replacement="")
final_SNPs_pos <- str_replace_all(string=final_SNPs_pos, pattern=":\\S+$", replacement="")
final_SNPs_pos <- str_replace_all(string=final_SNPs_pos, pattern=":\\S+$", replacement="")

bool_1  <- (pos1  %in%  final_SNPs_pos)  & (pos2  %in%  refSNP) 
length(bool_1)
length(bool_1[bool_1])

bool_2  <- (pos2  %in%  final_SNPs_pos)   &  (pos1  %in%  refSNP)
length(bool_2)
length(bool_2[bool_2])

pos1  <- pos1[ bool_1 | bool_2 ]
pos2  <- pos2[ bool_1 | bool_2 ]
LDcol <- LDcol[ bool_1 | bool_2 ]
length( pos1 )
length( pos2 )
length( LDcol )




LDscore <- vector( length = length(final_SNPs) )
i=0
for(onesnp in final_SNPs ){
  i =i+1
  mybool = 0
  for(j in c(1:length(LDcol)) ){
    if( (final_SNPs_pos[i] == pos1[j] ) | (final_SNPs_pos[i] == pos2[j]) ) {  LDscore[i] =  LDcol[j]; mybool=1; }
  }
  if(mybool==0){LDscore[i] = 0}
  if(final_SNPs_pos[i] == refSNP){LDscore[i] = 1}
}

LDscore
summary(LDscore)







bool_A_1  <- final_SNPs  %in%  final_SNPs_1
length(bool_A_1)
length(bool_A_1[bool_A_1])

bool_A_2  <- final_SNPs  %in%  final_SNPs_2
length(bool_A_2)
length(bool_A_2[bool_A_2])

bool_A_3  <- final_SNPs  %in%  final_SNPs_3
length(bool_A_3)
length(bool_A_3[bool_A_3])

bool_A_4  <- final_SNPs  %in%  final_SNPs_4
length(bool_A_4)
length(bool_A_4[bool_A_4])

bool_A_5  <- final_SNPs  %in%  final_SNPs_5
length(bool_A_5)
length(bool_A_5[bool_A_5])






length( final_distance )
length( final_pvalues_1 )
length( bool_A_1 )
length( LDscore )

final_SNPs2 = final_SNPs
final_SNPs2[ final_SNPs2 != SNP_1_1] = ""  
final_SNPs2[ final_SNPs2 == SNP_1_1] = "rs8102261"

library(ggrepel)

df_1 = data.frame(x = final_distance/1000,  y=  -log10(final_pvalues_1), type=bool_A_1, score=LDscore, snp=final_SNPs2 )
gp_1 = ggplot(df_1, aes(x=x, y=y, shape=type, colour=score, label = snp ) )  +  geom_point( size=1, alpha=1 )  +   scale_colour_gradient(low = "midnightblue",  high = "red", na.value = NA  ) +
  MyTheme_1_g(textSize1=6) +  xlab("distance (kb)" ) + ylab( "-log10(p-value)" )  +  ggtitle(  "BA9" ) + xlim( c(-1000 ,1000) ) +   geom_text_repel( box.padding = 1, max.overlaps = Inf )


df_2 = data.frame(x = final_distance/1000,  y=  -log10(final_pvalues_2), type=bool_A_2, score=LDscore, snp=final_SNPs2  )
gp_2 = ggplot(df_2, aes(x=x, y=y, shape=type, colour=score, label = snp))  +  geom_point( size=1, alpha=1 )  +   scale_colour_gradient(low = "midnightblue",  high = "red", na.value = NA  ) +
  MyTheme_1_g(textSize1=6) +  xlab("distance (kb)" ) + ylab( "-log10(p-value)" )  +  ggtitle(  "BA24" ) + xlim( c(-1000 ,1000) )+   geom_text_repel( box.padding = 1, max.overlaps = Inf )


df_3 = data.frame(x = final_distance/1000,  y=  -log10(final_pvalues_3), type=bool_A_3, score=LDscore, snp=final_SNPs2  )
gp_3 = ggplot(df_3, aes(x=x, y=y, shape=type, colour=score, label = snp))  +  geom_point( size=1, alpha=1 )  +   scale_colour_gradient(low = "midnightblue",  high = "red", na.value = NA  ) +
  MyTheme_1_g(textSize1=6) +  xlab("distance (kb)" ) + ylab( "-log10(p-value)" )  +  ggtitle( "C" ) + xlim( c(-1000 ,1000) )+   geom_text_repel( box.padding = 1, max.overlaps = Inf )


df_4 = data.frame(x = final_distance/1000,  y=  -log10(final_pvalues_4), type=bool_A_4, score=LDscore, snp=final_SNPs2  )
gp_4 = ggplot(df_4, aes(x=x, y=y, shape=type, colour=score, label = snp))  +  geom_point( size=1, alpha=1 )  +   scale_colour_gradient(low = "midnightblue",  high = "red", na.value = NA  ) +
  MyTheme_1_g(textSize1=6) +  xlab("distance (kb)" ) + ylab( "-log10(p-value)" )  +  ggtitle(  "H" ) + xlim( c(-1000 ,1000) )+   geom_text_repel( box.padding = 1, max.overlaps = Inf )


df_5 = data.frame(x = final_distance/1000,  y=  -log10(final_pvalues_5), type=bool_A_5, score=LDscore, snp=final_SNPs2  )
gp_5 = ggplot(df_5, aes(x=x, y=y, shape=type, colour=score, label = snp))  +  geom_point( size=1, alpha=1 )  +   scale_colour_gradient(low = "midnightblue",  high = "red", na.value = NA  ) +
  MyTheme_1_g(textSize1=6) +  xlab("distance (kb)" ) + ylab( "-log10(p-value)" )  +  ggtitle(  "T" ) + xlim( c(-1000 ,1000) )+   geom_text_repel( box.padding = 1, max.overlaps = Inf )



library(gridExtra)
plot_lst <- list( gp_1, gp_2, gp_3, gp_4, gp_5  )
ml <- marrangeGrob(plot_lst, nrow = 5, ncol = 1)
ggsave( paste( outDir_g, "/", "1-3.pdf", sep="" ) , ml, device="pdf", width=15, height=30, units="cm" )









 















