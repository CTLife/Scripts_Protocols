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
suppressPackageStartupMessages( library(MASS) )
suppressPackageStartupMessages( library(dendextend) )
suppressPackageStartupMessages( library(factoextra) ) 


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
    axis.line.y       = element_line(colour="transparent", linewidth=0.3, linetype=1, lineend=NULL),     ## line along y axis (element_line; inherits from axis.line)    
    legend.background    = element_rect(colour="transparent", linewidth=1, linetype=1, fill="transparent" ),  ## background of legend (element_rect; inherits from rect)
    legend.spacing       = grid::unit(1, "mm", data=NULL),                                                  ## extra space added around legend (unit). 
    legend.key           = element_rect(colour="transparent", linewidth=2, linetype=1, fill="transparent" ),  ## background underneath legend keys. 
    legend.key.size      = grid::unit(6,   "mm", data=NULL) ,                                                   ## size of legend keys   (unit; inherits from legend.key.size)
    legend.key.height    = grid::unit(6.5, "mm", data=NULL) ,                                                   ## key background height (unit; inherits from legend.key.size)
    legend.key.width     = grid::unit(8,   "mm", data=NULL) ,                                                   ## key background width  (unit; inherits from legend.key.size)
    legend.text          = element_text(family="serif", face=NULL, colour="black", size=textSize1, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),  ##legend item labels. 
    legend.text.align    = 0,                       ## alignment of legend labels (number from 0 (left) to 1 (right))
    legend.title         = element_blank(),         ## title of legend (element_text; inherits from title)
    legend.title.align   = 0,                       ## alignment of legend title (number from 0 (left) to 1 (right))
    legend.position      = "right",               ## the position of legends. ("left", "right", "bottom", "top", or two-element numeric vector)
    legend.direction     = "vertical",              ## layout of items in legends  ("horizontal" or "vertical")    
    legend.justification = "center",            ## anchor point for positioning legend inside plot ("center" or two-element numeric vector)   
    legend.box           = NULL,              ## arrangement of multiple legends ("horizontal" or "vertical")  
    legend.box.just      = NULL,              ## justification of each legend within the overall bounding box, when there are multiple legends ("top", "bottom", "left", or "right")       
    panel.background   = element_rect(colour="transparent", linewidt=0.0, linetype=1, fill="transparent" ),     ## background of plotting area, drawn underneath plot (element_rect; inherits from rect)
    panel.border       = element_rect(colour="black", linewidt=0.5, linetype=1, fill=NA ),                      ## border around plotting area, drawn on top of plot so that it covers tick marks and grid lines. This should be used with fill=NA (element_rect; inherits from rect)                                     
    panel.spacing      = grid::unit(1, "mm", data=NULL) ,                                               ## margin around facet panels (unit)   
    panel.spacing.x    = grid::unit(1, "mm", data=NULL) ,
    panel.spacing.y    = grid::unit(1, "mm", data=NULL) ,
    panel.grid         = element_blank(),                                                               ## grid lines (element_line; inherits from line)   
    panel.grid.major   = element_line(colour="transparent", linewidth=NULL, linetype=NULL, lineend=NULL) ,      ## major grid lines (element_line; inherits from panel.grid)  
    panel.grid.minor   = element_line(colour="transparent", linewidth=NULL, linetype=NULL, lineend=NULL) ,       ## minor grid lines (element_line; inherits from panel.grid)   
    panel.grid.major.x = element_line(colour="transparent", linewidth=NULL, linetype=NULL, lineend=NULL) ,      ## vertical major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.major.y = element_line(colour="transparent", linewidth=NULL, linetype=NULL, lineend=NULL) ,       ## horizontal major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.minor.x = element_line(colour="transparent", linewidth=NULL, linetype=NULL, lineend=NULL) ,       ## vertical minor grid lines (element_line; inherits from panel.grid.minor)
    panel.grid.minor.y = element_line(colour="transparent", linewidth=NULL, linetype=NULL, lineend=NULL) ,       ## horizontal minor grid lines (element_line; inherits from panel.grid.minor)    
    plot.background  = element_rect(colour="transparent", linewidth=NULL, linetype=NULL, fill="transparent" ),                                            ## background of the entire plot (element_rect; inherits from rect)   
    plot.title       = element_text(family="serif", face=NULL, colour="black", size=textSize1, hjust=0.5, vjust=0.5,   angle=NULL, lineheight=NULL),     ## plot title (text appearance) (element_text; inherits from title)   
    plot.margin      = grid::unit(c(5, 5, 5, 5), "mm", data=NULL),                                                                                  ## margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)    
    strip.background = element_rect(colour=NULL,    linewidth=NULL, linetype=NULL, fill=NULL ),                                                         ## background of facet labels (element_rect; inherits from rect)   
    strip.text       = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),        ## facet labels (element_text; inherits from text)
    strip.text.x     = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),        ## facet labels along horizontal direction (element_text; inherits from strip.text)
    strip.text.y     = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL)           ## facet labels along vertical direction (element_text; inherits from strip.text) 
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









 

###########################################################################################################################################################################
myFile1 <- "2_ANOVA/1_BA9/3.kept.txt"
DF1 <- read.table(myFile1, header=TRUE,   sep="\t", comment.char = "", quote ="" )    
dim(DF1)
DF1[1:10, 1:10]
colnames(DF1)

DF_region <- DF1[,1:6]
DF_value  <- DF1[,-c(1:6)]
dim(DF_region)
dim(DF_value)
colnames(DF_region)
colnames(DF_value)

DF_region[1:10, ]
DF_value[1:10, 1:10]
rownames(DF_region) = DF_region[,4]
rownames(DF_value) = DF_region[,4]
DF_region[1:10, ]
DF_value[1:10, 1:10]


myColName <- colnames(DF_value)
myColName
mySampleNames <- myColName
mySampleNames
myGroups <- myColName
## myGroups <- str_replace_all(string=myGroups, pattern=".sample\\d+",  replacement="")
myGroups


age1 <- c("sample19", "sample21", "sample22", "sample24" )
age2 <- c("sample6",  "sample26", "sample27", "sample14", "sample28", "sample13" )
age3 <- c("sample11", "sample12", "sample25", "sample7" , "sample30", "sample29", "sample31"  )
age4 <- c("sample1",  "sample15", "sample8",  "sample23" )
age5 <- c("sample10", "sample9",  "sample20", "sample5"  )



######################################################
# bool_1 <- myGroups  %in%  c("1_BA9")
# length(bool_1)  
# length(bool_1[bool_1]) 

mySampleNames_1 <- mySampleNames
DF_value_1 <- DF_value
dim(DF_value_1)
colnames(DF_value_1)
mySampleNames_1

bool_age1  <- mySampleNames_1  %in%  age1
bool_age2  <- mySampleNames_1  %in%  age2
bool_age3  <- mySampleNames_1  %in%  age3
bool_age4  <- mySampleNames_1  %in%  age4
bool_age5  <- mySampleNames_1  %in%  age5
length( bool_age1[bool_age1]  )
length( bool_age2[bool_age2]  )
length( bool_age3[bool_age3]  )
length( bool_age4[bool_age4]  )
length( bool_age5[bool_age5]  )

DF_age1 <- DF_value_1[ , bool_age1]
DF_age2 <- DF_value_1[ , bool_age2]
DF_age3 <- DF_value_1[ , bool_age3]
DF_age4 <- DF_value_1[ , bool_age4]
DF_age5 <- DF_value_1[ , bool_age5]
dim(DF_age1)
dim(DF_age2)
dim(DF_age3)
dim(DF_age4)
dim(DF_age5)

colnames(DF_age1)
colnames(DF_age2)
colnames(DF_age3)
colnames(DF_age4)
colnames(DF_age5)



outdir <- "7_clutering_pheatmap/1_BA9"
if( ! file.exists(outdir) ) { dir.create(path=outdir, recursive = TRUE) }


DF_age1_diff <- DF_age1
DF_age2_diff <- DF_age2
DF_age3_diff <- DF_age3
DF_age4_diff <- DF_age4
DF_age5_diff <- DF_age5
dim(DF_age1_diff)
dim(DF_age2_diff)
dim(DF_age3_diff)
dim(DF_age4_diff)
dim(DF_age5_diff)
DF_age1_diff[1:10,]


matrix_1 <- cbind( DF_age1_diff, rep(NA, nrow(DF_age1_diff)),  DF_age2_diff, rep(NA, nrow(DF_age1_diff)), DF_age3_diff, rep(NA, nrow(DF_age1_diff)), DF_age4_diff, rep(NA, nrow(DF_age1_diff)), DF_age5_diff  )
matrix_1A = matrix_1
dim( matrix_1 )
dim( matrix_1A )
matrix_1A[1:10,]




my_reset_outliers <- function(x, na.rm = TRUE ) { ## x is a numerical vector or matrix.
  qnt <- quantile(x, probs=c(0.1, 0.9) , type=1,  na.rm = na.rm )  
  y <- x
  y[x < qnt[1] ] <- qnt[1]
  y[x > qnt[2] ] <- qnt[2]    
  return(y)
}

 

for(i in c(1:nrow(matrix_1)) ){
  temp1 = as.numeric(as.vector( matrix_1[i,] ))
  temp1 = my_reset_outliers(temp1)
  sd1 = sd(temp1 , na.rm = TRUE)
  mean1 = mean(temp1 , na.rm = TRUE)
  if( sd1 > 0){
    matrix_1[i,] = (temp1 - mean1 )/ sd1
  }else{
    matrix_1[i,] =  temp1 - mean1 
  }
  
}

matrix_1[1:5,]
matrix_1[is.na(matrix_1)] = 0
matrix_1[1:5,]









numClusters = 9







#################################################################################################################################
my_col1=colorRampPalette( c("cyan4",   "cyan",  "white", "red", "red4" ),  bias = 1.0,  space = "rgb" )
library("pheatmap")

pdf( file = paste(outdir, "1.clusters.heatmap.pdf", sep="/"),  width=6, height=8  )
presutls1 = pheatmap::pheatmap(matrix_1, cluster_rows = T , cluster_cols = F,   clustering_distance_rows = "correlation",   clustering_method = "ward.D2",  cutree_rows = numClusters,    show_rownames = F, show_colnames = F , col=my_col1(100)     )         
dev.off() 
#################################################################################################################################



names(presutls1)
index1 = presutls1$tree_row$order
length(index1)
matrix_1A  = matrix_1A[ index1 , ]
matrix_1  = matrix_1[ index1 , ]
DF_region_1 = DF_region[index1, ]


write.table( index1 , file = paste(outdir, "5A.index1.txt", sep="/"), append = FALSE, 
             quote = FALSE, sep = "\t",  eol = "\n", na = "NA", dec = ".", row.names = T,  col.names = T )

write.table( cbind(DF_region_1 ,   matrix_1A) , file = paste(outdir, "5B.clusteredMatrix.txt", sep="/"), append = FALSE, 
             quote = FALSE, sep = "\t",  eol = "\n", na = "NA", dec = ".", row.names = T,  col.names = T )

write.table( cbind(DF_region_1 ,   matrix_1) , file = paste(outdir, "5C.clusteredMatrix.scaled.txt", sep="/"), append = FALSE, 
             quote = FALSE, sep = "\t",  eol = "\n", na = "NA", dec = ".", row.names = T,  col.names = T )



pdf( file = paste(outdir, "2.clusters.heatmap.pdf", sep="/"),  width=6, height=8  )
presutls2 = pheatmap::pheatmap(matrix_1, cluster_rows = F , cluster_cols = F,   clustering_distance_rows = "correlation",    clustering_method = "ward.D2",  show_rownames = F, show_colnames = F , col=my_col1(100)   )         
dev.off() 




pdf( file = paste(outdir, "3.clusters.heatmap.pdf", sep="/"),  width=6, height=8  )
presutls2 = pheatmap::pheatmap(matrix_1, cluster_rows = T , cluster_cols = F,   clustering_distance_rows = "correlation",    clustering_method = "ward.D2", cutree_rows = numClusters,      show_rownames = TRUE, show_colnames = T , col=my_col1(100) , fontsize=1 )         
dev.off() 



my_col1=colorRampPalette( c(   "cyan",  "black", "red" ),  bias = 1.0,  space = "rgb" )
library("pheatmap")

pdf( file = paste(outdir, "4A.clusters.heatmap.pdf", sep="/"),  width=6, height=8  )
presutls2 = pheatmap::pheatmap(matrix_1, cluster_rows = T , cluster_cols = F,   clustering_distance_rows = "correlation",   clustering_method = "ward.D2",  cutree_rows = numClusters,      show_rownames = F, show_colnames = F , col=my_col1(100)   )         
dev.off() 

pdf( file = paste(outdir, "4B.clusters.heatmap.pdf", sep="/"),  width=10, height=50  )
presutls2 = pheatmap::pheatmap(matrix_1, cluster_rows = F , cluster_cols = F,   clustering_distance_rows = "correlation",   clustering_method = "ward.D2",  cutree_rows = numClusters,      show_rownames = TRUE, show_colnames = T , col=my_col1(100) , fontsize=1 )         
dev.off() 


hc = presutls1$tree_row
lbl <- cutree(hc, numClusters)
## which(lbl==1)
lbl = lbl[index1]

write.table( lbl , file = paste(outdir, "6.groups.txt", sep="/"), append = FALSE, 
             quote = FALSE, sep = "\t",  eol = "\n", na = "NA", dec = ".", row.names = T,  col.names = F )


bool1 = ( names(lbl) == rownames(matrix_1A) )
length(bool1)
length(bool1[bool1])

finalMatrix = cbind(  lbl, DF_region_1, matrix_1A)
dim(finalMatrix)
finalMatrix[1:10,]


write.table( finalMatrix , file = paste(outdir, "7.finalMatrix.txt", sep="/"), append = FALSE, 
             quote = FALSE, sep = "\t",  eol = "\n", na = "NA", dec = ".", row.names = T,  col.names = T )
             
             
             
sink(paste(outdir, "8.numberofEachCluster.txt", sep="/"))
print("###########")
print("###########")
print(  table(lbl) )
print("###########")
print( unique(lbl) )
print("###########")
sink()             



 
###################################################### 


















 
