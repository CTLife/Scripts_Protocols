#! /usr/bin/env Rscript
##############################################################################################################################################################################################
## Differential analysis by RADAR https://scottzijiezhang.github.io/RADARmanual/workflow.html               
## Run " Rscript   RADAR_diff.R   -h " to get some help.
##############################################################################################################################################################################################





##############################################################################################################################################################################################
rm( list=ls() )    # remove all the objects from the R session

suppressPackageStartupMessages( library(optparse) )  ## To run the script in command lines.

getParameters_f <- function() {
  option_list_Local <- list(   ## Options list with associated default value.  16 options.
    optparse::make_option(opt_str=c("-s", "--samplesInformation"),
                          default="../../Groups_Types_93.txt",
                          type="character",   dest="samplesInformation",
                          help="Path to the design/target file with samples information.  [default: %default]."),
    
    optparse::make_option(opt_str=c("-i", "--input"),
                          default="../../../1C_expressionLevel/Input.TMM.matrix.txt",
                          type="character",   dest="input",
                          help="Input file with signal matrix.  [default: %default]."),
    
    optparse::make_option(opt_str=c("-o", "--outDir"),
                          default="RADAR_Differential",
                          type="character",   dest="outDir",
                          help="Path to the directory containing all the analysis results. [default: %default]."),

    optparse::make_option(opt_str=c("-t", "--type"),
                          default="region.",
                          type="character",   dest="type",
                          help="Group type: sex, age_group, age_group2, race, region. [default: %default]."),
    
    optparse::make_option(opt_str=c("-a", "--groupA"),
                          default="BA24",
                          type="character",   dest="groupA",
                          help="Label of group A. [default: %default]."),
    
    optparse::make_option(opt_str=c("-b", "--groupB"),
                          default="BA9",
                          type="character",   dest="groupB",
                          help="Label of group B. If this value = others, then group B = non-A.  [default: %default].")
                       
  )
  
  ## Now parse the command line to check which option is given and get associated values.
  parser_Local <- optparse::OptionParser(usage="usage: Rscript %prog [options]",
                                         option_list=option_list_Local, 
                                         description="Differential analysis.   Version 0.1; October 31, 2023.",                             
                                         epilogue="For comments, bug reports etc..., please contact Yong Peng <yongp@outlook.com>."
  )
  opt_Local <- optparse::parse_args(parser_Local, args=commandArgs(trailingOnly=TRUE), positional_arguments=0)$options
  return(opt_Local)
}
##############################################################################################################################################################################################





##############################################################################################################################################################################################
opt_g = getParameters_f()  

samplesInformation_g      <- opt_g$samplesInformation
input_g                   <- opt_g$input
outDir_g                  <- opt_g$outDir
type_g                    <- opt_g$type
groupA_g                  <- opt_g$groupA
groupB_g                  <- opt_g$groupB
 
## samplesInformation_g      <- "../../../1A_m6Alevel_matrix/Groups_Types_93.txt"
## input_g                   <- "../../../1A_m6Alevel_matrix/m6A-Peaks.All-93.no_zscore_quantile.noXY.bed"
## outDir_g                  <- "RADAR_Differential"
## type_g                    <- "region"
## groupA_g                  <- "BA9"
## groupB_g                  <- "others"
 
rm(getParameters_f)  
rm(opt_g)  

options(digits=10)
continue_on_error_g <- function() {
    print( "NOTE: THERE WAS AN ERROR HERE. We are continuing because we have set 'options(error=continue_on_error())'. " )
}
options( error=continue_on_error_g )  




if( ! file.exists(outDir_g)   ) { dir.create(outDir_g,   recursive = TRUE) }


sink( paste(outDir_g,  "/1A_Parameters.txt",  sep = "") )
    print( paste("samplesInformation_g" , samplesInformation_g,      sep=":  ") )
    print( paste("input_g"              , input_g,                   sep=":  ") )
    print( paste("outDir_g"             , outDir_g,                  sep=":  ") )
    print( paste("type_g"               , type_g,                    sep=":  ") )
    print( paste("groupA_g"             , groupA_g,                  sep=":  ") )
    print( paste("groupB_g"             , groupB_g,                  sep=":  ") )
sink()


## print to screen:
    print("########################################################################")
    print("################### Parameters:")
    print( paste("samplesInformation_g" , samplesInformation_g,      sep=":  ") )
    print( paste("input_g"              , input_g,                   sep=":  ") )
    print( paste("outDir_g"             , outDir_g,                  sep=":  ") )
    print( paste("type_g"               , type_g,                    sep=":  ") )
    print( paste("groupA_g"             , groupA_g,                  sep=":  ") )
    print( paste("groupB_g"             , groupB_g,                  sep=":  ") )
    print("########################################################################")
##############################################################################################################################################################################################





##############################################################################################################################################################################################
suppressPackageStartupMessages( library(RADAR) )
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


My_for_MultidimensionalScaling_1_g <- function(  path3,   dataFrame_temp3  ) {
  FigureTemp1_2two  <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech) )) + 
    geom_point(size=1, alpha=1  ) + xlab("Dimension 1") +   ylab("Dimension 2") +   
    #scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) + 
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5)))  
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1_2two ,  path1=path3, fileName1="A_MDS",  height1=3.5,  width1=6)
  
  FigureTemp2_2two  <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech) )) + 
    geom_point(size=1, alpha=0.7  )+ xlab("Dimension 1") +   ylab("Dimension 2") +   
    #scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) +  
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5))) 
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp2_2two ,  path1=path3, fileName1="B_MDS-alpha",   height1=3.5,  width1=6)
  
  FigureTemp3_2two  <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech) )) + 
    geom_point(size=1, alpha=1  ) + xlab("Dimension 1") +   ylab("Dimension 2") +   
    #scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) + 
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5))) 
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp3_2two ,  path1=path3, fileName1="C_MDS-smallDot",   height1=3.5,  width1=6)
  
  FigureTemp3_2two  <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech) )) + 
    geom_point(size=1, alpha=0.7  ) + xlab("Dimension 1") +   ylab("Dimension 2") +   
    #scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) + 
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5))) 
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp3_2two ,  path1=path3, fileName1="D_MDS-smallDot-alpha",   height1=3.5,  width1=6)
  
  FigureTemp4 <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech) )) + 
    geom_point(size=2, alpha=0.7  ) + xlab("Dimension 1") +   ylab("Dimension 2") +     
    #scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) +  
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5))) 
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4_2two ,  path1=path3, fileName1="E_MDS-big",   height1=3.5,  width1=6)
  
  FigureTemp5_2two  <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech), label=myLabel )) + 
    geom_point(size=2, alpha=0.7  ) + xlab("Dimension 1") +   ylab("Dimension 2") +   
    #scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) + 
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) + ggrepel::geom_text_repel(aes(label = myLabel), size=1) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5))) 
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp5_2two ,  path1=path3, fileName1="F_MDS-Labels",   height1=3.5,  width1=6)
  
  FigureTemp6_2two  <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech), label=myLabel )) + 
    geom_point(size=2, alpha=0.7  ) + xlab("Dimension 1") +   ylab("Dimension 2") +   
    #scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) + 
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) + ggrepel::geom_text_repel(aes(label = myLabel  ), size=1) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5))) 
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp6_2two ,  path1=path3, fileName1="G_MDS-Labels",   height1=3.5,  width1=6)
  
}


MyMultidimensionalScaling_1_g <- function(  meLevelMatrix2,   path2,   dataFrame_temp2 ) {
  if( ! file.exists(path2) ) { dir.create(path2, recursive = TRUE) }
  path2_temp1 = paste(path2, "/Classical-Metric-Multidimensional-Scaling", sep="")
  if( ! file.exists(path2_temp1) ) { dir.create(path2_temp1, recursive = TRUE) }
  res.dist_temp1 <- factoextra::get_dist( t(meLevelMatrix2) ,   method = "euclidean")
  mds_temp1 = cmdscale(res.dist_temp1,   k = 2)
  dataframeA_temp1  <- data.frame( myX=mds_temp1[,1],  myY=mds_temp1[,2],  myLabel=rownames(mds_temp1),
                                   mySex= as.vector(dataFrame_temp2$mysex),  myTech=as.vector(dataFrame_temp2$mytech)  ) 
  My_for_MultidimensionalScaling_1_g(  path3=path2_temp1,   dataFrame_temp3=dataframeA_temp1  )
  
  path2_temp2 = paste(path2, "/Nonmetric-Multidimensional-Scaling", sep="")
  if( ! file.exists(path2_temp2) ) { dir.create(path2_temp2, recursive = TRUE) }
  res.dist_temp2 <- res.dist_temp1
  mds_temp2 = MASS::isoMDS(res.dist_temp2,   k = 2)
  dataframeA_temp2  <- data.frame( myX=mds_temp2$points[,1],  myY=mds_temp2$points[,2],  myLabel=rownames(mds_temp2$points),
                                   mySex= as.vector(dataFrame_temp2$mysex),  myTech=as.vector(dataFrame_temp2$mytech)  ) 
  My_for_MultidimensionalScaling_1_g(  path3=path2_temp2,   dataFrame_temp3=dataframeA_temp2  )
  
  cat("\n\n\n\n\nMDS:")
  cat(mds_temp1)
  cat("\n\n")
  print(mds_temp2)
  cat("\n\n\n\n\n")
  
  sink( file=paste(path2, "MDS_info.txt", sep="/") )
  cat("\n\n\n\n\nMDS:\n\n")
  cat(mds_temp1)
  cat("\n\n\n\n\n\n\n")
  print(mds_temp2)
  cat("\n\n\n\n\n")
  sink() 
  
  
  ClassicalMetric_top20 = cmdscale(res.dist_temp1,   k = ncol(meLevelMatrix2)-1  ) 
  Nonmetric_top20       = MASS::isoMDS(res.dist_temp1,   k = ncol(meLevelMatrix2)-1    ) 
  
  sink( file=paste(path2, "MDS_info_top20.txt", sep="/") )
  cat( "Classical-Metric-Multidimensional-Scaling:\n" )
  cat(  ClassicalMetric_top20 )
  cat("\n\n\n\n\n\n\n")
  cat( "Nonmetric-Multidimensional-Scaling:\n" )
  print(  Nonmetric_top20  )
  cat("\n\n\n\n\n")
  sink() 
  
  
  write.table(ClassicalMetric_top20 , 
              file = paste(path2,   "ClassicalMetric_top20.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = TRUE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  write.table(   Nonmetric_top20$points  , 
                 file = paste(path2,   "Nonmetric_top20.txt",  sep="/"), 
                 append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
                 row.names = TRUE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  
  
}


myHierarchicalClustering_1_g  <- function(  mat_3three,   path_temp1,  width1, height1, groups1   )  {
  if( ! file.exists(path_temp1) ) { dir.create(path=path_temp1, recursive = TRUE) }
  
  res.dist1_3three <- factoextra::get_dist( t(mat_3three) ,   method = "euclidean")
  res.dist2_3three <- factoextra::get_dist( t(mat_3three) ,   method = "maximum"  )
  res.dist3_3three <- factoextra::get_dist( t(mat_3three) ,   method = "manhattan")
  res.dist4_3three <- factoextra::get_dist( t(mat_3three) ,   method = "canberra" )
  res.dist5_3three <- factoextra::get_dist( t(mat_3three) ,   method = "binary"   )
  res.dist6_3three <- factoextra::get_dist( t(mat_3three) ,   method = "minkowski")
  res.dist7_3three <- factoextra::get_dist( t(mat_3three) ,   method = "pearson"  )
  res.dist8_3three <- factoextra::get_dist( t(mat_3three) ,   method = "spearman" )
  
  pdf( file = paste(path_temp1, "1A_visualizing-distance-matrix.pdf",  sep="/"), width=12, height=10 )
  print( factoextra::fviz_dist(res.dist1_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) )
  print( factoextra::fviz_dist(res.dist2_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  print( factoextra::fviz_dist(res.dist3_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  print( factoextra::fviz_dist(res.dist4_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  print( factoextra::fviz_dist(res.dist5_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  print( factoextra::fviz_dist(res.dist6_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  print( factoextra::fviz_dist(res.dist7_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  print( factoextra::fviz_dist(res.dist8_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  dev.off() 
  
  sink( file = paste(path_temp1, "1B_all-distance-matrix.txt",  sep="/") )
  print("################### euclidean: ")
  print(res.dist1_3three ) 
  print("################### maximum: ")
  print(res.dist2_3three ) 
  print("################### manhattan: ")
  print(res.dist3_3three ) 
  print("################### canberra: ")
  print(res.dist4_3three ) 
  print("################### binary: ")
  print(res.dist5_3three ) 
  print("################### minkowski: ")
  print(res.dist6_3three ) 
  print("################### pearson: ")
  #print(res.dist7_3three ) 
  print("################### spearman: ")
  #print(res.dist8_3three ) 
  sink() 

  
  # Compute hierarchical clustering by "ward.D"
  res.hc1_3three <- hclust(res.dist1_3three, method = "ward.D"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "ward.D"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "ward.D"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "ward.D"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "ward.D"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "ward.D"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "ward.D"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "ward.D"  ) 
  
  pdf( file = paste(path_temp1, "2A_hierarchical-ward.D-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  factoextra::fviz_dend(res.hc1_3three,   type="rectangle", main="ward.D, euclidean"  )  )
  print(  factoextra::fviz_dend(res.hc2_3three,   type="rectangle", main="ward.D, maximum"    )  )
  print(  factoextra::fviz_dend(res.hc3_3three,   type="rectangle", main="ward.D, manhattan"  )  )
  print(  factoextra::fviz_dend(res.hc4_3three,   type="rectangle", main="ward.D, canberra"   )  )
  print(  factoextra::fviz_dend(res.hc5_3three,   type="rectangle", main="ward.D, binary"     )  )
  print(  factoextra::fviz_dend(res.hc6_3three,   type="rectangle", main="ward.D, minkowski"  )  )
  print(  factoextra::fviz_dend(res.hc7_3three,   type="rectangle", main="ward.D, pearson"    )  )
  print(  factoextra::fviz_dend(res.hc8_3three,   type="rectangle", main="ward.D, spearman"   )  )
  dev.off() 
  
  pdf( file = paste(path_temp1, "2B_hierarchical-ward.D-phylogenic.pdf",  sep="/")  )
  print(  factoextra::fviz_dend(res.hc1_3three,   type="phylogenic", main="ward.D, euclidean"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc2_3three,   type="phylogenic", main="ward.D, maximum"    , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc3_3three,   type="phylogenic", main="ward.D, manhattan"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc4_3three,   type="phylogenic", main="ward.D, canberra"   , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc5_3three,   type="phylogenic", main="ward.D, binary"     , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc6_3three,   type="phylogenic", main="ward.D, minkowski"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc7_3three,   type="phylogenic", main="ward.D, pearson"    , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc8_3three,   type="phylogenic", main="ward.D, spearman"   , repel = TRUE)  )          
  dev.off() 
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use <- as.numeric( groups1 ) 
  length(colors_to_use)
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "2C_hierarchical-ward.D-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="ward.D, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="ward.D, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="ward.D, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="ward.D, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="ward.D, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="ward.D, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="ward.D, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="ward.D, spearman"  )  )          
  dev.off() 
  
  
  
  
  
  
  # Compute hierarchical clustering by "ward.D2"
  res.hc1_3three1 <- hclust(res.dist1_3three, method = "ward.D2"  )   
  res.hc2_3three1 <- hclust(res.dist2_3three, method = "ward.D2"  )   
  res.hc3_3three1 <- hclust(res.dist3_3three, method = "ward.D2"  )  
  res.hc4_3three1 <- hclust(res.dist4_3three, method = "ward.D2"  )   
  res.hc5_3three1 <- hclust(res.dist5_3three, method = "ward.D2"  )  
  res.hc6_3three1 <- hclust(res.dist6_3three, method = "ward.D2"  )  
  res.hc7_3three1 <- hclust(res.dist7_3three, method = "ward.D2"  )  
  res.hc8_3three1 <- hclust(res.dist8_3three, method = "ward.D2"  ) 
  
  pdf( file = paste(path_temp1, "3A_hierarchical-ward.D2-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  factoextra::fviz_dend(res.hc1_3three1,   type="rectangle", main="ward.D2, euclidean"  )  )
  print(  factoextra::fviz_dend(res.hc2_3three1,   type="rectangle", main="ward.D2, maximum"    )  )
  print(  factoextra::fviz_dend(res.hc3_3three1,   type="rectangle", main="ward.D2, manhattan"  )  )
  print(  factoextra::fviz_dend(res.hc4_3three1,   type="rectangle", main="ward.D2, canberra"   )  )
  print(  factoextra::fviz_dend(res.hc5_3three1,   type="rectangle", main="ward.D2, binary"     )  )
  print(  factoextra::fviz_dend(res.hc6_3three1,   type="rectangle", main="ward.D2, minkowski"  )  )
  print(  factoextra::fviz_dend(res.hc7_3three1,   type="rectangle", main="ward.D2, pearson"    )  )
  print(  factoextra::fviz_dend(res.hc8_3three1,   type="rectangle", main="ward.D2, spearman"   )  )
  dev.off() 
  
  pdf( file = paste(path_temp1, "3B_hierarchical-ward.D2-phylogenic.pdf",  sep="/")  )
  print(  factoextra::fviz_dend(res.hc1_3three1,   type="phylogenic", main="ward.D2, euclidean"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc2_3three1,   type="phylogenic", main="ward.D2, maximum"    , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc3_3three1,   type="phylogenic", main="ward.D2, manhattan"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc4_3three1,   type="phylogenic", main="ward.D2, canberra"   , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc5_3three1,   type="phylogenic", main="ward.D2, binary"     , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc6_3three1,   type="phylogenic", main="ward.D2, minkowski"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc7_3three1,   type="phylogenic", main="ward.D2, pearson"    , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc8_3three1,   type="phylogenic", main="ward.D2, spearman"   , repel = TRUE)  )          
  dev.off() 
  
  res.hca.color1_3three1 <- as.dendrogram( res.hc1_3three1 )   
  res.hca.color2_3three1 <- as.dendrogram( res.hc2_3three1 )   
  res.hca.color3_3three1 <- as.dendrogram( res.hc3_3three1 )  
  res.hca.color4_3three1 <- as.dendrogram( res.hc4_3three1 )   
  res.hca.color5_3three1 <- as.dendrogram( res.hc5_3three1 )  
  res.hca.color6_3three1 <- as.dendrogram( res.hc6_3three1 )  
  res.hca.color7_3three1 <- as.dendrogram( res.hc7_3three1 )  
  res.hca.color8_3three1 <- as.dendrogram( res.hc8_3three1 ) 
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three1)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three1)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three1)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three1)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three1)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three1)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three1)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three1)]
  
  dendextend::labels_colors(res.hca.color1_3three1) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three1) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three1) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three1) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three1) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three1) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three1) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three1) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "3C_hierarchical-ward.D2-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three1,   main="ward.D2, euclidean" )  )
  print(  plot(res.hca.color2_3three1,   main="ward.D2, maximum"   )  )
  print(  plot(res.hca.color3_3three1,   main="ward.D2, manhattan" )  )
  print(  plot(res.hca.color4_3three1,   main="ward.D2, canberra"  )  )
  print(  plot(res.hca.color5_3three1,   main="ward.D2, binary"    )  )
  print(  plot(res.hca.color6_3three1,   main="ward.D2, minkowski" )  )
  print(  plot(res.hca.color7_3three1,   main="ward.D2, pearson"   )  )
  print(  plot(res.hca.color8_3three1,   main="ward.D2, spearman"  )  )          
  dev.off() 
  
  
  
  
  
  
  # Compute hierarchical clustering by "single"
  res.hc1_3three <- hclust(res.dist1_3three, method = "single"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "single"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "single"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "single"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "single"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "single"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "single"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "single"  ) 
  
  pdf( file = paste(path_temp1, "4A_hierarchical-single-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  factoextra::fviz_dend(res.hc1_3three,   type="rectangle", main="single, euclidean"  )  )
  print(  factoextra::fviz_dend(res.hc2_3three,   type="rectangle", main="single, maximum"    )  )
  print(  factoextra::fviz_dend(res.hc3_3three,   type="rectangle", main="single, manhattan"  )  )
  print(  factoextra::fviz_dend(res.hc4_3three,   type="rectangle", main="single, canberra"   )  )
  print(  factoextra::fviz_dend(res.hc5_3three,   type="rectangle", main="single, binary"     )  )
  print(  factoextra::fviz_dend(res.hc6_3three,   type="rectangle", main="single, minkowski"  )  )
  print(  factoextra::fviz_dend(res.hc7_3three,   type="rectangle", main="single, pearson"    )  )
  print(  factoextra::fviz_dend(res.hc8_3three,   type="rectangle", main="single, spearman"   )  )
  dev.off() 
  
  pdf( file = paste(path_temp1, "4B_hierarchical-single-phylogenic.pdf",  sep="/") )
  print(  factoextra::fviz_dend(res.hc1_3three,   type="phylogenic", main="single, euclidean"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc2_3three,   type="phylogenic", main="single, maximum"    , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc3_3three,   type="phylogenic", main="single, manhattan"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc4_3three,   type="phylogenic", main="single, canberra"   , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc5_3three,   type="phylogenic", main="single, binary"     , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc6_3three,   type="phylogenic", main="single, minkowski"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc7_3three,   type="phylogenic", main="single, pearson"    , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc8_3three,   type="phylogenic", main="single, spearman"   , repel = TRUE)  )         
  dev.off() 
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "4C_hierarchical-single-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="single, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="single, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="single, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="single, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="single, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="single, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="single, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="single, spearman"  )  )          
  dev.off() 
  
   
  
  
  # Compute hierarchical clustering by "complete"
  res.hc1_3three <- hclust(res.dist1_3three, method = "complete"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "complete"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "complete"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "complete"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "complete"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "complete"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "complete"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "complete"  ) 
  
  pdf( file = paste(path_temp1, "5A_hierarchical-complete-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  factoextra::fviz_dend(res.hc1_3three,   type="rectangle", main="complete, euclidean"  )  )
  print(  factoextra::fviz_dend(res.hc2_3three,   type="rectangle", main="complete, maximum"    )  )
  print(  factoextra::fviz_dend(res.hc3_3three,   type="rectangle", main="complete, manhattan"  )  )
  print(  factoextra::fviz_dend(res.hc4_3three,   type="rectangle", main="complete, canberra"   )  )
  print(  factoextra::fviz_dend(res.hc5_3three,   type="rectangle", main="complete, binary"     )  )
  print(  factoextra::fviz_dend(res.hc6_3three,   type="rectangle", main="complete, minkowski"  )  )
  print(  factoextra::fviz_dend(res.hc7_3three,   type="rectangle", main="complete, pearson"    )  )
  print(  factoextra::fviz_dend(res.hc8_3three,   type="rectangle", main="complete, spearman"   )  )
  dev.off() 
  
  pdf( file = paste(path_temp1, "5B_hierarchical-complete-phylogenic.pdf",  sep="/") )
  print(  factoextra::fviz_dend(res.hc1_3three,   type="phylogenic", main="complete, euclidean"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc2_3three,   type="phylogenic", main="complete, maximum"    , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc3_3three,   type="phylogenic", main="complete, manhattan"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc4_3three,   type="phylogenic", main="complete, canberra"   , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc5_3three,   type="phylogenic", main="complete, binary"     , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc6_3three,   type="phylogenic", main="complete, minkowski"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc7_3three,   type="phylogenic", main="complete, pearson"    , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc8_3three,   type="phylogenic", main="complete, spearman"   , repel = TRUE)  )          
  dev.off() 
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "5C_hierarchical-complete-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="complete, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="complete, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="complete, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="complete, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="complete, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="complete, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="complete, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="complete, spearman"  )  )          
  dev.off() 
  
  
  
  
  
  
  # Compute hierarchical clustering by "average"
  res.hc1_3three <- hclust(res.dist1_3three, method = "average"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "average"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "average"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "average"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "average"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "average"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "average"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "average"  ) 
  
  pdf( file = paste(path_temp1, "6A_hierarchical-average-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  factoextra::fviz_dend(res.hc1_3three,   type="rectangle", main="average, euclidean"  )  )
  print(  factoextra::fviz_dend(res.hc2_3three,   type="rectangle", main="average, maximum"    )  )
  print(  factoextra::fviz_dend(res.hc3_3three,   type="rectangle", main="average, manhattan"  )  )
  print(  factoextra::fviz_dend(res.hc4_3three,   type="rectangle", main="average, canberra"   )  )
  print(  factoextra::fviz_dend(res.hc5_3three,   type="rectangle", main="average, binary"     )  )
  print(  factoextra::fviz_dend(res.hc6_3three,   type="rectangle", main="average, minkowski"  )  )
  print(  factoextra::fviz_dend(res.hc7_3three,   type="rectangle", main="average, pearson"    )  )
  print(  factoextra::fviz_dend(res.hc8_3three,   type="rectangle", main="average, spearman"   )  )
  dev.off() 
  
  pdf( file = paste(path_temp1, "6B_hierarchical-average-phylogenic.pdf",  sep="/") )
  print(  factoextra::fviz_dend(res.hc1_3three,   type="phylogenic", main="average, euclidean"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc2_3three,   type="phylogenic", main="average, maximum"    , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc3_3three,   type="phylogenic", main="average, manhattan"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc4_3three,   type="phylogenic", main="average, canberra"   , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc5_3three,   type="phylogenic", main="average, binary"     , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc6_3three,   type="phylogenic", main="average, minkowski"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc7_3three,   type="phylogenic", main="average, pearson"    , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc8_3three,   type="phylogenic", main="average, spearman"   , repel = TRUE)  )          
  dev.off() 
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "6C_hierarchical-average-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="average, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="average, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="average, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="average, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="average, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="average, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="average, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="average, spearman"  )  )          
  dev.off() 
  
  
  
  
  
  
  # Compute hierarchical clustering by "mcquitty"
  res.hc1_3three <- hclust(res.dist1_3three, method = "mcquitty"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "mcquitty"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "mcquitty"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "mcquitty"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "mcquitty"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "mcquitty"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "mcquitty"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "mcquitty"  ) 
  
  pdf( file = paste(path_temp1, "7A_hierarchical-mcquitty-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  factoextra::fviz_dend(res.hc1_3three,   type="rectangle", main="mcquitty, euclidean"  )  )
  print(  factoextra::fviz_dend(res.hc2_3three,   type="rectangle", main="mcquitty, maximum"    )  )
  print(  factoextra::fviz_dend(res.hc3_3three,   type="rectangle", main="mcquitty, manhattan"  )  )
  print(  factoextra::fviz_dend(res.hc4_3three,   type="rectangle", main="mcquitty, canberra"   )  )
  print(  factoextra::fviz_dend(res.hc5_3three,   type="rectangle", main="mcquitty, binary"     )  )
  print(  factoextra::fviz_dend(res.hc6_3three,   type="rectangle", main="mcquitty, minkowski"  )  )
  print(  factoextra::fviz_dend(res.hc7_3three,   type="rectangle", main="mcquitty, pearson"    )  )
  print(  factoextra::fviz_dend(res.hc8_3three,   type="rectangle", main="mcquitty, spearman"   )  )
  dev.off() 
  
  pdf( file = paste(path_temp1, "7B_hierarchical-mcquitty-phylogenic.pdf",  sep="/") )
  print(  factoextra::fviz_dend(res.hc1_3three,   type="phylogenic", main="mcquitty, euclidean"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc2_3three,   type="phylogenic", main="mcquitty, maximum"    , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc3_3three,   type="phylogenic", main="mcquitty, manhattan"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc4_3three,   type="phylogenic", main="mcquitty, canberra"   , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc5_3three,   type="phylogenic", main="mcquitty, binary"     , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc6_3three,   type="phylogenic", main="mcquitty, minkowski"  , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc7_3three,   type="phylogenic", main="mcquitty, pearson"    , repel = TRUE)  )
  print(  factoextra::fviz_dend(res.hc8_3three,   type="phylogenic", main="mcquitty, spearman"   , repel = TRUE)  )          
  dev.off() 
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "7C_hierarchical-mcquitty-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="mcquitty, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="mcquitty, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="mcquitty, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="mcquitty, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="mcquitty, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="mcquitty, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="mcquitty, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="mcquitty, spearman"  )  )          
  dev.off() 
  
  
  
  
  
  
  # Compute hierarchical clustering by "median"
  res.hc1_3three <- hclust(res.dist1_3three, method = "median"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "median"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "median"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "median"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "median"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "median"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "median"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "median"  ) 
  
  pdf( file = paste(path_temp1, "8A_hierarchical-median-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  factoextra::fviz_dend(res.hc1_3three,   type="rectangle", main="median, euclidean"  )  )
  print(  factoextra::fviz_dend(res.hc2_3three,   type="rectangle", main="median, maximum"    )  )
  print(  factoextra::fviz_dend(res.hc3_3three,   type="rectangle", main="median, manhattan"  )  )
  print(  factoextra::fviz_dend(res.hc4_3three,   type="rectangle", main="median, canberra"   )  )
  print(  factoextra::fviz_dend(res.hc5_3three,   type="rectangle", main="median, binary"     )  )
  print(  factoextra::fviz_dend(res.hc6_3three,   type="rectangle", main="median, minkowski"  )  )
  print(  factoextra::fviz_dend(res.hc7_3three,   type="rectangle", main="median, pearson"    )  )
  print(  factoextra::fviz_dend(res.hc8_3three,   type="rectangle", main="median, spearman"   )  )
  dev.off() 
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "8C_hierarchical-median-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="median, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="median, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="median, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="median, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="median, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="median, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="median, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="median, spearman"  )  )          
  dev.off() 
  
  
  
  
  
  # Compute hierarchical clustering by "centroid"
  res.hc1_3three <- hclust(res.dist1_3three, method = "centroid"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "centroid"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "centroid"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "centroid"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "centroid"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "centroid"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "centroid"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "centroid"  ) 
  
  pdf( file = paste(path_temp1, "9A_hierarchical-centroid-rectangle.pdf",  sep="/"), width=width1, height=height1  )
  print(  factoextra::fviz_dend(res.hc1_3three,   type="rectangle", main="centroid, euclidean"  )  )
  print(  factoextra::fviz_dend(res.hc2_3three,   type="rectangle", main="centroid, maximum"    )  )
  print(  factoextra::fviz_dend(res.hc3_3three,   type="rectangle", main="centroid, manhattan"  )  )
  print(  factoextra::fviz_dend(res.hc4_3three,   type="rectangle", main="centroid, canberra"   )  )
  print(  factoextra::fviz_dend(res.hc5_3three,   type="rectangle", main="centroid, binary"     )  )
  print(  factoextra::fviz_dend(res.hc6_3three,   type="rectangle", main="centroid, minkowski"  )  )
  print(  factoextra::fviz_dend(res.hc7_3three,   type="rectangle", main="centroid, pearson"    )  )
  print(  factoextra::fviz_dend(res.hc8_3three,   type="rectangle", main="centroid, spearman"   )  )
  dev.off() 
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "9C_hierarchical-centroid-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="centroid, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="centroid, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="centroid, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="centroid, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="centroid, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="centroid, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="centroid, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="centroid, spearman"  )  )          
  dev.off() 
  
  
}

##############################################################################################################################################################################################





##############################################################################################################################################################################################
DF1 <- read.table(input_g, header=TRUE,   sep="\t", comment.char = "", quote ="" )    
dim(DF1)
matrix1   = DF1[ , -c(1:6)]
region1   = DF1[ ,  c(1:6)]
colname1A  = colnames(DF1)
colname1B  = colname1A[-c(1:6)]
## colname1A
## colname1B

sink( paste(outDir_g,  "/1B_input-matrix.infor.txt",  sep = "") )
    print("########################################################################")
    print("################### input matrix:")
    print( dim(DF1)  ) 
    print( dim(matrix1)  ) 
    print( dim(region1)  ) 
    print( length(colname1A)  )
    print( length(colname1B)  )
    print("########################################################################")
    print( paste("Max",             max(matrix1),                 sep=":  ") )
    print( paste("Min",             min(matrix1),                 sep=":  ") )
    print( paste("Mean",            mean( as.numeric(unlist(matrix1) ) ),                sep=":  ") )
    print("########################################################################")
    print( "#" )
    print(  summary(as.numeric(unlist(matrix1) )) )
    print( "#" )
    print("########################################################################")
    print( DF1[1:5, 1:9],            sep="  ") 
    print("########################################################################")
    print( colnames(DF1)[1:10],      sep="  ") 
    print("########################################################################")
    print( rownames(DF1)[1:10],      sep="  ")    
    print("########################################################################")
sink()
   
    




DF2 <- read.table( samplesInformation_g , header=T, sep="\t", quote = "", comment.char = "") 
dim(DF2)

sink( paste(outDir_g,  "/1C_samples.infor.txt",  sep = "") )
print("########################################################################")
print("################### matrix with samples information:" )
print( dim(DF2)  ) 
print("########################################################################")
print( DF2[1:5, ],            sep="  ") 
print("########################################################################")
print( colnames(DF2) ,      sep="  ") 
print("########################################################################")
print( rownames(DF2)[1:10],      sep="  ")    
print("########################################################################")
sink()
 



colname1B = str_replace_all( string=colname1B, pattern=".IN.featureCounts", replacement="" )
colname1B = str_replace_all( string=colname1B, pattern="^sample\\d+\\.\\.\\.", replacement="" )
matrix1 <- matrix(as.numeric(unlist(matrix1)), ncol = ncol(matrix1))   # Convert to numeric matrix 
colnames(matrix1) <- colname1B



index1 =  order(colname1B)  
matrix2 = matrix1[, index1]  ## columns sorted.

mycolname2 = DF2$name_2
index2 =  order(mycolname2) 
DF3 = DF2[index2, ]

bool1 = ( colnames(matrix2) == DF3[,2] )
length(bool1[bool1])


sink( paste(outDir_g,  "/1D_samples.groups.sorted.txt",  sep = "") )
print("########################################################################")
print( dim(DF3)  ) 
print("########################################################################")
print( DF3[1:5, ],            sep="  ") 
print("########################################################################")
print( table( DF3$sex ) )
print( table( DF3$age_group ) )
print( table( DF3$age_group2 ) )
print( table( DF3$race ) )
print( table( DF3$region ) )
print("########################################################################")
print( length(bool1[bool1])  )
print("########################################################################")
sink()
 


write.table( cbind(region1, matrix2),    file = paste(outDir_g, "2A.matrix.columnsSorted.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t", row.names = F, col.names = TRUE )
write.table( cbind(DF3, colnames(matrix2) ),    file = paste(outDir_g, "2B.matrix.samplesInfor.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t", row.names = F, col.names = TRUE )
##############################################################################################################################################################################################



 
##############################################################################################################################################################################################
col_index = which( colnames(DF3) == type_g)
boolA =   (DF3[, col_index] == groupA_g)  
boolB =   (DF3[, col_index] == groupB_g)  
if(groupB_g == "others"){ boolB = ! boolA }

write.table( cbind(DF3, colnames(matrix2), boolA, boolB ),    file = paste(outDir_g, "2C.matrix.bool.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t", row.names = F, col.names = TRUE )

matrix_A = matrix2[, boolA]
matrix_B = matrix2[, boolB]

sink( paste(outDir_g,  "/2D_bool.txt",  sep = "") )
print("########################################################################")
print( length(boolA)  ) 
print( length(boolA[boolA])  ) 
print("########################################################################")
print( length(boolB)  ) 
print( length(boolB[boolB])  ) 
print("########################################################################")
print( dim(matrix_A)  ) 
print( dim(matrix_B)  ) 
print("########################################################################")
print( "#"   ) 
print( colnames( matrix_A )  ) 
print("########################################################################")
print( colnames( matrix_B )  ) 
print("########################################################################")
sink()
 
 
myGroups <- as.factor(  c( rep("A", ncol(matrix_A)),   rep("B", ncol(matrix_B))  )   )
myRPKM2  <- cbind(matrix_A, matrix_B) 
myColors <- brewer.pal(6, "Set2")


bool1 = ( colnames(matrix2) == DF3[,2] )
length(bool1[bool1])


dim(DF3) 
DF4  <- rbind( DF3[boolA,],   DF3[boolB,] ) 
bool2 = ( colnames(myRPKM2) == DF4[,2] )
length(bool2[bool2])





sink( paste(outDir_g,  "/2E_two-groups.txt",  sep = "") )
print("########################################################################")
print( myGroups ) 
print( length( myGroups )  ) 
print( table(myGroups)  ) 
print("########################################################################")
print( dim(myRPKM2)  ) 
print("########################################################################")
print( myColors[myGroups]  ) 
print("########################################################################")
print( dim(region1)  ) 
print( dim(matrix_A)  ) 
print( dim(matrix_B)  ) 
print( dim(myRPKM2)  ) 
print("########################################################################")
sink()
 

write.table( cbind( region1, matrix_A ),    file = paste(outDir_g, "2F.Group1.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
             eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table( cbind( region1, matrix_B ),    file = paste(outDir_g, "2F.Group2.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
             eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table( cbind( region1, myRPKM2 ),    file = paste(outDir_g, "2F.selected.All.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
             eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

##############################################################################################################################################################################################

 


 

##############################################################################################################################################################################################
## RLE = log-ratio of read count to median read count across sample
pdf(paste(outDir_g, "3A.RLE.figures_for_rawCounts.pdf", sep="/"), width=20, height=6)
plotRLE( myRPKM2, outline=FALSE, ylim=c(-1.5, 1.5), col=myColors[myGroups] )
dev.off()


myRPKM2_L <- myRPKM2
min_L <- min( myRPKM2_L )
if(min_L <= 0){myRPKM2_L = myRPKM2_L - min_L + 1}

pdf(paste(outDir_g, "3B.PCA.figures_for_rawCounts.pdf", sep="/"), width=6, height=6)
plotPCA(myRPKM2_L, col=myColors[myGroups], cex=0.5, labels=F , k=4)
plotPCA(myRPKM2_L, col=myColors[myGroups], cex=0.5, labels=F , k=3)
plotPCA(myRPKM2_L, col=myColors[myGroups], cex=0.5, labels=F   )
plotPCA(myRPKM2_L, col=myColors[myGroups], cex=0.5, labels=TRUE)
dev.off()


quan_11  <- quantile(x=myRPKM2, probs = seq(0, 1, 0.01) )
myRPKM2A <- myRPKM2
down1    <- quan_11[3]
up1      <- quan_11[99]
myRPKM2A[myRPKM2A < down1] = down1
myRPKM2A[myRPKM2A > up1]   = up1

pdf(paste(outDir_g, "3C.hist.rawCounts.pdf", sep="/"), width=10, height=6)
hist(  as.vector( myRPKM2  ), breaks=1000  )
hist(  as.vector( myRPKM2A ), breaks=1000  )
dev.off()

 

## PCA
library(factoextra)
prcompObj2         <- prcomp( t(myRPKM2)  )
prcompObj2_matrix  <- prcompObj2$x
dim(prcompObj2_matrix)
prcompObj2_Contri  <- (prcompObj2$sdev)^2
prcompObj2_Contri  <- prcompObj2_Contri/sum(prcompObj2_Contri)
prcompObj2_Contri  <- prcompObj2_Contri * 100
prcompObj2_Contri  <- round(prcompObj2_Contri, 2)
label1_2two <-   paste( "PC1 ",  "(", prcompObj2_Contri[1], "%)", sep="" )
label2_2two <-   paste( "PC2 ",  "(", prcompObj2_Contri[2], "%)", sep="" )
label3_2two <-   paste( "PC3 ",  "(", prcompObj2_Contri[3], "%)", sep="" ) 

write.table(prcompObj2_Contri ,  file = paste(outDir_g,   "3D.PCA_info_allContribution.txt",  sep="/"), 
            append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".",  row.names = TRUE,  col.names = TRUE )

dataframeA_2two  <- data.frame( as.data.frame(prcompObj2_matrix),  myTech=myGroups, myType=myGroups, myLabel=colnames(myRPKM2) )


FigureTemp1_2two  <- ggplot( data = dataframeA_2two, aes(x = PC1, y = PC2,    color=as.factor(myType) )) + 
  geom_point(size=1, alpha=1  ) + xlab(label1_2two) +   ylab(label2_2two) +   
  scale_colour_manual(values= c("yellow4", "cyan", "purple", "green", "red", "blue")  ) +  
  MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
  guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5)))  
MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1_2two , path1=outDir_g, fileName1="3.1.PCA-1", height1=3.5, width1=5)

FigureTemp1_2two  <- ggplot( data = dataframeA_2two, aes(x = PC1, y = PC2,    color=as.factor(myTech) )) + 
  geom_point(size=1, alpha=1  ) + xlab(label1_2two) +   ylab(label2_2two) +   
  scale_colour_manual(values= c("yellow4", "cyan", "purple", "green", "red", "blue")  ) +  
  MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
  guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5)))  
MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1_2two , path1=outDir_g, fileName1="3a.1.PCA-1", height1=3.5, width1=5)

FigureTemp1_2two  <- ggplot( data = dataframeA_2two, aes(x = PC1, y = PC2,    color=as.factor(myTech) )) + 
  geom_point(size=2, alpha=0.5  ) + xlab(label1_2two) +   ylab(label2_2two) +   
  scale_colour_manual(values= c("yellow4", "cyan", "purple", "green", "red", "blue")  ) +  
  MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
  guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5)))  
MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1_2two , path1=outDir_g, fileName1="3b.1.PCA-2", height1=3.5, width1=5)

FigureTemp1_2two  <- ggplot( data = dataframeA_2two, aes(x = PC1, y = PC2, shape=as.factor(myType),   color=as.factor(myTech) )) + 
  geom_point(size=0.6, alpha=1  ) + xlab(label1_2two) +   ylab(label2_2two) +   
  scale_colour_manual(values= c("yellow4", "cyan", "purple", "green", "red", "blue")  ) +  
  MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
  guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5)))  
MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1_2two , path1=outDir_g, fileName1="3c.1.PCA-3", height1=3.5, width1=5)


FigureTemp1_2two  <- ggplot( data = dataframeA_2two, aes(x = PC1, y = PC2, shape=as.factor(myType),   color=as.factor(myTech) )) + 
  geom_point(size=1.5, alpha=0.7  ) + xlab(label1_2two) +   ylab(label2_2two) +   
  scale_colour_manual(values= c("yellow4", "cyan", "purple", "green", "red", "blue")  ) +  
  MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) + ggrepel::geom_text_repel(aes(label = myLabel), size=1) +
  guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5)))  
MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1_2two , path1=outDir_g, fileName1="3d.1.PCA-4", height1=3.5, width1=5)





## The function rcorr() [in Hmisc package] can be used to compute the significance levels for pearson and spearman correlations. 
## It returns both the correlation coefficients and the p-value of the correlation for all possible pairs of columns in the data table.
res2 <- rcorr(  as.matrix(myRPKM2, type="pearson") )
my_col3=colorRampPalette( c( "cyan4",  "cyan",  "white", "red",   "red4"),  bias = 1,  space = "rgb" )
correlaton_1 = as.matrix( res2$r )
correlaton_1 = round(correlaton_1, digits = 2)

pdf( file = paste(outDir_g, "3E.HierarchicalClustering.heatmap.pdf", sep="/"),  width=7, height=8  )
corrplot(correlaton_1, method = "color", type = "full",  title = "", is.corr = FALSE,  order = "original",  tl.col = "black", tl.srt = 90, col = my_col3(100) )
##text(  col(correlaton_1), nrow(correlaton_1)+1-row(correlaton_1),   correlaton_1, cex=1.5)
dev.off() 

pdf( file = paste(outDir_g, "3F.HierarchicalClustering.heatmap.cluster.pdf", sep="/"),  width=7, height=7  )
heatmap.2(x= correlaton_1 ,  dendrogram = "both",  scale =  "none" , na.rm=TRUE,  col=my_col3(100), trace = "none",     notecex=1.5, notecol="black", na.color=par("bg") )    
dev.off() 


myHierarchicalClustering_1_g(  mat_3three  = myRPKM2,   path_temp1=paste(outDir_g, "3G.HierarchicalClustering", sep="/"),  width1=15, height1=6,  groups1 = myGroups )

## dim(myRPKM2)
## length( myGroups )
###########################################################################################################################################################################




 

 



###########################################################################################################################################################################
pvalues_T = vector( length = nrow(myRPKM2) )
pvalues_W = vector( length = nrow(myRPKM2) )

for(i in c(1:nrow(myRPKM2)) ) {  
  x1  =  as.numeric( as.vector( matrix_A[i,] ) ) 
  y1  =  as.numeric( as.vector( matrix_B[i,] ) ) 
  re_T = t.test(x=x1, y = y1,  alternative = "two.sided")
  re_W = wilcox.test(x=x1, y = y1,  alternative = "two.sided" )
  pvalues_T[i] = re_T$p.value
  pvalues_W[i] = re_W$p.value
}

FDR_T = p.adjust( pvalues_T , method = "fdr", n = length(pvalues_T) )  ##  c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr", "none")
FDR_W = p.adjust( pvalues_W , method = "fdr", n = length(pvalues_W) )  ##  c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr", "none")


sink( paste(outDir_g,  "/4A_t.test_wilcox.test.txt",  sep = "") )
print("########################################################################pvalues_T ")
print( length( pvalues_T[ pvalues_T < 0.05] ) ) 
print( length( pvalues_T[ pvalues_T < 0.01] ) ) 
print( length( pvalues_T[ pvalues_T < 0.001] ) ) 
print("########################################################################pvalues_W ")
print( length( pvalues_W[ pvalues_W < 0.05] )  ) 
print( length( pvalues_W[ pvalues_W < 0.01] )  ) 
print( length( pvalues_W[ pvalues_W < 0.001] )  ) 
print("########################################################################")
print("########################################################################FDR_T ")
print( length( FDR_T[ FDR_T < 0.05] ) ) 
print( length( FDR_T[ FDR_T < 0.1] ) ) 
print( length( FDR_T[ FDR_T < 0.2] ) ) 
print("########################################################################FDR_W ")
print( length( FDR_W[ FDR_W < 0.05] )  ) 
print( length( FDR_W[ FDR_W < 0.1] )  ) 
print( length( FDR_W[ FDR_W < 0.2] )  ) 
print("########################################################################")
sink()
 


T_W_test <- cbind(pvalues_T,  FDR_T, pvalues_W,  FDR_W ) 
## dim(T_W_test)

write.table( cbind(region1, T_W_test),    file = paste(outDir_g, "4B_t.test_wilcox.test_resutls.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )
###########################################################################################################################################################################






###########################################################################################################################################################################
print("RADAR call......")
 
myRPKM2[1:5, 1:5]
dim(myRPKM2)
dim(region1)

radar1                               <- MeRIP.RADAR()
variable(radar1)                     <- data.frame( myGroups )    
radar1@ip_adjExpr_filtered           <- 2^myRPKM2
rownames(radar1@ip_adjExpr_filtered) <- region1[,4]
samplenames(radar1)                  <- colnames(myRPKM2)


DF4[1:5,]
X2  <- as.fumeric( DF4[, 7]  ) - 1  # sex as covariates
X3  <- as.fumeric( DF4[, 10] ) - 1  # age as covariates
X4  <- as.fumeric( DF4[, 11] ) - 1  # race as covariates
cov <- cbind(X2,X3,X4)
dim(cov)
variable(radar1) <- cbind(variable(radar1), cov)   ## only for different brain regions comparison


radar2 <- diffIP_parallel( radar1 , exclude = NULL, maxPsi = 100, fdrBy = "fdr", thread = 16 )

## getSlots( radar2 )
head(radar2@test.est)
dim(radar2@test.est)
colnames(radar2@test.est) 

 
save(radar2, file = paste(outDir_g, "5A.radar_out.RData", sep="/")   )
write.table(radar2@test.est,    file = paste(outDir_g, "5B.radar_out.data.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",    eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table( cbind(region1, radar2@test.est),    file = paste(outDir_g, "5B.radar_results.bed", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )
###########################################################################################################################################################################





###########################################################################################################################################################################

rots_re = radar2@test.est
rots_re2 = cbind(region1, rots_re, T_W_test)   
rots_re3 = cbind(rots_re2,  myRPKM2 )   
rots_re4 = cbind(rots_re2,  matrix2 )  
dim( rots_re2 )
dim( rots_re3 )
dim( rots_re4 )

write.table(rots_re2,    file = paste(outDir_g, "6A.All.test-resutls.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re3,    file = paste(outDir_g, "6B.All.2groups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re4,    file = paste(outDir_g, "6C.All.allGroups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )
###########################################################################################################################################################################




 














