



outDir_g = "Filter_Wilcox_ROTS"
if( ! file.exists(outDir_g)   ) { dir.create(outDir_g,   recursive = TRUE) }





##############################################################################################################################################################################################
DF_A <- read.table("1A.matrix.regions.txt",  header=TRUE,   sep="\t", comment.char = "" )     
dim( DF_A )

DF_A[1:20,  ]

rawMatrix_1 <- DF_A[, -c(1:3)]
dim(rawMatrix_1)
rawMatrix_1[1:10,  ] 




## Top 30%
TopXpercent <- function(x) {
  x = as.numeric( x )
  tempX = as.numeric( quantile(x, probs = seq(0, 1, 0.1),  na.rm = TRUE ) )   
  return( tempX[8] )
}

Xpercent     <- apply(X = rawMatrix_1,    MARGIN=1, FUN=TopXpercent )
length(Xpercent)
length(Xpercent[Xpercent > 10] )

bool1 <- (Xpercent > 10)
length(bool1)
length(bool1[bool1])




DF_B        <- DF_A[bool1 , ]
rawMatrix_2 <- rawMatrix_1[bool1 , ]
dim(DF_B)
dim(rawMatrix_2)
DF_B[1:5,]

write.table( DF_B ,   file = paste(outDir_g, "1.matrix_withRegions.txt", sep="/"), 
             append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".",  row.names = F,  col.names = F )

write.table( rawMatrix_2 ,   file = paste(outDir_g, "1.matrix.txt", sep="/"), 
             append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".",  row.names = F,  col.names = F )
 



rawMatrix_2[1:5, ]


matrix_A = rawMatrix_2[, c(1:3)]
matrix_B = rawMatrix_2[, c(4:6)]
matrix_C = rawMatrix_2[, c(7:9)]


myGroups <- as.factor(  c( rep("FTO", 3),   rep("m", 3  ),   rep("WT", 3  )   )  )
myRPKM2  <- cbind( as.matrix(matrix_A), as.matrix(matrix_B), as.matrix(matrix_C) )
dim(myRPKM2)
colnames(myRPKM2)

suppressPackageStartupMessages( library(RColorBrewer) )
myColors <- brewer.pal(6, "Set2")



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

## RLE = log-ratio of read count to median read count across sample
pdf(paste(outDir_g, "2A.RLE.figures_for_rawCounts.pdf", sep="/"), width=20, height=6)
plotRLE( myRPKM2, outline=FALSE, ylim=c(-2, 2), col=myColors[myGroups] )
dev.off()










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




myRPKM2[1:5, ]
MyBoxViolinPlot_1_f(vector2 = as.vector( myRPKM2 ),   sampleType2 = rep( colnames(myRPKM2) , each = nrow(myRPKM2) ),  
                    path2=outDir_g,   fileName2="2B",  title2="",  xLab2="Samples",  yLab2="Reads density",    
                    height2=4,   width2=4,   Ymin2=0, Ymax2=90)










###########################################################################################################################################################################
print("ROTS call......")
 
myGroups2 = as.vector(myGroups) 
myGroups2[myGroups2 == "FTO"] = 1
myGroups2[myGroups2 == "m"] = 2
myGroups2[myGroups2 == "WT"] = 3
myGroups2
table( myGroups2 )

rots_out <- ROTS(data= as.matrix(myRPKM2[, 1:6]) , groups=myGroups2[1:6], B = 1000,   paired = FALSE,   seed = 14,   log = FALSE,   progress = TRUE, verbose = TRUE)  ## log,	 A logical (deafult TRUE) indicating whether input data is log2 scaled. This information is only used to calculate log fold change.

save(rots_out, file = paste(outDir_g, "FTO-vs-m..5A.rots_out.RData", sep="/")   )
write.table(rots_out$data,    file = paste(outDir_g, "FTO-vs-m..FTO-vs-m..5B.rots_out.data.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",    eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )



sink( paste(outDir_g,  "/FTO-vs-m..5C_ROTS.txt",  sep = "") )
print("######################################################################## myBool for log2: ")
print("######################################################################## myGroups2 ")
print( length( myGroups2) ) 
print( myGroups2 ) 
print( table( myGroups2 ) ) 
print("######################################################################## rots_out ")
print(  names(rots_out)) 
print("########################################################################")
print("######################################################################## p ")
print( length( rots_out$pvalue[ rots_out$pvalue < 0.05] ) ) 
print( length( rots_out$pvalue[ rots_out$pvalue < 0.01] ) ) 
print( length( rots_out$pvalue[ rots_out$pvalue < 0.001] ) ) 
print("########################################################################FDR ")
print( length( rots_out$FDR[ rots_out$FDR < 0.2] )  ) 
print( length( rots_out$FDR[ rots_out$FDR < 0.1] )  ) 
print( length( rots_out$FDR[ rots_out$FDR < 0.05] )  ) 
print("########################################################################")
sink()
sink()


rots_re <- cbind( rots_out$d, rots_out$logfc, rots_out$pvalue, rots_out$FDR)
colnames(rots_re) <- c( " ROTS-statistic",  "ROTS.logFC", "ROTS.p-value",  "ROTS.FDR"  )

write.table( cbind(DF_B, rots_re),    file = paste(outDir_g, "FTO-vs-m..5D.rots_results.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
             eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )
###########################################################################################################################################################################





###########################################################################################################################################################################
print("########################################################################")
print("########################################################################")
rots_re2 = cbind(DF_B[,1:3], rots_re )   
rots_re3 = cbind(rots_re2,  myRPKM2 )   
rots_re4 = cbind(rots_re2,  DF_B )  
dim( rots_re2 )
dim( rots_re3 )
dim( rots_re4 )

write.table(rots_re2,    file = paste(outDir_g, "FTO-vs-m..6A.All.test-resutls.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re3,    file = paste(outDir_g, "FTO-vs-m..6B.All.2groups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re4,    file = paste(outDir_g, "FTO-vs-m..6C.All.allGroups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )
###########################################################################################################################################################################





###########################################################################################################################################################################
boolDiff1 = ( rots_out$FDR < 0.1  )
length( boolDiff1[boolDiff1] )

boolDiff =  boolDiff1  
length( boolDiff )
length( boolDiff[boolDiff] )

rots_re2A = rots_re2[boolDiff, ]
rots_re3A = rots_re3[boolDiff, ]
rots_re4A = rots_re4[boolDiff, ]
dim( rots_re2A )
dim( rots_re3A )
dim( rots_re4A )

write.table(rots_re2A,    file = paste(outDir_g, "FTO-vs-m..7A.Diff.test-resutls.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re3A,    file = paste(outDir_g, "FTO-vs-m..7B.Diff.2groups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re4A,    file = paste(outDir_g, "FTO-vs-m..7C.Diff.allGroups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

print("######################################################################## END ")
###########################################################################################################################################################################




up_bool   = ( rots_out$logfc > 1  )
down_bool = ( rots_out$logfc < -1  )
length( up_bool[up_bool  &  boolDiff1] )
length( down_bool[down_bool &  boolDiff1] )


upMatrix   <- rots_re3[up_bool    &  boolDiff1,  ]
downMatrix <- rots_re3[down_bool  &  boolDiff1,  ]
dim( upMatrix )
dim( downMatrix )


write.table(upMatrix,    file = paste(outDir_g, "FTO-vs-m..upMatrix.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(downMatrix,    file = paste(outDir_g, "FTO-vs-m..downMatrix.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

























###########################################################################################################################################################################
print("ROTS call......")

myGroups2 = as.vector(myGroups) 
myGroups2[myGroups2 == "FTO"] = 1
myGroups2[myGroups2 == "m"] = 2
myGroups2[myGroups2 == "WT"] = 3
myGroups2
table( myGroups2 )

rots_out <- ROTS(data= as.matrix(myRPKM2[, 4:9]) , groups=myGroups2[4:9], B = 1000,   paired = FALSE,   seed = 14,   log = FALSE,   progress = TRUE, verbose = TRUE)  ## log,	 A logical (deafult TRUE) indicating whether input data is log2 scaled. This information is only used to calculate log fold change.

save(rots_out, file = paste(outDir_g, "m-vs-WT..5A.rots_out.RData", sep="/")   )
write.table(rots_out$data,    file = paste(outDir_g, "m-vs-WT..5B.rots_out.data.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",    eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )



sink( paste(outDir_g,  "/m-vs-WT..5C_ROTS.txt",  sep = "") )
print("######################################################################## myBool for log2: ")
print("######################################################################## myGroups2 ")
print( length( myGroups2) ) 
print( myGroups2 ) 
print( table( myGroups2 ) ) 
print("######################################################################## rots_out ")
print(  names(rots_out)) 
print("########################################################################")
print("######################################################################## p ")
print( length( rots_out$pvalue[ rots_out$pvalue < 0.05] ) ) 
print( length( rots_out$pvalue[ rots_out$pvalue < 0.01] ) ) 
print( length( rots_out$pvalue[ rots_out$pvalue < 0.001] ) ) 
print("########################################################################FDR ")
print( length( rots_out$FDR[ rots_out$FDR < 0.2] )  ) 
print( length( rots_out$FDR[ rots_out$FDR < 0.1] )  ) 
print( length( rots_out$FDR[ rots_out$FDR < 0.05] )  ) 
print("########################################################################")
sink()
sink()


rots_re <- cbind( rots_out$d, rots_out$logfc, rots_out$pvalue, rots_out$FDR)
colnames(rots_re) <- c( " ROTS-statistic",  "ROTS.logFC", "ROTS.p-value",  "ROTS.FDR"  )

write.table( cbind(DF_B, rots_re),    file = paste(outDir_g, "m-vs-WT..5D.rots_results.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
             eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )
###########################################################################################################################################################################





###########################################################################################################################################################################
print("########################################################################")
print("########################################################################")
rots_re2 = cbind(DF_B[,1:3], rots_re )   
rots_re3 = cbind(rots_re2,  myRPKM2 )   
rots_re4 = cbind(rots_re2,  DF_B )  
dim( rots_re2 )
dim( rots_re3 )
dim( rots_re4 )

write.table(rots_re2,    file = paste(outDir_g, "m-vs-WT..6A.All.test-resutls.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re3,    file = paste(outDir_g, "m-vs-WT..6B.All.2groups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re4,    file = paste(outDir_g, "m-vs-WT..6C.All.allGroups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )
###########################################################################################################################################################################





###########################################################################################################################################################################
boolDiff1 = ( rots_out$FDR < 0.1  )
length( boolDiff1[boolDiff1] )

boolDiff =  boolDiff1  
length( boolDiff )
length( boolDiff[boolDiff] )

rots_re2A = rots_re2[boolDiff, ]
rots_re3A = rots_re3[boolDiff, ]
rots_re4A = rots_re4[boolDiff, ]
dim( rots_re2A )
dim( rots_re3A )
dim( rots_re4A )

write.table(rots_re2A,    file = paste(outDir_g, "m-vs-WT..7A.Diff.test-resutls.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re3A,    file = paste(outDir_g, "m-vs-WT..7B.Diff.2groups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re4A,    file = paste(outDir_g, "m-vs-WT..7C.Diff.allGroups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

print("######################################################################## END ")
###########################################################################################################################################################################




up_bool   = ( rots_out$logfc > 1  )
down_bool = ( rots_out$logfc < -1  )
length( up_bool[up_bool  &  boolDiff1] )
length( down_bool[down_bool &  boolDiff1] )


upMatrix   <- rots_re3[up_bool    &  boolDiff1,  ]
downMatrix <- rots_re3[down_bool  &  boolDiff1,  ]
dim( upMatrix )
dim( downMatrix )


write.table(upMatrix,    file = paste(outDir_g, "m-vs-WT..upMatrix.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(downMatrix,    file = paste(outDir_g, "m-vs-WT..downMatrix.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )












































###########################################################################################################################################################################
print("ROTS call......")

myGroups2 = as.vector(myGroups) 
myGroups2[myGroups2 == "FTO"] = 1
myGroups2[myGroups2 == "m"] = 2
myGroups2[myGroups2 == "WT"] = 3
myGroups2
table( myGroups2 )

rots_out <- ROTS(data= as.matrix(myRPKM2[, c(1,2,3,7,8,9)]) , groups=myGroups2[c(1,2,3,7,8,9)], B = 1000,   paired = FALSE,   seed = 14,   log = FALSE,   progress = TRUE, verbose = TRUE)  ## log,	 A logical (deafult TRUE) indicating whether input data is log2 scaled. This information is only used to calculate log fold change.

save(rots_out, file = paste(outDir_g, "FTO-vs-WT..5A.rots_out.RData", sep="/")   )
write.table(rots_out$data,    file = paste(outDir_g, "FTO-vs-WT..5B.rots_out.data.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",    eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )



sink( paste(outDir_g,  "/FTO-vs-WT..5C_ROTS.txt",  sep = "") )
print("######################################################################## myBool for log2: ")
print("######################################################################## myGroups2 ")
print( length( myGroups2) ) 
print( myGroups2 ) 
print( table( myGroups2 ) ) 
print("######################################################################## rots_out ")
print(  names(rots_out)) 
print("########################################################################")
print("######################################################################## p ")
print( length( rots_out$pvalue[ rots_out$pvalue < 0.05] ) ) 
print( length( rots_out$pvalue[ rots_out$pvalue < 0.01] ) ) 
print( length( rots_out$pvalue[ rots_out$pvalue < 0.001] ) ) 
print("########################################################################FDR ")
print( length( rots_out$FDR[ rots_out$FDR < 0.2] )  ) 
print( length( rots_out$FDR[ rots_out$FDR < 0.1] )  ) 
print( length( rots_out$FDR[ rots_out$FDR < 0.05] )  ) 
print("########################################################################")
sink()
sink()


rots_re <- cbind( rots_out$d, rots_out$logfc, rots_out$pvalue, rots_out$FDR)
colnames(rots_re) <- c( " ROTS-statistic",  "ROTS.logFC", "ROTS.p-value",  "ROTS.FDR"  )

write.table( cbind(DF_B, rots_re),    file = paste(outDir_g, "FTO-vs-WT..5D.rots_results.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
             eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )
###########################################################################################################################################################################





###########################################################################################################################################################################
print("########################################################################")
print("########################################################################")
rots_re2 = cbind(DF_B[,1:3], rots_re )   
rots_re3 = cbind(rots_re2,  myRPKM2 )   
rots_re4 = cbind(rots_re2,  DF_B )  
dim( rots_re2 )
dim( rots_re3 )
dim( rots_re4 )

write.table(rots_re2,    file = paste(outDir_g, "FTO-vs-WT..6A.All.test-resutls.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re3,    file = paste(outDir_g, "FTO-vs-WT..6B.All.2groups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re4,    file = paste(outDir_g, "FTO-vs-WT..6C.All.allGroups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )
###########################################################################################################################################################################





###########################################################################################################################################################################
boolDiff1 = ( rots_out$FDR < 0.1  )
length( boolDiff1[boolDiff1] )

boolDiff =  boolDiff1  
length( boolDiff )
length( boolDiff[boolDiff] )

rots_re2A = rots_re2[boolDiff, ]
rots_re3A = rots_re3[boolDiff, ]
rots_re4A = rots_re4[boolDiff, ]
dim( rots_re2A )
dim( rots_re3A )
dim( rots_re4A )

write.table(rots_re2A,    file = paste(outDir_g, "FTO-vs-WT..7A.Diff.test-resutls.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re3A,    file = paste(outDir_g, "FTO-vs-WT..7B.Diff.2groups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(rots_re4A,    file = paste(outDir_g, "FTO-vs-WT..7C.Diff.allGroups.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

print("######################################################################## END ")
###########################################################################################################################################################################




up_bool   = ( rots_out$logfc > 1  )
down_bool = ( rots_out$logfc < -1  )
length( up_bool[up_bool  &  boolDiff1] )
length( down_bool[down_bool &  boolDiff1] )


upMatrix   <- rots_re3[up_bool    &  boolDiff1,  ]
downMatrix <- rots_re3[down_bool  &  boolDiff1,  ]
dim( upMatrix )
dim( downMatrix )


write.table(upMatrix,    file = paste(outDir_g, "FTO-vs-WT..upMatrix.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )

write.table(downMatrix,    file = paste(outDir_g, "FTO-vs-WT..downMatrix.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )







