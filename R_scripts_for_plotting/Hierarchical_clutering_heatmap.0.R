##################################################################################################################
## Suffixes of all self-defined global variables must be "_g".
## Example:  
## Rscript  clutering_heatmap.R     3UTR    


library(factoextra) # clustering algorithms & visualization
library(flexclust)
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


args_g <- commandArgs(TRUE)
print("##########################")
print("args: ")
print(args_g[1])   
print("##########################")

input_matrix_g= args_g[1];     ## Input matrix file

# input_matrix_g = "find"

outDir_g = paste(input_matrix_g , ".Results.4",  sep="")
if( ! file.exists(outDir_g)          ) { dir.create(outDir_g, recursive = TRUE) }
##################################################################################################################





###################
DF1 <- read.table( paste(input_matrix_g,   ".txt",  sep="") , header=T, sep="\t", quote = "", comment.char = "") 
dim(DF1)
DF1[1:25,1:5]

matrix   = DF1[-c(1:16),]
COL_NAME = DF1[c(1:16), ]
dim(matrix)
dim(COL_NAME)

matrix[1:5,1:5]
COL_NAME[,1:5]

matrix2 <- matrix(as.numeric(unlist(matrix)), ncol = ncol(matrix))   # Convert to numeric matrix 
matrix2 <- 2^matrix2 - 1
matrix2[1:5,1:5]
max(matrix2)
min(matrix2)



reset_outliers2 <- function(x, na.rm = TRUE ) {
  qnt <- quantile(x, probs=c(0.15, 0.85) , type=1,  na.rm = na.rm )  
  y <- x
  y[x < qnt[1] ] <- qnt[1]
  y[x > qnt[2] ] <- qnt[2]    
  y
}

myScaleMatrix2 <- function( matrix_temp8, upper_temp8 = 1, lower_temp8 = -1 ) {
  rawMatrix_2 =   reset_outliers2(matrix_temp8)  
  rawMatrix_2 = lower_temp8 + (upper_temp8 - lower_temp8) * ( rawMatrix_2 - min(rawMatrix_2) )/( max(rawMatrix_2)- min(rawMatrix_2) )
  return(rawMatrix_2)
}


matrix3 = matrix2
for(i in c(1:nrow(matrix2)) ){
  matrix3[i,] = myScaleMatrix2( matrix_temp8 = matrix2[i,] , upper_temp8 = 1, lower_temp8 = 0 )
}
 
write.table(matrix3 ,  file = paste(outDir_g,   "0.rowScaled.matrix.txt",  sep="/"), 
            append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".",  row.names = TRUE,  col.names = TRUE )





################################################################################
my_col1=colorRampPalette( c(   "cyan",  "white", "red" ),  bias = 1.0,  space = "rgb" )
my_col2=colorRampPalette( c(   "cyan",  "white", "red" ),  bias = 1.2,  space = "rgb" )
my_col3=colorRampPalette( c(   "cyan",  "white", "red" ),  bias = 1.4,  space = "rgb" )
my_col4=colorRampPalette( c(   "cyan",  "white", "red" ),  bias = 1.6,  space = "rgb" )
my_col5=colorRampPalette( c(   "cyan",  "white", "red" ),  bias = 1.8,  space = "rgb" )

library("pheatmap")
pdf( file = paste(outDir_g, "1.Hierarchical.pdf", sep="/"),  width=6, height=8  )
     presutls1 = pheatmap::pheatmap(matrix3, cutree_cols = 2 , cluster_rows = F , cluster_cols = T,   clustering_distance_cols = "euclidean",  show_rownames = F, show_colnames = F , col=my_col1(100)  )
     presutls2 = pheatmap::pheatmap(matrix3, cutree_cols = 2 , cluster_rows = F , cluster_cols = T,   clustering_distance_cols = "euclidean",  show_rownames = F, show_colnames = F , col=my_col2(100)  )
     presutls3 = pheatmap::pheatmap(matrix3, cutree_cols = 2 , cluster_rows = F , cluster_cols = T,   clustering_distance_cols = "euclidean",  show_rownames = F, show_colnames = F , col=my_col3(100)  )
     presutls4 = pheatmap::pheatmap(matrix3, cutree_cols = 2 , cluster_rows = F , cluster_cols = T,   clustering_distance_cols = "euclidean",  show_rownames = F, show_colnames = F , col=my_col4(100)  )
     presutls5 = pheatmap::pheatmap(matrix3, cutree_cols = 2 , cluster_rows = F , cluster_cols = T,   clustering_distance_cols = "euclidean",  show_rownames = F, show_colnames = F , col=my_col5(100)  )
dev.off() 

names(presutls3)
index1 = presutls3$tree_col$order
COL_NAME2 = COL_NAME[ , index1  ]
matrix4   = matrix3[ , index1  ]

write.table(matrix4 ,  file = paste(outDir_g,   "1a.matrix.txt",  sep="/"), 
            append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".",  row.names = TRUE,  col.names = TRUE )

write.table(COL_NAME2 ,  file = paste(outDir_g,   "1b.header.txt",  sep="/"), 
            append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".",  row.names = TRUE,  col.names = TRUE )


COL_NAME2[,1:5]
COL_NAME3 = COL_NAME2[c(5,7,9,11,14,15 ),]
COL_NAME3[,1:5]


library(ComplexHeatmap) 
pdf( paste(outDir_g,   "2.header.pdf",  sep="/")  )
    Heatmap( COL_NAME3 , cluster_rows = FALSE, cluster_columns = FALSE )
    Heatmap( COL_NAME3[1,] , cluster_rows = FALSE, cluster_columns = FALSE )
dev.off()
























