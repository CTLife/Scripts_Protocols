
suppressPackageStartupMessages( library(optparse) )
library(ROTS)
library(GenomicRanges)
library(rtracklayer)
library(Rsubread)
library(Rsamtools)
library(ade4)
library(made4)
library(edgeR)
library(EDASeq)
library(RUVSeq)
library(RColorBrewer)
library(ggplot2)


outDir_g = "ANOVA"
if( ! file.exists(outDir_g)   ) { dir.create(outDir_g,   recursive = TRUE) }





##############################################################################################################################################################################################
DF_A <- read.table("1_BA9/merged.bed",  header=TRUE,   sep="\t", comment.char = "" )  
DF_B <- read.table("2_BA24/merged.bed", header=TRUE,   sep="\t", comment.char = "" )  
DF_C <- read.table("3_C/merged.bed",    header=TRUE,   sep="\t", comment.char = "" )  
DF_D <- read.table("4_H/merged.bed",    header=TRUE,   sep="\t", comment.char = "" )  
DF_E <- read.table("5_T/merged.bed",    header=TRUE,   sep="\t", comment.char = "" ) 

dim( DF_A )
dim( DF_B )
dim( DF_C )
dim( DF_D )
dim( DF_E )

rowname_A <- as.vector(DF_A[,4])
DF_A1  <- DF_A[, -c(1:6)]
rownames(DF_A1) <- rowname_A
colnames(DF_A1) <- as.vector( paste("groupA", colnames(DF_A1), sep=".") )
group_A = rep("A", ncol(DF_A1) )
dim( DF_A1 )
length(rowname_A)
length(group_A)

rowname_B <- as.vector(DF_B[,4])
DF_B1  <- DF_B[, -c(1:6)]
rownames(DF_B1) <- rowname_B
colnames(DF_B1) <- as.vector( paste("groupB", colnames(DF_B1), sep=".") )
group_B = rep("B", ncol(DF_B1) )
dim( DF_B1 )
length(rowname_B)
length(group_B)

rowname_C <- as.vector(DF_C[,4])
DF_C1  <- DF_C[, -c(1:6)]
rownames(DF_C1) <- rowname_C
colnames(DF_C1) <- as.vector( paste("groupC", colnames(DF_C1), sep=".") )
group_C = rep("C", ncol(DF_C1) )
dim( DF_C1 )
length(rowname_C)
length(group_C)

rowname_D <- as.vector(DF_D[,4])
DF_D1  <- DF_D[, -c(1:6)]
rownames(DF_D1) <- rowname_D
colnames(DF_D1) <- as.vector( paste("groupD", colnames(DF_D1), sep=".") )
group_D = rep("D", ncol(DF_D1) )
dim( DF_D1 )
length(rowname_D)
length(group_D)

rowname_E <- as.vector(DF_E[,4])
DF_E1  <- DF_E[, -c(1:6)]
rownames(DF_E1) <- rowname_E
colnames(DF_E1) <- as.vector( paste("groupE", colnames(DF_E1), sep=".") )
group_E = rep("E", ncol(DF_E1) )
dim( DF_E1 )
length(rowname_E)
length(group_E)


DF_all <- cbind(DF_A1, DF_B1, DF_C1, DF_D1, DF_E1)
DF_all[1:2,]






pvalues=c()
Fvalues=c()

for(i  in  c(1:nrow(DF_all))){
  my_data <- data.frame( value= as.numeric(as.vector(DF_all[i,])),  group=c(group_A, group_B, group_C, group_D, group_E) )
  res.aov <- aov(value ~ group, data = my_data)
  sum_test = unlist( summary(res.aov) )
  pvalues[i] = sum_test["Pr(>F)1"]
  Fvalues[i] = sum_test["F value1"]
}

length( pvalues )
length( Fvalues )

length(pvalues[pvalues<0.01])
 
write.table(DF_all,    file = paste(outDir_g, "1.DF_all.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )

write.table(cbind(pvalues, Fvalues),    file = paste(outDir_g, "2.ANOVA.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = TRUE )










myBool = (pvalues<0.01)
length(myBool[myBool])

DF_all2 = DF_all[myBool, ]
dim(DF_all2)
write.table(DF_all2,    file = paste(outDir_g, "3.DF_all2.selected.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )

DF_all2[1:5,]

suppressPackageStartupMessages( library(gplots) )

my_col2=colorRampPalette( c(    "green4", "green",  "white", "purple",  "purple4") , bias = 1  )  
pdf( file=paste(outDir_g, "4.heatmap.pdf", sep="/"),  height=12,   width=8 )
heatmap.2(x= as.matrix(DF_all2) ,
          # dendrogram control
          dendrogram = "row", 
          Rowv = TRUE,
          Colv=FALSE , 
          symm = FALSE,
          # data scaling
          scale =  "none" ,
          na.rm=TRUE,
          # colors
          col=my_col2(40),
          trace = "none", 
          # cell labeling
          ##cellnote= as.matrix(rawMatrix_1) ,
          notecex=0.5,
          notecol="white",
          na.color=par("bg") 
)
dev.off() 


insertOneColumn <- function(matrix1, newcol1, r) {
  matrix1[ , seq(r+1,ncol(matrix1)+1)] <- matrix1[, seq(r,ncol(matrix1)) ]
  matrix1[, r] <- newcol1
  return(matrix1)
}

rawMatrix_1a_add = insertOneColumn(DF_all2,              newcol1=rep(NA,   times=nrow(DF_all2)), r=22 )
rawMatrix_1a_add = insertOneColumn(rawMatrix_1a_add,     newcol1=rep(NA,   times=nrow(DF_all2)), r=42 )
rawMatrix_1a_add = insertOneColumn(rawMatrix_1a_add,     newcol1=rep(NA,   times=nrow(DF_all2)), r=63 )
rawMatrix_1a_add = insertOneColumn(rawMatrix_1a_add,     newcol1=rep(NA,   times=nrow(DF_all2)), r=84 )

rawMatrix_1a_add[1:2, ]


pdf( file=paste(outDir_g, "4.add.heatmap.pdf", sep="/"),  height=12,   width=8 )
heatmap.2(x= as.matrix(rawMatrix_1a_add) ,
          # dendrogram control
          dendrogram = "row", 
          Rowv = TRUE,
          Colv=FALSE , 
          symm = FALSE,
          # data scaling
          scale =  "none" ,
          na.rm=TRUE,
          # colors
          col=my_col2(40),
          trace = "none", 
          # cell labeling
          ##cellnote= as.matrix(rawMatrix_1) ,
          notecex=0.5,
          notecol="white",
          na.color=par("bg") 
)
dev.off() 

write.table(rawMatrix_1a_add,    file = paste(outDir_g, "3.add.DF_all2.selected.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )









 


reset_outliers2 <- function(x, na.rm = TRUE ) {
  qnt <- quantile(x, probs=c(0.1, 0.9) , type=1,  na.rm = na.rm )  
  y <- x
  y[x < qnt[1] ] <- qnt[1]
  y[x > qnt[2] ] <- qnt[2]    
  y
}
myScaleMatrix2 <- function( matrix_temp8, upper_temp8 = 1, lower_temp8 = -1 ) {
  rawMatrix_2 = reset_outliers2(matrix_temp8)  
  rawMatrix_2 = lower_temp8 + (upper_temp8 - lower_temp8) * ( rawMatrix_2 - min(rawMatrix_2) )/( max(rawMatrix_2)- min(rawMatrix_2) )
  return(rawMatrix_2)
}


DF_all3 = DF_all2
for(i in c(1:nrow(DF_all2)) ) {
  temp_1 = DF_all2[i,]
  DF_all3[i,] = myScaleMatrix2( matrix_temp8 = temp_1, upper_temp8 = 1, lower_temp8 = -1  )
}

  
pdf( file=paste(outDir_g, "5.rowScaled.heatmap.pdf", sep="/"),  height=12,   width=8 )
heatmap.2(x= as.matrix(DF_all3) ,
          # dendrogram control
          dendrogram = "row", 
          Rowv = TRUE,
          Colv=FALSE , 
          symm = FALSE,
          # data scaling
          scale =  "none" ,
          na.rm=TRUE,
          # colors
          col=my_col2(40),
          trace = "none", 
          # cell labeling
          ##cellnote= as.matrix(rawMatrix_1) ,
          notecex=0.5,
          notecol="white",
          na.color=par("bg") 
)
dev.off() 

dim(DF_all3)
write.table(DF_all3,    file = paste(outDir_g, "6.rowScaled.DF_all3.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )



rawMatrix_1a_add = insertOneColumn(DF_all3,              newcol1=rep(NA,   times=nrow(DF_all2)), r=22 )
rawMatrix_1a_add = insertOneColumn(rawMatrix_1a_add,     newcol1=rep(NA,   times=nrow(DF_all2)), r=42 )
rawMatrix_1a_add = insertOneColumn(rawMatrix_1a_add,     newcol1=rep(NA,   times=nrow(DF_all2)), r=63 )
rawMatrix_1a_add = insertOneColumn(rawMatrix_1a_add,     newcol1=rep(NA,   times=nrow(DF_all2)), r=84 )

pdf( file=paste(outDir_g, "5.add.rowScaled.heatmap.pdf", sep="/"),  height=12,   width=8 )
heatmap.2(x= as.matrix(rawMatrix_1a_add) ,
          # dendrogram control
          dendrogram = "row", 
          Rowv = TRUE,
          Colv=FALSE , 
          symm = FALSE,
          # data scaling
          scale =  "none" ,
          na.rm=TRUE,
          # colors
          col=my_col2(40),
          trace = "none", 
          # cell labeling
          ##cellnote= as.matrix(rawMatrix_1) ,
          notecex=0.5,
          notecol="white",
          na.color=par("bg") 
)
dev.off() 

write.table(rawMatrix_1a_add,    file = paste(outDir_g, "6.add.rowScaled.DF_all3.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )


   


















average_A <- rowMeans( DF_all2[,1:21])
average_B <- rowMeans( DF_all2[,22:40])
average_C <- rowMeans( DF_all2[,41:60])
average_D <- rowMeans( DF_all2[,61:80])
average_E <- rowMeans( DF_all2[,81:100])

average_Matrix <- cbind(average_A , average_B, average_C, average_D, average_E)
dim(average_Matrix)

write.table(average_Matrix,    file = paste(outDir_g, "7.average_Matrix.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )

pdf( file=paste(outDir_g, "8.heatmap.pdf", sep="/"),  height=8,   width=5 )
heatmap.2(x= as.matrix(average_Matrix) ,
          # dendrogram control
          dendrogram = "row", 
          Rowv = TRUE,
          Colv=FALSE , 
          symm = FALSE,
          # data scaling
          scale =  "none" ,
          na.rm=TRUE,
          # colors
          col=my_col2(40),
          trace = "none", 
          # cell labeling
          ##cellnote= as.matrix(rawMatrix_1) ,
          notecex=0.5,
          notecol="white",
          na.color=par("bg") 
)
dev.off() 












average_A <- rowMeans( DF_all3[,1:21])
average_B <- rowMeans( DF_all3[,22:40])
average_C <- rowMeans( DF_all3[,41:60])
average_D <- rowMeans( DF_all3[,61:80])
average_E <- rowMeans( DF_all3[,81:100])

average_Matrix <- cbind(average_A , average_B, average_C, average_D, average_E)
dim(average_Matrix)

write.table(average_Matrix,    file = paste(outDir_g, "9.average_Matrix.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )

my_col2=colorRampPalette( c(    "green4", "green",  "white", "purple",  "purple4") , bias = 1  )  
pdf( file=paste(outDir_g, "10.heatmap.pdf", sep="/"),  height=8,   width=5 )
heatmap.2(x= as.matrix(average_Matrix) ,
          # dendrogram control
          dendrogram = "row", 
          Rowv = TRUE,
          Colv=FALSE , 
          symm = FALSE,
          # data scaling
          scale =  "none" ,
          na.rm=TRUE,
          # colors
          col=my_col2(40),
          trace = "none", 
          # cell labeling
          ##cellnote= as.matrix(rawMatrix_1) ,
          notecex=0.5,
          notecol="white",
          na.color=par("bg") 
)
dev.off() 








