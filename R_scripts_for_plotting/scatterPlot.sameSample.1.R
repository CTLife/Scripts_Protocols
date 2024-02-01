
##############################################################################################################################################################################################
library(stringr)
library(ggplot2)
library(ggpubr)
options(bitmapType='cairo')


outDir_g     <-   "scatterPlot.sameSample.1"
if( ! file.exists(outDir_g)   ) { dir.create(outDir_g,   recursive = TRUE) }
##############################################################################################################################################################################################





##############################################################################################################################################################################################
 
RNAm6A_1   <- read.table(  "unfiltered_matrix/final_matrix.m6A.txt",       header=T,   sep="\t", comment.char = "", quote ="" )    
expres_1   <- read.table(  "unfiltered_matrix/geneExpression.txt",         header=T,   sep="\t", comment.char = "", quote ="" )  
splici_1   <- read.table(  "unfiltered_matrix/final_matrix.splicing.txt",  header=T,   sep="\t", comment.char = "", quote ="" )    
dim(RNAm6A_1)
dim(expres_1)
dim(splici_1)
RNAm6A_1[1:5, 1:9]
expres_1[1:5, 1:9] 
splici_1[1:5, 1:9]

colnames( RNAm6A_1 )
colnames( expres_1 )
colnames( splici_1 )


RNAm6A_ID <- RNAm6A_1$IDname
expres_ID <- expres_1$IDname
splici_ID <- splici_1$IDname

RNAm6A_gene <- RNAm6A_1$geneName
expres_gene <- expres_1$geneName
splici_gene <- splici_1$geneName

RNAm6A_value <- RNAm6A_1[,-c(1:2)]
expres_value <- expres_1[,-c(1:2)]
splici_value <- splici_1[,-c(1:2)]

length(RNAm6A_ID)
length(expres_ID)
length(splici_ID)

length(RNAm6A_gene)
length(expres_gene)
length(splici_gene)

dim(RNAm6A_value)
dim(expres_value)
dim(splici_value)




RNAm6A_colname <- colnames( RNAm6A_value )
expres_colname <- colnames( expres_value )
splici_colname <- colnames( splici_value )

bool1 <- expres_colname  %in%  RNAm6A_colname
bool2 <- splici_colname  %in%  RNAm6A_colname
length(bool1)
length(bool2)
length(bool1[bool1])
length(bool2[bool2])


expres_value <- expres_value[,bool1]
splici_value <- splici_value[,bool2]
dim(expres_value)
dim(splici_value)

RNAm6A_colname <- colnames( RNAm6A_value )
expres_colname <- colnames( expres_value )
splici_colname <- colnames( splici_value )
boolA <- (RNAm6A_colname == expres_colname)
boolB <- (RNAm6A_colname == splici_colname)
length(boolA)
length(boolB)
length(boolA[boolA])
length(boolB[boolB])








RNAm6A_expres_pairs <- data.frame()
RNAm6A_splici_pairs <- data.frame()
expres_splici_pairs <- data.frame()

length(RNAm6A_ID)
length(expres_ID)
length(splici_ID)

length(RNAm6A_gene)
length(expres_gene)
length(splici_gene)

RNAm6A_gene[1:10]
expres_gene[1:10]
splici_gene[1:10]



N1 = 1
for(i in c(1:length(RNAm6A_gene)) ){
  gene1 <- RNAm6A_gene[i]  
  
  if( !is.na(gene1) ){
  toolTemp <- expres_gene  %in%  gene1
  indexes  <- which( toolTemp )
  
  for(j in indexes ){
    RNAm6A_expres_pairs[N1, 1] = RNAm6A_ID[i]
    RNAm6A_expres_pairs[N1, 2] = RNAm6A_gene[i]
    RNAm6A_expres_pairs[N1, 3] = expres_ID[j]
    RNAm6A_expres_pairs[N1, 4] = expres_gene[j]
    RNAm6A_expres_pairs[N1, c(5:97)]   = RNAm6A_value[i,]
    RNAm6A_expres_pairs[N1, c(98:190)] = expres_value[j,]
    N1 = N1 + 1
  }
  
  }
}


dim(RNAm6A_expres_pairs)
RNAm6A_expres_pairs[1:10, 1:10]

colnames( RNAm6A_expres_pairs )  <- c( "RNAm6A_ID", "RNAm6A_gene", "expres_ID", "expres_gene", RNAm6A_colname,  expres_colname)

write.table(RNAm6A_expres_pairs, file = paste(outDir_g, "RNAm6A_expres_pairs.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE )


fig_BA9  <- list()
fig_BA24 <- list()
fig_C    <- list()
fig_H    <- list()
fig_T    <- list()
 
for(i in c(5:97) ){
  j = i + 93
  X2A = RNAm6A_expres_pairs[, i]
  Y2A = RNAm6A_expres_pairs[, j]
  if( colnames(RNAm6A_expres_pairs)[i] != colnames(RNAm6A_expres_pairs)[j] ){ print("Error!") }
  df_1 = data.frame( x= X2A , y= log2(Y2A+1) )
  
  p1 <- ggplot(df_1, aes(x = x, y = y)) + geom_point(shape=16, color="blue", alpha=0.2, size=0.1)  + #geom_abline(linewidth=0.2, linetype="dashed", color="black") + 
    ggtitle( colnames(RNAm6A_expres_pairs)[i] ) +     xlab("RNA m6A level") + ylab("Gene expression level")  +
    geom_smooth(method = "lm", linewidth=0.2, color="red") + stat_cor(method = "pearson", label.x = 0, label.y = 14, color="red", size=4) 
  
  if( (i>=5)  & (i<=22) ){ fig_BA24[[length(fig_BA24)+1]] = p1 }
  if( (i>=23) & (i<=41) ){ fig_BA9[[length(fig_BA9)+1]] = p1 }
  if( (i>=42) & (i<=59) ){ fig_C[[length(fig_C)+1]] = p1 }
  if( (i>=60) & (i<=77) ){ fig_H[[length(fig_H)+1]] = p1 }
  if( (i>=78) & (i<=97) ){ fig_T[[length(fig_T)+1]] = p1 }
  
}




png( paste(outDir_g, "/",   "fig_BA9.png", sep="") ,   width = 15, height = 19, units = "in", res = 900, pointsize = 4 )
  ggarrange( plotlist =  fig_BA9 ,    labels = "",  ncol = 4, nrow = 5) 
dev.off()



png( paste(outDir_g, "/",   "fig_BA24.png", sep="") ,   width = 15, height = 19, units = "in", res = 900, pointsize = 4 )
ggarrange( plotlist =  fig_BA24 ,    labels = "",  ncol = 4, nrow = 5) 
dev.off()




png( paste(outDir_g, "/",   "fig_C.png", sep="") ,   width = 15, height = 19, units = "in", res = 900, pointsize = 4 )
ggarrange( plotlist =  fig_C ,    labels = "",  ncol = 4, nrow = 5) 
dev.off()




png( paste(outDir_g, "/",   "fig_H.png", sep="") ,   width = 15, height = 19, units = "in", res = 900, pointsize = 4 )
ggarrange( plotlist =  fig_H ,    labels = "",  ncol = 4, nrow = 5) 
dev.off()




png( paste(outDir_g, "/",   "fig_T.png", sep="") ,   width = 15, height = 19, units = "in", res = 900, pointsize = 4 )
ggarrange( plotlist =  fig_T ,    labels = "",  ncol = 4, nrow = 5) 
dev.off()

















