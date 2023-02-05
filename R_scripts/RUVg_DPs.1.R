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
library(stringr)






##############################################################################################################################################################################################
rawMatrix_1 <- read.table("Kept.info.txt", header=TRUE,   sep="\t", comment.char = "" )   ## RPKM of IP reads
cat("########################(Raw dataframe, RPKM of IP reads):\n")
dim( rawMatrix_1 )
## rawMatrix_1[1:30,1:2]
myColNames = colnames(rawMatrix_1)
myRowNames = rownames(rawMatrix_1)
length( myColNames )
length( myRowNames )
## myColNames
## myRowNames[1:30]
myRowNames = str_replace_all(string=myRowNames, pattern=" ", replacement="" )
rownames(rawMatrix_1) = myRowNames
cat("########################\n\n\n\n\n")

myHeader = rawMatrix_1[c(1:14), ]
myRPKM   = rawMatrix_1[-c(1:14), ]
myGroups = rawMatrix_1[5, ]  ## disease state or others
dim( rawMatrix_1 )
dim( myHeader  )
dim( myRPKM )
length( myGroups )
## myHeader[,1:3]
## myRPKM[1:10,1:2]
## myGroups

myGroups = as.factor( as.vector(unlist(myGroups) ) )
##  myGroups
length( myGroups )
table(myGroups)

myColors <- brewer.pal(5, "Set2")
myColors[myGroups]


## myRPKM[1:5,1:3]
dim( myRPKM )
myRPKM2 <- matrix(as.numeric(unlist(myRPKM)), ncol = ncol(myRPKM))   # Convert to numeric matrix 
dim( myRPKM2 )
## myRPKM2[1:3,1:3]
colnames(myRPKM2) = colnames(myRPKM)
rownames(myRPKM2) = rownames(myRPKM)
## hist( as.vector(myRPKM2) , breaks=1000)
## hist( as.vector(myRPKM2) , breaks=1000, xlim=c(0, 100))
## hist(  myRPKM2[1,]  , breaks=1000)

## More methods: https://rdrr.io/bioc/EDASeq/man/SeqExpressionSet-class.html
set2 <- newSeqExpressionSet(counts=round(myRPKM2) ,  normalizedCounts = myRPKM2,   phenoData = data.frame(myGroups, row.names=colnames(myRPKM2)))
set2
pData(set2)
matrix2 = counts(set2)
norm_matrix2 = normCounts(set2)
dim(matrix2)
dim(norm_matrix2)
## matrix2[1:3,1:3]
## norm_matrix2[1:3,1:3]


max(myRPKM2)
median(myRPKM2)
min(myRPKM2)
## myRPKM2[myRPKM2>50]
## myRPKM2[myRPKM2<0.1]
##  x=c(0:20)
##  y=log2(x+1)
##  plot(x, y)
myRPKM3 = log2( myRPKM2+1)
max(myRPKM3)
median(myRPKM3)
min(myRPKM3)
## myRPKM3[1:5,1:3]
## myRPKM3[myRPKM3>5]
## myRPKM3[myRPKM3<0.1]

set3 <- newSeqExpressionSet(counts=round(myRPKM3) ,  normalizedCounts = myRPKM3,   phenoData = data.frame(myGroups, row.names=colnames(myRPKM3)))
set3
pData(set3)
matrix3 = counts(set3)
norm_matrix3 = normCounts(set3)
dim(matrix3)
dim(norm_matrix3)
## matrix3[1:3,1:3]
## norm_matrix3[1:3,1:3]







############################################################################################################################################
AllResults_g <- "RUVg_ROTS_Wilcoxon"
if( ! file.exists(AllResults_g) ) { dir.create(path=AllResults_g, recursive = TRUE) }


## RLE = log-ratio of read count to median read count across sample
pdf(paste(AllResults_g, "1a.RLE.figures_for_RPKM.pdf", sep="/"), width=20, height=6)
    plotRLE(set2, outline=FALSE, ylim=c(-2, 2), col=myColors[myGroups] )
dev.off()

pdf(paste(AllResults_g, "1b.PCA.figures_for_RPKM.pdf", sep="/"), width=6, height=6)
    plotPCA(set2, col=myColors[myGroups], cex=0.5, labels=F)
    plotPCA(set2, col=myColors[myGroups], cex=0.5, labels=TRUE)
dev.off()

write.table(norm_matrix2,    file = paste(AllResults_g, "1c.rawRPKM.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )




## RLE = log-ratio of read count to median read count across sample
pdf(paste(AllResults_g, "2a.RLE.figures_for_loggedRPKM.pdf", sep="/"), width=20, height=6)
plotRLE(set3, outline=FALSE, ylim=c(-2, 2), col=myColors[myGroups] )
dev.off()

pdf(paste(AllResults_g, "2b.PCA.figures_for_loggedRPKM.pdf", sep="/"), width=6, height=6)
plotPCA(set3, col=myColors[myGroups], cex=0.5, labels=F)
plotPCA(set3, col=myColors[myGroups], cex=0.5, labels=TRUE)
dev.off()

write.table(norm_matrix3,    file = paste(AllResults_g, "2c.loggedRPKM.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )






############################################################################################################################################
NormalizationStep_RUVg <- function(Set,  K=3) {    
          Groups <- pData(Set)$myGroups     
          Counts <- normCounts(Set)  ## logged, log2
          Counts2<- 2^Counts - 1 
          design <- model.matrix(~Groups, data=pData(Set))
          y      <- DGEList(counts=Counts2, group=Groups)
          y      <- calcNormFactors(y, method="upperquartile")
          y      <- estimateGLMCommonDisp(y, design)
          y      <- estimateGLMTagwiseDisp(y, design)
          fit    <- glmFit(y, design)
          lrt    <- glmLRT(fit, coef=2)
          ## topTags(lrt)
          ## RUVg
          {
              top        <- topTags(lrt, n=nrow(Set))$table
              empirical  <- rownames(Set)[which(! ( rownames(Set) %in% rownames(top)[1:(floor(nrow(top) * (25/100) ) )] ) ) ]  ## length(empirical)
              ruvg_norm  <- RUVg( Counts , empirical, k=K,  isLog=TRUE )     
              return(ruvg_norm)
          }
}


 

RUVg_norm <- NormalizationStep_RUVg(set3,   K=4)
names(RUVg_norm)
RUVg_matrix <- RUVg_norm$normalizedCounts
## RUVg_matrix[1:3, 1:3]
dim(RUVg_matrix)
min(RUVg_matrix)
max(RUVg_matrix)
## hist(RUVg_matrix[,1], breaks=1000)
## hist(  as.vector( RUVg_matrix ), breaks=1000)

write.table(RUVg_norm$W,    file = paste(AllResults_g, "3a.RUVg_norm.W.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )
write.table(RUVg_matrix,    file = paste(AllResults_g, "3b.RUVg_norm.normalizedCounts.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )

bool0 <- ( colnames(RUVg_matrix) == colnames(myHeader) )
bool1 <- ( colnames(RUVg_matrix) == colnames(myRPKM) )
bool2 <- ( rownames(RUVg_matrix) == rownames(myRPKM) )
print("############################################")
length(bool0)
length(bool0[bool0])
length(bool1)
length(bool1[bool1])
length(bool2)
length(bool2[bool2])
print("############################################")

RUVg_matrix2 <- rbind(myHeader , RUVg_matrix)
dim(RUVg_matrix2)
write.table(RUVg_matrix2,    file = paste(AllResults_g, "3c.normalizedCounts.withHeader.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )







set4 <- newSeqExpressionSet(counts=round(RUVg_matrix),  normalizedCounts = RUVg_matrix ,  phenoData = data.frame(myGroups, row.names=colnames(RUVg_matrix)))
set4
pData(set4)
matrix4 = counts(set4)
norm_matrix4 = normCounts(set4)
dim(matrix4)
dim(norm_matrix4)
## matrix4[1:3,1:3]
## norm_matrix4[1:3,1:3]



pdf(paste(AllResults_g, "4a.RLE.figures_for_RUVg_normalized.pdf", sep="/"), width=20, height=6)
    plotRLE(set4, outline=FALSE, ylim=c(-2, 2), col=myColors[myGroups] , na.rm=TRUE)
dev.off()
pdf(paste(AllResults_g, "4b.hist.RUVg_normalized.pdf", sep="/"), width=20, height=6)
    hist(  as.vector( RUVg_matrix ), breaks=1000)
dev.off()

 






bool3 = ( myGroups == names(table(myGroups))[1] )
bool4 = ( myGroups == names(table(myGroups))[2] )
length( bool3 )
length( bool4 )
length( bool3[bool3] )
length( bool4[bool4] )

myGroup1 = RUVg_matrix[ , bool3]
myGroup2 = RUVg_matrix[ , bool4]
dim(myGroup1)
dim(myGroup2)

write.table(myGroup1,    file = paste(AllResults_g, "5a.RUVg_norm.Group1.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )

write.table(myGroup2,    file = paste(AllResults_g, "5b.RUVg_norm.Group2.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )


pvalues = vector( length = nrow(RUVg_matrix) )
for(i in c(1:nrow(RUVg_matrix)) ) {  
  x1  =  2^as.numeric( as.vector( myGroup1[i,] ) ) - 1
  y1  =  2^as.numeric( as.vector( myGroup2[i,] ) ) - 1
  re1 = wilcox.test(x=x1, y = y1,  alternative = "two.sided" )
  pvalues[i] = re1$p.value
}

length( pvalues[ pvalues < 0.05] )
length( pvalues[ pvalues < 0.001] )
pvalues[1]
pvalues[nrow(RUVg_matrix)]
FDR = p.adjust( pvalues , method = "fdr", n = length(pvalues) )  ## # c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr", "none")
length( FDR[ FDR < 0.2] )

matrix6 <- cbind(pvalues, FDR)
rownames(matrix6) = rownames( RUVg_matrix )

write.table(matrix6,    file = paste(AllResults_g, "5c.wilcox.test.FDR.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )






############################################### DPs
print("ROTS call......")

myGroups2 = as.vector(myGroups) 
myGroups2[bool3] = 1
myGroups2[bool4] = 2
## myGroups2
rots_out <- ROTS(data= as.matrix(RUVg_matrix) , groups=myGroups2, B = 200,   paired = FALSE, K=floor(nrow(RUVg_matrix)/3),  seed = 14,   log = TRUE, progress = TRUE, verbose = TRUE)
names(rots_out)
save(rots_out, file = paste(AllResults_g, "6a.rots_out.RData", sep="/")   )
 
write.table(rots_out$data,    file = paste(AllResults_g, "6b.rots_out.data.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )

length(rots_out$pvalue[rots_out$pvalue<0.05])
length(rots_out$FDR[rots_out$FDR<0.2])
rots_re <- cbind( rots_out$d, rots_out$logfc, rots_out$pvalue, rots_out$FDR)
dim(rots_re) 
rots_re[1:5,]
colnames(rots_re) <- c( " ROTS-statistic",  "ROTS.logFC", "ROTS.p-value",  "ROTS.FDR"  )

write.table(rots_re,    file = paste(AllResults_g, "6c.rots_results.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )

 
   
bool8  <-  (rownames(matrix6) ==  rownames(rots_re )  )
length(bool8)
length(bool8[bool8])

rots_re2 = cbind(rots_re, matrix6)   
rots_re3 = cbind(RUVg_matrix, rots_re2 )   
dim( rots_re2 )
dim( rots_re3 )

write.table(rots_re2,    file = paste(AllResults_g, "6d.rots_results.wilcox.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )


write.table(rots_re3,    file = paste(AllResults_g, "6e.rots_results.withMatrix.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE )


 













   
