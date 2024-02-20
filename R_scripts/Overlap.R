
##############################################################################################################################################################################################
library(stringr)
library(ggplot2)
library(VennDiagram)

outDir_g     <-   "Overlap"
 
if( ! file.exists(outDir_g)   ) { dir.create(outDir_g,   recursive = TRUE) }



intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}



##############################################################################################################################################################################################




 


##############################################################################################################################################################################################
 
rawDF1 <- read.table(  "BA9.v8.egenes.txt",                     header=T,   sep="\t", comment.char = "", quote ="" )    
rawDF2 <- read.table(  "BA9.v8.signif_variant_gene_pairs.txt",  header=T,   sep="\t", comment.char = "", quote ="" )    
rawDF3 <- read.table(  "BA24.v8.egenes.txt" ,                     header=T,   sep="\t", comment.char = "", quote ="" )    
rawDF4 <- read.table(  "BA24.v8.signif_variant_gene_pairs.txt" ,  header=T,   sep="\t", comment.char = "", quote ="" )    
dim(rawDF1)
dim(rawDF2)
dim(rawDF3)
dim(rawDF4)



finalDF1 <- read.table(  "our_eQTLs/Final.1_BA9.full_info.txt",   header=TRUE,   sep="\t", comment.char = "", quote ="" )    
finalDF2 <- read.table(  "our_eQTLs/Final.2_BA24.full_info.txt",  header=TRUE,   sep="\t", comment.char = "", quote ="" )    
finalDF3 <- read.table(  "our_eQTLs/Final.3_C.full_info.txt" ,    header=TRUE,   sep="\t", comment.char = "", quote ="" )    
finalDF4 <- read.table(  "our_eQTLs/Final.4_H.full_info.txt" ,    header=TRUE,   sep="\t", comment.char = "", quote ="" )    
finalDF5 <- read.table(  "our_eQTLs/Final.5_T.full_info.txt" ,    header=TRUE,   sep="\t", comment.char = "", quote ="" )    
dim(finalDF1)
dim(finalDF2)
dim(finalDF3)
dim(finalDF4)
dim(finalDF5)


mashrall  <- read.table(  "get_lfsr.m2.times.txt",   header=TRUE,   sep="\t", comment.char = "", quote ="" )    
dim(mashrall)
mashrall[1:5]
numtimes = mashrall$numVec
bool1 = (numtimes == 1)
bool2 = (numtimes == 2)
bool3 = (numtimes == 3)
bool4 = (numtimes == 4)
bool5 = (numtimes == 5)
length( bool1 )
length( bool2  )
length( bool3 )
length( bool4  )
length( bool5 )
length( bool1[bool1] )
length( bool2[bool2] )
length( bool3[bool3] )
length( bool4[bool4] )
length( bool5[bool5] )


mashrPairs = rownames( mashrall )
mashr_5regions <- mashrPairs[bool1]
length(mashr_5regions)







rawDF1_genes <- rawDF1$gene_id
rawDF1_SNPs  <- rawDF1$variant_id
rawDF2_genes <- rawDF2$gene_id
rawDF2_SNPs  <- rawDF2$variant_id
rawDF3_genes <- rawDF3$gene_id
rawDF3_SNPs  <- rawDF3$variant_id
rawDF4_genes <- rawDF4$gene_id
rawDF4_SNPs  <- rawDF4$variant_id
length( rawDF1_genes )
length( rawDF1_SNPs  )
length( rawDF2_genes )
length( rawDF2_SNPs  )
length( rawDF3_genes )
length( rawDF3_SNPs  )
length( rawDF4_genes )
length( rawDF4_SNPs  )

length( unique( rawDF1_genes ) )
length( unique( rawDF1_SNPs  ) )
length( unique( rawDF2_genes ) )
length( unique( rawDF2_SNPs  ) )
length( unique( rawDF3_genes ) )
length( unique( rawDF3_SNPs  ) )
length( unique( rawDF4_genes ) )
length( unique( rawDF4_SNPs  ) )


 
rawDF1_q <- rawDF1$qval
rawDF3_q <- rawDF3$qval
boo_1_q  <- (rawDF1_q < 0.05)
boo_3_q  <- (rawDF3_q < 0.05)
length( boo_1_q )
length( boo_3_q  )
length( boo_1_q[boo_1_q] )
length( boo_3_q[boo_3_q]  )










GTEx_BA9_Gene  = rawDF2_genes
GTEx_BA9_SNP   = rawDF2_SNPs
GTEx_BA24_Gene = rawDF4_genes
GTEx_BA24_SNP  = rawDF4_SNPs

GTEx_BA9_Gene[1:10]
GTEx_BA9_SNP[1:10]
GTEx_BA24_Gene[1:10]
GTEx_BA24_SNP[1:10]

GTEx_BA9_Pairs  = paste(GTEx_BA9_Gene,  GTEx_BA9_SNP,   sep="...")
GTEx_BA24_Pairs = paste(GTEx_BA24_Gene, GTEx_BA24_SNP,  sep="...")
  

 



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





 


GTEx_BA9_Gene_A  <- str_replace_all(string=GTEx_BA9_Gene,  pattern=".\\d+$", replacement="")
GTEx_BA24_Gene_A <- str_replace_all(string=GTEx_BA24_Gene, pattern=".\\d+$", replacement="")

final_genes_1_A <- str_replace_all(string=final_genes_1, pattern=".\\d+$", replacement="")
final_genes_2_A <- str_replace_all(string=final_genes_2, pattern=".\\d+$", replacement="")
final_genes_3_A <- str_replace_all(string=final_genes_3, pattern=".\\d+$", replacement="")
final_genes_4_A <- str_replace_all(string=final_genes_4, pattern=".\\d+$", replacement="")
final_genes_5_A <- str_replace_all(string=final_genes_5, pattern=".\\d+$", replacement="")

GTEx_BA9_Gene_A[1:10]
GTEx_BA24_Gene_A[1:10]
final_genes_1_A[1:10]
final_genes_2_A[1:10]
final_genes_3_A[1:10]
final_genes_4_A[1:10]
final_genes_5_A[1:10]



GTEx_BA9_SNP_A  <- str_replace_all(string=GTEx_BA9_SNP,    pattern="_[^_]+$", replacement="")
GTEx_BA9_SNP_A  <- str_replace_all(string=GTEx_BA9_SNP_A,  pattern="_[^_]+$", replacement="")
GTEx_BA9_SNP_A  <- str_replace_all(string=GTEx_BA9_SNP_A,  pattern="_[^_]+$", replacement="")
GTEx_BA9_SNP_A  <- str_replace_all(string=GTEx_BA9_SNP_A,  pattern="_",       replacement=":")
 
GTEx_BA24_SNP_A  <- str_replace_all(string=GTEx_BA24_SNP,    pattern="_[^_]+$", replacement="")
GTEx_BA24_SNP_A  <- str_replace_all(string=GTEx_BA24_SNP_A,  pattern="_[^_]+$", replacement="")
GTEx_BA24_SNP_A  <- str_replace_all(string=GTEx_BA24_SNP_A,  pattern="_[^_]+$", replacement="")
GTEx_BA24_SNP_A  <- str_replace_all(string=GTEx_BA24_SNP_A,  pattern="_",       replacement=":")
 

final_SNPs_1_A  <- str_replace_all(string=final_SNPs_1,      pattern=":[^:]+$", replacement="")
final_SNPs_1_A  <- str_replace_all(string=final_SNPs_1_A,    pattern=":[^:]+$", replacement="")

final_SNPs_2_A  <- str_replace_all(string=final_SNPs_2,      pattern=":[^:]+$", replacement="")
final_SNPs_2_A  <- str_replace_all(string=final_SNPs_2_A,    pattern=":[^:]+$", replacement="")

final_SNPs_3_A  <- str_replace_all(string=final_SNPs_3,      pattern=":[^:]+$", replacement="")
final_SNPs_3_A  <- str_replace_all(string=final_SNPs_3_A,    pattern=":[^:]+$", replacement="")

final_SNPs_4_A  <- str_replace_all(string=final_SNPs_4,      pattern=":[^:]+$", replacement="")
final_SNPs_4_A  <- str_replace_all(string=final_SNPs_4_A,    pattern=":[^:]+$", replacement="")

final_SNPs_5_A  <- str_replace_all(string=final_SNPs_5,      pattern=":[^:]+$", replacement="")
final_SNPs_5_A  <- str_replace_all(string=final_SNPs_5_A,    pattern=":[^:]+$", replacement="")

GTEx_BA9_SNP_A[1:10]
GTEx_BA24_SNP_A[1:10]
final_SNPs_1_A[1:10]
final_SNPs_2_A[1:10]
final_SNPs_3_A[1:10]
final_SNPs_4_A[1:10]
final_SNPs_5_A[1:10]


GTEx_BA9_Pairs_A  = paste(GTEx_BA9_Gene_A,  GTEx_BA9_SNP_A,   sep="...")
GTEx_BA24_Pairs_A = paste(GTEx_BA24_Gene_A, GTEx_BA24_SNP_A,  sep="...")

pairsQTL_1_A <- paste(final_genes_1_A, final_SNPs_1_A, sep="...")
pairsQTL_2_A <- paste(final_genes_2_A, final_SNPs_2_A, sep="...")
pairsQTL_3_A <- paste(final_genes_3_A, final_SNPs_3_A, sep="...")
pairsQTL_4_A <- paste(final_genes_4_A, final_SNPs_4_A, sep="...")
pairsQTL_5_A <- paste(final_genes_5_A, final_SNPs_5_A, sep="...")
length(pairsQTL_1_A)
length(pairsQTL_2_A)
length(pairsQTL_3_A)
length(pairsQTL_4_A)
length(pairsQTL_5_A)




 

GTEx_BA9_Pairs_A[1:5]
GTEx_BA24_Pairs_A[1:5]
pairsQTL_1_A[1:5]
mashr_5regions[1:5]



mashr_5regions_A  <- str_replace_all(string=mashr_5regions,      pattern=":[AGCT]:[AGCT]\\S+$", replacement="")
mashr_5regions_A  <- str_replace_all(string=mashr_5regions_A,    pattern="_\\d+_SNP_", replacement="...")
mashr_5regions_A[1:5]



temp  <- intersect_all(  pairsQTL_1_A,   mashr_5regions_A    )
length( temp ) 
temp  <- intersect_all(  pairsQTL_2_A,   mashr_5regions_A    )
length( temp ) 
temp  <- intersect_all(  pairsQTL_3_A,   mashr_5regions_A    )
length( temp ) 
temp  <- intersect_all(  pairsQTL_4_A,   mashr_5regions_A    )
length( temp ) 
temp  <- intersect_all(  pairsQTL_5_A,   mashr_5regions_A    )
length( temp ) 


temp  <- intersect_all(  GTEx_BA9_Pairs_A,   mashr_5regions_A    )
length( temp ) 
temp  <- intersect_all(  GTEx_BA24_Pairs_A,   mashr_5regions_A    )
length( temp ) 


 


temp  <- intersect_all(  mashr_5regions_A,   pairsQTL_1_A    )
length( temp ) 
venn.diagram(x=list("mashr_5regions_A"=mashr_5regions_A,   "pairsQTL_1_A"=pairsQTL_1_A ), 
             filename= paste(outDir_g, "5a.mashrCommon..pairsQTL_1_A.png", sep="/"), 
             height = 5000,   width = 5000,  
             resolution = 1500,   imagetype = "png",   units = "px", 
             compression ="lzw",  na = "stop",  
             main = "mashrCommon.ours_BA9_pairs", sub = NULL, 
             main.pos= c(0.5, 1.05),  main.fontface = "plain",
             main.fontfamily = "serif",  main.col ="black",
             main.cex = 1,  main.just = c(0.5, 1), 
             sub.pos = c(0.5, 1.05),  sub.fontface = "plain", 
             sub.fontfamily ="serif", sub.col = "black", sub.cex = 1,
             sub.just =c(0.5, 1), #category.names = names(x), 
             force.unique =TRUE, print.mode = "raw", sigdigs = 3, 
             direct.area =FALSE, area.vector = 0, hyper.test = TRUE, total.population = 5000000, lower.tail=FALSE, 
             fill=c("green3", "yellow3" ), cat.col=c("green3", "yellow3" ), lwd=0, margin=0.1 )





temp  <- intersect_all(  mashr_5regions_A,   pairsQTL_2_A    )
length( temp ) 
venn.diagram(x=list("mashr_5regions_A"=mashr_5regions_A,   "pairsQTL_2_A"=pairsQTL_2_A ), 
             filename= paste(outDir_g, "5b.mashrCommon..pairsQTL_2_A.png", sep="/"), 
             height = 5000,   width = 5000,  
             resolution = 1500,   imagetype = "png",   units = "px", 
             compression ="lzw",  na = "stop",  
             main = "mashrCommon..ours_BA24_pairs", sub = NULL, 
             main.pos= c(0.5, 1.05),  main.fontface = "plain",
             main.fontfamily = "serif",  main.col ="black",
             main.cex = 1,  main.just = c(0.5, 1), 
             sub.pos = c(0.5, 1.05),  sub.fontface = "plain", 
             sub.fontfamily ="serif", sub.col = "black", sub.cex = 1,
             sub.just =c(0.5, 1), #category.names = names(x), 
             force.unique =TRUE, print.mode = "raw", sigdigs = 3, 
             direct.area =FALSE, area.vector = 0, hyper.test = TRUE, total.population = 5000000, lower.tail=FALSE, 
             fill=c("green3", "yellow3" ), cat.col=c("green3", "yellow3" ), lwd=0, margin=0.1 )




temp  <- intersect_all(  mashr_5regions_A,   pairsQTL_3_A    )
length( temp ) 
venn.diagram(x=list("mashr_5regions_A"=mashr_5regions_A,   "pairsQTL_3_A"=pairsQTL_3_A ), 
             filename= paste(outDir_g, "5c.mashrCommon..pairsQTL_3_A.png", sep="/"), 
             height = 5000,   width = 5000,  
             resolution = 1500,   imagetype = "png",   units = "px", 
             compression ="lzw",  na = "stop",  
             main = "mashrCommon..ours_C_pairs", sub = NULL, 
             main.pos= c(0.5, 1.05),  main.fontface = "plain",
             main.fontfamily = "serif",  main.col ="black",
             main.cex = 1,  main.just = c(0.5, 1), 
             sub.pos = c(0.5, 1.05),  sub.fontface = "plain", 
             sub.fontfamily ="serif", sub.col = "black", sub.cex = 1,
             sub.just =c(0.5, 1), #category.names = names(x), 
             force.unique =TRUE, print.mode = "raw", sigdigs = 3, 
             direct.area =FALSE, area.vector = 0, hyper.test = TRUE, total.population = 5000000, lower.tail=FALSE, 
             fill=c("green3", "yellow3" ), cat.col=c("green3", "yellow3" ), lwd=0, margin=0.1 )







temp  <- intersect_all(  mashr_5regions_A,   pairsQTL_4_A    )
length( temp ) 
venn.diagram(x=list("mashr_5regions_A"=mashr_5regions_A,   "pairsQTL_4_A"=pairsQTL_4_A ), 
             filename= paste(outDir_g, "5d.mashrCommon..pairsQTL_4_A.png", sep="/"), 
             height = 5000,   width = 5000,  
             resolution = 1500,   imagetype = "png",   units = "px", 
             compression ="lzw",  na = "stop",  
             main = "mashrCommon..ours_H_pairs", sub = NULL, 
             main.pos= c(0.5, 1.05),  main.fontface = "plain",
             main.fontfamily = "serif",  main.col ="black",
             main.cex = 1,  main.just = c(0.5, 1), 
             sub.pos = c(0.5, 1.05),  sub.fontface = "plain", 
             sub.fontfamily ="serif", sub.col = "black", sub.cex = 1,
             sub.just =c(0.5, 1), #category.names = names(x), 
             force.unique =TRUE, print.mode = "raw", sigdigs = 3, 
             direct.area =FALSE, area.vector = 0, hyper.test = TRUE, total.population = 5000000, lower.tail=FALSE, 
             fill=c("green3", "yellow3" ), cat.col=c("green3", "yellow3" ), lwd=0, margin=0.1 )





temp  <- intersect_all(  mashr_5regions_A,   pairsQTL_5_A    )
length( temp ) 
venn.diagram(x=list("mashr_5regions_A"=mashr_5regions_A,   "pairsQTL_5_A"=pairsQTL_5_A ), 
             filename= paste(outDir_g, "5e.mashrCommon..pairsQTL_5_A.png", sep="/"), 
             height = 5000,   width = 5000,  
             resolution = 1500,   imagetype = "png",   units = "px", 
             compression ="lzw",  na = "stop",  
             main = "mashrCommon..ours_T_pairs", sub = NULL, 
             main.pos= c(0.5, 1.05),  main.fontface = "plain",
             main.fontfamily = "serif",  main.col ="black",
             main.cex = 1,  main.just = c(0.5, 1), 
             sub.pos = c(0.5, 1.05),  sub.fontface = "plain", 
             sub.fontfamily ="serif", sub.col = "black", sub.cex = 1,
             sub.just =c(0.5, 1), #category.names = names(x), 
             force.unique =TRUE, print.mode = "raw", sigdigs = 3, 
             direct.area =FALSE, area.vector = 0, hyper.test = TRUE, total.population = 5000000, lower.tail=FALSE, 
             fill=c("green3", "yellow3" ), cat.col=c("green3", "yellow3" ), lwd=0, margin=0.1 )









temp  <- intersect_all(  mashr_5regions_A,   GTEx_BA9_Pairs_A    )
length( temp ) 
venn.diagram(x=list("mashr_5regions_A"=mashr_5regions_A,   "GTEx_BA9_Pairs_A"=GTEx_BA9_Pairs_A ), 
             filename= paste(outDir_g, "6a.mashrCommon..GTEx_BA9_Pairs_A.png", sep="/"), 
             height = 5000,   width = 5000,  
             resolution = 1500,   imagetype = "png",   units = "px", 
             compression ="lzw",  na = "stop",  
             main = "mashrCommon..GTEx_BA9_Pairs_A", sub = NULL, 
             main.pos= c(0.5, 1.05),  main.fontface = "plain",
             main.fontfamily = "serif",  main.col ="black",
             main.cex = 1,  main.just = c(0.5, 1), 
             sub.pos = c(0.5, 1.05),  sub.fontface = "plain", 
             sub.fontfamily ="serif", sub.col = "black", sub.cex = 1,
             sub.just =c(0.5, 1), #category.names = names(x), 
             force.unique =TRUE, print.mode = "raw", sigdigs = 3, 
             direct.area =FALSE, area.vector = 0, hyper.test = TRUE, total.population = 5000000, lower.tail=FALSE, 
             fill=c("green3", "yellow3" ), cat.col=c("green3", "yellow3" ), lwd=0, margin=0.1 )





temp  <- intersect_all(  mashr_5regions_A,   GTEx_BA24_Pairs_A    )
length( temp ) 
venn.diagram(x=list("mashr_5regions_A"=mashr_5regions_A,   "GTEx_BA24_Pairs_A"=GTEx_BA24_Pairs_A ), 
             filename= paste(outDir_g, "6b.mashrCommon..GTEx_BA24_Pairs_A.png", sep="/"), 
             height = 5000,   width = 5000,  
             resolution = 1500,   imagetype = "png",   units = "px", 
             compression ="lzw",  na = "stop",  
             main = "mashrCommon..GTEx_BA24_Pairs_A", sub = NULL, 
             main.pos= c(0.5, 1.05),  main.fontface = "plain",
             main.fontfamily = "serif",  main.col ="black",
             main.cex = 1,  main.just = c(0.5, 1), 
             sub.pos = c(0.5, 1.05),  sub.fontface = "plain", 
             sub.fontfamily ="serif", sub.col = "black", sub.cex = 1,
             sub.just =c(0.5, 1), #category.names = names(x), 
             force.unique =TRUE, print.mode = "raw", sigdigs = 3, 
             direct.area =FALSE, area.vector = 0, hyper.test = TRUE, total.population = 5000000, lower.tail=FALSE, 
             fill=c("green3", "yellow3" ), cat.col=c("green3", "yellow3" ), lwd=0, margin=0.1 )




temp  <- intersect_all(  GTEx_BA9_Pairs_A,   GTEx_BA24_Pairs_A    )
length( temp ) 








 