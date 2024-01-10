
######################################################################################################################################################
suppressPackageStartupMessages( library(GenomicFeatures) ) 
suppressPackageStartupMessages( library(GenomicRanges) ) 
suppressPackageStartupMessages( library(clusterProfiler) ) 
suppressPackageStartupMessages( library(ReactomePA) ) 
suppressPackageStartupMessages( library(ChIPseeker) ) 
suppressPackageStartupMessages( library(DOSE) ) 
suppressPackageStartupMessages( library(ggplot2) ) 
suppressPackageStartupMessages( library(topGO) ) 
suppressPackageStartupMessages( library(enrichplot) ) 
suppressPackageStartupMessages( library(ggupset) ) 
suppressPackageStartupMessages( library(ggimage) ) 
suppressPackageStartupMessages( library(stringr) ) 


refGenome_g = "hg38"
inputDir_g  = "Genes"
outDir_g    = "anno_multiple-gene-lists"
if( ! file.exists(outDir_g)  ) { dir.create(outDir_g,  recursive = TRUE)  }


my_txdb_g      <- ""
my_orgdb_g     <- ""
my_organism_g  <- ""
my_organism2_g <- ""

if(refGenome_g == "hg38") {
    suppressPackageStartupMessages( library(TxDb.Hsapiens.UCSC.hg38.knownGene) ) 
    suppressPackageStartupMessages( library(org.Hs.eg.db) )  
    my_txdb_g  <- TxDb.Hsapiens.UCSC.hg38.knownGene
    my_orgdb_g <- "org.Hs.eg.db"
    my_organism_g <- "human"     
    my_organism2_g <- "hsa"
}

if(refGenome_g == "mm10") {
    suppressPackageStartupMessages( library(TxDb.Mmusculus.UCSC.mm10.knownGene) ) 
    suppressPackageStartupMessages( library(org.Mm.eg.db) )  
    my_txdb_g  <- TxDb.Mmusculus.UCSC.mm10.knownGene
    my_orgdb_g <- "org.Mm.eg.db"
    my_organism_g <- "mouse"
    my_organism2_g <- "mmu"
}

if(refGenome_g == "danRer11") {
    suppressPackageStartupMessages( library(TxDb.Drerio.UCSC.danRer11.refGene) ) 
    suppressPackageStartupMessages( library(org.Dr.eg.db) )  
    my_txdb_g  <- TxDb.Drerio.UCSC.danRer11.refGene
    my_orgdb_g <- "org.Dr.eg.db"
    my_organism_g <- "zebrafish"
    my_organism2_g <- "dre"
}
 

## read the input files.
peakFiles          <- list.files(path = inputDir_g, pattern = ".txt$",  full.names = TRUE  )
peakFiles_onlyName <- list.files(path = inputDir_g, pattern = ".txt$",  full.names = FALSE )

print(peakFiles_onlyName)
print(peakFiles)
######################################################################################################################################################
  
 


  



######################################################################################################################################################
genes_multi_1 <- list()
for(i in c(1:length(peakFiles)) ) { 
  ## i = 2
  print( peakFiles[i] )
  genes = read.table(file = peakFiles[i], header = FALSE, sep = "\t", quote = "",  dec = "."  )
  print( dim(genes) )
  genes  = genes[,1] 
  print( length(genes) )
  genes = str_replace_all(string=genes, pattern="\\.\\d+$", replacement="")
  genes_multi_1[[i]] =  genes 
}


genes_multi_1[[1]]
genes_multi_1[[2]]
genes_multi_1[[3]]
genes_multi_1[[4]]
genes_multi_1[[5]]
genes_multi_1[[6]]


genes_multi_2 <- list(
  X1 =  genes_multi_1[[1]], 
  X2 =  genes_multi_1[[2]], 
  X3 =  genes_multi_1[[3]], 
  X4 =  genes_multi_1[[4]], 
  X5 =  genes_multi_1[[5]], 
  X6 =  genes_multi_1[[6]]  
)

names(genes_multi_2)
######################################################################################################################################################

 





######################################################################################################################################################
comp_enrichGO_BP <- compareCluster(geneCluster = genes_multi_2,   fun = "enrichGO", pvalueCutoff  = 0.1,
                                  pAdjustMethod = "BH", keyType="ENSEMBL",  OrgDb=my_orgdb_g,  ont = "BP" )

comp_enrichGO_BP_drop1 <- dropGO(comp_enrichGO_BP, level = c(1:4), term = NULL) ## drop specific GO terms or level
comp_enrichGO_BP_drop2 <- dropGO(comp_enrichGO_BP, level = c(1:6), term = NULL) ## drop specific GO terms or level
comp_enrichGO_BP_drop3 <- dropGO(comp_enrichGO_BP, level = c(1:8), term = NULL) ## drop specific GO terms or level

comp_enrichGO_BP_simplify       <- clusterProfiler::simplify(comp_enrichGO_BP,       cutoff=0.7, by="p.adjust", select_fun=min)
comp_enrichGO_BP_drop1_simplify <- clusterProfiler::simplify(comp_enrichGO_BP_drop1, cutoff=0.7, by="p.adjust", select_fun=min)
comp_enrichGO_BP_drop2_simplify <- clusterProfiler::simplify(comp_enrichGO_BP_drop2, cutoff=0.7, by="p.adjust", select_fun=min)
comp_enrichGO_BP_drop3_simplify <- clusterProfiler::simplify(comp_enrichGO_BP_drop3, cutoff=0.7, by="p.adjust", select_fun=min)

pdf( file=paste(outDir_g, "1A_comp_enrichGO_BP.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
    dotplot(comp_enrichGO_BP, showCategory = 15, title = "comp_enrichGO_BP")
    dotplot(comp_enrichGO_BP, showCategory = 10, title = "comp_enrichGO_BP")
    dotplot(comp_enrichGO_BP, showCategory = 5,  title = "comp_enrichGO_BP")
dev.off()

pdf( file=paste(outDir_g, "1B_comp_enrichGO_BP_drop1.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
    dotplot(comp_enrichGO_BP_drop1, showCategory = 15, title = "comp_enrichGO_BP")
    dotplot(comp_enrichGO_BP_drop1, showCategory = 10, title = "comp_enrichGO_BP")
    dotplot(comp_enrichGO_BP_drop1, showCategory = 5,  title = "comp_enrichGO_BP")
dev.off()

pdf( file=paste(outDir_g, "1C_comp_enrichGO_BP_drop2.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_BP_drop2, showCategory = 15, title = "comp_enrichGO_BP")
dotplot(comp_enrichGO_BP_drop2, showCategory = 10, title = "comp_enrichGO_BP")
dotplot(comp_enrichGO_BP_drop2, showCategory = 5,  title = "comp_enrichGO_BP")
dev.off()

pdf( file=paste(outDir_g, "1D_comp_enrichGO_BP_drop3.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_BP_drop3, showCategory = 15, title = "comp_enrichGO_BP")
dotplot(comp_enrichGO_BP_drop3, showCategory = 10, title = "comp_enrichGO_BP")
dotplot(comp_enrichGO_BP_drop3, showCategory = 5,  title = "comp_enrichGO_BP")
dev.off()

pdf( file=paste(outDir_g, "1E_comp_enrichGO_BP_simplify.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_BP_simplify, showCategory = 15, title = "comp_enrichGO_BP")
dotplot(comp_enrichGO_BP_simplify, showCategory = 10, title = "comp_enrichGO_BP")
dotplot(comp_enrichGO_BP_simplify, showCategory = 5,  title = "comp_enrichGO_BP")
dev.off()

pdf( file=paste(outDir_g, "1F_comp_enrichGO_BP_drop1_simplify.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_BP_drop1_simplify, showCategory = 15, title = "comp_enrichGO_BP")
dotplot(comp_enrichGO_BP_drop1_simplify, showCategory = 10, title = "comp_enrichGO_BP")
dotplot(comp_enrichGO_BP_drop1_simplify, showCategory = 5,  title = "comp_enrichGO_BP")
dev.off()

pdf( file=paste(outDir_g, "1G_comp_enrichGO_BP_drop2_simplify.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_BP_drop2_simplify, showCategory = 15, title = "comp_enrichGO_BP")
dotplot(comp_enrichGO_BP_drop2_simplify, showCategory = 10, title = "comp_enrichGO_BP")
dotplot(comp_enrichGO_BP_drop2_simplify, showCategory = 5,  title = "comp_enrichGO_BP")
dev.off()

pdf( file=paste(outDir_g, "1H_comp_enrichGO_BP_drop3_simplify.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_BP_drop3_simplify, showCategory = 15, title = "comp_enrichGO_BP")
dotplot(comp_enrichGO_BP_drop3_simplify, showCategory = 10, title = "comp_enrichGO_BP")
dotplot(comp_enrichGO_BP_drop3_simplify, showCategory = 5,  title = "comp_enrichGO_BP")
dev.off()
######################################################################################################################################################

 



 





######################################################################################################################################################
comp_enrichGO_MF <- compareCluster(geneCluster = genes_multi_2,   fun = "enrichGO", pvalueCutoff  = 0.1,
                                   pAdjustMethod = "BH", keyType="ENSEMBL",  OrgDb=my_orgdb_g,  ont = "MF" )

comp_enrichGO_MF_drop1 <- dropGO(comp_enrichGO_MF, level = c(1:4), term = NULL) ## drop specific GO terms or level
comp_enrichGO_MF_drop2 <- dropGO(comp_enrichGO_MF, level = c(1:6), term = NULL) ## drop specific GO terms or level
comp_enrichGO_MF_drop3 <- dropGO(comp_enrichGO_MF, level = c(1:8), term = NULL) ## drop specific GO terms or level

comp_enrichGO_MF_simplify       <- clusterProfiler::simplify(comp_enrichGO_MF,       cutoff=0.7, by="p.adjust", select_fun=min)
comp_enrichGO_MF_drop1_simplify <- clusterProfiler::simplify(comp_enrichGO_MF_drop1, cutoff=0.7, by="p.adjust", select_fun=min)
comp_enrichGO_MF_drop2_simplify <- clusterProfiler::simplify(comp_enrichGO_MF_drop2, cutoff=0.7, by="p.adjust", select_fun=min)
comp_enrichGO_MF_drop3_simplify <- clusterProfiler::simplify(comp_enrichGO_MF_drop3, cutoff=0.7, by="p.adjust", select_fun=min)

pdf( file=paste(outDir_g, "2A_comp_enrichGO_MF.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_MF, showCategory = 15, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF, showCategory = 10, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF, showCategory = 5,  title = "comp_enrichGO_MF")
dev.off()

pdf( file=paste(outDir_g, "2B_comp_enrichGO_MF_drop1.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_MF_drop1, showCategory = 15, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_drop1, showCategory = 10, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_drop1, showCategory = 5,  title = "comp_enrichGO_MF")
dev.off()

pdf( file=paste(outDir_g, "2C_comp_enrichGO_MF_drop2.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_MF_drop2, showCategory = 15, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_drop2, showCategory = 10, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_drop2, showCategory = 5,  title = "comp_enrichGO_MF")
dev.off()

pdf( file=paste(outDir_g, "2D_comp_enrichGO_MF_drop3.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_MF_drop3, showCategory = 15, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_drop3, showCategory = 10, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_drop3, showCategory = 5,  title = "comp_enrichGO_MF")
dev.off()

pdf( file=paste(outDir_g, "2E_comp_enrichGO_MF_simplify.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_MF_simplify, showCategory = 15, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_simplify, showCategory = 10, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_simplify, showCategory = 5,  title = "comp_enrichGO_MF")
dev.off()

pdf( file=paste(outDir_g, "2F_comp_enrichGO_MF_drop1_simplify.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_MF_drop1_simplify, showCategory = 15, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_drop1_simplify, showCategory = 10, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_drop1_simplify, showCategory = 5,  title = "comp_enrichGO_MF")
dev.off()

pdf( file=paste(outDir_g, "2G_comp_enrichGO_MF_drop2_simplify.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_MF_drop2_simplify, showCategory = 15, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_drop2_simplify, showCategory = 10, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_drop2_simplify, showCategory = 5,  title = "comp_enrichGO_MF")
dev.off()

pdf( file=paste(outDir_g, "2H_comp_enrichGO_MF_drop3_simplify.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_MF_drop3_simplify, showCategory = 15, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_drop3_simplify, showCategory = 10, title = "comp_enrichGO_MF")
dotplot(comp_enrichGO_MF_drop3_simplify, showCategory = 5,  title = "comp_enrichGO_MF")
dev.off()
######################################################################################################################################################









######################################################################################################################################################
comp_enrichGO_CC <- compareCluster(geneCluster = genes_multi_2,   fun = "enrichGO", pvalueCutoff  = 0.1,
                                   pAdjustMethod = "BH", keyType="ENSEMBL",  OrgDb=my_orgdb_g,  ont = "CC" )

comp_enrichGO_CC_drop1 <- dropGO(comp_enrichGO_CC, level = c(1:4), term = NULL) ## drop specific GO terms or level
comp_enrichGO_CC_drop2 <- dropGO(comp_enrichGO_CC, level = c(1:6), term = NULL) ## drop specific GO terms or level
comp_enrichGO_CC_drop3 <- dropGO(comp_enrichGO_CC, level = c(1:8), term = NULL) ## drop specific GO terms or level

comp_enrichGO_CC_simplify       <- clusterProfiler::simplify(comp_enrichGO_CC,       cutoff=0.7, by="p.adjust", select_fun=min)
comp_enrichGO_CC_drop1_simplify <- clusterProfiler::simplify(comp_enrichGO_CC_drop1, cutoff=0.7, by="p.adjust", select_fun=min)
comp_enrichGO_CC_drop2_simplify <- clusterProfiler::simplify(comp_enrichGO_CC_drop2, cutoff=0.7, by="p.adjust", select_fun=min)
comp_enrichGO_CC_drop3_simplify <- clusterProfiler::simplify(comp_enrichGO_CC_drop3, cutoff=0.7, by="p.adjust", select_fun=min)

pdf( file=paste(outDir_g, "3A_comp_enrichGO_CC.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_CC, showCategory = 15, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC, showCategory = 10, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC, showCategory = 5,  title = "comp_enrichGO_CC")
dev.off()

pdf( file=paste(outDir_g, "3B_comp_enrichGO_CC_drop1.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_CC_drop1, showCategory = 15, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_drop1, showCategory = 10, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_drop1, showCategory = 5,  title = "comp_enrichGO_CC")
dev.off()

pdf( file=paste(outDir_g, "3C_comp_enrichGO_CC_drop2.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_CC_drop2, showCategory = 15, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_drop2, showCategory = 10, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_drop2, showCategory = 5,  title = "comp_enrichGO_CC")
dev.off()

pdf( file=paste(outDir_g, "3D_comp_enrichGO_CC_drop3.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_CC_drop3, showCategory = 15, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_drop3, showCategory = 10, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_drop3, showCategory = 5,  title = "comp_enrichGO_CC")
dev.off()

pdf( file=paste(outDir_g, "3E_comp_enrichGO_CC_simplify.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_CC_simplify, showCategory = 15, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_simplify, showCategory = 10, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_simplify, showCategory = 5,  title = "comp_enrichGO_CC")
dev.off()

pdf( file=paste(outDir_g, "3F_comp_enrichGO_CC_drop1_simplify.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_CC_drop1_simplify, showCategory = 15, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_drop1_simplify, showCategory = 10, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_drop1_simplify, showCategory = 5,  title = "comp_enrichGO_CC")
dev.off()

pdf( file=paste(outDir_g, "3G_comp_enrichGO_CC_drop2_simplify.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_CC_drop2_simplify, showCategory = 15, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_drop2_simplify, showCategory = 10, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_drop2_simplify, showCategory = 5,  title = "comp_enrichGO_CC")
dev.off()

pdf( file=paste(outDir_g, "3H_comp_enrichGO_CC_drop3_simplify.pdf", sep="/"),  width=length(peakFiles_onlyName)*0.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichGO_CC_drop3_simplify, showCategory = 15, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_drop3_simplify, showCategory = 10, title = "comp_enrichGO_CC")
dotplot(comp_enrichGO_CC_drop3_simplify, showCategory = 5,  title = "comp_enrichGO_CC")
dev.off()
######################################################################################################################################################






######################################################################################################################################################
comp_enrichKEGG <- compareCluster(geneCluster   = genes_multi_2,
                                  fun           = "enrichKEGG",
                                  pvalueCutoff  = 0.1, 
                                  keyType       = "ENSEMBL", 
                                  pAdjustMethod = "BH"  )

pdf( file=paste(outDir_g, "4_comp_enrichKEGG.pdf", sep="/"),  width=length(peakFiles_onlyName)*1.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichKEGG, showCategory = 15, title = "comp_enrichKEGG")
dotplot(comp_enrichKEGG, showCategory = 10, title = "comp_enrichKEGG")
dotplot(comp_enrichKEGG, showCategory = 5,  title = "comp_enrichKEGG")
dev.off()




 
comp_enrichPathway <- compareCluster(geneCluster   = genes_multi_2,
                                     fun           = "enrichPathway",
                                     pvalueCutoff  = 0.1,
                                     keyType       = "ENSEMBL", 
                                     pAdjustMethod = "BH"  )

pdf( file=paste(outDir_g, "5_comp_enrichPathway.pdf", sep="/"),  width=length(peakFiles_onlyName)*1.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichPathway, showCategory = 15, title = "comp_enrichPathway")
dotplot(comp_enrichPathway, showCategory = 10, title = "comp_enrichPathway")
dotplot(comp_enrichPathway, showCategory = 5,  title = "comp_enrichPathway")
dev.off()



 
comp_enrichDO <- compareCluster(geneCluster   = genes_multi_2,
                                fun           = "enrichDO",
                                pvalueCutoff  = 0.1, 
                                keyType       = "ENSEMBL", 
                                pAdjustMethod = "BH"  )
 
pdf( file=paste(outDir_g, "6_comp_enrichDO.pdf", sep="/"),  width=length(peakFiles_onlyName)*1.5 + 3,  height=length(peakFiles_onlyName)*1.5 + 5 )
dotplot(comp_enrichDO, showCategory = 20, title = "comp_enrichDO")
dotplot(comp_enrichDO, showCategory = 10, title = "comp_enrichDO")
dotplot(comp_enrichDO, showCategory = 5,  title = "comp_enrichDO")
dev.off()
######################################################################################################################################################







 
