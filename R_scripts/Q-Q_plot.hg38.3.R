######################################################################################  
library(ggmanh)
library(MeRIPtools)
library(dplyr)
library(data.table)
library(CMplot)
library(liftOver)
library(GenomicRanges)
library(regioneR)
library(Repitools)
options(bitmapType='cairo')
######################################################################################  





######################################################################################  
trait_name <- "Type 2 diabetes mellitus"
out  <- "GCST010118"
df1  <- fread("32499647-GCST010118-EFO_0001360.h.tsv.gz",  header = "auto",  sep="auto")
dim1 <- dim(df1)
dim1
if( ! file.exists(out) ) { dir.create(path=out, recursive = TRUE) }
######################################################################################  





######################################################################################  
df1[1:5, ]
df1$chromosome <- paste("chr", df1$chromosome, sep="")
df1[1:5, ]

df1 <- data.frame( "chr"=df1$chromosome , "start"= as.numeric(df1$base_pair_location ), "end"=as.numeric(df1$base_pair_location ),  "pvalue"=as.numeric(df1$p_value )  )                    
dim(df1)
dim1
df1[1:5, ]

##df1       <- toGRanges( df1 )
##chainFile <- import.chain("../hg19ToHg38.over.chain")
##df1_hg38  <- liftOver(df1, chainFile)
##df1_hg38  <- unlist(df1_hg38)
##df1_hg38  <- annoGR2DF( df1_hg38 )

df1_hg38 <- df1
dim(df1_hg38)

df1_hg38[1:5, ]

write.table(x=df1_hg38, file = paste(out, "1.GWAS.hg38.txt", sep="/"),  append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F,  col.names = TRUE)

rm( df1 )
rm( dim1 )
rm( chainFile )
######################################################################################  





## QTLs
######################################################################################  
mRNA_eQTL             <- ""
mRNA_m6Apeaks_m6AQTL  <- ""
caRNA_knownRNA_eQTL   <- ""
caRNA_m6Apeaks_m6AQTL <- ""
paRNA_eQTL     <- ""
paRNA_m6AQTL   <- ""
eRNA_eQTL      <- ""
eRNA_m6AQTL    <- ""
repeats_eQTL   <- ""
repeats_m6AQTL <- ""

mRNA_eQTL             <- fread("/storage1/1_mRNA/1B_Yoruba-57LCLs_mRNA_QTLs/1A_Gene-Expression_OK/finalList/2_finalQTLs/2_finalQTLs.txt", sep="auto")
mRNA_m6Apeaks_m6AQTL  <- fread("/storage1/1_mRNA/1B_Yoruba-57LCLs_mRNA_QTLs/2C_All-m6A-Peaks_m6A-Level_OK/finalList/2_finalQTLs/2_finalQTLs.txt", sep="auto")

caRNA_knownRNA_eQTL   <- fread("/storage1/2_caRNA/2B_Yoruba-57LCLs_caRNA_QTLs/1A_Gene-Expression_OK/finalList/2_finalQTLs/2_finalQTLs.txt", sep="auto")
caRNA_m6Apeaks_m6AQTL <- fread("/storage1/2_caRNA/2B_Yoruba-57LCLs_caRNA_QTLs/2C_All-m6A-Peaks_m6A-Level/finalList/2_finalQTLs/2_finalQTLs.txt", sep="auto")

paRNA_eQTL_1     <- fread("/storage1/2_caRNA/2B_Yoruba-57LCLs_caRNA_QTLs/1B_paRNA-Expression_OK/finalList/2_finalQTLs/2_finalQTLs.txt", sep="auto")
paRNA_m6AQTL_1   <- fread("/storage1/2_caRNA/2B_Yoruba-57LCLs_caRNA_QTLs/3B_paRNA_m6A-Level/finalList/2_finalQTLs/2_finalQTLs.txt", sep="auto")
paRNA_eQTL_2     <- fread("/storage1/2_caRNA/2B_Yoruba-57LCLs_caRNA_QTLs/1E_antisense-paRNA-Expression_OK/finalList/2_finalQTLs/2_finalQTLs.txt", sep="auto")
paRNA_m6AQTL_2   <- fread("/storage1/2_caRNA/2B_Yoruba-57LCLs_caRNA_QTLs/3D_antisense-paRNA_m6A-Level/finalList/2_finalQTLs/2_finalQTLs.txt", sep="auto")

eRNA_eQTL      <- fread("/storage1/2_caRNA/2B_Yoruba-57LCLs_caRNA_QTLs/1C_eRNA-Expression_directional_OK/finalList/2_finalQTLs/2_finalQTLs.txt", sep="auto")
eRNA_m6AQTL    <- fread("/storage1/2_caRNA/2B_Yoruba-57LCLs_caRNA_QTLs/3A_eRNA_m6A-Level_2/finalList/2_finalQTLs/2_finalQTLs.txt", sep="auto")

repeats_eQTL   <- fread("/storage1/2_caRNA/2B_Yoruba-57LCLs_caRNA_QTLs/1D_repeatRNA-Expression_each-chromosome_OK/finalList/2_finalQTLs/2_finalQTLs.txt", sep="auto")
repeats_m6AQTL <- fread("/storage1/2_caRNA/2B_Yoruba-57LCLs_caRNA_QTLs/3C_repeatRNA_m6A-Level/finalList/2_finalQTLs/2_finalQTLs.txt", sep="auto")

paRNA_eQTL   <- rbind(paRNA_eQTL_1,   paRNA_eQTL_2)
paRNA_m6AQTL <- rbind(paRNA_m6AQTL_1, paRNA_m6AQTL_2)

dim( mRNA_eQTL )
dim( mRNA_m6Apeaks_m6AQTL )
dim( caRNA_knownRNA_eQTL   )
dim( caRNA_m6Apeaks_m6AQTL )
dim( paRNA_eQTL      )
dim( paRNA_m6AQTL    )
dim( eRNA_eQTL       )
dim( eRNA_m6AQTL     )
dim( repeats_eQTL    )
dim( repeats_m6AQTL  )
######################################################################################  





######################################################################################  
gwas <- df1_hg38
dim(gwas)
gwas[1:5, ]

gwas_name                   <-  paste(gwas[,1],                   gwas[,2],                     sep="..." )

mRNA_eQTL_name              <-  paste(mRNA_eQTL$V9,               mRNA_eQTL$V10,                sep="..." )
mRNA_m6Apeaks_m6AQTL_name   <-  paste(mRNA_m6Apeaks_m6AQTL$V9,    mRNA_m6Apeaks_m6AQTL$V10,     sep="..." )
caRNA_knownRNA_eQTL_name    <-  paste(caRNA_knownRNA_eQTL$V9,   caRNA_knownRNA_eQTL$V10,    sep="..." )
caRNA_m6Apeaks_m6AQTL_name  <-  paste(caRNA_m6Apeaks_m6AQTL$V9,   caRNA_m6Apeaks_m6AQTL$V10,    sep="..." )

paRNA_eQTL_name      <-  paste(paRNA_eQTL$V9,     paRNA_eQTL$V10,        sep="..." )
paRNA_m6AQTL_name    <-  paste(paRNA_m6AQTL$V9,   paRNA_m6AQTL$V10,      sep="..." )
eRNA_eQTL_name       <-  paste(eRNA_eQTL$V9,      eRNA_eQTL$V10,         sep="..." )
eRNA_m6AQTL_name     <-  paste(eRNA_m6AQTL$V9,    eRNA_m6AQTL$V10,       sep="..." )
repeats_eQTL_name    <-  paste(repeats_eQTL$V9,   repeats_eQTL$V10,      sep="..." )
repeats_m6AQTL_name  <-  paste(repeats_m6AQTL$V9, repeats_m6AQTL$V10,    sep="..." )

mRNA_eQTL_gwas             <- gwas[ gwas_name  %in%  mRNA_eQTL_name , ]
mRNA_m6Apeaks_m6AQTL_gwas  <- gwas[ gwas_name  %in%  mRNA_m6Apeaks_m6AQTL_name , ]
caRNA_knownRNA_eQTL_gwas   <- gwas[ gwas_name  %in%  caRNA_knownRNA_eQTL_name , ]
caRNA_m6Apeaks_m6AQTL_gwas <- gwas[ gwas_name  %in%  caRNA_m6Apeaks_m6AQTL_name , ]

paRNA_eQTL_gwas      <- gwas[ gwas_name  %in%  paRNA_eQTL_name , ]
paRNA_m6AQTL_gwas    <- gwas[ gwas_name  %in%  paRNA_m6AQTL_name , ]
eRNA_eQTL_gwas       <- gwas[ gwas_name  %in%  eRNA_eQTL_name , ]
eRNA_m6AQTL_gwas     <- gwas[ gwas_name  %in%  eRNA_m6AQTL_name , ]
repeats_eQTL_gwas    <- gwas[ gwas_name  %in%  repeats_eQTL_name , ]
repeats_m6AQTL_gwas  <- gwas[ gwas_name  %in%  repeats_m6AQTL_name , ]
 
dim(mRNA_eQTL_gwas)
dim(mRNA_m6Apeaks_m6AQTL_gwas)
dim(caRNA_knownRNA_eQTL_gwas)
dim(caRNA_m6Apeaks_m6AQTL_gwas)

dim(paRNA_eQTL_gwas)
dim(paRNA_m6AQTL_gwas)
dim(eRNA_eQTL_gwas)
dim(eRNA_m6AQTL_gwas)
dim(repeats_eQTL_gwas)
dim(repeats_m6AQTL_gwas)


write.table(x=mRNA_eQTL_gwas, file = paste(out, "2.mRNA_eQTL_gwas.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F,  col.names = T)

write.table(x=mRNA_m6Apeaks_m6AQTL_gwas, file = paste(out, "2.mRNA_m6Apeaks_m6AQTL_gwas.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F,  col.names = T)

write.table(x=caRNA_knownRNA_eQTL_gwas, file = paste(out, "2.caRNA_knownRNA_eQTL_gwas.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F,  col.names = T)

write.table(x=caRNA_m6Apeaks_m6AQTL_gwas, file = paste(out, "2.caRNA_m6Apeaks_m6AQTL_gwas.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F,  col.names = T)


write.table(x=paRNA_eQTL_gwas, file = paste(out, "2.paRNA_eQTL_gwas.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F,  col.names = T)

write.table(x=paRNA_m6AQTL_gwas, file = paste(out, "2.paRNA_m6AQTL_gwas.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F,  col.names = T)

write.table(x=eRNA_eQTL_gwas, file = paste(out, "2.eRNA_eQTL_gwas.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F,  col.names = T)

write.table(x=eRNA_m6AQTL_gwas, file = paste(out, "2.eRNA_m6AQTL_gwas.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F,  col.names = T)

write.table(x=repeats_eQTL_gwas, file = paste(out, "2.repeats_eQTL_gwas.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F,  col.names = T)

write.table(x=repeats_m6AQTL_gwas, file = paste(out, "2.repeats_m6AQTL_gwas.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F,  col.names = T)
######################################################################################  





## negative control
######################################################################################  
control  <- fread("/storage1/2_Figures/negative_control_SNPs_vSampler/For_caRNA-m6A-QTLs/caRNA-m6A-QTLs.control.hg38.final37244.txt", sep="auto", fill=TRUE)
dim(control)
control[1:5, ]

control_name   <-  paste(control$V1, control$V2,  sep="..." )
controlL_gwas  <- gwas[ gwas_name  %in%  control_name , ]

dim(controlL_gwas)

write.table(x=controlL_gwas, file = paste(out, "3.controlL_gwas.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = F,  col.names = T)

######################################################################################  





######################################################################################  
p_values_QQ <- list( 'mRNA knownRNA eQTLs'        =  mRNA_eQTL_gwas$pvalue ,
                     'mRNA m6A-peaks m6A-QTLs'    =  mRNA_m6Apeaks_m6AQTL_gwas$pvalue,
                     'caRNA knownRNA eQTLs'       =  caRNA_knownRNA_eQTL_gwas$pvalue ,
                     'caRNA m6A-peaks m6A-QTLs'   =  caRNA_m6Apeaks_m6AQTL_gwas$pvalue,
                     
                     'paRNA eQTLs'        =  paRNA_eQTL_gwas$pvalue ,
                     'paRNA m6A-QTLs'     =  paRNA_m6AQTL_gwas$pvalue,
                     'eRNA eQTLs'         =  eRNA_eQTL_gwas$pvalue ,
                     'eRNA m6A-QTLs'      =  eRNA_m6AQTL_gwas$pvalue,
                     'repeatRNA eQTLs'    =  repeats_eQTL_gwas$pvalue ,
                     'repeatRNA m6A-QTLs' =  repeats_m6AQTL_gwas$pvalue,
                     
                     'Negative control' =  controlL_gwas$pvalue, 
                     'Genome wide'      =  gwas$pvalue   
                    )
 
png( paste(out, "4.png", sep="/") , width=8000, height=4000, res=1000, pointsize = 1 ) 
qqplot.pvalue(p_values_QQ, pointSize = 0.1,  legendSize = 1) 
dev.off()

pdf( paste(out, "4.pdf", sep="/") , width=12, height=6  ) 
qqplot.pvalue(p_values_QQ,  pointSize = 0.1,  legendSize = 1)   
dev.off()

myColors <- c( "pink", "purple", "olivedrab1", "gold", "skyblue", "blue", "tan", "salmon4", "lightgreen", "green4", "gray25", "gray" )

png( paste(out, "4B.png", sep="/") , width=8000, height=4000, res=1000, pointsize = 1 ) 
qqplot.pvalue(p_values_QQ, pointSize = 0.1,  legendSize = 1) + 
  theme( axis.title = element_text(size = 14 ), axis.text =  element_text(size = 14 ), legend.title = element_blank(), legend.text =  element_text(size = 14 ), axis.line = element_line(linewidth = 1) ) + 
  scale_color_manual( values = myColors ) + ggtitle( trait_name ) 
dev.off()

######################################################################################  




 
######################################################################################  
T1 = 1e-30

gwas                        <- gwas[gwas$pvalue > T1,]
controlL_gwas               <- controlL_gwas[controlL_gwas$pvalue > T1,]
mRNA_eQTL_gwas              <- mRNA_eQTL_gwas[mRNA_eQTL_gwas$pvalue > T1,]
mRNA_m6Apeaks_m6AQTL_gwas   <- mRNA_m6Apeaks_m6AQTL_gwas[mRNA_m6Apeaks_m6AQTL_gwas$pvalue > T1,]
caRNA_knownRNA_eQTL_gwas    <- caRNA_knownRNA_eQTL_gwas[caRNA_knownRNA_eQTL_gwas$pvalue > T1,]
caRNA_m6Apeaks_m6AQTL_gwas  <- caRNA_m6Apeaks_m6AQTL_gwas[caRNA_m6Apeaks_m6AQTL_gwas$pvalue > T1,]

paRNA_eQTL_gwas      <- paRNA_eQTL_gwas[paRNA_eQTL_gwas$pvalue > T1,]
paRNA_m6AQTL_gwas    <- paRNA_m6AQTL_gwas[paRNA_m6AQTL_gwas$pvalue > T1,]
eRNA_eQTL_gwas       <- eRNA_eQTL_gwas[eRNA_eQTL_gwas$pvalue > T1,]
eRNA_m6AQTL_gwas     <- eRNA_m6AQTL_gwas[eRNA_m6AQTL_gwas$pvalue > T1,]
repeats_eQTL_gwas    <- repeats_eQTL_gwas[repeats_eQTL_gwas$pvalue > T1,]
repeats_m6AQTL_gwas  <- repeats_m6AQTL_gwas[repeats_m6AQTL_gwas$pvalue > T1,]

p_values_QQ <- list( 'mRNA knownRNA eQTLs'        =  mRNA_eQTL_gwas$pvalue ,
                     'mRNA m6A-peaks m6A-QTLs'    =  mRNA_m6Apeaks_m6AQTL_gwas$pvalue,
                     'caRNA knownRNA eQTLs'       =  caRNA_knownRNA_eQTL_gwas$pvalue ,
                     'caRNA m6A-peaks m6A-QTLs'   =  caRNA_m6Apeaks_m6AQTL_gwas$pvalue,
                     
                     'paRNA eQTLs'        =  paRNA_eQTL_gwas$pvalue ,
                     'paRNA m6A-QTLs'     =  paRNA_m6AQTL_gwas$pvalue,
                     'eRNA eQTLs'         =  eRNA_eQTL_gwas$pvalue ,
                     'eRNA m6A-QTLs'      =  eRNA_m6AQTL_gwas$pvalue,
                     'repeatRNA eQTLs'    =  repeats_eQTL_gwas$pvalue ,
                     'repeatRNA m6A-QTLs' =  repeats_m6AQTL_gwas$pvalue,
                     
                     'Negative control' =  controlL_gwas$pvalue, 
                     'Genome wide'      =  gwas$pvalue   
)

myColors <- c( "pink", "purple", "olivedrab1", "gold", "skyblue", "blue", "tan", "salmon4", "lightgreen", "green4", "gray25", "gray" )

png( paste(out, "5.png", sep="/") , width=8000, height=4000, res=1000, pointsize = 1 ) 
qqplot.pvalue(p_values_QQ, pointSize = 0.1,  legendSize = 1) + 
  theme( axis.title = element_text(size = 14 ), axis.text =  element_text(size = 14 ), legend.title = element_blank(), legend.text =  element_text(size = 14 ), axis.line = element_line(linewidth = 1) ) + 
  scale_color_manual( values = myColors ) + ggtitle( trait_name ) 
dev.off()


pdf( paste(out, "5.pdf", sep="/") , width=12, height=6  ) 
qqplot.pvalue(p_values_QQ,  pointSize = 0.1,  legendSize = 1)   + 
  theme( axis.title = element_text(size = 14 ), axis.text =  element_text(size = 14 ), 
         legend.title = element_blank(), legend.text =  element_text(size = 14 ), axis.line = element_line(size = 1) ) + 
  scale_color_manual(values = myColors ) + ggtitle( trait_name )    
dev.off()
######################################################################################  



 














