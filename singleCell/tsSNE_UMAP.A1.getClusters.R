 
library(stringr)
library(Signac)
library(genomation)
library(Seurat) 



AllResults_g <- "tSNE_UMAP.A1.getClusters"
if( ! file.exists(AllResults_g) ) { dir.create(path=AllResults_g, recursive = TRUE) }





###########################################################################################################################################################################
prefix1="all.merged.txt"
DF1 <- read.table(prefix1, header=TRUE,   sep="\t", comment.char = "" )    
dim(DF1)
DF1[1:5, 1:5]

DF_region <- DF1[ , c(1:3)]
DF_value  <- DF1[ , -c(1:3)]
dim(DF_region)
dim(DF_value)

myRowNames <- paste(DF_region[,1],DF_region[,2],DF_region[,3], c(1:nrow(DF_region)), sep="...")
length(myRowNames)
myRowNames[1:10]

rownames(DF_value) <- myRowNames



## Number of zeros  
numOfZero <- function(x) {
  return(length(which(x == 0)))
}


## Number of zeros for each row (gene)
numOfZero1 = apply(DF_value,  1, numOfZero )
length(numOfZero1)
length(numOfZero1[numOfZero1 < 0.9*ncol(DF_value) ])
 

## Number of zeros for each column (gene)
numOfZero2 = apply(DF_value,  2, numOfZero )
length(numOfZero2)
length(numOfZero2[numOfZero2 < 0.95*nrow(DF_value) ])






dim(DF_value)

bool1 <- as.vector(  numOfZero1 < 0.9*ncol(DF_value)  )
DF_value2 <- DF_value[ bool1, ] 
dim(DF_value2)

bool2 <- as.vector(  numOfZero2 < 0.95*nrow(DF_value)  )
DF_value3 <- DF_value2[, bool2 ] 
dim(DF_value3)


sink( paste(AllResults_g,   "dimension.txt",  sep="/")  )
print(dim(DF_value))
print(dim(DF_value3))
sink()





bed_regions <- cbind(DF_region, myRowNames, rep("1", nrow(DF_region)), rep(".", nrow(DF_region)) )
dim(bed_regions)

dim(DF_value3)
bed_regions <- bed_regions[bool1, ]
dim(bed_regions)

write.table(bed_regions ,  file = paste(AllResults_g,   "bed_regions.bed",  sep="/"), 
            append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".",  row.names = F,  col.names = F )


GRanges_1 <- readBed(paste(AllResults_g,   "bed_regions.bed",  sep="/"), track.line = FALSE, remove.unusual = FALSE,  zero.based = TRUE)


matrix3 <- data.matrix(DF_value3, rownames.force = NA)

chrom_assay <- CreateChromatinAssay(
  counts  = matrix3,
  #data=matrix3,
  min.cells = 0,
  min.features = 0,
  ranges = GRanges_1, 
  genome="hg38" )


 
 

metadata <- cbind( colnames(DF_value3), colnames(DF_value3) )
rownames(metadata) <- colnames(DF_value3)
metadata <- as.data.frame(metadata)

SeuratObject_1 <- CreateSeuratObject(
  counts = chrom_assay,
  assay = "peaks",
  meta.data = metadata
)


max(DF_value3)

SeuratObject_2 <- RunTFIDF(SeuratObject_1, scale.factor = 100)
SeuratObject_2 <- FindTopFeatures(SeuratObject_2, min.cutoff = 'q0')
SeuratObject_2 <- RunSVD(SeuratObject_2)


pdf( paste(AllResults_g,   "correlation.pdf",  sep="/")  )
  DepthCor(SeuratObject_2)
dev.off()


 



start1=1

 
 
## UMAP
SeuratObject_2A <- RunUMAP(object = SeuratObject_2, reduction = 'lsi', dims = start1:20)
SeuratObject_2A <- FindNeighbors(object = SeuratObject_2A, reduction = 'lsi', dims = start1:20)
SeuratObject_2A <- FindClusters(object  = SeuratObject_2A, verbose = FALSE, algorithm = 3, n.start = 20, n.iter = 50)

pdf( paste(AllResults_g,   "UMAP.3_3.pdf",  sep="/")  )
DimPlot(object = SeuratObject_2A, label = TRUE) + NoLegend()
dev.off()





write.table(SeuratObject_2A$seurat_clusters, file =  paste(AllResults_g,   "seurat_clusters.txt",  sep="/")  , append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,            col.names = F )


 


markers_0 <- FindMarkers(
  object = SeuratObject_2A,
  latent.vars = NULL,
  ident.1 = 0,
  ident.2 = NULL,
  group.by = NULL,
  subset.ident = NULL,
  assay = NULL,
  reduction = NULL
)

write.table(markers_0, file =  paste(AllResults_g,   "markers_0.txt",  sep="/")  , append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,            col.names = T )







markers_1 <- FindMarkers(
  object = SeuratObject_2A,
  latent.vars = NULL,
  ident.1 = 1,
  ident.2 = NULL,
  group.by = NULL,
  subset.ident = NULL,
  assay = NULL,
  reduction = NULL
)

write.table(markers_1, file =  paste(AllResults_g,   "markers_1.txt",  sep="/")  , append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,            col.names = T )






markers_2 <- FindMarkers(
  object = SeuratObject_2A,
  latent.vars = NULL,
  ident.1 = 2,
  ident.2 = NULL,
  group.by = NULL,
  subset.ident = NULL,
  assay = NULL,
  reduction = NULL
)

write.table(markers_2, file =  paste(AllResults_g,   "markers_2.txt",  sep="/")  , append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,            col.names = T )







markers_3 <- FindMarkers(
  object = SeuratObject_2A,
  latent.vars = NULL,
  ident.1 = 3,
  ident.2 = NULL,
  group.by = NULL,
  subset.ident = NULL,
  assay = NULL,
  reduction = NULL
)

write.table(markers_3, file =  paste(AllResults_g,   "markers_3.txt",  sep="/")  , append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,            col.names = T )





markers_4 <- FindMarkers(
  object = SeuratObject_2A,
  latent.vars = NULL,
  ident.1 = 4,
  ident.2 = NULL,
  group.by = NULL,
  subset.ident = NULL,
  assay = NULL,
  reduction = NULL,
  logfc.threshold = 0.1, 
  min.pct = 0.01,
  
)

write.table(markers_4, file =  paste(AllResults_g,   "markers_4.txt",  sep="/")  , append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,            col.names = T )

















