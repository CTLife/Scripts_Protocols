## args[1]: file name, only csv or txt.
## args[2]: column number of gene ID.
## args[3]: if "1", input file with header; other: no header.
## for other reference genomes using: listDatasets( useMart('ensembl') )
## run this script, such as:  Rscript  Convert-gene-name.hg38.R    1_ENST/1_BA9.hyper.genes.txt    1    0


args <- commandArgs(TRUE)
## args <- c( "1_ENST/1_BA9.hyper.genes.txt", 1, 0 )

print("args: ")
print(args[1])
print(args[2])
print(args[3])
print("#############")

geneID.col <- as.numeric(args[2])
print( geneID.col )

header = FALSE
if (args[3] == "1"){
  header = TRUE
} 
print( header )


## read file by suffix of file
require(tools)
file.type <- file_ext(args[1])
file.type




file.name   = "" 
input.table = "" 
if (file.type == "txt"){
  file.name   <- sub('.txt$', '', basename(args[1]))
  input.table <- read.table(args[1], header=header, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE)
}else if(file.type == "csv"){
  file.name   <- sub('.csv$', '', basename(args[1]))
  input.table <- read.csv(args[1], header=header, comment.char="", stringsAsFactors=FALSE)
}else{
  stop("Only csv and txt are supported now.")
}
file.name    
dim( input.table  )
input.table[, geneID.col]






####################################################################################
require(biomaRt)
outPath = "Results"
if( ! file.exists(outPath)  ) { dir.create(outPath,  recursive = TRUE)  }
   
sink( paste( outPath, "z.lists.txt", sep="/" ) )
print(  listDatasets( useMart('ensembl') )  )
sink()

mart <- useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl")
print( mart )

sink(  paste( outPath, "z.Log.txt", sep="/" )   )
print(  listAttributes(mart) )
print("##########################")
print(  listFilters(mart) )
sink()


filterName <- "ensembl_transcript_id_version"   ## for others, by using listFilters(mart)
results <- getBM(attributes = c("ensembl_gene_id",  "ensembl_transcript_id",  "ensembl_peptide_id", "ensembl_transcript_id_version", "description",  "refseq_mrna", "refseq_ncrna", "external_gene_name" ),                           
                 filters = filterName,  values = input.table[, geneID.col],   mart = mart)       

head(results)
dim(results)
colnames(input.table)[geneID.col] <- filterName
input.table <- merge(input.table, results, by=filterName, all.x=TRUE)



write.table(x=input.table, file = paste(outPath, "/", file.name, "_symbol.txt", sep=""), append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,  col.names = TRUE  )





