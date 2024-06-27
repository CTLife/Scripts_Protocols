##############################################################################################################################################################################################
suppressPackageStartupMessages( library(optparse) )  ## To run the script in command lines.

GetParameters_f <- function() {
  OptionList_l <- list(   ## Options list with associated default value.  16 options.
      optparse::make_option(opt_str=c("-i", "--input"),
      default="2A_multiBamSummary-bins/1000bp-bins.rawCounts.txt",
      type="character",   dest="input",
      help="Input file [default: %default]."),

      optparse::make_option(opt_str=c("-o", "--outDir"),
      default="4_getFinalMatrix/rawCounts_1kbBins",
      type="character",   dest="outDir",
      help="Path to the directory containing all the analysis results [default: %default]."),

      optparse::make_option(opt_str=c("-t", "--top"),
      default=10,
      type="integer",   dest="top",
      help=" Top x%, x=10,20,30,40,50,........ [default: %default].")         
   )
  
  ## Now parse the command line to check which option is given and get associated values.
  Parser_l <- optparse::OptionParser(usage="usage: %prog [options]",  option_list=OptionList_l, 
      description=" Version 1.1, December 1st, 2023.",                             
      epilogue="For comments, bug reports etc..., please contact Yong Peng <yongp@outlook.com>."
  )
  Opt_l <- optparse::parse_args(Parser_l, args=commandArgs(trailingOnly=TRUE), positional_arguments=0)$options
  return(Opt_l)
}
##############################################################################################################################################################################################



 

##############################################################################################################################################################################################
Opt_g = GetParameters_f()  

input_g   <- Opt_g$input
outDir_g  <- Opt_g$outDir
top_g     <- as.integer(Opt_g$top)
 
rm(GetParameters_f)  
rm(Opt_g)  
if( ! file.exists(outDir_g)   ) { dir.create(outDir_g,   recursive = TRUE) }
##############################################################################################################################################################################################


 
 

##############################################################################################################################################################################################
DF_A <- read.table(input_g,  header=TRUE,   sep="\t", comment.char = "" )     
dim( DF_A )
##DF_A[1:20,  ]

rawMatrix_1 <- DF_A[, -c(1:3)]
dim(rawMatrix_1)
##rawMatrix_1[1:10,  ] 


index1 <- 11 - top_g/10

## Top 30%
TopXpercent <- function(x) {
  x = as.numeric( x )
  tempX = as.numeric( quantile(x, probs = seq(0, 1, 0.1),  na.rm = TRUE ) )   
  return( tempX[index1] )
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

write.table( DF_B ,   file = paste(outDir_g, "1A.matrix.regions.txt", sep="/"), 
             append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".",  row.names = F,  col.names = T )

write.table( rawMatrix_2 ,   file = paste(outDir_g, "1B.matrix.txt", sep="/"), 
             append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".",  row.names = F,  col.names = T )

write.table( DF_A[bool1 , 1:3] ,   file = paste(outDir_g, "1C.regions.txt", sep="/"), 
             append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".",  row.names = F,  col.names = T )
 

 
