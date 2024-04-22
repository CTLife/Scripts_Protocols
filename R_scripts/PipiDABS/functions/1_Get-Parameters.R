##############################################################################################################################################################################################
## PipiDABS: A Comprehensive and User-Friendly Pipeline for Downstream DNA Methylation Analysis of High-throughput Bisulfite Sequencing Data.                      
## Author: Yong Peng, yongp@outlook.com.
## Version 0.3.0, December 1st, 2019.
## Run "PipiDABS -h" to get some help.
##############################################################################################################################################################################################





##############################################################################################################################################################################################
suppressPackageStartupMessages( library(optparse) )  ## To run the script in command lines.

GetParameters_f <- function() {
	OptionList_l <- list(   ## Options list with associated default value.  16 options.
  		optparse::make_option(opt_str=c("-i", "--samplesInformation"),
			default="samplesInformation.example.txt",
			type="character",   dest="samplesInformation",
			help="Path to the design/target file with samples information. The table header (the first line) of this file must be 'sampleName samplePath groups covariates batchEffects'. For more details, please visit https://github.com/CTLife/PipiDABS [default: %default]."),

  		optparse::make_option(opt_str=c("-o", "--outDir"),
			default="outDir-PipiDABS-results",
			type="character",   dest="outDir",
			help="Path to the directory containing all the analysis results [default: %default]."),

  		optparse::make_option(opt_str=c("-l", "--lowestCoverage"),
			default=6,
			type="integer",   dest="lowestCoverage",
			help="An integer for read counts. Cytosine bases having lower coverage than this count is discarded for further analysis. That is to say, a cytosine will be kept if its coverage >= this value [default: %default]."),         

  		optparse::make_option(opt_str=c("-p", "--proportionCoveredSamples"),
			default=0.80,
			type="double",   dest="proportionCoveredSamples",
			help="The proportion (0.00~1.00) of covered samples in each group. If you need to cover all samples, please set this value to 1.00 [default: %default]."),

  		optparse::make_option(opt_str=c("-g", "--refGenome"),
			default="hg38",
			type="character",   dest="refGenome",
			help="A string that defines the genome assembly or reference genome for your input files, such as hg38, hg19, mm10 and mm9 [default: %default]."),

  		optparse::make_option(opt_str=c("-c", "--cytosineContext"),
			default="CpG",
			type="character",   dest="cytosineContext",
			help="Context of cytosine sites. This value should be CpG, CHG or CHH [default: %default]."),

  		optparse::make_option(opt_str=c("-q", "--qvalue"),
			default=0.05,
			type="double",   dest="qvalue",
			help="The q-value threshold for differentially methylated cytosines or regions  (DMCs or DMRs) [default: %default]."),

  		optparse::make_option(opt_str=c("-d", "--differenceOfMethylation"),
			default=0.20,
			type="double",   dest="differenceOfMethylation",
			help="Threshold (0.00~1.00) of DNA methylation level differences between two groups for DMCs and DMRs [default: %default]."),

  		optparse::make_option(opt_str=c("-G", "--genomewide"),
			default=T,
			type="logical",   dest="genomewide",
			help="Whether to perform genomewide analysis (T or F) [default: %default]."),

  		optparse::make_option(opt_str=c("-w", "--windowSize"),
			default=200,
			type="integer",   dest="windowSize",
			help="An integer for the size of tiling windows (bp). It is valid only for '--genomewide=T' [default: %default]."),

  		optparse::make_option(opt_str=c("-s", "--stepSize"),
			default=200,
			type="integer",   dest="stepSize",
			help="An integer for the step size of the tiling windows (bp). It is valid only for '--genomewide=T'  [default: %default]."),

  		optparse::make_option(opt_str=c("-m", "--minBases"),
			default=3,
			type="integer",   dest="minBases",
			help="Minimum number of bases to be covered in a given tiling window. For WGBS data, this value should be larger. It is valid only for '--genomewide=T'  [default: %default]."),

  		optparse::make_option(opt_str=c("-r", "--specificRegions"),
                        default="NA_NoGenomicRegions", 
                        type="character",   dest="specificRegions",
                        help="Analyze BS-seq data for some specific genomic regions or not, such as promoters and enhancers. This value is the directory of bed files with genomic regions. Please don't set this value, if no bed files need to be analyzed [default: %default]." ),

  		optparse::make_option(opt_str=c("-M", "--minBasesSpecificRegions"),
			default=3,
			type="integer",   dest="minBasesSpecificRegions",
			help="Minimum number of bases to be covered in a given specific segion, such as promoters and enhancers. For WGBS data, this value should be larger. The option '--specificRegions' is required [default: %default]."),

  		optparse::make_option(opt_str=c("-t", "--topPercentageCoverage"),
			default=0.01,
			type="double",   dest="topPercentageCoverage",
			help="If your samples are suffering from PCR bias, it would be useful to discard bases with very high read coverage. For RRBS data, we suggest that set this value to 0.001~0.050. If we set this value to 0.01, then the top 1% C sites with the highest coverage will be removed for further analysis [default: %default].") ,

  		optparse::make_option(opt_str=c("-n", "--normalize"),  
                        default="none", 
                        type="character",   dest="normalize",
                        help="Normalize coverage values between samples using a scaling factor derived from differences between mean or median of coverage distributions. A string 'none', 'mean' or 'median' which denotes no value, median or mean should be used to calculate scaling factor [default: %default]." )                                            
	)
	
	## Now parse the command line to check which option is given and get associated values.
	Parser_l <- optparse::OptionParser(usage="usage: %prog [options]",  option_list=OptionList_l, 
			description="PipiDABS: A Comprehensive and User-Friendly Pipeline for Downstream DNA Methylation Analysis of High-throughput Bisulfite Sequencing Data. Version 0.3.0, December 1st, 2019.",                             
			epilogue="For comments, bug reports etc..., please visit https://github.com/CTLife/PipiDABS or contact Yong Peng <yongp@outlook.com>."
	)
	Opt_l <- optparse::parse_args(Parser_l, args=commandArgs(trailingOnly=TRUE), positional_arguments=0)$options
	return(Opt_l)
}
##############################################################################################################################################################################################



 

##############################################################################################################################################################################################
Opt_g = GetParameters_f()  

SamplesInformation_g      <- Opt_g$samplesInformation
OutDir_g                  <- Opt_g$outDir
LowestCoverage_g          <- as.integer(Opt_g$lowestCoverage)
ProportionCoveredSamples_g<- Opt_g$proportionCoveredSamples
RefGenome_g               <- Opt_g$refGenome
CytosineContext_g         <- Opt_g$cytosineContext
Qvalue_g                  <- Opt_g$qvalue
DifferenceOfMethylation_g <- Opt_g$differenceOfMethylation
Genomewide_g              <- Opt_g$genomewide
WindowSize_g              <- as.integer(Opt_g$windowSize)
StepSize_g                <- as.integer(Opt_g$stepSize)
MinBases_g                <- as.integer(Opt_g$minBases)
SpecificRegions_g         <- Opt_g$specificRegions
MinBasesSpecificRegions_g <- as.integer(Opt_g$minBasesSpecificRegions)  
TopPercentageCoverage_g   <- Opt_g$topPercentageCoverage * 100
Normalize_g               <- Opt_g$normalize

rm(GetParameters_f)  
rm(Opt_g)  

options(digits=10)
continueOnError_f <- function() {
    print( "NOTE: THERE WAS AN ERROR HERE. We are continuing because we have set 'options(error=continue_on_error())'. " )
}
options( error=continueOnError_f )  ## This option is very important.
##############################################################################################################################################################################################


 


