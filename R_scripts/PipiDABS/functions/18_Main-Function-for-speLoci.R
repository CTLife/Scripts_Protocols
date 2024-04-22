 


#dataFrame_temp2 <- data.frame(
#  mysampleID  = c(mySampleID_NC_g,  mySampleID_IVF_fresh_g),
#  mytreatment = c(myTreatment_NC_g, myTreatment_IVF_fresh_g),
#  mysex       = c(Sex_NC_g,  Sex_IVF_fresh_g),
#  mytech      = c(Tech_NC_g, Tech_IVF_fresh_g)    
#)
myMainFunction_speRegions_g  <- function(  myobj_temp2,   path_temp2, speRegions_temp2,   qvalue_temp2, differenceOfMethylation_temp2, mergeDistance_temp2=100,   binBases_temp2, dataFrame_temp2, numCores_temp2=8  ) {                                                   
  if( ! file.exists(path_temp2) ) { dir.create(path_temp2, recursive = TRUE) }
  sink( file=paste(path_temp2, "values_of_parameters.txt", sep="/") )
     print("path_temp2:")
     print(path_temp2)
     cat("\n\n\n")
     print("qvalue_temp2:")
     print(qvalue_temp2)
     cat("\n\n\n")
     print("differenceOfMethylation_temp2:")
     print(differenceOfMethylation_temp2)
     cat("\n\n\n")
     print("mergeDistance_temp2:")
     print(mergeDistance_temp2)
     cat("\n\n\n")
     print("speRegions_temp2:")
     print(speRegions_temp2)
     cat("\n\n\n")
     print("binBases_temp2:")
     print(binBases_temp2)
     cat("\n\n\n")
     print("dataFrame_temp2:")
     print(dataFrame_temp2)
     cat("\n\n\n")
     print("numCores_temp2:")
     print(numCores_temp2)
     cat("\n\n\n")
  sink()   

  meth_2two <- reorganize(myobj_temp2,  sample.ids = as.vector(dataFrame_temp2$mysampleID),  treatment  = as.vector(dataFrame_temp2$mytreatment) )
  
  path_temp2_sub1 = paste(path_temp2, "1_stats_information", sep="/")
  if( ! file.exists(path_temp2_sub1) ) { dir.create(path_temp2_sub1, recursive = TRUE) }
  
  write.table(dataFrame_temp2 , 
              file = paste(path_temp2_sub1,   "0_dataFrame.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  sink( file=paste(path_temp2_sub1,  "1_select-subSets.txt", sep="/")  )
  print( length(meth_2two) )
  print( getSampleID(meth_2two) )
  print( getTreatment(meth_2two) )
  sink()
  
  meth_2two = regionCounts(meth_2two,  speRegions_temp2,   cov.bases=binBases_temp2,  strand.aware=FALSE)  
  mat_2two  = percMethylation( meth_2two )
  
  sink( file=paste(path_temp2_sub1 , "2_dimensions-tiles-merged.txt", sep="/")  )
  print("#########dimensions:")
  print( dim(meth_2two)  )   
  print( dim(mat_2two)   )
  sink()
  
  
  write.table(meth_2two , 
              file = paste(path_temp2_sub1,   "3A_meth-tiles.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  write.table(mat_2two , 
              file = paste(path_temp2_sub1,   "3B_mat-tiles.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  write.table(getData(meth_2two)[,1:4] , 
              file = paste(path_temp2_sub1,   "3C_regions-tiles.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  
  
  meth_sub4_matrix_g <- getData(meth_2two)
  mySamples_firstRow = colnames( mat_2two )
  myCoverageMatrix = mat_2two 
  myMeLevelMatrix  = mat_2two 
  for(i  in  c(1: ncol(myCoverageMatrix)) )  {
    myCoverageMatrix[,i] = meth_sub4_matrix_g[,3*i+2]
    myMeLevelMatrix[,i]  = 100*meth_sub4_matrix_g[,3*i+3]/meth_sub4_matrix_g[,3*i+2] 
  }
  
  write.table(myCoverageMatrix , 
              file = paste(path_temp2_sub1,   "4A_totalCoverage_C+T.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  write.table(myMeLevelMatrix , 
              file = paste(path_temp2_sub1,   "4B_same-as-3B_mat-tiles.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  
  myCoverageMatrix_vector = as.vector( myCoverageMatrix ) 
  myMeLevelMatrix_vector  = as.vector( myMeLevelMatrix ) 
  
  
  pdf( file=paste(path_temp2_sub1, "5_Coverage_MeLevel_Cor_poolAllSamples.pdf", sep="/")  )
  par(mfrow=c(3,1))
  myTempBool3 =  (myCoverageMatrix_vector<=50) 
  myTempDataframe3 = data.frame(x3=myCoverageMatrix_vector[myTempBool3], y3=myMeLevelMatrix_vector[myTempBool3])
  boxplot(y3 ~ x3, data = myTempDataframe3  , xlab="Coverage", ylab="Methylation Level (%)", main="All Samples") 
  myTempFunction100 <- function() {
    myTempBool4 =  ( (myCoverageMatrix_vector>50)  & (myCoverageMatrix_vector<=100) )
    myTempDataframe4 = data.frame(x4=myCoverageMatrix_vector[myTempBool4], y4=myMeLevelMatrix_vector[myTempBool4] )
    boxplot(y4 ~ x4, data = myTempDataframe4  , xlab="Coverage", ylab="Methylation Level (%)", main="All Samples") 
    myTempBool5 =  ( (myCoverageMatrix_vector>100)  & (myCoverageMatrix_vector<=150) )
    myTempDataframe5 = data.frame(x5=myCoverageMatrix_vector[myTempBool5], y5=myMeLevelMatrix_vector[myTempBool5] )
    boxplot(y5 ~ x5, data = myTempDataframe5 , xlab="Coverage", ylab="Methylation Level (%)", main="All Samples" ) 
    myTempBool6 =  ( (myCoverageMatrix_vector>150)  & (myCoverageMatrix_vector<=200) )
    myTempDataframe6 = data.frame(x6=myCoverageMatrix_vector[myTempBool6], y6=myMeLevelMatrix_vector[myTempBool6] )
    boxplot(y6 ~ x6, data = myTempDataframe6 , xlab="Coverage", ylab="Methylation Level (%)", main="All Samples" ) 
    myTempBool6 =  ( (myCoverageMatrix_vector>200)  & (myCoverageMatrix_vector<=300) )
    myTempDataframe6 = data.frame(x6=myCoverageMatrix_vector[myTempBool6], y6=myMeLevelMatrix_vector[myTempBool6] )
    boxplot(y6 ~ x6, data = myTempDataframe6 , xlab="Coverage", ylab="Methylation Level (%)", main="All Samples" ) 
    myTempBool7 = (myCoverageMatrix_vector>300) 
    myTempDataframe7 = data.frame(x7=myCoverageMatrix_vector[myTempBool7], y7=myMeLevelMatrix_vector[myTempBool7] )
    boxplot(y7 ~ x7, data = myTempDataframe7 , xlab="Coverage", ylab="Methylation Level (%)", main="All Samples" ) 
  }
  tryCatch(
    myTempFunction100(),
    error = function(err){"Tiles_000111222"}
  )
  dev.off()
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
 
  
  
  sink( file=paste(path_temp2_sub1, "5A_Coverage.allSample.txt", sep="/")  )
  cat(   "min", "\t", "mean", "\t",  "median", "\t",  "max", "\t", "sample", "\n"  )    
  cat(   min(myCoverageMatrix_vector, na.rm = TRUE), "\t", mean(myCoverageMatrix_vector, na.rm = TRUE), "\t", 
         median(myCoverageMatrix_vector, na.rm = TRUE), "\t",  max(myCoverageMatrix_vector, na.rm = TRUE), "\t", "all", "\n"  )    
  sink()
  
  sink( file=paste(path_temp2_sub1, "5B_MeLevel.allSample.txt", sep="/")  )
  cat(   "min", "\t", "mean", "\t",  "median", "\t",  "max", "\t", "sample", "\n"  )    
  cat(   min(myMeLevelMatrix_vector, na.rm = TRUE), "\t", mean(myMeLevelMatrix_vector, na.rm = TRUE), "\t",  
         median(myMeLevelMatrix_vector, na.rm = TRUE), "\t",  max(myMeLevelMatrix_vector, na.rm = TRUE), "\t", "all", "\n"  )    
  sink()
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
   

  pdf( file=paste(path_temp2_sub1, "6A_Coverage.allSample.pdf", sep="/")  )
  print( hist(myCoverageMatrix_vector) )
  print( qplot(myCoverageMatrix_vector, binwidth=10)  )
  print( qplot(myCoverageMatrix_vector, binwidth=5) ) 
  print( qplot(myCoverageMatrix_vector, binwidth=1) )  
  print( qplot(myCoverageMatrix_vector, binwidth=10, xlim=c(0, 2000) )  )
  print( qplot(myCoverageMatrix_vector, binwidth=5,  xlim=c(0, 2000) ) ) 
  print( qplot(myCoverageMatrix_vector, binwidth=1,  xlim=c(0, 2000) ) )  
  print( qplot(myCoverageMatrix_vector, binwidth=5,  xlim=c(0, 1000) ) ) 
  print( qplot(myCoverageMatrix_vector, binwidth=5,  xlim=c(1000, 2000) ) )     
  dev.off() 
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
  
  pdf( file=paste(path_temp2_sub1, "6B_MeLevel.allSample.pdf", sep="/")  )
  print( hist(myMeLevelMatrix_vector) )
  print( qplot(myMeLevelMatrix_vector, binwidth=10)  )
  print( qplot(myMeLevelMatrix_vector, binwidth=5) ) 
  print( qplot(myMeLevelMatrix_vector, binwidth=1)  ) 
  dev.off() 
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
 
  pdf( file=paste(path_temp2_sub1, "7A_Coverage_eachSample.pdf", sep="/")  )
  ## par(mfrow=c(2,2))
  for(i  in  c(1: ncol(myCoverageMatrix)) )  {
    myTempVec1 = myCoverageMatrix[,i]
    myTitle1 = as.vector( dataFrame_temp2$mysampleID )[i]
    print( hist(myTempVec1, main=myTitle1) )
    print( qplot(myTempVec1, binwidth=10, main=myTitle1)  )
    print( qplot(myTempVec1, binwidth=5, main=myTitle1)  )
    print( qplot(myTempVec1, binwidth=1, main=myTitle1)  )
  }
  dev.off()
 
  pdf( file=paste(path_temp2_sub1, "7B_MeLevel_eachSample.pdf", sep="/")  )
  ## par(mfrow=c(2,2))
  for(i  in  c(1: ncol(myCoverageMatrix)) )  {
    myTempVec1 = myMeLevelMatrix[,i]
    myTitle1 = as.vector( dataFrame_temp2$mysampleID )[i]
    print( hist(myTempVec1, main=myTitle1) )
    print( qplot(myTempVec1, binwidth=10, main=myTitle1)  )
    print( qplot(myTempVec1, binwidth=5, main=myTitle1)  )
    print( qplot(myTempVec1, binwidth=1, main=myTitle1)  )
  }
  dev.off()
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
 
 
  myNumberSamples = length( as.vector(dataFrame_temp2$mysampleID) )   
  
  path_temp2_sub2 = paste(path_temp2, "2_HierarchicalClustering_byMethylKit", sep="/")
  if( ! file.exists(path_temp2_sub2) ) { dir.create(path_temp2_sub2, recursive = TRUE) }
  tryCatch(
      MyCluster_3_g(  mymeth2=meth_2two ,  path2=path_temp2_sub2,     file2="HierarchicalClustering_byMethylKit_",   width2 = myNumberSamples/3 + 2 ,   height2=myNumberSamples/20 + 2 ),
      error = function(err){"789000"}
  )
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
   
  ## MDS（multidimensional scaling）多维尺度分析
  ## Classical (Metric) Multidimensional Scaling
  path_temp2_sub3 = paste(path_temp2, "3_MultidimensionalScaling", sep="/")
  if( ! file.exists(path_temp2_sub3) ) { dir.create(path_temp2_sub3, recursive = TRUE) }
  MyMultidimensionalScaling_1_g(  meLevelMatrix2=mat_2two,   path2=path_temp2_sub3,   dataFrame_temp2=dataFrame_temp2  )
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
   
  path_temp2_sub4 = paste(path_temp2, "4_PCAinfor_byMethylKit", sep="/")
  if( ! file.exists(path_temp2_sub4) ) { dir.create(path_temp2_sub4, recursive = TRUE) }
  tryCatch(
      MyPCA_3A_g(  mymeth2=meth_2two ,  path2=path_temp2_sub4,     file2="PCAinfor_byMethylKit_",   width2=7,   height2=5,  dataFrame_temp2=dataFrame_temp2   ),
      error = function(err){"123000"}
  )
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
                                                                 
  path_temp2_sub5 = paste(path_temp2, "5_DMR", sep="/")
  if( ! file.exists(path_temp2_sub5) ) { dir.create(path_temp2_sub5, recursive = TRUE) }
  tryCatch(
        myDiff_DMC_DMR_g(  methobj2=meth_2two,   path2=path_temp2_sub5,    qvalue2=qvalue_temp2, differenceOfMethylation2=differenceOfMethylation_temp2,  mergeDistance2=mergeDistance_temp2,   numCores2=numCores_temp2, dataFrame2=dataFrame_temp2  )  ,
        error = function(err){"5_DMR_000"}
  )
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
   


  meth_region_rmNA   =   meth_2two[ ! is.na( rowSums(mat_2two) ), ]
  
  path_temp2_sub7 = paste(path_temp2, "6_Correlation", sep="/")
  if( ! file.exists(path_temp2_sub7) ) { dir.create(path_temp2_sub7, recursive = TRUE) }
  
  sink( file=paste(path_temp2_sub7, "1A_pearsonCorrelation-regions_rmNA.txt", sep="/")  )
  cat( getCorrelation( meth_region_rmNA , method = "pearson",   plot=FALSE  ) )
  sink()
  
  sink( file=paste(path_temp2_sub7, "1B_spearmanCorrelation-regions_rmNA.txt", sep="/")  )
  getCorrelation(  meth_region_rmNA , method = "spearman",  plot=FALSE  )
  sink()
    tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
   

  myCorRaw_1 <- function() {
    sink( file=paste(path_temp2_sub7, "2A_pearsonCorrelation-regions_rmNA.txt", sep="/")  )
    cat( getCorrelation( meth_2two , method = "pearson",   plot=FALSE  ) )
    sink()
  
    sink( file=paste(path_temp2_sub7, "2B_spearmanCorrelation-regions_rmNA.txt", sep="/")  )
    getCorrelation(  meth_2two , method = "spearman",  plot=FALSE  )
    sink()
  }
  tryCatch(
        myCorRaw_1() ,
        error = function(err){"000_myCorRaw_1"}
  )
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
 
  

  path_temp2_sub7 = paste(path_temp2, "7_Correlation_Figures", sep="/") 
  if( ! file.exists(path_temp2_sub7) ) { dir.create(path_temp2_sub7, recursive = TRUE) }    
  myCorCov_matrix_1(matrix_temp10=mat_2two,   path_temp10=path_temp2_sub7, my_col1=colorRampPalette(  c( "blue", "skyblue", "black", "pink", "red") ) )
   tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
 
  path_temp2_sub8 = paste(path_temp2, "8_Correlation_Figures_anotherColor", sep="/") 
  if( ! file.exists(path_temp2_sub8) ) { dir.create(path_temp2_sub8, recursive = TRUE) }    
  myCorCov_matrix_1(matrix_temp10=mat_2two,   path_temp10=path_temp2_sub8, my_col1=colorRampPalette(  c( "cyan",  "black",    "red") ) )
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
 
  path_temp2_sub9 = paste(path_temp2, "9_ForEachClass", sep="/") 
  if( ! file.exists(path_temp2_sub9) ) { dir.create(path_temp2_sub9, recursive = TRUE) }    
  forEachType_f(path_temp=path_temp2_sub9, matrix_temp=mat_2two, dataFrame_temp=dataFrame_temp2)
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
 

} 








myMainFunction_speRegions_All_g  <- function(   myobj_temp3,   path_temp3,   qvalue_temp3, differenceOfMethylation_temp3, mergeDistance_temp3=100,   binBases_temp3, dataFrame_temp3, numCores_temp3=8  )  {
    ####################################
    if( ! is.na(SpecificLoci_1_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_1_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_1.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_1.obj_g$targets
    }  
    tryCatch(   
         myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "1_SpecificLoci-1",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ) ,
         error = function(err){"00000000000_1_SpecificLoci_1"}
    )
    }
   
    ####################################
    if( ! is.na(SpecificLoci_2_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_2_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_2.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_2.obj_g$targets
    }  
    tryCatch( 
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "2_SpecificLoci-2",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_2_SpecificLoci_2"}
    )
    }
 
    ####################################
    if( ! is.na(SpecificLoci_3_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_3_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_3.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_3.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "3_SpecificLoci-3",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_3_SpecificLoci_3"}
    )
    }
 
    #################################### 
    if( ! is.na(SpecificLoci_4_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_4_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_4.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_4.obj_g$targets
    }  
    tryCatch( 
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "4_SpecificLoci-4",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_4_SpecificLoci_4"}
    )
    }   

    ####################################
    if( ! is.na(SpecificLoci_5_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_5_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_5.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_5.obj_g$targets
    }    
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "5_SpecificLoci-5",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_5_SpecificLoci_5"}
    )
    }
 
    ####################################
    if( ! is.na(SpecificLoci_6_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_6_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_6.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_6.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "6_SpecificLoci-6",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_6_SpecificLoci_6"}
    )
    }
 
    ####################################
    if( ! is.na(SpecificLoci_7_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_7_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_7.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_7.obj_g$targets
    }  
    tryCatch(  
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "7_SpecificLoci_7",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_7_SpecificLoci_7"}
    )
    }
 
    #################################### 
    if( ! is.na(SpecificLoci_8_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_8_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_8.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_8.obj_g$targets
    }   
    tryCatch( 
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "8_SpecificLoci-8",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_8_SpecificLoci_8"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_9_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_9_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_9.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_9.obj_g$targets
    }  
    tryCatch( 
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "9_SpecificLoci-9",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_9_SpecificLoci_9"}
    )
    }
 
    ####################################
    if( ! is.na(SpecificLoci_10_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_10_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_10.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_10.obj_g$targets
    }  
    tryCatch( 
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "10_SpecificLoci-10",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3                            
                               ),
    		error = function(err){"00000000000_10_SpecificLoci_10"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_11_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_11_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_11.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_11.obj_g$targets
    } 
    tryCatch( 
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "11_SpecificLoci-11",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_11_SpecificLoci_11"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_12_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_12_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_12.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_12.obj_g$targets
    }  
    tryCatch(   
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "12_SpecificLoci-12",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_12_SpecificLoci_12"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_13_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_13_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_13.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_13.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "13_SpecificLoci-13",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_13_SpecificLoci_13"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_14_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_14_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_14.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_14.obj_g$targets
    }   
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "14_SpecificLoci-14",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_14_SpecificLoci_14"}
    )
  }

    ####################################
    if( ! is.na(SpecificLoci_15_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_15_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_15.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_15.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "15_SpecificLoci-15",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_15_SpecificLoci_15"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_16_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_16_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_16.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_16.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "16_SpecificLoci-16",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_16_SpecificLoci_16"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_17_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_17_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_17.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_17.obj_g$targets
    } 
    tryCatch(  
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "17_SpecificLoci-17",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_17_SpecificLoci_17"}
    )
    }
 
    ####################################
    if( ! is.na(SpecificLoci_18_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_18_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_18.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_18.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "18_SpecificLoci-18",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_18_SpecificLoci_18"}
    )
   }

    ####################################
    if( ! is.na(SpecificLoci_19_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_19_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_19.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_19.obj_g$targets
    } 
    tryCatch(   
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "19_SpecificLoci-19",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_19_SpecificLoci_19"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_20_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_20_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_20.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_20.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "20_SpecificLoci-20",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_20_SpecificLoci_20"}
    )
    }
 
    ####################################
    if( ! is.na(SpecificLoci_21_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_21_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_21.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_21.obj_g$targets
    } 
    tryCatch( 
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "21_SpecificLoci-21",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_21_SpecificLoci_21"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_22_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_22_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_22.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_22.obj_g$targets
    }  
    tryCatch(   
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "22_SpecificLoci-22",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_22_SpecificLoci_22"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_23_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_23_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_23.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_23.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "23_SpecificLoci-23",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_23_SpecificLoci_23"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_24_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_24_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_24.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_24.obj_g$targets
    }   
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "24_SpecificLoci-24",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_24_SpecificLoci_24"}
    )
  }

    ####################################
    if( ! is.na(SpecificLoci_25_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_25_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_25.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_25.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "25_SpecificLoci-25",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_25_SpecificLoci_25"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_26_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_26_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_26.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_26.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "26_SpecificLoci-26",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_26_SpecificLoci_26"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_27_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_27_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_27.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_27.obj_g$targets
    } 
    tryCatch(  
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "27_SpecificLoci-27",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_27_SpecificLoci_27"}
    )
    }
 
    ####################################
    if( ! is.na(SpecificLoci_28_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_28_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_28.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_28.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "28_SpecificLoci-28",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_28_SpecificLoci_28"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_29_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_29_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_29.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_29.obj_g$targets
    } 
    tryCatch(   
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "29_SpecificLoci-29",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_29_SpecificLoci_29"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_30_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_30_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_30.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_30.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "30_SpecificLoci-30",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_30_SpecificLoci_30"}
    )
    }
 
 

    ####################################
    if( ! is.na(SpecificLoci_31_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_31_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_31.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_31.obj_g$targets
    } 
    tryCatch( 
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "31_SpecificLoci-31",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_31_SpecificLoci_31"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_32_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_32_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_32.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_32.obj_g$targets
    }  
    tryCatch(   
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "32_SpecificLoci-32",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_32_SpecificLoci_32"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_33_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_33_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_33.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_33.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "33_SpecificLoci-33",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_33_SpecificLoci_33"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_34_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_34_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_34.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_34.obj_g$targets
    }   
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "34_SpecificLoci-34",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_34_SpecificLoci_34"}
    )
  }

    ####################################
    if( ! is.na(SpecificLoci_35_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_35_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_35.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_35.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "35_SpecificLoci-35",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_35_SpecificLoci_35"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_36_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_36_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_36.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_36.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "36_SpecificLoci-36",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_36_SpecificLoci_36"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_37_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_37_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_37.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_37.obj_g$targets
    } 
    tryCatch(  
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "37_SpecificLoci-37",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_37_SpecificLoci_37"}
    )
    }
 
    ####################################
    if( ! is.na(SpecificLoci_38_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_38_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_38.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_38.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "38_SpecificLoci-38",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_38_SpecificLoci_38"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_39_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_39_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_39.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_39.obj_g$targets
    } 
    tryCatch(   
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "39_SpecificLoci-39",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_39_SpecificLoci_39"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_40_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_40_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_40.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_40.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "40_SpecificLoci-40",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_40_SpecificLoci_40"}
    )
    }
 
 

    ####################################
    if( ! is.na(SpecificLoci_41_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_41_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_41.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_41.obj_g$targets
    } 
    tryCatch( 
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "41_SpecificLoci-41",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_41_SpecificLoci_41"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_42_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_42_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_42.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_42.obj_g$targets
    }  
    tryCatch(   
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "42_SpecificLoci-42",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_42_SpecificLoci_42"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_43_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_43_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_43.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_43.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "43_SpecificLoci-43",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_43_SpecificLoci_43"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_44_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_44_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_44.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_44.obj_g$targets
    }   
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "44_SpecificLoci-44",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_44_SpecificLoci_44"}
    )
  }

    ####################################
    if( ! is.na(SpecificLoci_45_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_45_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_45.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_45.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "45_SpecificLoci-45",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_45_SpecificLoci_45"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_46_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_46_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_46.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_46.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "46_SpecificLoci-46",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_46_SpecificLoci_46"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_47_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_47_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_47.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_47.obj_g$targets
    } 
    tryCatch(  
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "47_SpecificLoci-47",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_47_SpecificLoci_47"}
    )
    }
 
    ####################################
    if( ! is.na(SpecificLoci_48_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_48_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_48.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_48.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "48_SpecificLoci-48",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_48_SpecificLoci_48"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_49_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_49_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_49.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_49.obj_g$targets
    } 
    tryCatch(   
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "49_SpecificLoci-49",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_49_SpecificLoci_49"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_50_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_50_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_50.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_50.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "50_SpecificLoci-50",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_50_SpecificLoci_50"}
    )
    }
 
 

    ####################################
    if( ! is.na(SpecificLoci_51_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_51_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_51.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_51.obj_g$targets
    } 
    tryCatch( 
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "51_SpecificLoci-51",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_51_SpecificLoci_51"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_52_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_52_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_52.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_52.obj_g$targets
    }  
    tryCatch(   
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "52_SpecificLoci-52",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_52_SpecificLoci_52"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_53_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_53_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_53.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_53.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "53_SpecificLoci-53",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_53_SpecificLoci_53"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_54_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_54_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_54.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_54.obj_g$targets
    }   
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "54_SpecificLoci-54",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_54_SpecificLoci_54"}
    )
  }

    ####################################
    if( ! is.na(SpecificLoci_55_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_55_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_55.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_55.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "55_SpecificLoci-55",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_55_SpecificLoci_55"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_56_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_56_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_56.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_56.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "56_SpecificLoci-56",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_56_SpecificLoci_56"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_57_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_57_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_57.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_57.obj_g$targets
    } 
    tryCatch(  
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "57_SpecificLoci-57",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_57_SpecificLoci_57"}
    )
    }
 
    ####################################
    if( ! is.na(SpecificLoci_58_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_58_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_58.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_58.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "58_SpecificLoci-58",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_58_SpecificLoci_58"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_59_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_59_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_59.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_59.obj_g$targets
    } 
    tryCatch(   
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "59_SpecificLoci-59",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_59_SpecificLoci_59"}
    )
    }

    ####################################
    if( ! is.na(SpecificLoci_60_g) ) {
    speRegions_temp2_bool = NA
    if( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_60_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {  
           speRegions_temp2_bool = SpecificLoci_60.obj_g$promoters
    }else{
           speRegions_temp2_bool = SpecificLoci_60.obj_g$targets
    }  
    tryCatch(
    		myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "60_SpecificLoci-60",  sep="/"), 
                                speRegions_temp2=speRegions_temp2_bool,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                ),
    		error = function(err){"00000000000_60_SpecificLoci_60"}
    )
    }
 
 
                                                                                
}






















