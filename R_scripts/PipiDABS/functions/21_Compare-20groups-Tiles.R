



myCompare_20groups_tiles_g <- function(methylBase_temp_1,  myPath_temp_1,  qvalue_temp_1,  differenceOfMethylation_temp_1,   windowSize_temp_1,     stepSize_temp_1,    mergeDistance_temp_1=100,   minBases_temp_1,  numCores_temp_1=8 ) {                                             

if( (! is.null(group_1_g)) && (! is.null(group_2_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_1_g,   SampleID_oneGroup_2_g),
      mytreatment = c(Treatment_oneGroup_1_g,  Treatment_oneGroup_2_g),
      mysex       = c(Covariate_oneGroup_1_g,        Covariate_oneGroup_2_g),
      mytech      = c(Group_oneGroup_1_g,       Group_oneGroup_2_g), 
      myfiles     = c(FileLong_oneGroup_1_g,      FileLong_oneGroup_2_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/1_",  group_1_g, "_vs_", group_2_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_1_g)) && (! is.null(group_3_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_1_g,   SampleID_oneGroup_3_g),
      mytreatment = c(Treatment_oneGroup_1_g,  Treatment_oneGroup_3_g),
      mysex       = c(Covariate_oneGroup_1_g,        Covariate_oneGroup_3_g),
      mytech      = c(Group_oneGroup_1_g,       Group_oneGroup_3_g), 
      myfiles     = c(FileLong_oneGroup_1_g,      FileLong_oneGroup_3_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/2_",  group_1_g, "_vs_", group_3_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_1_g)) && (! is.null(group_4_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_1_g,   SampleID_oneGroup_4_g),
      mytreatment = c(Treatment_oneGroup_1_g,  Treatment_oneGroup_4_g),
      mysex       = c(Covariate_oneGroup_1_g,        Covariate_oneGroup_4_g),
      mytech      = c(Group_oneGroup_1_g,       Group_oneGroup_4_g), 
      myfiles     = c(FileLong_oneGroup_1_g,      FileLong_oneGroup_4_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/3_",  group_1_g, "_vs_", group_4_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_1_g)) && (! is.null(group_5_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_1_g,   SampleID_oneGroup_5_g),
      mytreatment = c(Treatment_oneGroup_1_g,  Treatment_oneGroup_5_g),
      mysex       = c(Covariate_oneGroup_1_g,        Covariate_oneGroup_5_g),
      mytech      = c(Group_oneGroup_1_g,       Group_oneGroup_5_g), 
      myfiles     = c(FileLong_oneGroup_1_g,      FileLong_oneGroup_5_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/4_",  group_1_g, "_vs_", group_5_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_1_g)) && (! is.null(group_6_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_1_g,   SampleID_oneGroup_6_g),
      mytreatment = c(Treatment_oneGroup_1_g,  Treatment_oneGroup_6_g),
      mysex       = c(Covariate_oneGroup_1_g,        Covariate_oneGroup_6_g),
      mytech      = c(Group_oneGroup_1_g,       Group_oneGroup_6_g), 
      myfiles     = c(FileLong_oneGroup_1_g,      FileLong_oneGroup_6_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/5_",  group_1_g, "_vs_", group_6_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_1_g)) && (! is.null(group_7_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_1_g,   SampleID_oneGroup_7_g),
      mytreatment = c(Treatment_oneGroup_1_g,  Treatment_oneGroup_7_g),
      mysex       = c(Covariate_oneGroup_1_g,        Covariate_oneGroup_7_g),
      mytech      = c(Group_oneGroup_1_g,       Group_oneGroup_7_g), 
      myfiles     = c(FileLong_oneGroup_1_g,      FileLong_oneGroup_7_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/6_",  group_1_g, "_vs_", group_7_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_1_g)) && (! is.null(group_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_1_g,   SampleID_oneGroup_8_g),
      mytreatment = c(Treatment_oneGroup_1_g,  Treatment_oneGroup_8_g),
      mysex       = c(Covariate_oneGroup_1_g,        Covariate_oneGroup_8_g),
      mytech      = c(Group_oneGroup_1_g,       Group_oneGroup_8_g), 
      myfiles     = c(FileLong_oneGroup_1_g,      FileLong_oneGroup_8_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/7_",  group_1_g, "_vs_", group_8_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_1_g)) && (! is.null(group_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_1_g,   SampleID_oneGroup_9_g),
      mytreatment = c(Treatment_oneGroup_1_g,  Treatment_oneGroup_9_g),
      mysex       = c(Covariate_oneGroup_1_g,        Covariate_oneGroup_9_g),
      mytech      = c(Group_oneGroup_1_g,       Group_oneGroup_9_g), 
      myfiles     = c(FileLong_oneGroup_1_g,      FileLong_oneGroup_9_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/8_",  group_1_g, "_vs_", group_9_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}


if( (! is.null(group_1_g)) && (! is.null(group_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_1_g,   SampleID_oneGroup_10_g),
      mytreatment = c(Treatment_oneGroup_1_g,  Treatment_oneGroup_10_g),
      mysex       = c(Covariate_oneGroup_1_g,        Covariate_oneGroup_10_g),
      mytech      = c(Group_oneGroup_1_g,       Group_oneGroup_10_g), 
      myfiles     = c(FileLong_oneGroup_1_g,      FileLong_oneGroup_10_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/9_",  group_1_g, "_vs_", group_10_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}




###################################
if( (! is.null(group_2_g)) && (! is.null(group_3_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_2_g,   SampleID_oneGroup_3_g),
      mytreatment = c(Treatment_oneGroup_2_g,  Treatment_oneGroup_3_g),
      mysex       = c(Covariate_oneGroup_2_g,        Covariate_oneGroup_3_g),
      mytech      = c(Group_oneGroup_2_g,       Group_oneGroup_3_g), 
      myfiles     = c(FileLong_oneGroup_2_g,      FileLong_oneGroup_3_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/12_",  group_2_g, "_vs_", group_3_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_2_g)) && (! is.null(group_4_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_2_g,   SampleID_oneGroup_4_g),
      mytreatment = c(Treatment_oneGroup_2_g,  Treatment_oneGroup_4_g),
      mysex       = c(Covariate_oneGroup_2_g,        Covariate_oneGroup_4_g),
      mytech      = c(Group_oneGroup_2_g,       Group_oneGroup_4_g), 
      myfiles     = c(FileLong_oneGroup_2_g,      FileLong_oneGroup_4_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/13_",  group_2_g, "_vs_", group_4_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_2_g)) && (! is.null(group_5_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_2_g,   SampleID_oneGroup_5_g),
      mytreatment = c(Treatment_oneGroup_2_g,  Treatment_oneGroup_5_g),
      mysex       = c(Covariate_oneGroup_2_g,        Covariate_oneGroup_5_g),
      mytech      = c(Group_oneGroup_2_g,       Group_oneGroup_5_g), 
      myfiles     = c(FileLong_oneGroup_2_g,      FileLong_oneGroup_5_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/14_",  group_2_g, "_vs_", group_5_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_2_g)) && (! is.null(group_6_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_2_g,   SampleID_oneGroup_6_g),
      mytreatment = c(Treatment_oneGroup_2_g,  Treatment_oneGroup_6_g),
      mysex       = c(Covariate_oneGroup_2_g,        Covariate_oneGroup_6_g),
      mytech      = c(Group_oneGroup_2_g,       Group_oneGroup_6_g), 
      myfiles     = c(FileLong_oneGroup_2_g,      FileLong_oneGroup_6_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/15_",  group_2_g, "_vs_", group_6_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_2_g)) && (! is.null(group_7_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_2_g,   SampleID_oneGroup_7_g),
      mytreatment = c(Treatment_oneGroup_2_g,  Treatment_oneGroup_7_g),
      mysex       = c(Covariate_oneGroup_2_g,        Covariate_oneGroup_7_g),
      mytech      = c(Group_oneGroup_2_g,       Group_oneGroup_7_g), 
      myfiles     = c(FileLong_oneGroup_2_g,      FileLong_oneGroup_7_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/16_",  group_2_g, "_vs_", group_7_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_2_g)) && (! is.null(group_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_2_g,   SampleID_oneGroup_8_g),
      mytreatment = c(Treatment_oneGroup_2_g,  Treatment_oneGroup_8_g),
      mysex       = c(Covariate_oneGroup_2_g,        Covariate_oneGroup_8_g),
      mytech      = c(Group_oneGroup_2_g,       Group_oneGroup_8_g), 
      myfiles     = c(FileLong_oneGroup_2_g,      FileLong_oneGroup_8_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/17_",  group_2_g, "_vs_", group_8_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_2_g)) && (! is.null(group_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_2_g,   SampleID_oneGroup_9_g),
      mytreatment = c(Treatment_oneGroup_2_g,  Treatment_oneGroup_9_g),
      mysex       = c(Covariate_oneGroup_2_g,        Covariate_oneGroup_9_g),
      mytech      = c(Group_oneGroup_2_g,       Group_oneGroup_9_g), 
      myfiles     = c(FileLong_oneGroup_2_g,      FileLong_oneGroup_9_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/18_",  group_2_g, "_vs_", group_9_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}


if( (! is.null(group_2_g)) && (! is.null(group_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_2_g,   SampleID_oneGroup_10_g),
      mytreatment = c(Treatment_oneGroup_2_g,  Treatment_oneGroup_10_g),
      mysex       = c(Covariate_oneGroup_2_g,        Covariate_oneGroup_10_g),
      mytech      = c(Group_oneGroup_2_g,       Group_oneGroup_10_g), 
      myfiles     = c(FileLong_oneGroup_2_g,      FileLong_oneGroup_10_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/19_",  group_2_g, "_vs_", group_10_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}





###################################
if( (! is.null(group_3_g)) && (! is.null(group_4_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_3_g,   SampleID_oneGroup_4_g),
      mytreatment = c(Treatment_oneGroup_3_g,  Treatment_oneGroup_4_g),
      mysex       = c(Covariate_oneGroup_3_g,        Covariate_oneGroup_4_g),
      mytech      = c(Group_oneGroup_3_g,       Group_oneGroup_4_g), 
      myfiles     = c(FileLong_oneGroup_3_g,      FileLong_oneGroup_4_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/23_",  group_3_g, "_vs_", group_4_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_3_g)) && (! is.null(group_5_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_3_g,   SampleID_oneGroup_5_g),
      mytreatment = c(Treatment_oneGroup_3_g,  Treatment_oneGroup_5_g),
      mysex       = c(Covariate_oneGroup_3_g,        Covariate_oneGroup_5_g),
      mytech      = c(Group_oneGroup_3_g,       Group_oneGroup_5_g), 
      myfiles     = c(FileLong_oneGroup_3_g,      FileLong_oneGroup_5_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/24_",  group_3_g, "_vs_", group_5_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_3_g)) && (! is.null(group_6_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_3_g,   SampleID_oneGroup_6_g),
      mytreatment = c(Treatment_oneGroup_3_g,  Treatment_oneGroup_6_g),
      mysex       = c(Covariate_oneGroup_3_g,        Covariate_oneGroup_6_g),
      mytech      = c(Group_oneGroup_3_g,       Group_oneGroup_6_g), 
      myfiles     = c(FileLong_oneGroup_3_g,      FileLong_oneGroup_6_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/25_",  group_3_g, "_vs_", group_6_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_3_g)) && (! is.null(group_7_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_3_g,   SampleID_oneGroup_7_g),
      mytreatment = c(Treatment_oneGroup_3_g,  Treatment_oneGroup_7_g),
      mysex       = c(Covariate_oneGroup_3_g,        Covariate_oneGroup_7_g),
      mytech      = c(Group_oneGroup_3_g,       Group_oneGroup_7_g), 
      myfiles     = c(FileLong_oneGroup_3_g,      FileLong_oneGroup_7_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/26_",  group_3_g, "_vs_", group_7_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_3_g)) && (! is.null(group_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_3_g,   SampleID_oneGroup_8_g),
      mytreatment = c(Treatment_oneGroup_3_g,  Treatment_oneGroup_8_g),
      mysex       = c(Covariate_oneGroup_3_g,        Covariate_oneGroup_8_g),
      mytech      = c(Group_oneGroup_3_g,       Group_oneGroup_8_g), 
      myfiles     = c(FileLong_oneGroup_3_g,      FileLong_oneGroup_8_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/27_",  group_3_g, "_vs_", group_8_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_3_g)) && (! is.null(group_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_3_g,   SampleID_oneGroup_9_g),
      mytreatment = c(Treatment_oneGroup_3_g,  Treatment_oneGroup_9_g),
      mysex       = c(Covariate_oneGroup_3_g,        Covariate_oneGroup_9_g),
      mytech      = c(Group_oneGroup_3_g,       Group_oneGroup_9_g), 
      myfiles     = c(FileLong_oneGroup_3_g,      FileLong_oneGroup_9_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/28_",  group_3_g, "_vs_", group_9_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}


if( (! is.null(group_3_g)) && (! is.null(group_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_3_g,   SampleID_oneGroup_10_g),
      mytreatment = c(Treatment_oneGroup_3_g,  Treatment_oneGroup_10_g),
      mysex       = c(Covariate_oneGroup_3_g,        Covariate_oneGroup_10_g),
      mytech      = c(Group_oneGroup_3_g,       Group_oneGroup_10_g), 
      myfiles     = c(FileLong_oneGroup_3_g,      FileLong_oneGroup_10_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/29_",  group_3_g, "_vs_", group_10_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}




 ###################################
if( (! is.null(group_4_g)) && (! is.null(group_5_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_4_g,   SampleID_oneGroup_5_g),
      mytreatment = c(Treatment_oneGroup_4_g,  Treatment_oneGroup_5_g),
      mysex       = c(Covariate_oneGroup_4_g,        Covariate_oneGroup_5_g),
      mytech      = c(Group_oneGroup_4_g,       Group_oneGroup_5_g), 
      myfiles     = c(FileLong_oneGroup_4_g,      FileLong_oneGroup_5_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/34_",  group_4_g, "_vs_", group_5_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_4_g)) && (! is.null(group_6_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_4_g,   SampleID_oneGroup_6_g),
      mytreatment = c(Treatment_oneGroup_4_g,  Treatment_oneGroup_6_g),
      mysex       = c(Covariate_oneGroup_4_g,        Covariate_oneGroup_6_g),
      mytech      = c(Group_oneGroup_4_g,       Group_oneGroup_6_g), 
      myfiles     = c(FileLong_oneGroup_4_g,      FileLong_oneGroup_6_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/35_",  group_4_g, "_vs_", group_6_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_4_g)) && (! is.null(group_7_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_4_g,   SampleID_oneGroup_7_g),
      mytreatment = c(Treatment_oneGroup_4_g,  Treatment_oneGroup_7_g),
      mysex       = c(Covariate_oneGroup_4_g,        Covariate_oneGroup_7_g),
      mytech      = c(Group_oneGroup_4_g,       Group_oneGroup_7_g), 
      myfiles     = c(FileLong_oneGroup_4_g,      FileLong_oneGroup_7_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/36_",  group_4_g, "_vs_", group_7_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_4_g)) && (! is.null(group_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_4_g,   SampleID_oneGroup_8_g),
      mytreatment = c(Treatment_oneGroup_4_g,  Treatment_oneGroup_8_g),
      mysex       = c(Covariate_oneGroup_4_g,        Covariate_oneGroup_8_g),
      mytech      = c(Group_oneGroup_4_g,       Group_oneGroup_8_g), 
      myfiles     = c(FileLong_oneGroup_4_g,      FileLong_oneGroup_8_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/37_",  group_4_g, "_vs_", group_8_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_4_g)) && (! is.null(group_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_4_g,   SampleID_oneGroup_9_g),
      mytreatment = c(Treatment_oneGroup_4_g,  Treatment_oneGroup_9_g),
      mysex       = c(Covariate_oneGroup_4_g,        Covariate_oneGroup_9_g),
      mytech      = c(Group_oneGroup_4_g,       Group_oneGroup_9_g), 
      myfiles     = c(FileLong_oneGroup_4_g,      FileLong_oneGroup_9_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/38_",  group_4_g, "_vs_", group_9_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}


if( (! is.null(group_4_g)) && (! is.null(group_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_4_g,   SampleID_oneGroup_10_g),
      mytreatment = c(Treatment_oneGroup_4_g,  Treatment_oneGroup_10_g),
      mysex       = c(Covariate_oneGroup_4_g,        Covariate_oneGroup_10_g),
      mytech      = c(Group_oneGroup_4_g,       Group_oneGroup_10_g), 
      myfiles     = c(FileLong_oneGroup_4_g,      FileLong_oneGroup_10_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/39_",  group_4_g, "_vs_", group_10_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}




 
################################### 
if( (! is.null(group_5_g)) && (! is.null(group_6_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_5_g,   SampleID_oneGroup_6_g),
      mytreatment = c(Treatment_oneGroup_5_g,  Treatment_oneGroup_6_g),
      mysex       = c(Covariate_oneGroup_5_g,        Covariate_oneGroup_6_g),
      mytech      = c(Group_oneGroup_5_g,       Group_oneGroup_6_g), 
      myfiles     = c(FileLong_oneGroup_5_g,      FileLong_oneGroup_6_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/45_",  group_5_g, "_vs_", group_6_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_5_g)) && (! is.null(group_7_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_5_g,   SampleID_oneGroup_7_g),
      mytreatment = c(Treatment_oneGroup_5_g,  Treatment_oneGroup_7_g),
      mysex       = c(Covariate_oneGroup_5_g,        Covariate_oneGroup_7_g),
      mytech      = c(Group_oneGroup_5_g,       Group_oneGroup_7_g), 
      myfiles     = c(FileLong_oneGroup_5_g,      FileLong_oneGroup_7_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/46_",  group_5_g, "_vs_", group_7_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_5_g)) && (! is.null(group_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_5_g,   SampleID_oneGroup_8_g),
      mytreatment = c(Treatment_oneGroup_5_g,  Treatment_oneGroup_8_g),
      mysex       = c(Covariate_oneGroup_5_g,        Covariate_oneGroup_8_g),
      mytech      = c(Group_oneGroup_5_g,       Group_oneGroup_8_g), 
      myfiles     = c(FileLong_oneGroup_5_g,      FileLong_oneGroup_8_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/47_",  group_5_g, "_vs_", group_8_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_5_g)) && (! is.null(group_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_5_g,   SampleID_oneGroup_9_g),
      mytreatment = c(Treatment_oneGroup_5_g,  Treatment_oneGroup_9_g),
      mysex       = c(Covariate_oneGroup_5_g,        Covariate_oneGroup_9_g),
      mytech      = c(Group_oneGroup_5_g,       Group_oneGroup_9_g), 
      myfiles     = c(FileLong_oneGroup_5_g,      FileLong_oneGroup_9_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/48_",  group_5_g, "_vs_", group_9_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}


if( (! is.null(group_5_g)) && (! is.null(group_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_5_g,   SampleID_oneGroup_10_g),
      mytreatment = c(Treatment_oneGroup_5_g,  Treatment_oneGroup_10_g),
      mysex       = c(Covariate_oneGroup_5_g,        Covariate_oneGroup_10_g),
      mytech      = c(Group_oneGroup_5_g,       Group_oneGroup_10_g), 
      myfiles     = c(FileLong_oneGroup_5_g,      FileLong_oneGroup_10_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/49_",  group_5_g, "_vs_", group_10_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}




################################### 
if( (! is.null(group_6_g)) && (! is.null(group_7_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_6_g,   SampleID_oneGroup_7_g),
      mytreatment = c(Treatment_oneGroup_6_g,  Treatment_oneGroup_7_g),
      mysex       = c(Covariate_oneGroup_6_g,        Covariate_oneGroup_7_g),
      mytech      = c(Group_oneGroup_6_g,       Group_oneGroup_7_g), 
      myfiles     = c(FileLong_oneGroup_6_g,      FileLong_oneGroup_7_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/56_",  group_6_g, "_vs_", group_7_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_6_g)) && (! is.null(group_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_6_g,   SampleID_oneGroup_8_g),
      mytreatment = c(Treatment_oneGroup_6_g,  Treatment_oneGroup_8_g),
      mysex       = c(Covariate_oneGroup_6_g,        Covariate_oneGroup_8_g),
      mytech      = c(Group_oneGroup_6_g,       Group_oneGroup_8_g), 
      myfiles     = c(FileLong_oneGroup_6_g,      FileLong_oneGroup_8_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/57_",  group_6_g, "_vs_", group_8_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_6_g)) && (! is.null(group_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_6_g,   SampleID_oneGroup_9_g),
      mytreatment = c(Treatment_oneGroup_6_g,  Treatment_oneGroup_9_g),
      mysex       = c(Covariate_oneGroup_6_g,        Covariate_oneGroup_9_g),
      mytech      = c(Group_oneGroup_6_g,       Group_oneGroup_9_g), 
      myfiles     = c(FileLong_oneGroup_6_g,      FileLong_oneGroup_9_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/58_",  group_6_g, "_vs_", group_9_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}


if( (! is.null(group_6_g)) && (! is.null(group_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_6_g,   SampleID_oneGroup_10_g),
      mytreatment = c(Treatment_oneGroup_6_g,  Treatment_oneGroup_10_g),
      mysex       = c(Covariate_oneGroup_6_g,        Covariate_oneGroup_10_g),
      mytech      = c(Group_oneGroup_6_g,       Group_oneGroup_10_g), 
      myfiles     = c(FileLong_oneGroup_6_g,      FileLong_oneGroup_10_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/59_",  group_6_g, "_vs_", group_10_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}





###################################
if( (! is.null(group_7_g)) && (! is.null(group_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_7_g,   SampleID_oneGroup_8_g),
      mytreatment = c(Treatment_oneGroup_7_g,  Treatment_oneGroup_8_g),
      mysex       = c(Covariate_oneGroup_7_g,        Covariate_oneGroup_8_g),
      mytech      = c(Group_oneGroup_7_g,       Group_oneGroup_8_g), 
      myfiles     = c(FileLong_oneGroup_7_g,      FileLong_oneGroup_8_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/67_",  group_7_g, "_vs_", group_8_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}

if( (! is.null(group_7_g)) && (! is.null(group_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_7_g,   SampleID_oneGroup_9_g),
      mytreatment = c(Treatment_oneGroup_7_g,  Treatment_oneGroup_9_g),
      mysex       = c(Covariate_oneGroup_7_g,        Covariate_oneGroup_9_g),
      mytech      = c(Group_oneGroup_7_g,       Group_oneGroup_9_g), 
      myfiles     = c(FileLong_oneGroup_7_g,      FileLong_oneGroup_9_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/68_",  group_7_g, "_vs_", group_9_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}


if( (! is.null(group_7_g)) && (! is.null(group_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_7_g,   SampleID_oneGroup_10_g),
      mytreatment = c(Treatment_oneGroup_7_g,  Treatment_oneGroup_10_g),
      mysex       = c(Covariate_oneGroup_7_g,        Covariate_oneGroup_10_g),
      mytech      = c(Group_oneGroup_7_g,       Group_oneGroup_10_g), 
      myfiles     = c(FileLong_oneGroup_7_g,      FileLong_oneGroup_10_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/69_",  group_7_g, "_vs_", group_10_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}




 
###################################
if( (! is.null(group_8_g)) && (! is.null(group_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_8_g,   SampleID_oneGroup_9_g),
      mytreatment = c(Treatment_oneGroup_8_g,  Treatment_oneGroup_9_g),
      mysex       = c(Covariate_oneGroup_8_g,        Covariate_oneGroup_9_g),
      mytech      = c(Group_oneGroup_8_g,       Group_oneGroup_9_g), 
      myfiles     = c(FileLong_oneGroup_8_g,      FileLong_oneGroup_9_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/78_",  group_8_g, "_vs_", group_9_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}


if( (! is.null(group_8_g)) && (! is.null(group_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_8_g,   SampleID_oneGroup_10_g),
      mytreatment = c(Treatment_oneGroup_8_g,  Treatment_oneGroup_10_g),
      mysex       = c(Covariate_oneGroup_8_g,        Covariate_oneGroup_10_g),
      mytech      = c(Group_oneGroup_8_g,       Group_oneGroup_10_g), 
      myfiles     = c(FileLong_oneGroup_8_g,      FileLong_oneGroup_10_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/79_",  group_8_g, "_vs_", group_10_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}



###################################
if( (! is.null(group_9_g)) && (! is.null(group_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_oneGroup_9_g,   SampleID_oneGroup_10_g),
      mytreatment = c(Treatment_oneGroup_9_g,  Treatment_oneGroup_10_g),
      mysex       = c(Covariate_oneGroup_9_g,        Covariate_oneGroup_10_g),
      mytech      = c(Group_oneGroup_9_g,       Group_oneGroup_10_g), 
      myfiles     = c(FileLong_oneGroup_9_g,      FileLong_oneGroup_10_g)
    )
    myMainFunction_1_g(myobj_temp2 = methylBase_temp_1,     
                       path_temp2  = paste(myPath_temp_1, "/89_",  group_9_g, "_vs_", group_10_g,  sep="")  ,   
                       qvalue_temp2=qvalue_temp_1,  differenceOfMethylation_temp2=differenceOfMethylation_temp_1, mergeDistance_temp2=mergeDistance_temp_1,       
                       binSize_temp2=windowSize_temp_1, stepSize_temp2=stepSize_temp_1, binBases_temp2=minBases_temp_1, dataFrame_temp2=dataFrame_temp1_A, numCores_temp2=numCores_temp_1 
                      ) 
}





}


 

