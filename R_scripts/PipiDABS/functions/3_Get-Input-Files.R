##############################################################################################################################################################################################
cat("##############################################\n")
SamplesInfoMatrix_g = read.table(file=SamplesInformation_g,   header = TRUE, sep = "\t" )
print( "SamplesInfoMatrix_g:" )
print(  SamplesInfoMatrix_g   )
cat("\n\n")
print("Type of variable SamplesInfoMatrix_g :")
print( class(SamplesInfoMatrix_g) )
print( mode(SamplesInfoMatrix_g) )
print( typeof(SamplesInfoMatrix_g) )
cat("\n\n")
print("Dimensions of variable SamplesInfoMatrix_g:")
print(  dim(SamplesInfoMatrix_g)   )
cat("\n\n\n\n\n")

cat("##############################################\n")
cat("Convert dataframe to matrix......\n")
SamplesInfoMatrix_g = as.matrix( SamplesInfoMatrix_g )
print( "SamplesInfoMatrix_g:" )
print(  SamplesInfoMatrix_g   )
cat("\n\n")
print("Type of variable SamplesInfoMatrix_g :")
print( class(SamplesInfoMatrix_g) )
print( mode(SamplesInfoMatrix_g) )
print( typeof(SamplesInfoMatrix_g) )
cat("\n\n")
print("Dimensions of variable SamplesInfoMatrix_g:")
print(  dim(SamplesInfoMatrix_g)   )
cat("\n\n\n\n\n")
##############################################################################################################################################################################################





##############################################################################################################################################################################################
cat("##############################################\n")
samplesInformation_colname_g = as.vector(colnames(SamplesInfoMatrix_g))
print("samplesInformation_colname_g:")
print( samplesInformation_colname_g  )
cat("\n\n\n")

if( identical( samplesInformation_colname_g, c("sampleName","samplePath","groups","covariates","batchEffects") ) ) {
    print("The heaer (first row) of samples_information_file is right!!!") 
    cat("\n\n\n\n\n")
}else{
    stop("Error: The heaer (first row) of samples_information_file is wrong!!!")
}

GroupIDs_g = unique(SamplesInfoMatrix_g[,3])
CovarIDs_g = unique(SamplesInfoMatrix_g[,4])
BatchIDs_g = unique(SamplesInfoMatrix_g[,5])

cat("\n\n\n\n\n")
##############################################################################################################################################################################################


 


##############################################################################################################################################################################################
## For each group, covariate and batchEffect.
group_1_g  = c()
group_2_g  = c()
group_3_g  = c()
group_4_g  = c()
group_5_g  = c()
group_6_g  = c()
group_7_g  = c()
group_8_g  = c()
group_9_g  = c()
group_10_g = c()
group_11_g = c()
group_12_g = c()
group_13_g = c()
group_14_g = c()
group_15_g = c()
group_16_g = c()
group_17_g = c()
group_18_g = c()
group_19_g = c()
group_20_g = c()  
color_group_1_g  = c()
color_group_2_g  = c()
color_group_3_g  = c()
color_group_4_g  = c()
color_group_5_g  = c()
color_group_6_g  = c()
color_group_7_g  = c()
color_group_8_g  = c()
color_group_9_g  = c()
color_group_10_g = c()
color_group_11_g = c()
color_group_12_g = c()
color_group_13_g = c()
color_group_14_g = c()
color_group_15_g = c()
color_group_16_g = c()
color_group_17_g = c()
color_group_18_g = c()
color_group_19_g = c()
color_group_20_g = c()

covariate_1_g  = c()
covariate_2_g  = c()
covariate_3_g  = c()
covariate_4_g  = c()
covariate_5_g  = c()
covariate_6_g  = c()
covariate_7_g  = c()
covariate_8_g  = c()
covariate_9_g  = c()
covariate_10_g = c()
covariate_11_g = c()
covariate_12_g = c()
covariate_13_g = c()
covariate_14_g = c()
covariate_15_g = c()
covariate_16_g = c()
covariate_17_g = c()
covariate_18_g = c()
covariate_19_g = c()
covariate_20_g = c()  
shape_covariate_1_g  = c()
shape_covariate_2_g  = c()
shape_covariate_3_g  = c()
shape_covariate_4_g  = c()
shape_covariate_5_g  = c()
shape_covariate_6_g  = c()
shape_covariate_7_g  = c()
shape_covariate_8_g  = c()
shape_covariate_9_g  = c()
shape_covariate_10_g = c()
shape_covariate_11_g = c()
shape_covariate_12_g = c()
shape_covariate_13_g = c()
shape_covariate_14_g = c()
shape_covariate_15_g = c()
shape_covariate_16_g = c()
shape_covariate_17_g = c()
shape_covariate_18_g = c()
shape_covariate_19_g = c()
shape_covariate_20_g = c()

batchEffect_1_g  = c()
batchEffect_2_g  = c()
batchEffect_3_g  = c()
batchEffect_4_g  = c()
batchEffect_5_g  = c()
batchEffect_6_g  = c()
batchEffect_7_g  = c()
batchEffect_8_g  = c()
batchEffect_9_g  = c()
batchEffect_10_g = c()
batchEffect_11_g = c()
batchEffect_12_g = c()
batchEffect_13_g = c()
batchEffect_14_g = c()
batchEffect_15_g = c()
batchEffect_16_g = c()
batchEffect_17_g = c()
batchEffect_18_g = c()
batchEffect_19_g = c()
batchEffect_20_g = c()  
size_batchEffect_1_g  = c()
size_batchEffect_2_g  = c()
size_batchEffect_3_g  = c()
size_batchEffect_4_g  = c()
size_batchEffect_5_g  = c()
size_batchEffect_6_g  = c()
size_batchEffect_7_g  = c()
size_batchEffect_8_g  = c()
size_batchEffect_9_g  = c()
size_batchEffect_10_g = c()
size_batchEffect_11_g = c()
size_batchEffect_12_g = c()
size_batchEffect_13_g = c()
size_batchEffect_14_g = c()
size_batchEffect_15_g = c()
size_batchEffect_16_g = c()
size_batchEffect_17_g = c()
size_batchEffect_18_g = c()
size_batchEffect_19_g = c()
size_batchEffect_20_g = c()

for(i in c(1:length(GroupIDs_g)) ) {
    if(i == 1)  {group_1_g  = as.character( GroupIDs_g[i] ); color_group_1_g  = "red"        }  
    if(i == 2)  {group_2_g  = as.character( GroupIDs_g[i] ); color_group_2_g  = "cyan"       }  
    if(i == 3)  {group_3_g  = as.character( GroupIDs_g[i] ); color_group_3_g  = "blue"       }  
    if(i == 4)  {group_4_g  = as.character( GroupIDs_g[i] ); color_group_4_g  = "green"      }  
    if(i == 5)  {group_5_g  = as.character( GroupIDs_g[i] ); color_group_5_g  = "purple"     }  
    if(i == 6)  {group_6_g  = as.character( GroupIDs_g[i] ); color_group_6_g  = "pink"       }  
    if(i == 7)  {group_7_g  = as.character( GroupIDs_g[i] ); color_group_7_g  = "orange"     }  
    if(i == 8)  {group_8_g  = as.character( GroupIDs_g[i] ); color_group_8_g  = "lightblue"  }  
    if(i == 9)  {group_9_g  = as.character( GroupIDs_g[i] ); color_group_9_g  = "yellow"     }  
    if(i == 10) {group_10_g = as.character( GroupIDs_g[i] ); color_group_10_g = "slategray1" } 
    if(i == 11) {group_11_g = as.character( GroupIDs_g[i] ); color_group_11_g = "red4"       }  
    if(i == 12) {group_12_g = as.character( GroupIDs_g[i] ); color_group_12_g = "orange4"    }  
    if(i == 13) {group_13_g = as.character( GroupIDs_g[i] ); color_group_13_g = "blue4"      }  
    if(i == 14) {group_14_g = as.character( GroupIDs_g[i] ); color_group_14_g = "cyan4"      }  
    if(i == 15) {group_15_g = as.character( GroupIDs_g[i] ); color_group_15_g = "purple4"    }  
    if(i == 16) {group_16_g = as.character( GroupIDs_g[i] ); color_group_16_g = "pink4"      }  
    if(i == 17) {group_17_g = as.character( GroupIDs_g[i] ); color_group_17_g = "green4"     }  
    if(i == 18) {group_18_g = as.character( GroupIDs_g[i] ); color_group_18_g = "lightblue4" }  
    if(i == 19) {group_19_g = as.character( GroupIDs_g[i] ); color_group_19_g = "yellow4"    }  
    if(i == 20) {group_20_g = as.character( GroupIDs_g[i] ); color_group_20_g = "slategray4" } 
}

for(i in c(1:length(CovarIDs_g)) ) {
    if(i == 1)  {covariate_1_g  = as.character( CovarIDs_g[i] ); shape_covariate_1_g  = 16 }  
    if(i == 2)  {covariate_2_g  = as.character( CovarIDs_g[i] ); shape_covariate_2_g  = 17 }  
    if(i == 3)  {covariate_3_g  = as.character( CovarIDs_g[i] ); shape_covariate_3_g  = 15 }  
    if(i == 4)  {covariate_4_g  = as.character( CovarIDs_g[i] ); shape_covariate_4_g  = 11 }  
    if(i == 5)  {covariate_5_g  = as.character( CovarIDs_g[i] ); shape_covariate_5_g  = 12 }  
    if(i == 6)  {covariate_6_g  = as.character( CovarIDs_g[i] ); shape_covariate_6_g  = 13 }  
    if(i == 7)  {covariate_7_g  = as.character( CovarIDs_g[i] ); shape_covariate_7_g  = 14 }  
    if(i == 8)  {covariate_8_g  = as.character( CovarIDs_g[i] ); shape_covariate_8_g  = 18 }  
    if(i == 9)  {covariate_9_g  = as.character( CovarIDs_g[i] ); shape_covariate_9_g  = 0  }  
    if(i == 10) {covariate_10_g = as.character( CovarIDs_g[i] ); shape_covariate_10_g = 1  } 
    if(i == 11) {covariate_11_g = as.character( CovarIDs_g[i] ); shape_covariate_11_g = 2  }  
    if(i == 12) {covariate_12_g = as.character( CovarIDs_g[i] ); shape_covariate_12_g = 3  }  
    if(i == 13) {covariate_13_g = as.character( CovarIDs_g[i] ); shape_covariate_13_g = 4  }  
    if(i == 14) {covariate_14_g = as.character( CovarIDs_g[i] ); shape_covariate_14_g = 5  }  
    if(i == 15) {covariate_15_g = as.character( CovarIDs_g[i] ); shape_covariate_15_g = 6  }  
    if(i == 16) {covariate_16_g = as.character( CovarIDs_g[i] ); shape_covariate_16_g = 7  }  
    if(i == 17) {covariate_17_g = as.character( CovarIDs_g[i] ); shape_covariate_17_g = 8  }  
    if(i == 18) {covariate_18_g = as.character( CovarIDs_g[i] ); shape_covariate_18_g = 9  }  
    if(i == 19) {covariate_19_g = as.character( CovarIDs_g[i] ); shape_covariate_19_g = 10 }  
    if(i == 20) {covariate_20_g = as.character( CovarIDs_g[i] ); shape_covariate_20_g = 25 } 
}

for(i in c(1:length(BatchIDs_g)) ) {
    if(i == 1)  {batchEffect_1_g  = as.character( BatchIDs_g[i] ); size_batchEffect_1_g  = 1    }  
    if(i == 2)  {batchEffect_2_g  = as.character( BatchIDs_g[i] ); size_batchEffect_2_g  = 2    }  
    if(i == 3)  {batchEffect_3_g  = as.character( BatchIDs_g[i] ); size_batchEffect_3_g  = 3    }  
    if(i == 4)  {batchEffect_4_g  = as.character( BatchIDs_g[i] ); size_batchEffect_4_g  = 4    }  
    if(i == 5)  {batchEffect_5_g  = as.character( BatchIDs_g[i] ); size_batchEffect_5_g  = 5    }  
    if(i == 6)  {batchEffect_6_g  = as.character( BatchIDs_g[i] ); size_batchEffect_6_g  = 6    }  
    if(i == 7)  {batchEffect_7_g  = as.character( BatchIDs_g[i] ); size_batchEffect_7_g  = 7    }  
    if(i == 8)  {batchEffect_8_g  = as.character( BatchIDs_g[i] ); size_batchEffect_8_g  = 8    }  
    if(i == 9)  {batchEffect_9_g  = as.character( BatchIDs_g[i] ); size_batchEffect_9_g  = 9    }  
    if(i == 10) {batchEffect_10_g = as.character( BatchIDs_g[i] ); size_batchEffect_10_g = 10   } 
    if(i == 11) {batchEffect_11_g = as.character( BatchIDs_g[i] ); size_batchEffect_11_g = 0.2  }  
    if(i == 12) {batchEffect_12_g = as.character( BatchIDs_g[i] ); size_batchEffect_12_g = 0.3  }  
    if(i == 13) {batchEffect_13_g = as.character( BatchIDs_g[i] ); size_batchEffect_13_g = 0.4  }  
    if(i == 14) {batchEffect_14_g = as.character( BatchIDs_g[i] ); size_batchEffect_14_g = 0.5  }  
    if(i == 15) {batchEffect_15_g = as.character( BatchIDs_g[i] ); size_batchEffect_15_g = 0.6  }  
    if(i == 16) {batchEffect_16_g = as.character( BatchIDs_g[i] ); size_batchEffect_16_g = 0.7  }  
    if(i == 17) {batchEffect_17_g = as.character( BatchIDs_g[i] ); size_batchEffect_17_g = 0.8  }  
    if(i == 18) {batchEffect_18_g = as.character( BatchIDs_g[i] ); size_batchEffect_18_g = 0.9  }  
    if(i == 19) {batchEffect_19_g = as.character( BatchIDs_g[i] ); size_batchEffect_19_g = 0.1  }  
    if(i == 20) {batchEffect_20_g = as.character( BatchIDs_g[i] ); size_batchEffect_20_g = 0.05 } 
}
##############################################################################################################################################################################################





##############################################################################################################################################################################################
## For each sample.
FileLong_oneGroup_1_g  = c()
FileLong_oneGroup_2_g  = c()
FileLong_oneGroup_3_g  = c()
FileLong_oneGroup_4_g  = c()
FileLong_oneGroup_5_g  = c()
FileLong_oneGroup_6_g  = c()
FileLong_oneGroup_7_g  = c()
FileLong_oneGroup_8_g  = c()
FileLong_oneGroup_9_g  = c()
FileLong_oneGroup_10_g = c()
FileLong_oneGroup_11_g = c()
FileLong_oneGroup_12_g = c()
FileLong_oneGroup_13_g = c()
FileLong_oneGroup_14_g = c()
FileLong_oneGroup_15_g = c()
FileLong_oneGroup_16_g = c()
FileLong_oneGroup_17_g = c()
FileLong_oneGroup_18_g = c()
FileLong_oneGroup_19_g = c()
FileLong_oneGroup_20_g = c()  
  
FileShort_oneGroup_1_g  = c()
FileShort_oneGroup_2_g  = c()
FileShort_oneGroup_3_g  = c()
FileShort_oneGroup_4_g  = c()
FileShort_oneGroup_5_g  = c()
FileShort_oneGroup_6_g  = c()
FileShort_oneGroup_7_g  = c()
FileShort_oneGroup_8_g  = c()
FileShort_oneGroup_9_g  = c()
FileShort_oneGroup_10_g = c()
FileShort_oneGroup_11_g = c()
FileShort_oneGroup_12_g = c()
FileShort_oneGroup_13_g = c()
FileShort_oneGroup_14_g = c()
FileShort_oneGroup_15_g = c()
FileShort_oneGroup_16_g = c()
FileShort_oneGroup_17_g = c()
FileShort_oneGroup_18_g = c()
FileShort_oneGroup_19_g = c()
FileShort_oneGroup_20_g = c()  

Group_oneGroup_1_g  = c()
Group_oneGroup_2_g  = c()
Group_oneGroup_3_g  = c()
Group_oneGroup_4_g  = c()
Group_oneGroup_5_g  = c()
Group_oneGroup_6_g  = c()
Group_oneGroup_7_g  = c()
Group_oneGroup_8_g  = c()
Group_oneGroup_9_g  = c()
Group_oneGroup_10_g = c()
Group_oneGroup_11_g = c()
Group_oneGroup_12_g = c()
Group_oneGroup_13_g = c()
Group_oneGroup_14_g = c()
Group_oneGroup_15_g = c()
Group_oneGroup_16_g = c()
Group_oneGroup_17_g = c()
Group_oneGroup_18_g = c()
Group_oneGroup_19_g = c()
Group_oneGroup_20_g = c()  

Covariate_oneGroup_1_g  = c()
Covariate_oneGroup_2_g  = c()
Covariate_oneGroup_3_g  = c()
Covariate_oneGroup_4_g  = c()
Covariate_oneGroup_5_g  = c()
Covariate_oneGroup_6_g  = c()
Covariate_oneGroup_7_g  = c()
Covariate_oneGroup_8_g  = c()
Covariate_oneGroup_9_g  = c()
Covariate_oneGroup_10_g = c()
Covariate_oneGroup_11_g = c()
Covariate_oneGroup_12_g = c()
Covariate_oneGroup_13_g = c()
Covariate_oneGroup_14_g = c()
Covariate_oneGroup_15_g = c()
Covariate_oneGroup_16_g = c()
Covariate_oneGroup_17_g = c()
Covariate_oneGroup_18_g = c()
Covariate_oneGroup_19_g = c()
Covariate_oneGroup_20_g = c()  

Batch_oneGroup_1_g  = c()
Batch_oneGroup_2_g  = c()
Batch_oneGroup_3_g  = c()
Batch_oneGroup_4_g  = c()
Batch_oneGroup_5_g  = c()
Batch_oneGroup_6_g  = c()
Batch_oneGroup_7_g  = c()
Batch_oneGroup_8_g  = c()
Batch_oneGroup_9_g  = c()
Batch_oneGroup_10_g = c()
Batch_oneGroup_11_g = c()
Batch_oneGroup_12_g = c()
Batch_oneGroup_13_g = c()
Batch_oneGroup_14_g = c()
Batch_oneGroup_15_g = c()
Batch_oneGroup_16_g = c()
Batch_oneGroup_17_g = c()
Batch_oneGroup_18_g = c()
Batch_oneGroup_19_g = c()
Batch_oneGroup_20_g = c()  

Treatment_oneGroup_1_g  = c()
Treatment_oneGroup_2_g  = c()
Treatment_oneGroup_3_g  = c()
Treatment_oneGroup_4_g  = c()
Treatment_oneGroup_5_g  = c()
Treatment_oneGroup_6_g  = c()
Treatment_oneGroup_7_g  = c()
Treatment_oneGroup_8_g  = c()
Treatment_oneGroup_9_g  = c()
Treatment_oneGroup_10_g = c()
Treatment_oneGroup_11_g = c()
Treatment_oneGroup_12_g = c()
Treatment_oneGroup_13_g = c()
Treatment_oneGroup_14_g = c()
Treatment_oneGroup_15_g = c()
Treatment_oneGroup_16_g = c()
Treatment_oneGroup_17_g = c()
Treatment_oneGroup_18_g = c()
Treatment_oneGroup_19_g = c()
Treatment_oneGroup_20_g = c()  

SampleID_oneGroup_1_g  = c()
SampleID_oneGroup_2_g  = c()
SampleID_oneGroup_3_g  = c()
SampleID_oneGroup_4_g  = c()
SampleID_oneGroup_5_g  = c()
SampleID_oneGroup_6_g  = c()
SampleID_oneGroup_7_g  = c()
SampleID_oneGroup_8_g  = c()
SampleID_oneGroup_9_g  = c()
SampleID_oneGroup_10_g = c()
SampleID_oneGroup_11_g = c()
SampleID_oneGroup_12_g = c()
SampleID_oneGroup_13_g = c()
SampleID_oneGroup_14_g = c()
SampleID_oneGroup_15_g = c()
SampleID_oneGroup_16_g = c()
SampleID_oneGroup_17_g = c()
SampleID_oneGroup_18_g = c()
SampleID_oneGroup_19_g = c()
SampleID_oneGroup_20_g = c()    
 

for(i in c(1:nrow(SamplesInfoMatrix_g)) ) {
    myTempVec1    = as.vector( unlist(SamplesInfoMatrix_g[i,] ) )  
    myTempVec1[2] = gsub(pattern="/$", replacement="", x=myTempVec1[2], ignore.case = FALSE, perl = TRUE )  

    if( (! is.null(group_1_g)) && ( myTempVec1[3] == group_1_g ) )  {
            FileShort_oneGroup_1_g[length(FileShort_oneGroup_1_g)+1] = myTempVec1[1]
            FileLong_oneGroup_1_g[length(FileLong_oneGroup_1_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_1_g[length(Group_oneGroup_1_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_1_g[length(Covariate_oneGroup_1_g)+1] = myTempVec1[4]  
            Batch_oneGroup_1_g[length(Batch_oneGroup_1_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_1_g[length(Treatment_oneGroup_1_g)+1] = 1
            SampleID_oneGroup_1_g[length(SampleID_oneGroup_1_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_1_g)+1, sep="") 
    }

    if( (! is.null(group_2_g)) && ( myTempVec1[3] == group_2_g ) )  {
            FileShort_oneGroup_2_g[length(FileShort_oneGroup_2_g)+1] = myTempVec1[1]
            FileLong_oneGroup_2_g[length(FileLong_oneGroup_2_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_2_g[length(Group_oneGroup_2_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_2_g[length(Covariate_oneGroup_2_g)+1] = myTempVec1[4]  
            Batch_oneGroup_2_g[length(Batch_oneGroup_2_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_2_g[length(Treatment_oneGroup_2_g)+1] = 2
            SampleID_oneGroup_2_g[length(SampleID_oneGroup_2_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_2_g)+1, sep="") 
    }

    if( (! is.null(group_3_g)) && ( myTempVec1[3] == group_3_g ) )  {
            FileShort_oneGroup_3_g[length(FileShort_oneGroup_3_g)+1] = myTempVec1[1]
            FileLong_oneGroup_3_g[length(FileLong_oneGroup_3_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_3_g[length(Group_oneGroup_3_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_3_g[length(Covariate_oneGroup_3_g)+1] = myTempVec1[4]  
            Batch_oneGroup_3_g[length(Batch_oneGroup_3_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_3_g[length(Treatment_oneGroup_3_g)+1] = 3
            SampleID_oneGroup_3_g[length(SampleID_oneGroup_3_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_3_g)+1, sep="") 
    }
 
    if( (! is.null(group_4_g)) && ( myTempVec1[3] == group_4_g ) )  {
            FileShort_oneGroup_4_g[length(FileShort_oneGroup_4_g)+1] = myTempVec1[1]
            FileLong_oneGroup_4_g[length(FileLong_oneGroup_4_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_4_g[length(Group_oneGroup_4_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_4_g[length(Covariate_oneGroup_4_g)+1] = myTempVec1[4]  
            Batch_oneGroup_4_g[length(Batch_oneGroup_4_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_4_g[length(Treatment_oneGroup_4_g)+1] = 4
            SampleID_oneGroup_4_g[length(SampleID_oneGroup_4_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_4_g)+1, sep="") 
    }
    
 
    if( (! is.null(group_5_g)) && ( myTempVec1[3] == group_5_g ) )  {
            FileShort_oneGroup_5_g[length(FileShort_oneGroup_5_g)+1] = myTempVec1[1]
            FileLong_oneGroup_5_g[length(FileLong_oneGroup_5_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_5_g[length(Group_oneGroup_5_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_5_g[length(Covariate_oneGroup_5_g)+1] = myTempVec1[4]  
            Batch_oneGroup_5_g[length(Batch_oneGroup_5_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_5_g[length(Treatment_oneGroup_5_g)+1] = 5
            SampleID_oneGroup_5_g[length(SampleID_oneGroup_5_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_5_g)+1, sep="") 
    }
    
    if( (! is.null(group_6_g)) && ( myTempVec1[3] == group_6_g ) )  {
            FileShort_oneGroup_6_g[length(FileShort_oneGroup_6_g)+1] = myTempVec1[1]
            FileLong_oneGroup_6_g[length(FileLong_oneGroup_6_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_6_g[length(Group_oneGroup_6_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_6_g[length(Covariate_oneGroup_6_g)+1] = myTempVec1[4]  
            Batch_oneGroup_6_g[length(Batch_oneGroup_6_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_6_g[length(Treatment_oneGroup_6_g)+1] = 6
            SampleID_oneGroup_6_g[length(SampleID_oneGroup_6_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_6_g)+1, sep="") 
    }

    if( (! is.null(group_7_g)) && ( myTempVec1[3] == group_7_g ) )  {
            FileShort_oneGroup_7_g[length(FileShort_oneGroup_7_g)+1] = myTempVec1[1]
            FileLong_oneGroup_7_g[length(FileLong_oneGroup_7_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_7_g[length(Group_oneGroup_7_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_7_g[length(Covariate_oneGroup_7_g)+1] = myTempVec1[4]  
            Batch_oneGroup_7_g[length(Batch_oneGroup_7_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_7_g[length(Treatment_oneGroup_7_g)+1] = 7
            SampleID_oneGroup_7_g[length(SampleID_oneGroup_7_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_7_g)+1, sep="") 
    }
   
    if( (! is.null(group_8_g)) && ( myTempVec1[3] == group_8_g ) )  {
            FileShort_oneGroup_8_g[length(FileShort_oneGroup_8_g)+1] = myTempVec1[1]
            FileLong_oneGroup_8_g[length(FileLong_oneGroup_8_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_8_g[length(Group_oneGroup_8_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_8_g[length(Covariate_oneGroup_8_g)+1] = myTempVec1[4]  
            Batch_oneGroup_8_g[length(Batch_oneGroup_8_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_8_g[length(Treatment_oneGroup_8_g)+1] = 8
            SampleID_oneGroup_8_g[length(SampleID_oneGroup_8_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_8_g)+1, sep="") 
    }
    
    if( (! is.null(group_9_g)) && ( myTempVec1[3] == group_9_g ) )  {
            FileShort_oneGroup_9_g[length(FileShort_oneGroup_9_g)+1] = myTempVec1[1]
            FileLong_oneGroup_9_g[length(FileLong_oneGroup_9_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_9_g[length(Group_oneGroup_9_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_9_g[length(Covariate_oneGroup_9_g)+1] = myTempVec1[4]  
            Batch_oneGroup_9_g[length(Batch_oneGroup_9_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_9_g[length(Treatment_oneGroup_9_g)+1] = 9
            SampleID_oneGroup_9_g[length(SampleID_oneGroup_9_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_9_g)+1, sep="") 
    }
    
    if( (! is.null(group_10_g)) && ( myTempVec1[3] == group_10_g ) )  {
            FileShort_oneGroup_10_g[length(FileShort_oneGroup_10_g)+1] = myTempVec1[1]
            FileLong_oneGroup_10_g[length(FileLong_oneGroup_10_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_10_g[length(Group_oneGroup_10_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_10_g[length(Covariate_oneGroup_10_g)+1] = myTempVec1[4]  
            Batch_oneGroup_10_g[length(Batch_oneGroup_10_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_10_g[length(Treatment_oneGroup_10_g)+1] = 10
            SampleID_oneGroup_10_g[length(SampleID_oneGroup_10_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_10_g)+1, sep="") 
    }
    
    if( (! is.null(group_11_g)) && ( myTempVec1[3] == group_11_g ) )  {
            FileShort_oneGroup_11_g[length(FileShort_oneGroup_11_g)+1] = myTempVec1[1]
            FileLong_oneGroup_11_g[length(FileLong_oneGroup_11_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_11_g[length(Group_oneGroup_11_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_11_g[length(Covariate_oneGroup_11_g)+1] = myTempVec1[4]  
            Batch_oneGroup_11_g[length(Batch_oneGroup_11_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_11_g[length(Treatment_oneGroup_11_g)+1] = 11
            SampleID_oneGroup_11_g[length(SampleID_oneGroup_11_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_11_g)+1, sep="") 
    }
 
    if( (! is.null(group_12_g)) && ( myTempVec1[3] == group_12_g ) )  {
            FileShort_oneGroup_12_g[length(FileShort_oneGroup_12_g)+1] = myTempVec1[1]
            FileLong_oneGroup_12_g[length(FileLong_oneGroup_12_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_12_g[length(Group_oneGroup_12_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_12_g[length(Covariate_oneGroup_12_g)+1] = myTempVec1[4]  
            Batch_oneGroup_12_g[length(Batch_oneGroup_12_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_12_g[length(Treatment_oneGroup_12_g)+1] = 12
            SampleID_oneGroup_12_g[length(SampleID_oneGroup_12_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_12_g)+1, sep="") 
    }
    

    if( (! is.null(group_13_g)) && ( myTempVec1[3] == group_13_g ) )  {
            FileShort_oneGroup_13_g[length(FileShort_oneGroup_13_g)+1] = myTempVec1[1]
            FileLong_oneGroup_13_g[length(FileLong_oneGroup_13_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_13_g[length(Group_oneGroup_13_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_13_g[length(Covariate_oneGroup_13_g)+1] = myTempVec1[4]  
            Batch_oneGroup_13_g[length(Batch_oneGroup_13_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_13_g[length(Treatment_oneGroup_13_g)+1] = 13
            SampleID_oneGroup_13_g[length(SampleID_oneGroup_13_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_13_g)+1, sep="") 
    }
    

    if( (! is.null(group_14_g)) && ( myTempVec1[3] == group_14_g ) )  {
            FileShort_oneGroup_14_g[length(FileShort_oneGroup_14_g)+1] = myTempVec1[1]
            FileLong_oneGroup_14_g[length(FileLong_oneGroup_14_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_14_g[length(Group_oneGroup_14_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_14_g[length(Covariate_oneGroup_14_g)+1] = myTempVec1[4]  
            Batch_oneGroup_14_g[length(Batch_oneGroup_14_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_14_g[length(Treatment_oneGroup_14_g)+1] = 14
            SampleID_oneGroup_14_g[length(SampleID_oneGroup_14_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_14_g)+1, sep="") 
    }
    

    if( (! is.null(group_15_g)) && ( myTempVec1[3] == group_15_g ) )  {
            FileShort_oneGroup_15_g[length(FileShort_oneGroup_15_g)+1] = myTempVec1[1]
            FileLong_oneGroup_15_g[length(FileLong_oneGroup_15_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_15_g[length(Group_oneGroup_15_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_15_g[length(Covariate_oneGroup_15_g)+1] = myTempVec1[4]  
            Batch_oneGroup_15_g[length(Batch_oneGroup_15_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_15_g[length(Treatment_oneGroup_15_g)+1] = 15
            SampleID_oneGroup_15_g[length(SampleID_oneGroup_15_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_15_g)+1, sep="") 
    }
    

    if( (! is.null(group_16_g)) && ( myTempVec1[3] == group_16_g ) )  {
            FileShort_oneGroup_16_g[length(FileShort_oneGroup_16_g)+1] = myTempVec1[1]
            FileLong_oneGroup_16_g[length(FileLong_oneGroup_16_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_16_g[length(Group_oneGroup_16_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_16_g[length(Covariate_oneGroup_16_g)+1] = myTempVec1[4]  
            Batch_oneGroup_16_g[length(Batch_oneGroup_16_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_16_g[length(Treatment_oneGroup_16_g)+1] = 16
            SampleID_oneGroup_16_g[length(SampleID_oneGroup_16_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_16_g)+1, sep="") 
    }
    

    if( (! is.null(group_17_g)) && ( myTempVec1[3] == group_17_g ) )  {
            FileShort_oneGroup_17_g[length(FileShort_oneGroup_17_g)+1] = myTempVec1[1]
            FileLong_oneGroup_17_g[length(FileLong_oneGroup_17_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_17_g[length(Group_oneGroup_17_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_17_g[length(Covariate_oneGroup_17_g)+1] = myTempVec1[4]  
            Batch_oneGroup_17_g[length(Batch_oneGroup_17_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_17_g[length(Treatment_oneGroup_17_g)+1] = 17
            SampleID_oneGroup_17_g[length(SampleID_oneGroup_17_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_17_g)+1, sep="") 
    }
    

    if( (! is.null(group_18_g)) && ( myTempVec1[3] == group_18_g ) )  {
            FileShort_oneGroup_18_g[length(FileShort_oneGroup_18_g)+1] = myTempVec1[1]
            FileLong_oneGroup_18_g[length(FileLong_oneGroup_18_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_18_g[length(Group_oneGroup_18_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_18_g[length(Covariate_oneGroup_18_g)+1] = myTempVec1[4]  
            Batch_oneGroup_18_g[length(Batch_oneGroup_18_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_18_g[length(Treatment_oneGroup_18_g)+1] = 18
            SampleID_oneGroup_18_g[length(SampleID_oneGroup_18_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_18_g)+1, sep="") 
    }
    

    if( (! is.null(group_19_g)) && ( myTempVec1[3] == group_19_g ) )  {
            FileShort_oneGroup_19_g[length(FileShort_oneGroup_19_g)+1] = myTempVec1[1]
            FileLong_oneGroup_19_g[length(FileLong_oneGroup_19_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_19_g[length(Group_oneGroup_19_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_19_g[length(Covariate_oneGroup_19_g)+1] = myTempVec1[4]  
            Batch_oneGroup_19_g[length(Batch_oneGroup_19_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_19_g[length(Treatment_oneGroup_19_g)+1] = 19
            SampleID_oneGroup_19_g[length(SampleID_oneGroup_19_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_19_g)+1, sep="") 
    }
    

    if( (! is.null(group_20_g)) && ( myTempVec1[3] == group_20_g ) )  {
            FileShort_oneGroup_20_g[length(FileShort_oneGroup_20_g)+1] = myTempVec1[1]
            FileLong_oneGroup_20_g[length(FileLong_oneGroup_20_g)+1]   = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Group_oneGroup_20_g[length(Group_oneGroup_20_g)+1]         = myTempVec1[3]  
            Covariate_oneGroup_20_g[length(Covariate_oneGroup_20_g)+1] = myTempVec1[4]  
            Batch_oneGroup_20_g[length(Batch_oneGroup_20_g)+1]         = myTempVec1[5]  
            Treatment_oneGroup_20_g[length(Treatment_oneGroup_20_g)+1] = 20
            SampleID_oneGroup_20_g[length(SampleID_oneGroup_20_g)+1]   = paste(myTempVec1[3], "_", length(SampleID_oneGroup_20_g)+1, sep="") 
    }
       
}

FileShort_All_vector_g <- c( FileShort_oneGroup_1_g, FileShort_oneGroup_2_g, FileShort_oneGroup_3_g, FileShort_oneGroup_4_g, FileShort_oneGroup_5_g, FileShort_oneGroup_6_g, FileShort_oneGroup_7_g, FileShort_oneGroup_8_g, FileShort_oneGroup_9_g, FileShort_oneGroup_10_g,
                                      FileShort_oneGroup_11_g,FileShort_oneGroup_12_g,FileShort_oneGroup_13_g,FileShort_oneGroup_14_g,FileShort_oneGroup_15_g,FileShort_oneGroup_16_g,FileShort_oneGroup_17_g,FileShort_oneGroup_18_g,FileShort_oneGroup_19_g,FileShort_oneGroup_20_g   
                                    )
FileShort_All_list_g <- as.list( FileShort_All_vector_g )


FileLong_All_vector_g <- c( FileLong_oneGroup_1_g, FileLong_oneGroup_2_g, FileLong_oneGroup_3_g, FileLong_oneGroup_4_g, FileLong_oneGroup_5_g, FileLong_oneGroup_6_g, FileLong_oneGroup_7_g, FileLong_oneGroup_8_g, FileLong_oneGroup_9_g, FileLong_oneGroup_10_g,
                                     FileLong_oneGroup_11_g,FileLong_oneGroup_12_g,FileLong_oneGroup_13_g,FileLong_oneGroup_14_g,FileLong_oneGroup_15_g,FileLong_oneGroup_16_g,FileLong_oneGroup_17_g,FileLong_oneGroup_18_g,FileLong_oneGroup_19_g,FileLong_oneGroup_20_g   
                                   )
FileLong_All_list_g <- as.list( FileLong_All_vector_g )


Group_All_vector_g <- c( Group_oneGroup_1_g, Group_oneGroup_2_g, Group_oneGroup_3_g, Group_oneGroup_4_g, Group_oneGroup_5_g, Group_oneGroup_6_g, Group_oneGroup_7_g, Group_oneGroup_8_g, Group_oneGroup_9_g, Group_oneGroup_10_g,
                                  Group_oneGroup_11_g,Group_oneGroup_12_g,Group_oneGroup_13_g,Group_oneGroup_14_g,Group_oneGroup_15_g,Group_oneGroup_16_g,Group_oneGroup_17_g,Group_oneGroup_18_g,Group_oneGroup_19_g,Group_oneGroup_20_g   
                                )
Group_All_list_g <- as.list( Group_All_vector_g )


Covariate_All_vector_g <- c( Covariate_oneGroup_1_g, Covariate_oneGroup_2_g, Covariate_oneGroup_3_g, Covariate_oneGroup_4_g, Covariate_oneGroup_5_g, Covariate_oneGroup_6_g, Covariate_oneGroup_7_g, Covariate_oneGroup_8_g, Covariate_oneGroup_9_g, Covariate_oneGroup_10_g,
                             Covariate_oneGroup_11_g,Covariate_oneGroup_12_g,Covariate_oneGroup_13_g,Covariate_oneGroup_14_g,Covariate_oneGroup_15_g,Covariate_oneGroup_16_g,Covariate_oneGroup_17_g,Covariate_oneGroup_18_g,Covariate_oneGroup_19_g,Covariate_oneGroup_20_g   
                           )
Covariate_All_list_g <- as.list( Covariate_All_vector_g )


batchEffect_All_vector_g <- c( Batch_oneGroup_1_g, Batch_oneGroup_2_g, Batch_oneGroup_3_g, Batch_oneGroup_4_g, Batch_oneGroup_5_g, Batch_oneGroup_6_g, Batch_oneGroup_7_g, Batch_oneGroup_8_g, Batch_oneGroup_9_g, Batch_oneGroup_10_g,
                               Batch_oneGroup_11_g,Batch_oneGroup_12_g,Batch_oneGroup_13_g,Batch_oneGroup_14_g,Batch_oneGroup_15_g,Batch_oneGroup_16_g,Batch_oneGroup_17_g,Batch_oneGroup_18_g,Batch_oneGroup_19_g,Batch_oneGroup_20_g   
                             )
batchEffect_All_list_g <- as.list( batchEffect_All_vector_g )


Treatment_All_vector_g <- c( Treatment_oneGroup_1_g, Treatment_oneGroup_2_g, Treatment_oneGroup_3_g, Treatment_oneGroup_4_g, Treatment_oneGroup_5_g, Treatment_oneGroup_6_g, Treatment_oneGroup_7_g, Treatment_oneGroup_8_g, Treatment_oneGroup_9_g, Treatment_oneGroup_10_g,
                                      Treatment_oneGroup_11_g,Treatment_oneGroup_12_g,Treatment_oneGroup_13_g,Treatment_oneGroup_14_g,Treatment_oneGroup_15_g,Treatment_oneGroup_16_g,Treatment_oneGroup_17_g,Treatment_oneGroup_18_g,Treatment_oneGroup_19_g,Treatment_oneGroup_20_g   
                                    )
Treatment_All_list_g <- as.list( Treatment_All_vector_g )


SampleID_All_vector_g <- c( SampleID_oneGroup_1_g, SampleID_oneGroup_2_g, SampleID_oneGroup_3_g, SampleID_oneGroup_4_g, SampleID_oneGroup_5_g, SampleID_oneGroup_6_g, SampleID_oneGroup_7_g, SampleID_oneGroup_8_g, SampleID_oneGroup_9_g, SampleID_oneGroup_10_g,
                                     SampleID_oneGroup_11_g,SampleID_oneGroup_12_g,SampleID_oneGroup_13_g,SampleID_oneGroup_14_g,SampleID_oneGroup_15_g,SampleID_oneGroup_16_g,SampleID_oneGroup_17_g,SampleID_oneGroup_18_g,SampleID_oneGroup_19_g,SampleID_oneGroup_20_g   
                                   )
SampleID_All_list_g <- as.list( SampleID_All_vector_g )


cat("Length of variables: \n")
print( length( FileShort_All_vector_g ) )
print( length( FileShort_All_list_g ) )
print( length( FileLong_All_vector_g ) )
print( length( FileLong_All_list_g ) )
print( length( Covariate_All_vector_g ) )
print( length( Covariate_All_list_g ) )
print( length( batchEffect_All_vector_g ) )
print( length( batchEffect_All_list_g ) )
print( length( Group_All_vector_g ) )
print( length( Group_All_list_g ) )
print( length( Treatment_All_vector_g ) )
print( length( Treatment_All_list_g ) )
print( length( SampleID_All_vector_g ) )
print( length( SampleID_All_list_g ) )
cat("\n\n\n\n\n\n")
##############################################################################################################################################################################################

    



##############################################################################################################################################################################################
cat("##############################################\n")
allSamples_info_g = cbind(FileShort_All_vector_g,   FileLong_All_vector_g,   Covariate_All_vector_g,  batchEffect_All_vector_g, 
                          Group_All_vector_g,       Treatment_All_vector_g,  SampleID_All_vector_g)

write.table(allSamples_info_g , file = paste(Part2_g,   "1_All-Samples-Information.txt",  sep="/"), 
            append =FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = TRUE )

ColorOfGroup_All_g  = c( color_group_1_g,  color_group_2_g,  color_group_3_g,  color_group_4_g,  color_group_5_g,  color_group_6_g,  color_group_7_g,  color_group_8_g,  color_group_9_g,  color_group_10_g,
                        color_group_11_g, color_group_12_g, color_group_13_g, color_group_14_g, color_group_15_g, color_group_16_g, color_group_17_g, color_group_18_g, color_group_19_g, color_group_20_g 
                       ) 
ShapeOfCovariate_All_g  = c( shape_covariate_1_g,  shape_covariate_2_g,  shape_covariate_3_g,  shape_covariate_4_g,  shape_covariate_5_g,  shape_covariate_6_g,  shape_covariate_7_g,  shape_covariate_8_g,  shape_covariate_9_g,  shape_covariate_10_g,
                             shape_covariate_11_g, shape_covariate_12_g, shape_covariate_13_g, shape_covariate_14_g, shape_covariate_15_g, shape_covariate_16_g, shape_covariate_17_g, shape_covariate_18_g, shape_covariate_19_g, shape_covariate_20_g 
                           ) 
SizeOfBatch_All_g  = c( size_batchEffect_1_g,  size_batchEffect_2_g,  size_batchEffect_3_g,  size_batchEffect_4_g,  size_batchEffect_5_g,  size_batchEffect_6_g,  size_batchEffect_7_g,  size_batchEffect_8_g,  size_batchEffect_9_g,  size_batchEffect_10_g,
                        size_batchEffect_11_g, size_batchEffect_12_g, size_batchEffect_13_g, size_batchEffect_14_g, size_batchEffect_15_g, size_batchEffect_16_g, size_batchEffect_17_g, size_batchEffect_18_g, size_batchEffect_19_g, size_batchEffect_20_g                                       
                      ) 

print("Length of three vectors")
cat("\n")
print("ColorOfGroup_All_g:")
print( length(ColorOfGroup_All_g) )
cat("\n")
print("ShapeOfCovariate_All_g:")
print( length(ShapeOfCovariate_All_g) )
cat("\n")
print("SizeOfBatch_All_g:")
print( length(SizeOfBatch_All_g) )
cat("\n\n\n") 

sink( file = paste(Part2_g,  "2_Values-of-Three-Vectors.txt",  sep="/") )
	print("ColorOfGroup_All_g:")
	print( ColorOfGroup_All_g )
	cat("\n\n")
	print("ShapeOfCovariate_All_g:")
	print( ShapeOfCovariate_All_g )
	cat("\n\n")
	print("SizeOfBatch_All_g:")
	print( SizeOfBatch_All_g )
	cat("\n\n\n") 
sink() 
 
CovariateToShape_f <- function(  myCovariate_vector   ) {
  myShape_vector  = myCovariate_vector
  for(i in c(1:length(myCovariate_vector)) ) {
    if( (! is.null(covariate_1_g))  && (myShape_vector[i] == covariate_1_g)  ) { myShape_vector[i] =  shape_covariate_1_g  }
    if( (! is.null(covariate_2_g))  && (myShape_vector[i] == covariate_2_g)  ) { myShape_vector[i] =  shape_covariate_2_g  }
    if( (! is.null(covariate_3_g))  && (myShape_vector[i] == covariate_3_g)  ) { myShape_vector[i] =  shape_covariate_3_g  }
    if( (! is.null(covariate_4_g))  && (myShape_vector[i] == covariate_4_g)  ) { myShape_vector[i] =  shape_covariate_4_g  }
    if( (! is.null(covariate_5_g))  && (myShape_vector[i] == covariate_5_g)  ) { myShape_vector[i] =  shape_covariate_5_g  }
    if( (! is.null(covariate_6_g))  && (myShape_vector[i] == covariate_6_g)  ) { myShape_vector[i] =  shape_covariate_6_g  }
    if( (! is.null(covariate_7_g))  && (myShape_vector[i] == covariate_7_g)  ) { myShape_vector[i] =  shape_covariate_7_g  }
    if( (! is.null(covariate_8_g))  && (myShape_vector[i] == covariate_8_g)  ) { myShape_vector[i] =  shape_covariate_8_g  }
    if( (! is.null(covariate_9_g))  && (myShape_vector[i] == covariate_9_g)  ) { myShape_vector[i] =  shape_covariate_9_g  }
    if( (! is.null(covariate_10_g)) && (myShape_vector[i] == covariate_10_g) ) { myShape_vector[i] =  shape_covariate_10_g }
    if( (! is.null(covariate_11_g)) && (myShape_vector[i] == covariate_11_g) ) { myShape_vector[i] =  shape_covariate_11_g }
    if( (! is.null(covariate_12_g)) && (myShape_vector[i] == covariate_12_g) ) { myShape_vector[i] =  shape_covariate_12_g }
    if( (! is.null(covariate_13_g)) && (myShape_vector[i] == covariate_13_g) ) { myShape_vector[i] =  shape_covariate_13_g }
    if( (! is.null(covariate_14_g)) && (myShape_vector[i] == covariate_14_g) ) { myShape_vector[i] =  shape_covariate_14_g }
    if( (! is.null(covariate_15_g)) && (myShape_vector[i] == covariate_15_g) ) { myShape_vector[i] =  shape_covariate_15_g }
    if( (! is.null(covariate_16_g)) && (myShape_vector[i] == covariate_16_g) ) { myShape_vector[i] =  shape_covariate_16_g }
    if( (! is.null(covariate_17_g)) && (myShape_vector[i] == covariate_17_g) ) { myShape_vector[i] =  shape_covariate_17_g }
    if( (! is.null(covariate_18_g)) && (myShape_vector[i] == covariate_18_g) ) { myShape_vector[i] =  shape_covariate_18_g }
    if( (! is.null(covariate_19_g)) && (myShape_vector[i] == covariate_19_g) ) { myShape_vector[i] =  shape_covariate_19_g }
    if( (! is.null(covariate_20_g)) && (myShape_vector[i] == covariate_20_g) ) { myShape_vector[i] =  shape_covariate_20_g }
  }
  myShape_vector = as.numeric(myShape_vector)
  names(myShape_vector) = myCovariate_vector
  return(myShape_vector)
}
 
BatchToSize_f <- function(  mybatchEffect_vector   ) {
  mySize_vector  = mybatchEffect_vector
  for(i in c(1:length(mybatchEffect_vector)) ) {
    if( (! is.null(batchEffect_1_g))  && (mySize_vector[i] == batchEffect_1_g)  ) { mySize_vector[i] =  size_batchEffect_1_g  }
    if( (! is.null(batchEffect_2_g))  && (mySize_vector[i] == batchEffect_2_g)  ) { mySize_vector[i] =  size_batchEffect_2_g  }
    if( (! is.null(batchEffect_3_g))  && (mySize_vector[i] == batchEffect_3_g)  ) { mySize_vector[i] =  size_batchEffect_3_g  }
    if( (! is.null(batchEffect_4_g))  && (mySize_vector[i] == batchEffect_4_g)  ) { mySize_vector[i] =  size_batchEffect_4_g  }
    if( (! is.null(batchEffect_5_g))  && (mySize_vector[i] == batchEffect_5_g)  ) { mySize_vector[i] =  size_batchEffect_5_g  }
    if( (! is.null(batchEffect_6_g))  && (mySize_vector[i] == batchEffect_6_g)  ) { mySize_vector[i] =  size_batchEffect_6_g  }
    if( (! is.null(batchEffect_7_g))  && (mySize_vector[i] == batchEffect_7_g)  ) { mySize_vector[i] =  size_batchEffect_7_g  }
    if( (! is.null(batchEffect_8_g))  && (mySize_vector[i] == batchEffect_8_g)  ) { mySize_vector[i] =  size_batchEffect_8_g  }
    if( (! is.null(batchEffect_9_g))  && (mySize_vector[i] == batchEffect_9_g)  ) { mySize_vector[i] =  size_batchEffect_9_g  }
    if( (! is.null(batchEffect_10_g)) && (mySize_vector[i] == batchEffect_10_g) ) { mySize_vector[i] =  size_batchEffect_10_g }
    if( (! is.null(batchEffect_11_g)) && (mySize_vector[i] == batchEffect_11_g) ) { mySize_vector[i] =  size_batchEffect_11_g }
    if( (! is.null(batchEffect_12_g)) && (mySize_vector[i] == batchEffect_12_g) ) { mySize_vector[i] =  size_batchEffect_12_g }
    if( (! is.null(batchEffect_13_g)) && (mySize_vector[i] == batchEffect_13_g) ) { mySize_vector[i] =  size_batchEffect_13_g }
    if( (! is.null(batchEffect_14_g)) && (mySize_vector[i] == batchEffect_14_g) ) { mySize_vector[i] =  size_batchEffect_14_g }
    if( (! is.null(batchEffect_15_g)) && (mySize_vector[i] == batchEffect_15_g) ) { mySize_vector[i] =  size_batchEffect_15_g }
    if( (! is.null(batchEffect_16_g)) && (mySize_vector[i] == batchEffect_16_g) ) { mySize_vector[i] =  size_batchEffect_16_g }
    if( (! is.null(batchEffect_17_g)) && (mySize_vector[i] == batchEffect_17_g) ) { mySize_vector[i] =  size_batchEffect_17_g }
    if( (! is.null(batchEffect_18_g)) && (mySize_vector[i] == batchEffect_18_g) ) { mySize_vector[i] =  size_batchEffect_18_g }
    if( (! is.null(batchEffect_19_g)) && (mySize_vector[i] == batchEffect_19_g) ) { mySize_vector[i] =  size_batchEffect_19_g }
    if( (! is.null(batchEffect_20_g)) && (mySize_vector[i] == batchEffect_20_g) ) { mySize_vector[i] =  size_batchEffect_20_g }
  }
  mySize_vector = as.numeric(mySize_vector)
  names(mySize_vector) = mybatchEffect_vector
  return(mySize_vector)
}
 
GroupToColor_f <- function(  myGroup_vector   ) {
  myColor_vector = myGroup_vector
  for(i in c(1:length(myGroup_vector)) ) {
    if( (! is.null(group_1_g))  && (myColor_vector[i] == group_1_g)  ) { myColor_vector[i] =  color_group_1_g  }
    if( (! is.null(group_2_g))  && (myColor_vector[i] == group_2_g)  ) { myColor_vector[i] =  color_group_2_g  }
    if( (! is.null(group_3_g))  && (myColor_vector[i] == group_3_g)  ) { myColor_vector[i] =  color_group_3_g  }
    if( (! is.null(group_4_g))  && (myColor_vector[i] == group_4_g)  ) { myColor_vector[i] =  color_group_4_g  }
    if( (! is.null(group_5_g))  && (myColor_vector[i] == group_5_g)  ) { myColor_vector[i] =  color_group_5_g  }
    if( (! is.null(group_6_g))  && (myColor_vector[i] == group_6_g)  ) { myColor_vector[i] =  color_group_6_g  }
    if( (! is.null(group_7_g))  && (myColor_vector[i] == group_7_g)  ) { myColor_vector[i] =  color_group_7_g  }
    if( (! is.null(group_8_g))  && (myColor_vector[i] == group_8_g)  ) { myColor_vector[i] =  color_group_8_g  }
    if( (! is.null(group_9_g))  && (myColor_vector[i] == group_9_g)  ) { myColor_vector[i] =  color_group_9_g  }
    if( (! is.null(group_10_g)) && (myColor_vector[i] == group_10_g) ) { myColor_vector[i] =  color_group_10_g }
    if( (! is.null(group_11_g)) && (myColor_vector[i] == group_11_g) ) { myColor_vector[i] =  color_group_11_g }
    if( (! is.null(group_12_g)) && (myColor_vector[i] == group_12_g) ) { myColor_vector[i] =  color_group_12_g }
    if( (! is.null(group_13_g)) && (myColor_vector[i] == group_13_g) ) { myColor_vector[i] =  color_group_13_g }
    if( (! is.null(group_14_g)) && (myColor_vector[i] == group_14_g) ) { myColor_vector[i] =  color_group_14_g }
    if( (! is.null(group_15_g)) && (myColor_vector[i] == group_15_g) ) { myColor_vector[i] =  color_group_15_g }
    if( (! is.null(group_16_g)) && (myColor_vector[i] == group_16_g) ) { myColor_vector[i] =  color_group_16_g }
    if( (! is.null(group_17_g)) && (myColor_vector[i] == group_17_g) ) { myColor_vector[i] =  color_group_17_g }
    if( (! is.null(group_18_g)) && (myColor_vector[i] == group_18_g) ) { myColor_vector[i] =  color_group_18_g }
    if( (! is.null(group_19_g)) && (myColor_vector[i] == group_19_g) ) { myColor_vector[i] =  color_group_19_g }
    if( (! is.null(group_20_g)) && (myColor_vector[i] == group_20_g) ) { myColor_vector[i] =  color_group_20_g }
  }
  ##myColor_vector = as.numeric(myColor_vector)
  names(myColor_vector) = myGroup_vector
  return(myColor_vector)
}
 
GroupToNumber_f <- function(  myGroup_vector   ) {
  myColor_vector = myGroup_vector
  for(i in c(1:length(myGroup_vector)) ) {
    if( (! is.null(group_1_g))  && (myColor_vector[i] == group_1_g)  ) { myColor_vector[i] =  1 }
    if( (! is.null(group_2_g))  && (myColor_vector[i] == group_2_g)  ) { myColor_vector[i] =  2  }
    if( (! is.null(group_3_g))  && (myColor_vector[i] == group_3_g)  ) { myColor_vector[i] =  3  }
    if( (! is.null(group_4_g))  && (myColor_vector[i] == group_4_g)  ) { myColor_vector[i] =  4  }
    if( (! is.null(group_5_g))  && (myColor_vector[i] == group_5_g)  ) { myColor_vector[i] =  5  }
    if( (! is.null(group_6_g))  && (myColor_vector[i] == group_6_g)  ) { myColor_vector[i] =  6  }
    if( (! is.null(group_7_g))  && (myColor_vector[i] == group_7_g)  ) { myColor_vector[i] =  7  }
    if( (! is.null(group_8_g))  && (myColor_vector[i] == group_8_g)  ) { myColor_vector[i] =  8  }
    if( (! is.null(group_9_g))  && (myColor_vector[i] == group_9_g)  ) { myColor_vector[i] =  9  }
    if( (! is.null(group_10_g)) && (myColor_vector[i] == group_10_g) ) { myColor_vector[i] =  10 }
    if( (! is.null(group_11_g)) && (myColor_vector[i] == group_11_g) ) { myColor_vector[i] =  11 }
    if( (! is.null(group_12_g)) && (myColor_vector[i] == group_12_g) ) { myColor_vector[i] =  12 }
    if( (! is.null(group_13_g)) && (myColor_vector[i] == group_13_g) ) { myColor_vector[i] =  13 }
    if( (! is.null(group_14_g)) && (myColor_vector[i] == group_14_g) ) { myColor_vector[i] =  14 }
    if( (! is.null(group_15_g)) && (myColor_vector[i] == group_15_g) ) { myColor_vector[i] =  15 }
    if( (! is.null(group_16_g)) && (myColor_vector[i] == group_16_g) ) { myColor_vector[i] =  16 }
    if( (! is.null(group_17_g)) && (myColor_vector[i] == group_17_g) ) { myColor_vector[i] =  17 }
    if( (! is.null(group_18_g)) && (myColor_vector[i] == group_18_g) ) { myColor_vector[i] =  18 }
    if( (! is.null(group_19_g)) && (myColor_vector[i] == group_19_g) ) { myColor_vector[i] =  19 }
    if( (! is.null(group_20_g)) && (myColor_vector[i] == group_20_g) ) { myColor_vector[i] =  20 }
  }
  ##myColor_vector = as.numeric(myColor_vector)
  #names(myColor_vector) = myGroup_vector
  return(myColor_vector)
}


sink( paste(Part2_g, "/3_All-Files.txt",  sep="")  )
    cat("\n\n FileLong_oneGroup_1_g:\n")
    print( FileLong_oneGroup_1_g  )
    cat("\n\n FileLong_oneGroup_2_g:\n")
    print( FileLong_oneGroup_2_g  )
    cat("\n\n FileLong_oneGroup_3_g:\n")
    print( FileLong_oneGroup_3_g  )
    cat("\n\n FileLong_oneGroup_4_g:\n")
    print( FileLong_oneGroup_4_g  )
    cat("\n\n FileLong_oneGroup_5_g:\n")
    print( FileLong_oneGroup_5_g  )
    cat("\n\n FileLong_oneGroup_6_g:\n")
    print( FileLong_oneGroup_6_g  )
    cat("\n\n FileLong_oneGroup_7_g:\n")
    print( FileLong_oneGroup_7_g  )
    cat("\n\n FileLong_oneGroup_8_g:\n")
    print( FileLong_oneGroup_8_g  )
    cat("\n\n FileLong_oneGroup_9_g:\n")
    print( FileLong_oneGroup_9_g  )
    cat("\n\n FileLong_oneGroup_10_g:\n")
    print( FileLong_oneGroup_10_g  )
    cat("\n\n FileLong_oneGroup_11_g:\n")
    print( FileLong_oneGroup_11_g  )
    cat("\n\n FileLong_oneGroup_12_g:\n")
    print( FileLong_oneGroup_12_g  )
    cat("\n\n FileLong_oneGroup_13_g:\n")
    print( FileLong_oneGroup_13_g  )
    cat("\n\n FileLong_oneGroup_14_g:\n")
    print( FileLong_oneGroup_14_g  )
    cat("\n\n FileLong_oneGroup_15_g:\n")
    print( FileLong_oneGroup_15_g  )
    cat("\n\n FileLong_oneGroup_16_g:\n")
    print( FileLong_oneGroup_16_g  )
    cat("\n\n FileLong_oneGroup_17_g:\n")
    print( FileLong_oneGroup_17_g  )
    cat("\n\n FileLong_oneGroup_18_g:\n")
    print( FileLong_oneGroup_18_g  )
    cat("\n\n FileLong_oneGroup_19_g:\n")
    print( FileLong_oneGroup_19_g  )
    cat("\n\n FileLong_oneGroup_20_g:\n")
    print( FileLong_oneGroup_20_g  )
sink()  

sink( paste(Part2_g, "/4_All-Files-ShortName.txt",  sep="")  )
    cat("\n\n FileShort_oneGroup_1_g:\n")
    print( FileShort_oneGroup_1_g  )
    cat("\n\n FileShort_oneGroup_2_g:\n")
    print( FileShort_oneGroup_2_g  )
    cat("\n\n FileShort_oneGroup_3_g:\n")
    print( FileShort_oneGroup_3_g  )
    cat("\n\n FileShort_oneGroup_4_g:\n")
    print( FileShort_oneGroup_4_g  )
    cat("\n\n FileShort_oneGroup_5_g:\n")
    print( FileShort_oneGroup_5_g  )
    cat("\n\n FileShort_oneGroup_6_g:\n")
    print( FileShort_oneGroup_6_g  )
    cat("\n\n FileShort_oneGroup_7_g:\n")
    print( FileShort_oneGroup_7_g  )
    cat("\n\n FileShort_oneGroup_8_g:\n")
    print( FileShort_oneGroup_8_g  )
    cat("\n\n FileShort_oneGroup_9_g:\n")
    print( FileShort_oneGroup_9_g  )
    cat("\n\n FileShort_oneGroup_10_g:\n")
    print( FileShort_oneGroup_10_g  )
    cat("\n\n FileShort_oneGroup_11_g:\n")
    print( FileShort_oneGroup_11_g  )
    cat("\n\n FileShort_oneGroup_12_g:\n")
    print( FileShort_oneGroup_12_g  )
    cat("\n\n FileShort_oneGroup_13_g:\n")
    print( FileShort_oneGroup_13_g  )
    cat("\n\n FileShort_oneGroup_14_g:\n")
    print( FileShort_oneGroup_14_g  )
    cat("\n\n FileShort_oneGroup_15_g:\n")
    print( FileShort_oneGroup_15_g  )
    cat("\n\n FileShort_oneGroup_16_g:\n")
    print( FileShort_oneGroup_16_g  )
    cat("\n\n FileShort_oneGroup_17_g:\n")
    print( FileShort_oneGroup_17_g  )
    cat("\n\n FileShort_oneGroup_18_g:\n")
    print( FileShort_oneGroup_18_g  )
    cat("\n\n FileShort_oneGroup_19_g:\n")
    print( FileShort_oneGroup_19_g  )
    cat("\n\n FileShort_oneGroup_20_g:\n")
    print( FileShort_oneGroup_20_g  )
sink()  

number_1_g  = length( FileLong_oneGroup_1_g )  
number_2_g  = length( FileLong_oneGroup_2_g )  
number_3_g  = length( FileLong_oneGroup_3_g )  
number_4_g  = length( FileLong_oneGroup_4_g )  
number_5_g  = length( FileLong_oneGroup_5_g )  
number_6_g  = length( FileLong_oneGroup_6_g )  
number_7_g  = length( FileLong_oneGroup_7_g )  
number_8_g  = length( FileLong_oneGroup_8_g )  
number_9_g  = length( FileLong_oneGroup_9_g )  
number_10_g = length( FileLong_oneGroup_10_g)  
number_11_g = length( FileLong_oneGroup_11_g )  
number_12_g = length( FileLong_oneGroup_12_g )  
number_13_g = length( FileLong_oneGroup_13_g )  
number_14_g = length( FileLong_oneGroup_14_g )  
number_15_g = length( FileLong_oneGroup_15_g )  
number_16_g = length( FileLong_oneGroup_16_g )  
number_17_g = length( FileLong_oneGroup_17_g )  
number_18_g = length( FileLong_oneGroup_18_g )  
number_19_g = length( FileLong_oneGroup_19_g )  
number_20_g = length( FileLong_oneGroup_20_g)  

## ceiling(), floor(), round()
numberCovered_1_g  = round( number_1_g * ProportionCoveredSamples_g )   
numberCovered_2_g  = round( number_2_g * ProportionCoveredSamples_g )      
numberCovered_3_g  = round( number_3_g * ProportionCoveredSamples_g )      
numberCovered_4_g  = round( number_4_g * ProportionCoveredSamples_g )      
numberCovered_5_g  = round( number_5_g * ProportionCoveredSamples_g )      
numberCovered_6_g  = round( number_6_g * ProportionCoveredSamples_g )      
numberCovered_7_g  = round( number_7_g * ProportionCoveredSamples_g )      
numberCovered_8_g  = round( number_8_g * ProportionCoveredSamples_g )      
numberCovered_9_g  = round( number_9_g * ProportionCoveredSamples_g )      
numberCovered_10_g = round( number_10_g * ProportionCoveredSamples_g )    
numberCovered_11_g = round( number_11_g * ProportionCoveredSamples_g )      
numberCovered_12_g = round( number_12_g * ProportionCoveredSamples_g )      
numberCovered_13_g = round( number_13_g * ProportionCoveredSamples_g )      
numberCovered_14_g = round( number_14_g * ProportionCoveredSamples_g )      
numberCovered_15_g = round( number_15_g * ProportionCoveredSamples_g )      
numberCovered_16_g = round( number_16_g * ProportionCoveredSamples_g )      
numberCovered_17_g = round( number_17_g * ProportionCoveredSamples_g )      
numberCovered_18_g = round( number_18_g * ProportionCoveredSamples_g )      
numberCovered_19_g = round( number_19_g * ProportionCoveredSamples_g )      
numberCovered_20_g = round( number_20_g * ProportionCoveredSamples_g )     


sink( paste(Part2_g, "/5_Number-of-Samples-of-Each-Group.txt",  sep="")  )
    print( length( FileLong_oneGroup_1_g  ) )
    print( length( FileLong_oneGroup_2_g  ) )
    print( length( FileLong_oneGroup_3_g  ) )
    print( length( FileLong_oneGroup_4_g  ) )
    print( length( FileLong_oneGroup_5_g  ) )
    print( length( FileLong_oneGroup_6_g  ) )
    print( length( FileLong_oneGroup_7_g  ) )
    print( length( FileLong_oneGroup_8_g  ) )
    print( length( FileLong_oneGroup_9_g  ) )
    print( length( FileLong_oneGroup_10_g ) )
    print( length( FileLong_oneGroup_11_g ) )
    print( length( FileLong_oneGroup_12_g ) )
    print( length( FileLong_oneGroup_13_g ) )
    print( length( FileLong_oneGroup_14_g ) )
    print( length( FileLong_oneGroup_15_g ) )
    print( length( FileLong_oneGroup_16_g ) )
    print( length( FileLong_oneGroup_17_g ) )
    print( length( FileLong_oneGroup_18_g ) )
    print( length( FileLong_oneGroup_19_g ) )
    print( length( FileLong_oneGroup_20_g ) )
    cat("\n\n\n\n\n")  
    print( length( FileShort_oneGroup_1_g ) )
    print( length( FileShort_oneGroup_2_g ) )
    print( length( FileShort_oneGroup_3_g ) )
    print( length( FileShort_oneGroup_4_g ) )
    print( length( FileShort_oneGroup_5_g ) )
    print( length( FileShort_oneGroup_6_g ) )
    print( length( FileShort_oneGroup_7_g ) )
    print( length( FileShort_oneGroup_8_g ) )
    print( length( FileShort_oneGroup_9_g ) )
    print( length( FileShort_oneGroup_10_g ) )
    print( length( FileShort_oneGroup_11_g ) )
    print( length( FileShort_oneGroup_12_g ) )
    print( length( FileShort_oneGroup_13_g ) )
    print( length( FileShort_oneGroup_14_g ) )
    print( length( FileShort_oneGroup_15_g ) )
    print( length( FileShort_oneGroup_16_g ) )
    print( length( FileShort_oneGroup_17_g ) )
    print( length( FileShort_oneGroup_18_g ) )
    print( length( FileShort_oneGroup_19_g ) )
    print( length( FileShort_oneGroup_20_g ) )
    cat("\n\n\n\n\n")  
    print( number_1_g )
    print( number_2_g )
    print( number_3_g )
    print( number_4_g )
    print( number_5_g )
    print( number_6_g )
    print( number_7_g )
    print( number_8_g )
    print( number_9_g )
    print( number_10_g )
    print( number_11_g )
    print( number_12_g )
    print( number_13_g )
    print( number_14_g )
    print( number_15_g )
    print( number_16_g )
    print( number_17_g )
    print( number_18_g )
    print( number_19_g )
    print( number_20_g )
    cat("\n\n\n\n\n")  
sink() 


sink( paste(Part2_g, "/6_Number-of-Covered-Samples-of-Each-Group.txt",  sep="")  )
    print( numberCovered_1_g )
    print( numberCovered_2_g )
    print( numberCovered_3_g )
    print( numberCovered_4_g )
    print( numberCovered_5_g )
    print( numberCovered_6_g )
    print( numberCovered_7_g )
    print( numberCovered_8_g )
    print( numberCovered_9_g )
    print( numberCovered_10_g )
    print( numberCovered_11_g )
    print( numberCovered_12_g )
    print( numberCovered_13_g )
    print( numberCovered_14_g )
    print( numberCovered_15_g )
    print( numberCovered_16_g )
    print( numberCovered_17_g )
    print( numberCovered_18_g )
    print( numberCovered_19_g )
    print( numberCovered_20_g )  
sink() 
##############################################################################################################################################################################################




 


