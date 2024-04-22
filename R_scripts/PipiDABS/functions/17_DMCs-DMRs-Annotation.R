myDMRs_annotation_bool <- function(  myDiffDMR_55,   SpecificLoci_55, mySpecificLoci.obj_55,  file_55, path_55  ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_55, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
        Ann_SpecificLoci_1A = genomation::annotateWithGeneParts( as(myDiffDMR_55,  "GRanges"),  mySpecificLoci.obj_55)
        sink( file=paste(path_55, file_55, sep="/")   )
        print(  genomation::getFeatsWithTargetsStats(  Ann_SpecificLoci_1A,  percentage=TRUE) ) 
        cat("\n\n\n\n\n")
        print(Ann_SpecificLoci_1A)
        sink()
        pdf( file=paste(path_55, "/", file_55, ".pdf", sep="")   )
        print(genomation::plotTargetAnnotation(Ann_SpecificLoci_1A,  precedence=TRUE,   main=SpecificLoci_55 ) )
        dev.off() 
    }else{
          Ann_SpecificLoci_2 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_55, "GRanges"),
                                         feature = mySpecificLoci.obj_55$targets,  flank = mySpecificLoci.obj_55$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path_55, file_55, sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_SpecificLoci_2,  percentage=TRUE) ) 
          print(Ann_SpecificLoci_2)
          sink()
          pdf( file=paste(path_55, "/", file_55, ".pdf", sep="")   )
          print(genomation::plotTargetAnnotation(Ann_SpecificLoci_2, precedence=TRUE, main=SpecificLoci_55))
          dev.off()  
    }  
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
  
}



myDMRs_annotation_g <- function(  myDiffDMR_5,   path2_5  ) {
    print(myDiffDMR_5)
    cat("\n\n\n\n")
    print(path2_5)
    if( ! file.exists(path2_5) ) { dir.create(path2_5, recursive = TRUE) }
  

    if( ! is.na(SpecificLoci_1_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_1_g, mySpecificLoci.obj_55=SpecificLoci_1.obj_g, file_55="1_SpecificLoci_1.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_2_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_2_g, mySpecificLoci.obj_55=SpecificLoci_2.obj_g, file_55="2_SpecificLoci_2.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_3_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_3_g, mySpecificLoci.obj_55=SpecificLoci_3.obj_g, file_55="3_SpecificLoci_3.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_4_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_4_g, mySpecificLoci.obj_55=SpecificLoci_4.obj_g, file_55="4_SpecificLoci_4.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_5_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_5_g, mySpecificLoci.obj_55=SpecificLoci_5.obj_g, file_55="5_SpecificLoci_5.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_6_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_6_g, mySpecificLoci.obj_55=SpecificLoci_6.obj_g, file_55="6_SpecificLoci_6.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_7_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_7_g, mySpecificLoci.obj_55=SpecificLoci_7.obj_g, file_55="7_SpecificLoci_7.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_8_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_8_g, mySpecificLoci.obj_55=SpecificLoci_8.obj_g, file_55="8_SpecificLoci_8.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_9_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_9_g, mySpecificLoci.obj_55=SpecificLoci_9.obj_g, file_55="9_SpecificLoci_9.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_10_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_10_g, mySpecificLoci.obj_55=SpecificLoci_10.obj_g, file_55="10_SpecificLoci_10.txt", path_55=path2_5  )                 
    }
 
 
  

    if( ! is.na(SpecificLoci_11_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_11_g, mySpecificLoci.obj_55=SpecificLoci_11.obj_g, file_55="11_SpecificLoci_11.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_12_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_12_g, mySpecificLoci.obj_55=SpecificLoci_12.obj_g, file_55="12_SpecificLoci_12.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_13_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_13_g, mySpecificLoci.obj_55=SpecificLoci_13.obj_g, file_55="13_SpecificLoci_13.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_14_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_14_g, mySpecificLoci.obj_55=SpecificLoci_14.obj_g, file_55="14_SpecificLoci_14.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_15_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_15_g, mySpecificLoci.obj_55=SpecificLoci_15.obj_g, file_55="15_SpecificLoci_15.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_16_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_16_g, mySpecificLoci.obj_55=SpecificLoci_16.obj_g, file_55="16_SpecificLoci_16.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_17_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_17_g, mySpecificLoci.obj_55=SpecificLoci_17.obj_g, file_55="17_SpecificLoci_17.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_18_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_18_g, mySpecificLoci.obj_55=SpecificLoci_18.obj_g, file_55="18_SpecificLoci_18.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_19_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_19_g, mySpecificLoci.obj_55=SpecificLoci_19.obj_g, file_55="19_SpecificLoci_19.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_20_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_20_g, mySpecificLoci.obj_55=SpecificLoci_20.obj_g, file_55="20_SpecificLoci_20.txt", path_55=path2_5  )                 
    }
 


 
 
  

    if( ! is.na(SpecificLoci_21_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_21_g, mySpecificLoci.obj_55=SpecificLoci_21.obj_g, file_55="21_SpecificLoci_21.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_22_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_22_g, mySpecificLoci.obj_55=SpecificLoci_22.obj_g, file_55="22_SpecificLoci_22.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_23_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_23_g, mySpecificLoci.obj_55=SpecificLoci_23.obj_g, file_55="23_SpecificLoci_23.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_24_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_24_g, mySpecificLoci.obj_55=SpecificLoci_24.obj_g, file_55="24_SpecificLoci_24.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_25_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_25_g, mySpecificLoci.obj_55=SpecificLoci_25.obj_g, file_55="25_SpecificLoci_25.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_26_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_26_g, mySpecificLoci.obj_55=SpecificLoci_26.obj_g, file_55="26_SpecificLoci_26.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_27_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_27_g, mySpecificLoci.obj_55=SpecificLoci_27.obj_g, file_55="27_SpecificLoci_27.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_28_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_28_g, mySpecificLoci.obj_55=SpecificLoci_28.obj_g, file_55="28_SpecificLoci_28.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_29_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_29_g, mySpecificLoci.obj_55=SpecificLoci_29.obj_g, file_55="29_SpecificLoci_29.txt", path_55=path2_5  )                 
    }

    if( ! is.na(SpecificLoci_30_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   SpecificLoci_55=SpecificLoci_30_g, mySpecificLoci.obj_55=SpecificLoci_30.obj_g, file_55="30_SpecificLoci_30.txt", path_55=path2_5  )                 
    }
 
}  









