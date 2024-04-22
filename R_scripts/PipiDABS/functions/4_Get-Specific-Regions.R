##############################################################################################################################################################################################
suppressPackageStartupMessages( library(genomation) )
BedFiles_g  <- list.files(path = SpecificRegions_g, pattern = ".bed",  full.names = TRUE  )
cat("\n\nBedFiles_g:\n")
cat(BedFiles_g, sep = "\n")
cat("\n\n\n\n\n")

NumberOfBedFiles_g = length(BedFiles_g) 
cat("\n\nNumber of bed files:  ")
cat(NumberOfBedFiles_g)
cat("\n\n\n\n\n")

SpecificLoci_1_g  = NA
SpecificLoci_2_g  = NA
SpecificLoci_3_g  = NA
SpecificLoci_4_g  = NA
SpecificLoci_5_g  = NA
SpecificLoci_6_g  = NA
SpecificLoci_7_g  = NA
SpecificLoci_8_g  = NA
SpecificLoci_9_g  = NA
SpecificLoci_10_g = NA
SpecificLoci_11_g = NA
SpecificLoci_12_g = NA
SpecificLoci_13_g = NA
SpecificLoci_14_g = NA
SpecificLoci_15_g = NA
SpecificLoci_16_g = NA
SpecificLoci_17_g = NA
SpecificLoci_18_g = NA
SpecificLoci_19_g = NA
SpecificLoci_20_g = NA
SpecificLoci_21_g = NA
SpecificLoci_22_g = NA
SpecificLoci_23_g = NA
SpecificLoci_24_g = NA
SpecificLoci_25_g = NA
SpecificLoci_26_g = NA
SpecificLoci_27_g = NA
SpecificLoci_28_g = NA
SpecificLoci_29_g = NA
SpecificLoci_30_g = NA
SpecificLoci_31_g = NA
SpecificLoci_32_g = NA
SpecificLoci_33_g = NA
SpecificLoci_34_g = NA
SpecificLoci_35_g = NA
SpecificLoci_36_g = NA
SpecificLoci_37_g = NA
SpecificLoci_38_g = NA
SpecificLoci_39_g = NA
SpecificLoci_40_g = NA
SpecificLoci_41_g = NA
SpecificLoci_42_g = NA
SpecificLoci_43_g = NA
SpecificLoci_44_g = NA
SpecificLoci_45_g = NA
SpecificLoci_46_g = NA
SpecificLoci_47_g = NA
SpecificLoci_48_g = NA
SpecificLoci_49_g = NA
SpecificLoci_50_g = NA
SpecificLoci_51_g = NA
SpecificLoci_52_g = NA
SpecificLoci_53_g = NA
SpecificLoci_54_g = NA
SpecificLoci_55_g = NA
SpecificLoci_56_g = NA
SpecificLoci_57_g = NA
SpecificLoci_58_g = NA
SpecificLoci_59_g = NA
SpecificLoci_60_g = NA

for(i in c(1:length(BedFiles_g)) ) {
  if(i == 1)  {SpecificLoci_1_g  = BedFiles_g[i] }
  if(i == 2)  {SpecificLoci_2_g  = BedFiles_g[i] }
  if(i == 3)  {SpecificLoci_3_g  = BedFiles_g[i] }
  if(i == 4)  {SpecificLoci_4_g  = BedFiles_g[i] }
  if(i == 5)  {SpecificLoci_5_g  = BedFiles_g[i] }
  if(i == 6)  {SpecificLoci_6_g  = BedFiles_g[i] }
  if(i == 7)  {SpecificLoci_7_g  = BedFiles_g[i] }
  if(i == 8)  {SpecificLoci_8_g  = BedFiles_g[i] }
  if(i == 9)  {SpecificLoci_9_g  = BedFiles_g[i] }
  if(i == 10) {SpecificLoci_10_g = BedFiles_g[i] }
  if(i == 11) {SpecificLoci_11_g = BedFiles_g[i] }
  if(i == 12) {SpecificLoci_12_g = BedFiles_g[i] }
  if(i == 13) {SpecificLoci_13_g = BedFiles_g[i] }
  if(i == 14) {SpecificLoci_14_g = BedFiles_g[i] }
  if(i == 15) {SpecificLoci_15_g = BedFiles_g[i] }
  if(i == 16) {SpecificLoci_16_g = BedFiles_g[i] }
  if(i == 17) {SpecificLoci_17_g = BedFiles_g[i] }
  if(i == 18) {SpecificLoci_18_g = BedFiles_g[i] }
  if(i == 19) {SpecificLoci_19_g = BedFiles_g[i] }
  if(i == 20) {SpecificLoci_20_g = BedFiles_g[i] }
  if(i == 21) {SpecificLoci_21_g = BedFiles_g[i] }
  if(i == 22) {SpecificLoci_22_g = BedFiles_g[i] }
  if(i == 23) {SpecificLoci_23_g = BedFiles_g[i] }
  if(i == 24) {SpecificLoci_24_g = BedFiles_g[i] }
  if(i == 25) {SpecificLoci_25_g = BedFiles_g[i] }
  if(i == 26) {SpecificLoci_26_g = BedFiles_g[i] }
  if(i == 27) {SpecificLoci_27_g = BedFiles_g[i] }
  if(i == 28) {SpecificLoci_28_g = BedFiles_g[i] }
  if(i == 29) {SpecificLoci_29_g = BedFiles_g[i] }
  if(i == 30) {SpecificLoci_30_g = BedFiles_g[i] }
  if(i == 31) {SpecificLoci_31_g = BedFiles_g[i] }
  if(i == 32) {SpecificLoci_32_g = BedFiles_g[i] }
  if(i == 33) {SpecificLoci_33_g = BedFiles_g[i] }
  if(i == 34) {SpecificLoci_34_g = BedFiles_g[i] }
  if(i == 35) {SpecificLoci_35_g = BedFiles_g[i] }
  if(i == 36) {SpecificLoci_36_g = BedFiles_g[i] }
  if(i == 37) {SpecificLoci_37_g = BedFiles_g[i] }
  if(i == 38) {SpecificLoci_38_g = BedFiles_g[i] }
  if(i == 39) {SpecificLoci_39_g = BedFiles_g[i] }
  if(i == 40) {SpecificLoci_40_g = BedFiles_g[i] }
  if(i == 41) {SpecificLoci_41_g = BedFiles_g[i] }
  if(i == 42) {SpecificLoci_42_g = BedFiles_g[i] }
  if(i == 43) {SpecificLoci_43_g = BedFiles_g[i] }
  if(i == 44) {SpecificLoci_44_g = BedFiles_g[i] }
  if(i == 45) {SpecificLoci_45_g = BedFiles_g[i] }
  if(i == 46) {SpecificLoci_46_g = BedFiles_g[i] }
  if(i == 47) {SpecificLoci_47_g = BedFiles_g[i] }
  if(i == 48) {SpecificLoci_48_g = BedFiles_g[i] }
  if(i == 49) {SpecificLoci_49_g = BedFiles_g[i] }
  if(i == 50) {SpecificLoci_50_g = BedFiles_g[i] }
  if(i == 51) {SpecificLoci_51_g = BedFiles_g[i] }
  if(i == 52) {SpecificLoci_52_g = BedFiles_g[i] }
  if(i == 53) {SpecificLoci_53_g = BedFiles_g[i] }
  if(i == 54) {SpecificLoci_54_g = BedFiles_g[i] }
  if(i == 55) {SpecificLoci_55_g = BedFiles_g[i] }
  if(i == 56) {SpecificLoci_56_g = BedFiles_g[i] }
  if(i == 57) {SpecificLoci_57_g = BedFiles_g[i] }
  if(i == 58) {SpecificLoci_58_g = BedFiles_g[i] }
  if(i == 59) {SpecificLoci_59_g = BedFiles_g[i] }
  if(i == 60) {SpecificLoci_60_g = BedFiles_g[i] }
}
##############################################################################################################################################################################################





##############################################################################################################################################################################################
SpecificLoci_1.obj_g  = NA
SpecificLoci_2.obj_g  = NA
SpecificLoci_3.obj_g  = NA
SpecificLoci_4.obj_g  = NA
SpecificLoci_5.obj_g  = NA
SpecificLoci_6.obj_g  = NA
SpecificLoci_7.obj_g  = NA
SpecificLoci_8.obj_g  = NA
SpecificLoci_9.obj_g  = NA
SpecificLoci_10.obj_g = NA
SpecificLoci_11.obj_g = NA
SpecificLoci_12.obj_g = NA
SpecificLoci_13.obj_g = NA
SpecificLoci_14.obj_g = NA
SpecificLoci_15.obj_g = NA
SpecificLoci_16.obj_g = NA
SpecificLoci_17.obj_g = NA
SpecificLoci_18.obj_g = NA
SpecificLoci_19.obj_g = NA
SpecificLoci_20.obj_g = NA
SpecificLoci_21.obj_g = NA
SpecificLoci_22.obj_g = NA
SpecificLoci_23.obj_g = NA
SpecificLoci_24.obj_g = NA
SpecificLoci_25.obj_g = NA
SpecificLoci_26.obj_g = NA
SpecificLoci_27.obj_g = NA
SpecificLoci_28.obj_g = NA
SpecificLoci_29.obj_g = NA
SpecificLoci_30.obj_g = NA
SpecificLoci_31.obj_g = NA
SpecificLoci_32.obj_g = NA
SpecificLoci_33.obj_g = NA
SpecificLoci_34.obj_g = NA
SpecificLoci_35.obj_g = NA
SpecificLoci_36.obj_g = NA
SpecificLoci_37.obj_g = NA
SpecificLoci_38.obj_g = NA
SpecificLoci_39.obj_g = NA
SpecificLoci_40.obj_g = NA
SpecificLoci_41.obj_g = NA
SpecificLoci_42.obj_g = NA
SpecificLoci_43.obj_g = NA
SpecificLoci_44.obj_g = NA
SpecificLoci_45.obj_g = NA
SpecificLoci_46.obj_g = NA
SpecificLoci_47.obj_g = NA
SpecificLoci_48.obj_g = NA
SpecificLoci_49.obj_g = NA
SpecificLoci_50.obj_g = NA
SpecificLoci_51.obj_g = NA
SpecificLoci_52.obj_g = NA
SpecificLoci_53.obj_g = NA
SpecificLoci_54.obj_g = NA
SpecificLoci_55.obj_g = NA
SpecificLoci_56.obj_g = NA
SpecificLoci_57.obj_g = NA
SpecificLoci_58.obj_g = NA
SpecificLoci_59.obj_g = NA
SpecificLoci_60.obj_g = NA

my_up_flank   = 2000
my_down_flank = 500
my_flank      = 2000

if( ! is.na(SpecificLoci_1_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_1_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_1.obj_g = genomation::readTranscriptFeatures( SpecificLoci_1_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_1.obj_g = genomation::readFeatureFlank( SpecificLoci_1_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_2_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_2_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_2.obj_g = genomation::readTranscriptFeatures( SpecificLoci_2_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_2.obj_g = genomation::readFeatureFlank( SpecificLoci_2_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_3_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_3_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_3.obj_g = genomation::readTranscriptFeatures( SpecificLoci_3_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_3.obj_g = genomation::readFeatureFlank( SpecificLoci_3_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_4_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_4_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_4.obj_g = genomation::readTranscriptFeatures( SpecificLoci_4_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_4.obj_g = genomation::readFeatureFlank( SpecificLoci_4_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_5_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_5_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_5.obj_g = genomation::readTranscriptFeatures( SpecificLoci_5_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_5.obj_g = genomation::readFeatureFlank( SpecificLoci_5_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_6_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_6_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_6.obj_g = genomation::readTranscriptFeatures( SpecificLoci_6_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_6.obj_g = genomation::readFeatureFlank( SpecificLoci_6_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_7_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_7_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_7.obj_g = genomation::readTranscriptFeatures( SpecificLoci_7_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_7.obj_g = genomation::readFeatureFlank( SpecificLoci_7_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_8_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_8_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_8.obj_g = genomation::readTranscriptFeatures( SpecificLoci_8_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_8.obj_g = genomation::readFeatureFlank( SpecificLoci_8_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_9_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_9_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_9.obj_g = genomation::readTranscriptFeatures( SpecificLoci_9_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_9.obj_g = genomation::readFeatureFlank( SpecificLoci_9_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_10_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_10_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_10.obj_g = genomation::readTranscriptFeatures( SpecificLoci_10_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_10.obj_g = genomation::readFeatureFlank( SpecificLoci_10_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_11_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_11_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_11.obj_g = genomation::readTranscriptFeatures( SpecificLoci_11_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_11.obj_g = genomation::readFeatureFlank( SpecificLoci_11_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_12_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_12_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_12.obj_g = genomation::readTranscriptFeatures( SpecificLoci_12_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_12.obj_g = genomation::readFeatureFlank( SpecificLoci_12_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_13_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_13_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_13.obj_g = genomation::readTranscriptFeatures( SpecificLoci_13_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_13.obj_g = genomation::readFeatureFlank( SpecificLoci_13_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_14_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_14_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_14.obj_g = genomation::readTranscriptFeatures( SpecificLoci_14_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_14.obj_g = genomation::readFeatureFlank( SpecificLoci_14_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_15_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_15_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_15.obj_g = genomation::readTranscriptFeatures( SpecificLoci_15_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_15.obj_g = genomation::readFeatureFlank( SpecificLoci_15_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_16_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_16_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_16.obj_g = genomation::readTranscriptFeatures( SpecificLoci_16_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_16.obj_g = genomation::readFeatureFlank( SpecificLoci_16_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_17_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_17_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_17.obj_g = genomation::readTranscriptFeatures( SpecificLoci_17_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_17.obj_g = genomation::readFeatureFlank( SpecificLoci_17_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_18_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_18_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_18.obj_g = genomation::readTranscriptFeatures( SpecificLoci_18_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_18.obj_g = genomation::readFeatureFlank( SpecificLoci_18_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_19_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_19_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_19.obj_g = genomation::readTranscriptFeatures( SpecificLoci_19_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_19.obj_g = genomation::readFeatureFlank( SpecificLoci_19_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_20_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_20_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_20.obj_g = genomation::readTranscriptFeatures( SpecificLoci_20_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_20.obj_g = genomation::readFeatureFlank( SpecificLoci_20_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }  
}

if( ! is.na(SpecificLoci_21_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_21_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_21.obj_g = genomation::readTranscriptFeatures( SpecificLoci_21_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_21.obj_g = genomation::readFeatureFlank( SpecificLoci_21_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_22_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_22_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_22.obj_g = genomation::readTranscriptFeatures( SpecificLoci_22_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_22.obj_g = genomation::readFeatureFlank( SpecificLoci_22_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_23_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_23_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_23.obj_g = genomation::readTranscriptFeatures( SpecificLoci_23_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_23.obj_g = genomation::readFeatureFlank( SpecificLoci_23_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_24_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_24_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_24.obj_g = genomation::readTranscriptFeatures( SpecificLoci_24_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_24.obj_g = genomation::readFeatureFlank( SpecificLoci_24_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_25_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_25_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_25.obj_g = genomation::readTranscriptFeatures( SpecificLoci_25_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_25.obj_g = genomation::readFeatureFlank( SpecificLoci_25_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_26_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_26_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_26.obj_g = genomation::readTranscriptFeatures( SpecificLoci_26_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_26.obj_g = genomation::readFeatureFlank( SpecificLoci_26_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_27_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_27_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_27.obj_g = genomation::readTranscriptFeatures( SpecificLoci_27_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_27.obj_g = genomation::readFeatureFlank( SpecificLoci_27_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_28_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_28_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_28.obj_g = genomation::readTranscriptFeatures( SpecificLoci_28_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_28.obj_g = genomation::readFeatureFlank( SpecificLoci_28_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_29_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_29_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_29.obj_g = genomation::readTranscriptFeatures( SpecificLoci_29_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_29.obj_g = genomation::readFeatureFlank( SpecificLoci_29_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_30_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_30_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_30.obj_g = genomation::readTranscriptFeatures( SpecificLoci_30_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_30.obj_g = genomation::readFeatureFlank( SpecificLoci_30_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }       
}

if( ! is.na(SpecificLoci_31_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_31_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_31.obj_g = genomation::readTranscriptFeatures( SpecificLoci_31_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_31.obj_g = genomation::readFeatureFlank( SpecificLoci_31_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_32_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_32_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_32.obj_g = genomation::readTranscriptFeatures( SpecificLoci_32_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_32.obj_g = genomation::readFeatureFlank( SpecificLoci_32_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_33_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_33_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_33.obj_g = genomation::readTranscriptFeatures( SpecificLoci_33_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_33.obj_g = genomation::readFeatureFlank( SpecificLoci_33_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_34_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_34_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_34.obj_g = genomation::readTranscriptFeatures( SpecificLoci_34_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_34.obj_g = genomation::readFeatureFlank( SpecificLoci_34_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_35_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_35_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_35.obj_g = genomation::readTranscriptFeatures( SpecificLoci_35_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_35.obj_g = genomation::readFeatureFlank( SpecificLoci_35_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_36_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_36_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_36.obj_g = genomation::readTranscriptFeatures( SpecificLoci_36_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_36.obj_g = genomation::readFeatureFlank( SpecificLoci_36_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_37_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_37_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_37.obj_g = genomation::readTranscriptFeatures( SpecificLoci_37_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_37.obj_g = genomation::readFeatureFlank( SpecificLoci_37_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_38_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_38_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_38.obj_g = genomation::readTranscriptFeatures( SpecificLoci_38_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_38.obj_g = genomation::readFeatureFlank( SpecificLoci_38_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_39_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_39_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_39.obj_g = genomation::readTranscriptFeatures( SpecificLoci_39_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_39.obj_g = genomation::readFeatureFlank( SpecificLoci_39_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_40_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_40_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_40.obj_g = genomation::readTranscriptFeatures( SpecificLoci_40_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_40.obj_g = genomation::readFeatureFlank( SpecificLoci_40_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_41_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_41_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_41.obj_g = genomation::readTranscriptFeatures( SpecificLoci_41_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_41.obj_g = genomation::readFeatureFlank( SpecificLoci_41_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_42_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_42_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_42.obj_g = genomation::readTranscriptFeatures( SpecificLoci_42_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_42.obj_g = genomation::readFeatureFlank( SpecificLoci_42_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_43_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_43_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_43.obj_g = genomation::readTranscriptFeatures( SpecificLoci_43_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_43.obj_g = genomation::readFeatureFlank( SpecificLoci_43_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_44_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_44_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_44.obj_g = genomation::readTranscriptFeatures( SpecificLoci_44_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_44.obj_g = genomation::readFeatureFlank( SpecificLoci_44_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_45_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_45_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_45.obj_g = genomation::readTranscriptFeatures( SpecificLoci_45_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_45.obj_g = genomation::readFeatureFlank( SpecificLoci_45_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_46_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_46_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_46.obj_g = genomation::readTranscriptFeatures( SpecificLoci_46_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_46.obj_g = genomation::readFeatureFlank( SpecificLoci_46_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_47_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_47_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_47.obj_g = genomation::readTranscriptFeatures( SpecificLoci_47_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_47.obj_g = genomation::readFeatureFlank( SpecificLoci_47_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_48_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_48_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_48.obj_g = genomation::readTranscriptFeatures( SpecificLoci_48_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_48.obj_g = genomation::readFeatureFlank( SpecificLoci_48_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_49_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_49_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_49.obj_g = genomation::readTranscriptFeatures( SpecificLoci_49_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_49.obj_g = genomation::readFeatureFlank( SpecificLoci_49_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_50_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_50_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_50.obj_g = genomation::readTranscriptFeatures( SpecificLoci_50_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_50.obj_g = genomation::readFeatureFlank( SpecificLoci_50_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_51_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_51_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_51.obj_g = genomation::readTranscriptFeatures( SpecificLoci_51_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_51.obj_g = genomation::readFeatureFlank( SpecificLoci_51_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_52_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_52_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_52.obj_g = genomation::readTranscriptFeatures( SpecificLoci_52_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_52.obj_g = genomation::readFeatureFlank( SpecificLoci_52_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_53_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_53_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_53.obj_g = genomation::readTranscriptFeatures( SpecificLoci_53_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_53.obj_g = genomation::readFeatureFlank( SpecificLoci_53_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_54_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_54_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_54.obj_g = genomation::readTranscriptFeatures( SpecificLoci_54_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_54.obj_g = genomation::readFeatureFlank( SpecificLoci_54_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_55_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_55_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_55.obj_g = genomation::readTranscriptFeatures( SpecificLoci_55_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_55.obj_g = genomation::readFeatureFlank( SpecificLoci_55_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_56_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_56_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_56.obj_g = genomation::readTranscriptFeatures( SpecificLoci_56_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_56.obj_g = genomation::readFeatureFlank( SpecificLoci_56_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_57_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_57_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_57.obj_g = genomation::readTranscriptFeatures( SpecificLoci_57_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_57.obj_g = genomation::readFeatureFlank( SpecificLoci_57_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_58_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_58_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_58.obj_g = genomation::readTranscriptFeatures( SpecificLoci_58_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_58.obj_g = genomation::readFeatureFlank( SpecificLoci_58_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_59_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_59_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_59.obj_g = genomation::readTranscriptFeatures( SpecificLoci_59_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_59.obj_g = genomation::readFeatureFlank( SpecificLoci_59_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(SpecificLoci_60_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=SpecificLoci_60_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            SpecificLoci_60.obj_g = genomation::readTranscriptFeatures( SpecificLoci_60_g, remove.unusual=FALSE, up.flank=my_up_flank,  down.flank=my_down_flank,  unique.prom=TRUE )    
    }else{
            SpecificLoci_60.obj_g = genomation::readFeatureFlank( SpecificLoci_60_g, flank=my_flank, feature.flank.name=c("targets",  "shores") )         
    }     
}   
##############################################################################################################################################################################################





