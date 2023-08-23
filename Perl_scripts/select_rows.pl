#!/usr/bin/env perl
use strict;
use warnings;


########## Keys and Values ##########
my %args = @ARGV;



########## Set Defaults ##########
my $file1_g      = '2_moreGeneName/1_BA9.hyper.genes_symbol.txt';  ## small list
my $file2_g      = 'GeneExpression/1_BA9.bed';  # full list
my $file3_g      = '3_selest/1_BA9.hyper';   ## output folder 

########## Get Arguments ##########
if ( exists $args{'-file1'}   )    { $file1_g  = $args{'-file1'};   }
if ( exists $args{'-file2'}   )    { $file2_g  = $args{'-file2'};   }
if ( exists $args{'-file3'}   )    { $file3_g  = $args{'-file3'};   }

########### Conditions #############
$file1_g      =~ m/^\S+$/ or die;
$file2_g      =~ m/^\S+$/ or die;
$file3_g      =~ m/^\S+$/ or die;


######### Example ###########
# perl   select_rows.pl   -file1  2_moreGeneName/1_BA9.hyper.genes_symbol.txt      -file2   GeneExpression/1_BA9.bed      -file3  3_selest/1_BA9.hyper



my $outpath = $file3_g ;
system( "mkdir -p  $outpath " );

open(INPUT1,    "<",   "$file1_g")     or die "$!"; 
open(INPUT2,    "<",   "$file2_g")     or die "$!"; 
my @lines1 = <INPUT1>; 
my @lines2 = <INPUT2>; 

open(FH1,   ">",   "$outpath/find.txt")        or die "$!";   
open(FH2,   ">",   "$outpath/no_find.txt")     or die "$!";   

print    FH1   $lines2[0] ;

for (my $i=0; $i<=$#lines1; $i++) {        
    my $temp1 = $lines1[$i];
    $temp1 =~ m/^(\S+)\s+(\S+)\s+/ or die;
    my $phenotype1 = $2;

    my $bool1 = 0;
    for (my $j=1; $j<=$#lines2; $j++) {
        my $temp2 = $lines2[$j];
        $temp2 =~ m/^(\S+)\s+(\d+)\s+(\d+)\s+(\S+)\s+/ or die "\n$temp2\n\n";
        my $phenotype2 = $4; 
        if($phenotype2 =~ m/$phenotype1\./)  {print    FH1   $temp2 ;     $bool1=1;   last; }
    }

    if( ($bool1 == 0) ) {print    FH2   $temp1;}
 
}




