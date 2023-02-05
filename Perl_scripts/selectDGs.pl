#!/usr/bin/env  perl
use  strict;
use  warnings;
use  v5.12;
## Perl5 version >= 5.12
###################################################################################################################################################################################################





###################################################################################################################################################################################################
my $input_g  = '';  ## such as "6d.rots_results.wilcox.txt"   
my $output_g = '';  ## such as "selectDGs"

{
## Help Infromation
my $HELP = '
        ------------------------------------------------------------------------------------------------------------------------------------------------------
        ------------------------------------------------------------------------------------------------------------------------------------------------------     
        Usage:
               perl selectDGs.pl    [-version]    [-help]    [-in inputFile]    [-out outDir]      
        For instance:
               nohup time  perl selectDGs.pl   -in 6d.rots_results.wilcox.txt   -out selectDGs     > selectDGs.runLog.txt  2>&1    &

        ----------------------------------------------------------------------------------------------------------
        Optional arguments:
        -version        Show version number of this program and exit.

        -help           Show this help message and exit.

        Required arguments:
        -in inputDir        "inputDir" is the name of input file.  (no default)

        -out outDir         "outDir" is the name of output path that contains your running results of this step.  (no default)
        -----------------------------------------------------------------------------------------------------------

        ------------------------------------------------------------------------------------------------------------------------------------------------------
        ------------------------------------------------------------------------------------------------------------------------------------------------------
';

## Version Infromation
my $version = "    2023-02-04.";

## Keys and Values
if ($#ARGV   == -1)   { say  "\n$HELP\n";  exit 0;  }       ## when there are no any command argumants.
if ($#ARGV%2 ==  0)   { @ARGV = (@ARGV, "-help") ;  }       ## when the number of command argumants is odd.
my %args = @ARGV;

## Initialize  Variables
$input_g  = '6d.rots_results.wilcox.txt';     ## This is only an initialization value or suggesting value, not default value.
$output_g = 'selectDGs';      ## This is only an initialization value or suggesting value, not default value.

## Available Arguments 
my $available = "   -version    -help   -in   -out  -f ";
my $boole = 0;
while( my ($key, $value) = each %args ) {
    if ( ($key =~ m/^\-/) and ($available !~ m/\s$key\s/) ) {say    "\n\tCann't recognize $key";  $boole = 1; }
}
if($boole == 1) {
    say  "\tThe Command Line Arguments are wrong!";
    say  "\tPlease see help message by using 'perl selectDGs.pl  -help' \n";
    exit 0;
}

## Get Arguments 
if ( exists $args{'-version' }   )     { say  "\n$version\n";    exit 0; }
if ( exists $args{'-help'    }   )     { say  "\n$HELP\n";       exit 0; }
if ( exists $args{'-in'      }   )     { $input_g  = $args{'-in'      }; }else{say   "\n -in   is required.\n";   say  "\n$HELP\n";    exit 0; }
if ( exists $args{'-out'     }   )     { $output_g = $args{'-out'     }; }else{say   "\n -out  is required.\n";   say  "\n$HELP\n";    exit 0; }

## Conditions
$input_g  =~ m/^\S+$/    ||  die   "\n\n$HELP\n\n";
$output_g =~ m/^\S+$/    ||  die   "\n\n$HELP\n\n";

## Print Command Arguments to Standard Output
say  "\n
        ################ Arguments ###############################
                Input   File:  $input_g
                Output  Path:  $output_g
        ###############################################################
\n";
}
###################################################################################################################################################################################################





###################################################################################################################################################################################################
say   "\n\n\n\n\n\n##################################################################################################";
say   "Running ......";
sub myMakeDir  {
    my $path = $_[0];
    if ( !( -e $path) )  { system("mkdir  -p  $path"); }
    if ( !( -e $path) )  { mkdir $path  ||  die;       }
}

&myMakeDir($output_g);

open(my $DH1, "<",   $input_g )  ||  die;
open(my $DH4a, ">", "$output_g/upGenes.txt"  )  ||  die;
open(my $DH4b, ">", "$output_g/downGenes.txt"  )   ||  die;
open(my $DH5a, ">", "$output_g/upGenes.onlyGene.txt"  )  ||  die;
open(my $DH5b, ">", "$output_g/downGenes.onlyGene.txt"  )   ||  die;

my @lines1 = <$DH1>;
###################################################################################################################################################################################################





###################################################################################################################################################################################################
say   "\n\n\n\n\n\n##################################################################################################";



print $DH4a "Gene\tROTSstatistic\tROTSlogFC\tROTSpvalue\twilcoxon_pvalues\n";
print $DH4b "Gene\tROTSstatistic\tROTSlogFC\tROTSpvalue\twilcoxon_pvalues\n";

for ( my $j=1; $j<=$#lines1; $j++ ) { ##  name  ROTS-statistic    ROTS.logFC  ROTS.p-value    ROTS.FDR    pvalues     FDR
        	$lines1[$j] =~ m/^(\S+)\t(\S+)\t(\S+)\t(\S+)\t(\S+)\t(\S+)\t(\S+)\n$/  or  die  "\n\n\n#$lines1[$j]#\n\n\n";
        	my $name = $1; 
        	my $ROTSstatistic    = $2;     
        	my $ROTSlogFC  = $3;   
        	my $ROTSpvalue = $4;     
        	my $ROTS_FDR   = $5;   
        	my $wilcoxon_pvalues    = $6; 
        	my $wilcoxon_FDR  = $7;        

        	if( ($ROTSlogFC >  0.5)  and  ($ROTSpvalue <  0.05)  and  ($wilcoxon_FDR < 0.05)  ) { print $DH4a  "$name\t$ROTSstatistic\t$ROTSlogFC\t$ROTSpvalue\t$wilcoxon_pvalues\n"; }    
        	if( ($ROTSlogFC < -0.5)  and  ($ROTSpvalue <  0.05)  and  ($wilcoxon_FDR < 0.05)  ) { print $DH4b  "$name\t$ROTSstatistic\t$ROTSlogFC\t$ROTSpvalue\t$wilcoxon_pvalues\n"; }    
        	if( ($ROTSlogFC >  0.5)  and  ($ROTSpvalue <  0.05)  and  ($wilcoxon_FDR < 0.05)  ) { print $DH5a  "$name\n"; }    
        	if( ($ROTSlogFC < -0.5)  and  ($ROTSpvalue <  0.05)  and  ($wilcoxon_FDR < 0.05)  ) { print $DH5b  "$name\n"; }  
}



print("\n\n\n\n\n#########################################\n");
###################################################################################################################################################################################################






###################################################################################################################################################################################################
say   "\n\n\n\n\n\n##################################################################################################";
say   "\tJob Done! Cheers! \n\n\n\n\n";





## END




