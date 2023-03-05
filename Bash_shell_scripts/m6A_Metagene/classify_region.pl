#!/usr/bin/env  perl
use  strict;
use  warnings;
use  v5.12;
## Perl5 version >= 5.12
## Suffixes of all self-defined global variables must be "_g".
###################################################################################################################################################################################################





###################################################################################################################################################################################################
my $input_g  = '';  ## such as "3UTR.Matrix_K_G7.cdt", global variable.
my $output_g = '';  ## such as "classify_region", global variable.
{
## Help Infromation
my $HELP = '
        ------------------------------------------------------------------------------------------------------------------------------------------------------
        ------------------------------------------------------------------------------------------------------------------------------------------------------
        Usage:
               perl  classify_region.pl    [-version]    [-help]     [-in inputDir]    [-out outDir]
        For instance:
               perl  classify_region.pl    -in 3UTR.Matrix_K_G7.cdt   -out classify_region    > classify_region.runLog.txt

        Optional arguments:
        -version        Show version number of this program and exit.
        -help           Show this help message and exit.

        Required arguments:
        -in inputDir        "inputDir" is the name of your input file.  (no default)
        -out outDir         "outDir" is the name of output path that contains your running results of this script.  (no default)
        ------------------------------------------------------------------------------------------------------------------------------------------------------
        ------------------------------------------------------------------------------------------------------------------------------------------------------
';

## Version Infromation
my $version = "    Version 0.1,  2020-02-01.";

## Keys and Values
if ($#ARGV   == -1)   { say  "\n$HELP\n";  exit 0;  }       ## when there are no any command argumants.
if ($#ARGV%2 ==  0)   { @ARGV = (@ARGV, "-help") ;  }       ## when the number of command argumants is odd.
my %args = @ARGV;

## Initialize  Variables
$input_g  = '3UTR.Matrix_K_G7.cdt';          ## This is only an initialization value or suggesting value, not default value.
$output_g = 'classify_region';               ## This is only an initialization value or suggesting value, not default value.

## Available Arguments
my $available = "   -version    -help    -in   -out  ";
my $boole = 0;
while( my ($key, $value) = each %args ) {
    if ( ($key =~ m/^\-/) and ($available !~ m/\s$key\s/) ) {say    "\n\tCann't recognize $key";  $boole = 1; }
}
if($boole == 1) {
    say  "\tThe Command Line Arguments are wrong!";
    say  "\tPlease see help message by using 'perl  classify_region.pl  -help' \n";
    exit 0;
}

## Get Arguments
if ( exists $args{'-version' }   )     { say  "\n$version\n";    exit 0; }
if ( exists $args{'-help'    }   )     { say  "\n$HELP\n";       exit 0; }
if ( exists $args{'-in'      }   )     { $input_g  = $args{'-in'      }; }else{say   "\n -in     is required.\n";   say  "\n$HELP\n";    exit 0; }
if ( exists $args{'-out'     }   )     { $output_g = $args{'-out'     }; }else{say   "\n -out    is required.\n";   say  "\n$HELP\n";    exit 0; }

## Conditions
$input_g  =~ m/^\S+$/    ||  die   "\n\n$HELP\n\n";
$output_g =~ m/^\S+$/    ||  die   "\n\n$HELP\n\n";

## Print Command Arguments to Standard Output
say  "\n
        ################ Arguments ###############################
                Input   File:  $input_g
                Output  Path:  $output_g
        ##########################################################
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

open(FHin,  "<",   $input_g)   or  die;
###################################################################################################################################################################################################





###################################################################################################################################################################################################
my @lines = <FHin>;

my $cluster1 = 1;
my $num1 = 0;

for(my $i=2; $i<=$#lines; $i++) {
    $lines[$i] =~ m/^(\S+)\:(\d+)\-(\d+)\.\.\.A\.\.\.(\d+)\s/ or die;
    my $chr = $1;	
    my $start = $2;	
    my $end = $3;	
    my $num2 = $4;	
    my $name="$chr:$start-$end";
    
    if(  $num2 < $num1  ) { $cluster1 = $cluster1+1; }
    $num1 = $num2;
    
    open(tempA, ">>", "$output_g/cluster.$cluster1.bed")  or  die;
    print tempA "$chr\t$start\t$end\t$name\n";
}


















#####
