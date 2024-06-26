#!/usr/bin/env  perl5
use  strict;
use  warnings;
use  v5.22;
## Perl5 version >= 5.22
## You can create a symbolic link for perl5 by using "sudo  ln  /usr/bin/perl   /usr/bin/perl5" in Ubuntu.
## Suffixes of all self-defined global variables must be "_g".
###################################################################################################################################################################################################





###################################################################################################################################################################################################
my $input_g  = '';  ## such as "5_noDups/2_Bowtie2"
my $BEDdir_g = '';  ## such as "GenomicRegions"
my $output_g = '';  ## such as "deepTools/2B_multiBamSummary-BED"

{
## Help Infromation
my $HELP = '
        ------------------------------------------------------------------------------------------------------------------------------------------------------
        ------------------------------------------------------------------------------------------------------------------------------------------------------
        Usage:
               perl  2B_multiBamSummary-BED.pl    [-version]    [-help]      [-in inputDir]     [-bed bedDir]     [-out outDir]
        For instance:
               nohup perl  2B_multiBamSummary-BED.pl    -in 5_noDups/2_Bowtie2   -bed GenomicRegions    -out deepTools/2B_multiBamSummary-BED    > 2B_multiBamSummary-BED.runLog.txt 2>&1   &

        ----------------------------------------------------------------------------------------------------------
        Optional arguments:
        -version        Show version number of this program and exit.

        -help           Show this help message and exit.

        Required arguments:
        -in inputDir        "inputDir" is the name of input path that contains your BigWig files.  (no default)
        -bed bedDir         "bedDir"   is the name of input path that contains your BED files with genomic regions.  (no default)
        -out outDir         "outDir"   is the name of output path that contains your running results of this step.  (no default)
        -----------------------------------------------------------------------------------------------------------

        For more details please visit https://deeptools.readthedocs.io/en/develop/index.html

        ------------------------------------------------------------------------------------------------------------------------------------------------------
        ------------------------------------------------------------------------------------------------------------------------------------------------------
';

## Version Infromation
my $version = "version 1.1,  2024-06-05.";

## Keys and Values
if ($#ARGV   == -1)   { say  "\n$HELP\n";  exit 0;  }       ## when there are no any command argumants.
if ($#ARGV%2 ==  0)   { @ARGV = (@ARGV, "-help") ;  }       ## when the number of command argumants is odd.
my %args = @ARGV;

## Initialize  Variables
$input_g    = '5_noDups/2_Bowtie2';     ## This is only an initialization value or suggesting value, not default value.
$BEDdir_g   = 'GenomicRegions';         ## This is only an initialization value or suggesting value, not default value.
$output_g   = 'deepTools/2B_multiBamSummary-BED';   ## This is only an initialization value or suggesting value, not default value.

## Available Arguments
my $available = "   -version    -help     -bed   -in   -out  ";
my $boole = 0;
while( my ($key, $value) = each %args ) {
    if ( ($key =~ m/^\-/) and ($available !~ m/\s$key\s/) ) {say    "\n\tCann't recognize $key";  $boole = 1; }
}
if($boole == 1) {
    say  "\tThe Command Line Arguments are wrong!";
    say  "\tPlease see help message by using 'perl  2B_multiBamSummary-BED.pl  -help' \n";
    exit 0;
}

## Get Arguments
if ( exists $args{'-version' }   )     { say  "\n$version\n";    exit 0; }
if ( exists $args{'-help'    }   )     { say  "\n$HELP\n";       exit 0; }
if ( exists $args{'-in'      }   )     { $input_g  = $args{'-in'      }; }else{say   "\n -in     is required.\n";   say  "\n$HELP\n";    exit 0; }
if ( exists $args{'-bed'     }   )     { $BEDdir_g = $args{'-bed'     }; }else{say   "\n -bed    is required.\n";   say  "\n$HELP\n";    exit 0; }
if ( exists $args{'-out'     }   )     { $output_g = $args{'-out'     }; }else{say   "\n -out    is required.\n";   say  "\n$HELP\n";    exit 0; }

## Conditions
$input_g  =~ m/^\S+$/    ||  die   "\n\n$HELP\n\n";
$BEDdir_g =~ m/^\S+$/    ||  die   "\n\n$HELP\n\n";
$output_g =~ m/^\S+$/    ||  die   "\n\n$HELP\n\n";

## Print Command Arguments to Standard Output
say  "\n
        ################ Arguments ###############################
                Input       Path:  $input_g
                Genomic  Regions:  $BEDdir_g
                Output      Path:  $output_g
        ###############################################################
\n";
}
###################################################################################################################################################################################################





###################################################################################################################################################################################################
say    "\n\n\n\n\n\n##################################################################################################";
say    "Running......";

sub myMakeDir  {
    my $path = $_[0];
    if ( !( -e $path) )  { system("mkdir  -p  $path"); }
    if ( !( -e $path) )  { mkdir $path  ||  die; }
}

&myMakeDir($output_g);

opendir(my $DH_input_g, $input_g)   ||  die;
opendir(my $DH_bed_g,   $BEDdir_g)  ||  die;
my @inputFiles_g = readdir($DH_input_g);
my @BED_Files_g  = readdir($DH_bed_g);
###################################################################################################################################################################################################





###################################################################################################################################################################################################
say   "\n\n\n\n\n\n##################################################################################################";
say   "Checking all the necessary softwares in this step......" ;
sub printVersion  {
    my $software = $_[0];
    system("echo    '##############################################################################'  >> $output_g/VersionsOfSoftwares.txt   2>&1");
    system("echo    '#########$software'                                                              >> $output_g/VersionsOfSoftwares.txt   2>&1");
    system("$software                                                                                 >> $output_g/VersionsOfSoftwares.txt   2>&1");
    system("echo    '\n\n\n\n\n\n'                                                                    >> $output_g/VersionsOfSoftwares.txt   2>&1");
}
&printVersion("deeptools --version");
###################################################################################################################################################################################################





###################################################################################################################################################################################################
say   "\n\n\n\n\n\n##################################################################################################";
say   "Detecting BAM files in input folder ......";
my @BigWigfiles_g = ();

{
open(seqFiles_FH, ">", "$output_g/BAM-Files.txt")  or  die; 
for ( my $i=0; $i<=$#inputFiles_g; $i++ ) {     
    next unless $inputFiles_g[$i] =~ m/\.bam$/;
    next unless $inputFiles_g[$i] !~ m/^[.]/;
    next unless $inputFiles_g[$i] !~ m/[~]$/;
    next unless $inputFiles_g[$i] !~ m/^unpaired/;
    say    "\t......$inputFiles_g[$i]"; 
    $BigWigfiles_g[$#BigWigfiles_g+1] =  $inputFiles_g[$i];
    say        "\t\t\t\tBAM file:  $inputFiles_g[$i]\n";
    say   seqFiles_FH  "BAM file:  $inputFiles_g[$i]\n";
}

say   seqFiles_FH  "\n\n\n\n\n";  
say   seqFiles_FH  "All BAM files:@BigWigfiles_g\n\n\n";
say        "\t\t\t\tAll BAM files:@BigWigfiles_g\n\n";

my $num1 = $#BigWigfiles_g + 1;
say seqFiles_FH   "\nThere are $num1 BAM files.\n";
say         "\t\t\t\tThere are $num1 BAM files.\n";
}

my @BigWigfiles_g2 =  @BigWigfiles_g;    
for ( my $i=0; $i<=$#BigWigfiles_g2; $i++ ) { 
   $BigWigfiles_g2[$i] = "$input_g/$BigWigfiles_g2[$i]";   ## add path  
}
###################################################################################################################################################################################################





###################################################################################################################################################################################################
say   "\n\n\n\n\n\n##################################################################################################";
say   "Using multiBamSummary  BED-file, plotCorrelation and plotPCA ......";

for ( my $i=0; $i<=$#BED_Files_g; $i++ ) { 
    next unless $BED_Files_g[$i] =~ m/\.bed$/;
    next unless $BED_Files_g[$i] !~ m/^[.]/;
    next unless $BED_Files_g[$i] !~ m/[~]$/;

    my $output_sub1 = "$output_g/$BED_Files_g[$i]";   
    &myMakeDir($output_sub1);

    system("multiBamSummary   BED-file   --bamfiles @BigWigfiles_g2    --BED $BEDdir_g/$BED_Files_g[$i]   --extendReads   --smartLabels   --numberOfProcessors 16    --verbose    --outRawCounts $output_sub1/$BED_Files_g[$i].RawCounts.txt   --outFileName $output_sub1/$BED_Files_g[$i].RawCounts.npz    >> $output_sub1/$BED_Files_g[$i].runLog.txt   2>&1");                               
    
}
###################################################################################################################################################################################################    





###################################################################################################################################################################################################
say   "\n\n\n\n\n\n##################################################################################################";
say   "\tJob Done! Cheers! \n\n\n\n\n";


 

  
## END







