#!/usr/bin/env  perl5
use  strict;
use  warnings;
use  v5.22;
## Perl5 version >= 5.22
## You can create a symbolic link for perl5 by using "sudo  ln  /usr/bin/perl   /usr/bin/perl5" in Ubuntu.
## Suffixes of all self-defined global variables must be "_g".
###################################################################################################################################################################################################





###################################################################################################################################################################################################
my $input_g  = '';  ## such as "3_Methylation/2_ToHuman"
my $output_g = '';  ## such as "4_format_bismarkCov.CpG/2_ToHuman"

{
## Help Infromation
my $HELP = '
        ------------------------------------------------------------------------------------------------------------------------------------------------------
        ------------------------------------------------------------------------------------------------------------------------------------------------------

                If this script works well, you do not need to check the the versions of the softwares or packages whcih are used in this script.
                And you do not need to exactly match the versions of the softwares or packages.
                If some errors or warnings are reported, please check the versions of softwares or packages.

                The versions of softwares or packages are used in this script:  
                        Perl,      5.34.0
                        biscuit,   1.4.0    

        Usage:
               perl  4_format_bismarkCov.CpG.pl    [-version]    [-help]     [-in inputDir]    [-out outDir]
        For instance:
               nohup perl  4_format_bismarkCov.CpG.pl   -in 3_Methylation/2_ToHuman    -out 4_format_bismarkCov.CpG/2_ToHuman    > 4_format_bismarkCov.CpG.runLog.txt 2>&1   &

        ----------------------------------------------------------------------------------------------------------
        Optional arguments:
        -version        Show version number of this program and exit.

        -help           Show this help message and exit.

        Required arguments:
        -in inputDir        "inputDir" is the name of input path that contains your BAM files.  (no default)

        -out outDir         "outDir" is the name of output path that contains your running results of this step.  (no default)
        -----------------------------------------------------------------------------------------------------------

        For more details about this pipeline and other NGS data analysis piplines, please visit https://github.com/CTLife/2ndGS_Pipelines
        ------------------------------------------------------------------------------------------------------------------------------------------------------
        ------------------------------------------------------------------------------------------------------------------------------------------------------
';

## Version Infromation
my $version = "    The 8th Step , version 1.3, 2024-02-20.";

## Keys and Values
if ($#ARGV   == -1)   { say  "\n$HELP\n";  exit 0;  }       ## when there are no any command argumants.
if ($#ARGV%2 ==  0)   { @ARGV = (@ARGV, "-help") ;  }       ## when the number of command argumants is odd.
my %args = @ARGV;

## Initialize  Variables
$input_g  = '3_Methylation/2_ToHuman';               ## This is only an initialization value or suggesting value, not default value.
$output_g = '4_format_bismarkCov.CpG/2_ToHuman';     ## This is only an initialization value or suggesting value, not default value.

## Available Arguments
my $available = "   -version    -help      -in   -out  ";
my $boole = 0;
while( my ($key, $value) = each %args ) {
    if ( ($key =~ m/^\-/) and ($available !~ m/\s$key\s/) ) {say    "\n\tCann't recognize $key";  $boole = 1; }
}
if($boole == 1) {
    say  "\tThe Command Line Arguments are wrong!";
    say  "\tPlease see help message by using 'perl  4_format_bismarkCov.CpG.pl  -help' \n";
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
                Input       Path:  $input_g
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

my $output2_g = "$output_g/QC_Results";
&myMakeDir($output_g);
&myMakeDir($output2_g);

opendir(my $DH_input_g, $input_g)  ||  die;
my @inputFiles_g = readdir($DH_input_g);
my $numCores_g   = 32;
###################################################################################################################################################################################################





###################################################################################################################################################################################################
say   "\n\n\n\n\n\n##################################################################################################";
say   "Checking all the necessary softwares in this step......" ;

sub printVersion  {
    my $software = $_[0];
    system("echo    '##############################################################################'  >> $output2_g/VersionsOfSoftwares.txt   2>&1");
    system("echo    '#########$software'                                                              >> $output2_g/VersionsOfSoftwares.txt   2>&1");
    system("$software                                                                                 >> $output2_g/VersionsOfSoftwares.txt   2>&1");
    system("echo    '\n\n\n\n\n\n'                                                                    >> $output2_g/VersionsOfSoftwares.txt   2>&1");
}

&printVersion("wgbstools    --version");
&printVersion("biscuit        version");
###################################################################################################################################################################################################





###################################################################################################################################################################################################
say   "\n\n\n\n\n\n##################################################################################################";
say   "Detecting BAM files in input folder ......";
my @BAMfiles_g = ();
{
open(seqFiles_FH, ">", "$output2_g/BAM-Files.txt")  or  die; 
for ( my $i=0; $i<=$#inputFiles_g; $i++ ) {     
    next unless $inputFiles_g[$i] =~ m/\.CpG.txt$/;
    next unless $inputFiles_g[$i] !~ m/^[.]/;
    next unless $inputFiles_g[$i] !~ m/[~]$/;
    next unless $inputFiles_g[$i] !~ m/^unpaired/;
    say    "\t......$inputFiles_g[$i]"; 
    $BAMfiles_g[$#BAMfiles_g+1] =  $inputFiles_g[$i];
    say        "\t\t\t\tBAM file:  $inputFiles_g[$i]\n";
    say   seqFiles_FH  "BAM file:  $inputFiles_g[$i]\n";
}

say   seqFiles_FH  "\n\n\n\n\n";  
say   seqFiles_FH  "All BAM files:@BAMfiles_g\n\n\n";
say        "\t\t\t\tAll BAM files:@BAMfiles_g\n\n";
my $num1 = $#BAMfiles_g + 1;
say seqFiles_FH   "\nThere are $num1 BAM files.\n";
say         "\t\t\t\tThere are $num1 BAM files.\n";
}
###################################################################################################################################################################################################





###################################################################################################################################################################################################
say   "\n\n\n\n\n\n##################################################################################################";
say   "Extract mC ......";
for (my $i=0; $i<=$#BAMfiles_g; $i++) {
    say   "\t......$BAMfiles_g[$i]";
    my $temp = $BAMfiles_g[$i]; 
    $temp =~ s/\.CpG\.txt$//  ||  die; 

    my $input   = "$input_g/$temp.CpG.txt";
    my $output1 = "$output_g/$temp.CpG.cov";
    my $output2 = "$output_g/$temp.CpG.bed";

    open(INPUT1,  "<",   $input    )     or  die "$!"; 
    open(OUT1,    ">",   $output1  )     or  die "$!";  
    open(OUT2,    ">",   $output2  )     or  die "$!"; 
    my @lines1 = <INPUT1>; 
           
    for(my $k=1; $k<=$#lines1; $k++ ){
        	$lines1[$k] =~ m/^(\S+)\t(\d+)\t(\d+)\t(\S+)\t(\S+)\t(\S+)\t(\S+)\t(\d+)\t(\d+)\t(\d+)\n$/  or  die  "\n\n\n#$lines1[$k]#\n\n\n";
        	my $chr    = $1; 
        	my $start  = $2; 
        	my $end    = $3; 
        	my $type   = $5; 
        	my $my5mer     = $7;  
        	my $methylated = $9; 
        	my $unmeth     = $10;   

            ($type eq "CG")  or  die;
            my $total = $methylated + $unmeth; 
            my $percentage = 100*$methylated/$total;

        	my $newname = $lines1[$k];
            $newname =~ s/\s+/.../g or die;

            if( ($total > 5) and ($chr =~ m/^chr\d+$/) ){
                 print OUT1 "$chr\t$start\t$end\t$percentage\t$methylated\t$unmeth\n";
                 print OUT2 "$chr\t$start\t$end\t$newname\t$percentage\t.\n";
            }
    }    

}
###################################################################################################################################################################################################




###################################################################################################################################################################################################
say   "\n\n\n\n\n\n##################################################################################################";
say   "\tJob Done! Cheers! \n\n\n\n\n";



  

## END
