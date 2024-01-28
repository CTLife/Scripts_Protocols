#!/usr/bin/env  perl
use  strict;
use  warnings;
use  v5.12;
## Perl5 version >= 5.12
###################################################################################################################################################################################################





###################################################################################################################################################################################################
my $input_g  = '';  ## such as "BG_RPM/Input_minus"   
my $output_g = '';  ## such as "1_apaiq_identification_test"

{
## Help Infromation
my $HELP = '
        ------------------------------------------------------------------------------------------------------------------------------------------------------
        ------------------------------------------------------------------------------------------------------------------------------------------------------     
        Usage:
               perl 1_apaiq_identification_test.pl    [-version]    [-help]    [-in inputFile]    [-out outDir]      
        For instance:
               nohup time  perl 1_apaiq_identification_test.pl   -in BG_RPM/Input_minus   -out 1_apaiq_identification_test     > 1_apaiq_identification_test.runLog.txt  2>&1    &

        ----------------------------------------------------------------------------------------------------------
        Optional arguments:
        -version        Show version number of this program and exit.

        -help           Show this help message and exit.

        Required arguments:
        -in inputDir        "inputDir" is the name of input path.  (no default)

        -out outDir         "outDir" is the name of output path that contains your running results of this step.  (no default)
        -----------------------------------------------------------------------------------------------------------

        ------------------------------------------------------------------------------------------------------------------------------------------------------
        ------------------------------------------------------------------------------------------------------------------------------------------------------
';

## Version Infromation
my $version = "    2024-02-01.";

## Keys and Values
if ($#ARGV   == -1)   { say  "\n$HELP\n";  exit 0;  }       ## when there are no any command argumants.
if ($#ARGV%2 ==  0)   { @ARGV = (@ARGV, "-help") ;  }       ## when the number of command argumants is odd.
my %args = @ARGV;

## Initialize  Variables
$input_g  = 'BG_RPM/Input_minus';             ## This is only an initialization value or suggesting value, not default value.
$output_g = '1_apaiq_identification_test';    ## This is only an initialization value or suggesting value, not default value.

## Available Arguments 
my $available = "   -version    -help   -in   -out    ";
my $boole = 0;
while( my ($key, $value) = each %args ) {
    if ( ($key =~ m/^\-/) and ($available !~ m/\s$key\s/) ) {say    "\n\tCann't recognize $key";  $boole = 1; }
}
if($boole == 1) {
    say  "\tThe Command Line Arguments are wrong!";
    say  "\tPlease see help message by using 'perl 1_apaiq_identification_test.pl  -help' \n";
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
say   "\n##################################################################################################";
say   "Running ......";
sub myMakeDir  {
    my $path = $_[0];
    if ( !( -e $path) )  { system("mkdir  -p  $path"); }
    if ( !( -e $path) )  { mkdir $path  ||  die;       }
}

&myMakeDir($output_g);
opendir(my $files_g, $input_g)  ||  die;
my @inputFiles_g = readdir($files_g);
###################################################################################################################################################################################################





###################################################################################################################################################################################################
say   "\n\n##################################################################################################";


my $genome_fa="/media/yp/1one/MyProgramFiles_02302023/14_Genomes/Gencode/Human_GRCh38.p13/GRCh38.primary_assembly.genome.fa";
my $model="APAIQ_hg38_mm10_mm39/model/snu398_model.ckpt";
my $polyADB="APAIQ_hg38_mm10_mm39/pas_db/polyADB3_gencode.pAs.bed";


my $NUM = 0;

for ( my $j=0; $j<= $#inputFiles_g; $j++ ) {        
        my $file = "$input_g/$inputFiles_g[$j]";
        next unless $file =~ m/_rev.bg$/;
        $NUM = $NUM + 1;
        next unless $NUM < 3;

        my $bg_minus = $file;
        my $bg_plus  = $file;
        $bg_plus =~ s/_rev.bg$/_fwd.bg/ or die;
        $bg_plus =~ s/Input_minus/Input_plus/ or die;
        
        my $name=$inputFiles_g[$j];
        $name =~ s/_rev.bg$// or die;
        say("$bg_minus , $bg_plus");
        
        ## ISR
        ##system( "apaiq   --input_plus=$bg_minus  --input_minus=$bg_plus   --out_dir=$output_g/  --fa_file=$genome_fa   --name=$name   --DB_file $polyADB   --model $model   --t 1   --RNASeqRCThreshold  0.05     >  $output_g/$name.runLog 2>&1    "); 
          system( "apaiq   --input_plus=$bg_plus   --input_minus=$bg_minus  --out_dir=$output_g/  --fa_file=$genome_fa   --name=$name   --DB_file $polyADB   --model $model   --t 1   --RNASeqRCThreshold  0.05     >  $output_g/$name.runLog 2>&1    "); 

}



print("\n\n#########################################\n");
###################################################################################################################################################################################################






###################################################################################################################################################################################################
say   "\n\n##################################################################################################";
say   "\tJob Done! Cheers! \n";





## END




