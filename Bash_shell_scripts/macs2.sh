
outdir="MACS2"
genome_size_g="hs"







A1_fwd=R22313349-library30_fwd
A1_rev=R22313349-library30_rev
A2_fwd=R22313350-library31_fwd
A2_rev=R22313350-library31_rev
A3_fwd=R22317158-library38-p-293T_fwd
A3_rev=R22317158-library38-p-293T_rev
A4_fwd=R22318247-library41-PFA-IgG_fwd
A4_rev=R22318247-library41-PFA-IgG_rev
A5_fwd=R22318248-library42-L-m6A_fwd
A5_rev=R22318248-library42-L-m6A_rev
A6_fwd=R22318249-library43-L-IgG_fwd
A6_rev=R22318249-library43-L-IgG_rev


#################
chip1=$A1_fwd
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam      --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $chip1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    

chip1=$A1_rev
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam      --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $chip1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    





#################
chip1=$A2_fwd
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam      --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $chip1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    

chip1=$A2_rev
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam      --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $chip1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    




#################
chip1=$A3_fwd
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam      --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $chip1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    

chip1=$A3_rev
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam      --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $chip1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    





#################
chip1=$A4_fwd
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam      --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $chip1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    

chip1=$A4_rev
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam      --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $chip1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    





#################
chip1=$A5_fwd
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam      --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $chip1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    

chip1=$A5_rev
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam      --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $chip1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    





#################
chip1=$A6_fwd
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam      --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $chip1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    

chip1=$A6_rev
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam      --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $chip1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    

 













input1=$A4_fwd
chip1=$A3_fwd
name1=$A3_fwd.withInpput
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam    --control  splitBAM/$input1.bam   --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $name1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    



input1=$A4_rev
chip1=$A3_rev
name1=$A3_rev.withInpput
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam    --control  splitBAM/$input1.bam   --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $name1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    







input1=$A6_fwd
chip1=$A5_fwd
name1=$A5_fwd.withInpput
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam    --control  splitBAM/$input1.bam   --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $name1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    



input1=$A6_rev
chip1=$A5_rev
name1=$A5_rev.withInpput
outputDir="$outdir/$chip1"
mkdir -p   $outputDir
macs2  callpeak   --treatment splitBAM/$chip1.bam    --control  splitBAM/$input1.bam   --format BAM   --gsize $genome_size_g   --keep-dup all      \
                  --outdir $outputDir    --name $name1   --qvalue 0.05     --nomodel   --extsize 100          > $outputDir/$chip1.runLog.txt    2>&1                    





















