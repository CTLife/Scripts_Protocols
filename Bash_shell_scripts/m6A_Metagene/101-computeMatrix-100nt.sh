input="4-bigwig/minus-Strand"
out="101-Matrix-100nt/minus-Strand"
mkdir  -p  $out/DRACH

computeMatrix scale-regions    --scoreFileName  $input/HEK293.bw $input/Hela.bw $input/HepG2.bw     --regionsFileName  100-classified-regions/100nt/3UTR.bed     \
   --outFileName $out/3UTR.gz  --outFileNameMatrix $out/3UTR.Matrix   --outFileSortedRegions $out/3UTR.SortedRegions    \
   --regionBodyLength 1    --upstream 0   --downstream 0   --binSize 1      --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0     -p 4  --averageTypeBins sum     --skipZeros   



computeMatrix scale-regions    --scoreFileName  $input/HEK293.bw $input/Hela.bw $input/HepG2.bw     --regionsFileName  100-classified-regions/100nt/5UTR.bed     \
   --outFileName $out/5UTR.gz  --outFileNameMatrix $out/5UTR.Matrix   --outFileSortedRegions $out/5UTR.SortedRegions    \
   --regionBodyLength 1    --upstream 0   --downstream 0   --binSize 1      --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0     -p 4  --averageTypeBins sum     --skipZeros   



computeMatrix scale-regions    --scoreFileName  $input/HEK293.bw $input/Hela.bw $input/HepG2.bw     --regionsFileName  100-classified-regions/100nt/CDS.bed     \
   --outFileName $out/CDS.gz  --outFileNameMatrix $out/CDS.Matrix   --outFileSortedRegions $out/CDS.SortedRegions    \
   --regionBodyLength 1    --upstream 0   --downstream 0   --binSize 1      --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0     -p 4  --averageTypeBins sum     --skipZeros   









input="4-bigwig/plus-Strand"
out="101-Matrix-100nt/plus-Strand"
mkdir  -p  $out/DRACH

computeMatrix scale-regions    --scoreFileName  $input/HEK293.bw $input/Hela.bw $input/HepG2.bw     --regionsFileName  100-classified-regions/100nt/3UTR.bed     \
   --outFileName $out/3UTR.gz  --outFileNameMatrix $out/3UTR.Matrix   --outFileSortedRegions $out/3UTR.SortedRegions    \
   --regionBodyLength 1    --upstream 0   --downstream 0   --binSize 1      --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0     -p 4  --averageTypeBins sum     --skipZeros   



computeMatrix scale-regions    --scoreFileName  $input/HEK293.bw $input/Hela.bw $input/HepG2.bw     --regionsFileName  100-classified-regions/100nt/5UTR.bed     \
   --outFileName $out/5UTR.gz  --outFileNameMatrix $out/5UTR.Matrix   --outFileSortedRegions $out/5UTR.SortedRegions    \
   --regionBodyLength 1    --upstream 0   --downstream 0   --binSize 1      --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0     -p 4  --averageTypeBins sum     --skipZeros   



computeMatrix scale-regions    --scoreFileName  $input/HEK293.bw $input/Hela.bw $input/HepG2.bw     --regionsFileName  100-classified-regions/100nt/CDS.bed     \
   --outFileName $out/CDS.gz  --outFileNameMatrix $out/CDS.Matrix   --outFileSortedRegions $out/CDS.SortedRegions    \
   --regionBodyLength 1    --upstream 0   --downstream 0   --binSize 1      --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0     -p 4  --averageTypeBins sum     --skipZeros   










