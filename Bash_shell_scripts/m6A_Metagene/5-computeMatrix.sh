input="4-bigwig/minus-Strand"
out="5-Matrix/minus-Strand"
mkdir  -p  $out/DRACH

region_g="/home/yp/LuluHu_m6A-SAC-seq/Downstream_v1/byMutation/1-Each/PublicData_hg38/GTF_Gencode27/minusStrand.gtf";

computeMatrix scale-regions    --scoreFileName  $input/HEK293.bw     --regionsFileName  $region_g     \
   --outFileName $out/HEK293.gz  --outFileNameMatrix $out/HEK293.Matrix   --outFileSortedRegions $out/HEK293.SortedRegions    \
   --regionBodyLength 1000    --upstream 400   --downstream 800   --binSize 20   --metagene   \
   --transcriptID  transcript   --exonID     CDS   --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0   --skipZeros  -p 4


computeMatrix scale-regions    --scoreFileName  $input/Hela.bw     --regionsFileName  $region_g     \
   --outFileName $out/Hela.gz  --outFileNameMatrix $out/Hela.Matrix   --outFileSortedRegions $out/Hela.SortedRegions    \
   --regionBodyLength 1000    --upstream 400   --downstream 800   --binSize 20   --metagene   \
   --transcriptID  transcript   --exonID     CDS   --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0   --skipZeros  -p 4


computeMatrix scale-regions    --scoreFileName  $input/Hela-RIP.bw     --regionsFileName  $region_g     \
   --outFileName $out/Hela-RIP.gz  --outFileNameMatrix $out/Hela-RIP.Matrix   --outFileSortedRegions $out/Hela-RIP.SortedRegions    \
   --regionBodyLength 1000    --upstream 400   --downstream 800   --binSize 20   --metagene   \
   --transcriptID  transcript   --exonID     CDS   --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0   --skipZeros  -p 4



computeMatrix scale-regions    --scoreFileName  $input/HepG2.bw     --regionsFileName  $region_g     \
   --outFileName $out/HepG2.gz  --outFileNameMatrix $out/HepG2.Matrix   --outFileSortedRegions $out/HepG2.SortedRegions    \
   --regionBodyLength 1000    --upstream 400   --downstream 800   --binSize 20   --metagene   \
   --transcriptID  transcript   --exonID     CDS   --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0   --skipZeros  -p 4


computeMatrix scale-regions    --scoreFileName  $input/HelaRIP_lu6_r1.bw     --regionsFileName  $region_g     \
   --outFileName $out/HelaRIP_lu6_r1.gz  --outFileNameMatrix $out/HelaRIP_lu6_r1.Matrix   --outFileSortedRegions $out/HelaRIP_lu6_r1.SortedRegions    \
   --regionBodyLength 1000    --upstream 400   --downstream 800   --binSize 20   --metagene   \
   --transcriptID  transcript   --exonID     CDS   --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0   --skipZeros  -p 4








input="4-bigwig/plus-Strand"
out="5-Matrix/plus-Strand"
mkdir  -p  $out/DRACH
region_g="/home/yp/LuluHu_m6A-SAC-seq/Downstream_v1/byMutation/1-Each/PublicData_hg38/GTF_Gencode27/plusStrand.gtf";

computeMatrix scale-regions    --scoreFileName  $input/HEK293.bw     --regionsFileName  $region_g     \
   --outFileName $out/HEK293.gz  --outFileNameMatrix $out/HEK293.Matrix   --outFileSortedRegions $out/HEK293.SortedRegions    \
   --regionBodyLength 1000    --upstream 400   --downstream 800   --binSize 20   --metagene   \
   --transcriptID  transcript   --exonID     CDS   --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0   --skipZeros  -p 4


computeMatrix scale-regions    --scoreFileName  $input/Hela.bw     --regionsFileName  $region_g     \
   --outFileName $out/Hela.gz  --outFileNameMatrix $out/Hela.Matrix   --outFileSortedRegions $out/Hela.SortedRegions    \
   --regionBodyLength 1000    --upstream 400   --downstream 800   --binSize 20   --metagene   \
   --transcriptID  transcript   --exonID     CDS   --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0   --skipZeros  -p 4


computeMatrix scale-regions    --scoreFileName  $input/Hela-RIP.bw     --regionsFileName  $region_g     \
   --outFileName $out/Hela-RIP.gz  --outFileNameMatrix $out/Hela-RIP.Matrix   --outFileSortedRegions $out/Hela-RIP.SortedRegions    \
   --regionBodyLength 1000    --upstream 400   --downstream 800   --binSize 20   --metagene   \
   --transcriptID  transcript   --exonID     CDS   --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0   --skipZeros  -p 4



computeMatrix scale-regions    --scoreFileName  $input/HepG2.bw     --regionsFileName  $region_g     \
   --outFileName $out/HepG2.gz  --outFileNameMatrix $out/HepG2.Matrix   --outFileSortedRegions $out/HepG2.SortedRegions    \
   --regionBodyLength 1000    --upstream 400   --downstream 800   --binSize 20   --metagene   \
   --transcriptID  transcript   --exonID     CDS   --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0   --skipZeros  -p 4




computeMatrix scale-regions    --scoreFileName  $input/HelaRIP_lu6_r1.bw     --regionsFileName  $region_g     \
   --outFileName $out/HelaRIP_lu6_r1.gz  --outFileNameMatrix $out/HelaRIP_lu6_r1.Matrix   --outFileSortedRegions $out/HelaRIP_lu6_r1.SortedRegions    \
   --regionBodyLength 1000    --upstream 400   --downstream 800   --binSize 20   --metagene   \
   --transcriptID  transcript   --exonID     CDS   --smartLabels  --missingDataAsZero   \
   --startLabel start   --endLabel end   --unscaled5prime  0   --unscaled3prime  0   --skipZeros  -p 4






