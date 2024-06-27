input_g=1_bamCoverage
BEDdir_g=GenomicRegions
output_sub1=6B_computeMatrix-scaleRegions.Multiple/allPeaks
mkdir -p $output_sub1

computeMatrix    scale-regions      --scoreFileName    $input_g/*.bw    \
                                    --regionsFileName  $BEDdir_g/allPeaks.bed     \
                                    --smartLabels   --numberOfProcessors 16    --verbose    \
                                    -o                         $output_sub1/1.gzipped-matrix.gz    \
                                    --outFileNameMatrix        $output_sub1/1.matrix.txt   \
                                    --outFileSortedRegions     $output_sub1/1.Regions.txt    \
                                    --regionBodyLength  800   --upstream 1000   --downstream 1000  \
                                    --binSize 10    --missingDataAsZero     >> $output_sub1/1.runLog.txt                            


 

plotProfile   --matrixFile    $output_sub1/1.gzipped-matrix.gz   \
              --outFileName           $output_sub1/plotProfile.curves.pdf  \
              --outFileSortedRegions  $output_sub1/plotProfile.curves.txt   \
              --outFileNameData       $output_sub1/plotProfile.curves.tab.txt                 \
              --dpi 1200  --averageType mean  --plotType lines     --verbose    

