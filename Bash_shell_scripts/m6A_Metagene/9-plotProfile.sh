input="7-mergedMatrix"
output="9-plotProfile"
mkdir  -p  $output


plotProfile   --matrixFile $input/Common3.gz   --outFileName $output/Common3.pdf   \
              --outFileSortedRegions $output/Common3.SortedRegions.txt   \
              --outFileNameData  $output/Common3.tab.txt   --yMin 0  --yMax 0.12            \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                
 


plotProfile   --matrixFile $input/FP.gz   --outFileName $output/FP.pdf   \
              --outFileSortedRegions $output/FP.SortedRegions.txt   \
              --outFileNameData  $output/FP.tab.txt   --yMin 0  --yMax 0.12            \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                
 

plotProfile   --matrixFile $input/Specific.gz   --outFileName $output/Specific.pdf   \
              --outFileSortedRegions $output/Specific.SortedRegions.txt   \
              --outFileNameData  $output/Specific.tab.txt   --yMin 0  --yMax 0.12            \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                
 

plotProfile   --matrixFile $input/TP.gz   --outFileName $output/TP.pdf   \
              --outFileSortedRegions $output/TP.SortedRegions.txt   \
              --outFileNameData  $output/TP.tab.txt   --yMin 0  --yMax 0.12            \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                
 

