input="5-Matrix/minus-Strand"
output="6-plotProfile/minus-Strand"
mkdir  -p  $output/DRACH


plotProfile   --matrixFile $input/HEK293.gz   --outFileName $output/HEK293.pdf   \
              --outFileSortedRegions $output/HEK293.SortedRegions.txt   \
              --outFileNameData  $output/HEK293.tab.txt   --yMin 0  --yMax 0.05             \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                

 plotProfile   --matrixFile $input/Hela.gz   --outFileName $output/Hela.pdf   \
              --outFileSortedRegions $output/Hela.SortedRegions.txt   \
              --outFileNameData  $output/Hela.tab.txt   --yMin 0  --yMax 0.05             \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                

plotProfile   --matrixFile $input/Hela-RIP.gz   --outFileName $output/Hela-RIP.pdf   \
              --outFileSortedRegions $output/Hela-RIP.SortedRegions.txt   \
              --outFileNameData  $output/Hela-RIP.tab.txt   --yMin 0  --yMax 0.1             \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                

plotProfile   --matrixFile $input/HepG2.gz   --outFileName $output/HepG2.pdf   \
              --outFileSortedRegions $output/HepG2.SortedRegions.txt   \
              --outFileNameData  $output/HepG2.tab.txt   --yMin 0  --yMax 0.05             \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                


plotProfile   --matrixFile $input/HelaRIP_lu6_r1.gz   --outFileName $output/HelaRIP_lu6_r1.pdf   \
              --outFileSortedRegions $output/HelaRIP_lu6_r1.SortedRegions.txt   \
              --outFileNameData  $output/HelaRIP_lu6_r1.tab.txt   --yMin 0  --yMax 5             \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                








input="5-Matrix/minus-Strand"
output="6-plotProfile/minus-Strand_unlimit"
mkdir  -p  $output/DRACH


plotProfile   --matrixFile $input/HEK293.gz   --outFileName $output/HEK293.pdf   \
              --outFileSortedRegions $output/HEK293.SortedRegions.txt   \
              --outFileNameData  $output/HEK293.tab.txt                \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                

 plotProfile   --matrixFile $input/Hela.gz   --outFileName $output/Hela.pdf   \
              --outFileSortedRegions $output/Hela.SortedRegions.txt   \
              --outFileNameData  $output/Hela.tab.txt                \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                

plotProfile   --matrixFile $input/Hela-RIP.gz   --outFileName $output/Hela-RIP.pdf   \
              --outFileSortedRegions $output/Hela-RIP.SortedRegions.txt   \
              --outFileNameData  $output/Hela-RIP.tab.txt                \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                

plotProfile   --matrixFile $input/HepG2.gz   --outFileName $output/HepG2.pdf   \
              --outFileSortedRegions $output/HepG2.SortedRegions.txt   \
              --outFileNameData  $output/HepG2.tab.txt                \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                

plotProfile   --matrixFile $input/HelaRIP_lu6_r1.gz   --outFileName $output/HelaRIP_lu6_r1.pdf   \
              --outFileSortedRegions $output/HelaRIP_lu6_r1.SortedRegions.txt   \
              --outFileNameData  $output/HelaRIP_lu6_r1.tab.txt                \
              --averageType mean  --plotType lines     --verbose    --startLabel start   --endLabel  end                


