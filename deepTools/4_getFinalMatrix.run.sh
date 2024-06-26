Rscript 4_getFinalMatrix.R  -i 2A_multiBamSummary-bins/1000bp-bins.rawCounts.txt     -o 4_getFinalMatrix/rawCounts_1kbBins  -t 20
Rscript 4_getFinalMatrix.R  -i 3A_multiBigwigSummary-bins/C.matrix.1000bp-bins.txt   -o 4_getFinalMatrix/RPKM_1kbBins       -t 20


Rscript 4_getFinalMatrix.R  -i 2B_multiBamSummary-BED/allPeaks.bed/allPeaks.bed.RawCounts.txt     -o 4_getFinalMatrix/rawCounts_allPeaks  -t 20

Rscript 4_getFinalMatrix.R  -i 3B_multiBigwigSummary-BED/allPeaks.bed/matrix.txt   -o 4_getFinalMatrix/RPKM_allPeaks       -t 20




