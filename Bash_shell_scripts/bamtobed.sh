mkdir  "BED"


bedtools  bamtobed  -i 3_STAR/R22313349-library30.bam             >  BED/library30.bed
bedtools  bamtobed  -i 3_STAR/R22313350-library31.bam             >  BED/library31.bed
bedtools  bamtobed  -i 3_STAR/R22317158-library38-p-293T.bam      >  BED/library38.bed
bedtools  bamtobed  -i 3_STAR/R22318247-library41-PFA-IgG.bam     >  BED/library41.bed
bedtools  bamtobed  -i 3_STAR/R22318248-library42-L-m6A.bam       >  BED/library42.bed
bedtools  bamtobed  -i 3_STAR/R22318249-library43-L-IgG.bam       >  BED/library43.bed



sort -k1,1   -k2,2n   BED/library30.bed    >  BED/library30.sorted.bed   
sort -k1,1   -k2,2n   BED/library31.bed    >  BED/library31.sorted.bed   
sort -k1,1   -k2,2n   BED/library38.bed    >  BED/library38.sorted.bed   
sort -k1,1   -k2,2n   BED/library41.bed    >  BED/library41.sorted.bed   
sort -k1,1   -k2,2n   BED/library42.bed    >  BED/library42.sorted.bed   
sort -k1,1   -k2,2n   BED/library43.bed    >  BED/library43.sorted.bed   
 
