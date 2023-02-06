#    venn                Venn diagram of intersection of genomic regions or list sets (upto 6-way).
#    upset               UpSet diagram of intersection of genomic regions or list sets.
#    pairwise            Pairwise intersection and heatmap of N genomic region sets in <BED/GTF/GFF> format.


A0=BED/2022120610.list.txt
A1=BED/2022120615.list.txt
A2=BED/20221221t7VN.list.txt

outdir="Overlap"
mkdir  $outdir 


## venn
##############################################################################
outpath1=$outdir/venn
mkdir  $outpath1
intervene  venn  --input  \
$A0  \
$A1  \
$A2  \
--type list    --save-overlaps   --output $outpath1  \
--dpi 1200  --figtype pdf   --figsize 12 12  --fontsize 16
##############################################################################


## upset
##############################################################################
outpath2=$outdir/upset
mkdir  $outpath2
intervene  upset  --input  \
$A0  \
$A1  \
$A2  \
--type   list   --save-overlaps   --output $outpath2  --ninter 100       \
--dpi 1200  --figtype pdf   --figsize 12 6   
##############################################################################


## pairwise
##############################################################################
outpath1="$outdir/pairwise.fraction"
mkdir -p $outpath1

intervene  pairwise  \
--input  \
$A0  \
$A1  \
$A2  \
--type list    --compute  frac  --htype color   --output $outpath1  \
--dpi 1200  --figtype svg   --figsize 10 10  --fontsize 16



outpath2="$outdir/pairwise.count"
mkdir -p $outpath2

intervene  pairwise  \
--input  \
$A0  \
$A1  \
$A2  \
--type list    --compute  count  --htype color  --output $outpath2  \
--dpi 1200  --figtype svg   --figsize 10 10   --fontsize 16
##############################################################################




















 






