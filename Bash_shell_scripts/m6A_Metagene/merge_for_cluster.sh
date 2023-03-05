out=merge
mkdir $out
cat minus-Strand/3UTR.Matrix  plus-Strand/3UTR.Matrix  > $out/3UTR.Matrix
cat minus-Strand/5UTR.Matrix  plus-Strand/5UTR.Matrix  > $out/5UTR.Matrix
cat minus-Strand/CDS.Matrix   plus-Strand/CDS.Matrix   > $out/CDS.Matrix


cat minus-Strand/3UTR.SortedRegions  plus-Strand/3UTR.SortedRegions  > $out/3UTR.SortedRegions
cat minus-Strand/5UTR.SortedRegions  plus-Strand/5UTR.SortedRegions  > $out/5UTR.SortedRegions
cat minus-Strand/CDS.SortedRegions   plus-Strand/CDS.SortedRegions   > $out/CDS.SortedRegions

