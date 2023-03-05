out="3-bedgraph/minus-Strand"
mkdir  -p  $out



awk  '{ print $1"\t"$2"\t"$3"\t"$5 }'   2-sepStrand/minus-Strand/HEK293.bed       > $out/HEK293.bedGraph
awk  '{ print $1"\t"$2"\t"$3"\t"$5 }'   2-sepStrand/minus-Strand/Hela.bed         > $out/Hela.bedGraph
awk  '{ print $1"\t"$2"\t"$3"\t"$5 }'   2-sepStrand/minus-Strand/Hela-RIP.bed     > $out/Hela-RIP.bedGraph
awk  '{ print $1"\t"$2"\t"$3"\t"$5 }'   2-sepStrand/minus-Strand/HepG2.bed        > $out/HepG2.bedGraph


sort -k1,1 -k2,2n   $out/HEK293.bedGraph     > $out/HEK293.sorted.bedGraph 
sort -k1,1 -k2,2n   $out/Hela.bedGraph       > $out/Hela.sorted.bedGraph 
sort -k1,1 -k2,2n   $out/Hela-RIP.bedGraph   > $out/Hela-RIP.sorted.bedGraph 
sort -k1,1 -k2,2n   $out/HepG2.bedGraph      > $out/HepG2.sorted.bedGraph 








out="3-bedgraph/plus-Strand"
mkdir  -p  $out

awk  '{ print $1"\t"$2"\t"$3"\t"$5 }'   2-sepStrand/plus-Strand/HEK293.bed   > $out/HEK293.bedGraph
awk  '{ print $1"\t"$2"\t"$3"\t"$5 }'   2-sepStrand/plus-Strand/Hela.bed        > $out/Hela.bedGraph
awk  '{ print $1"\t"$2"\t"$3"\t"$5 }'   2-sepStrand/plus-Strand/Hela-RIP.bed  > $out/Hela-RIP.bedGraph
awk  '{ print $1"\t"$2"\t"$3"\t"$5 }'   2-sepStrand/plus-Strand/HepG2.bed        > $out/HepG2.bedGraph

sort -k1,1 -k2,2n   $out/HEK293.bedGraph     > $out/HEK293.sorted.bedGraph 
sort -k1,1 -k2,2n   $out/Hela.bedGraph          > $out/Hela.sorted.bedGraph 
sort -k1,1 -k2,2n   $out/Hela-RIP.bedGraph    > $out/Hela-RIP.sorted.bedGraph 
sort -k1,1 -k2,2n   $out/HepG2.bedGraph          > $out/HepG2.sorted.bedGraph 








