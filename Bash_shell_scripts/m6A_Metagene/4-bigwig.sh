input="3-bedgraph/minus-Strand"
out="4-bigwig/minus-Strand"
mkdir  -p  $out
chromSize_g="hg38.genome.chromsize";


bedGraphToBigWig  $input/HEK293.sorted.bedGraph          $chromSize_g         $out/HEK293.bw  
bedGraphToBigWig  $input/Hela.sorted.bedGraph         $chromSize_g         $out/Hela.bw  
bedGraphToBigWig  $input/Hela-RIP.sorted.bedGraph               $chromSize_g         $out/Hela-RIP.bw  
bedGraphToBigWig  $input/HepG2.sorted.bedGraph               $chromSize_g         $out/HepG2.bw  







input="3-bedgraph/plus-Strand"
out="4-bigwig/plus-Strand"
mkdir  -p  $out
chromSize_g="hg38.genome.chromsize";

bedGraphToBigWig  $input/HEK293.sorted.bedGraph          $chromSize_g         $out/HEK293.bw  
bedGraphToBigWig  $input/Hela.sorted.bedGraph         $chromSize_g         $out/Hela.bw  
bedGraphToBigWig  $input/Hela-RIP.sorted.bedGraph               $chromSize_g         $out/Hela-RIP.bw  
bedGraphToBigWig  $input/HepG2.sorted.bedGraph               $chromSize_g         $out/HepG2.bw  




