indir=1-rawBED
out=2-sepStrand/plus-Strand
mkdir -p  $out
cat  $indir/HEK293.bed       | awk  -F "\t"  '$6=="+"{print $0}'  > $out/HEK293.bed
cat  $indir/Hela.bed         | awk  -F "\t"  '$6=="+"{print $0}'  > $out/Hela.bed
cat  $indir/Hela-RIP.bed     | awk  -F "\t"  '$6=="+"{print $0}'  > $out/Hela-RIP.bed
cat  $indir/HepG2.bed        | awk  -F "\t"  '$6=="+"{print $0}'  > $out/HepG2.bed   



indir=1-rawBED
out=2-sepStrand/minus-Strand
mkdir -p  $out
cat  $indir/HEK293.bed       | awk  -F "\t"  '$6=="-"{print $0}'  > $out/HEK293.bed
cat  $indir/Hela.bed         | awk  -F "\t"  '$6=="-"{print $0}'  > $out/Hela.bed
cat  $indir/Hela-RIP.bed     | awk  -F "\t"  '$6=="-"{print $0}'  > $out/Hela-RIP.bed
cat  $indir/HepG2.bed        | awk  -F "\t"  '$6=="-"{print $0}'  > $out/HepG2.bed   





 
