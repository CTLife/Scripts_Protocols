input1="5-Matrix/minus-Strand"
input2="5-Matrix/plus-Strand"

out="7-mergedMatrix"
mkdir  -p  $out/DRACH
 

zcat  $input1/DRACH/Common3.gz     > $out/DRACH/Common3.1 
zcat  $input1/DRACH/FP.gz          > $out/DRACH/FP.1
zcat  $input1/DRACH/Specific.gz    > $out/DRACH/Specific.1
zcat  $input1/DRACH/TP.gz          > $out/DRACH/TP.1
 
zcat  $input2/DRACH/Common3.gz      |  grep -P "^chr"      > $out/DRACH/Common3.2 
zcat  $input2/DRACH/FP.gz           |  grep -P "^chr"      > $out/DRACH/FP.2
zcat  $input2/DRACH/Specific.gz     |  grep -P "^chr"      > $out/DRACH/Specific.2
zcat  $input2/DRACH/TP.gz           |  grep -P "^chr"      > $out/DRACH/TP.2
 
cat  $out/DRACH/Common3.1     $out/DRACH/Common3.2     > $out/Common3 
cat  $out/DRACH/FP.1          $out/DRACH/FP.2          > $out/FP
cat  $out/DRACH/Specific.1    $out/DRACH/Specific.2    > $out/Specific
cat  $out/DRACH/TP.1          $out/DRACH/TP.2          > $out/TP




