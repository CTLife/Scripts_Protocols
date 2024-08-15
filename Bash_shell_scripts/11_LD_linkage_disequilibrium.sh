prefix1=BA24_samples
vcftools  --vcf 8_selectSNPs/$prefix1.recode.vcf   --hap-r2     --ld-window-bp 100000   --out 11_LD/$prefix1.ld_100kb  > 11_LD/$prefix1.runLog.txt 2>&1     

prefix1=BA9_samples
vcftools  --vcf 8_selectSNPs/$prefix1.recode.vcf   --hap-r2     --ld-window-bp 100000   --out 11_LD/$prefix1.ld_100kb  > 11_LD/$prefix1.runLog.txt 2>&1     

prefix1=C_samples
vcftools  --vcf 8_selectSNPs/$prefix1.recode.vcf   --hap-r2     --ld-window-bp 100000   --out 11_LD/$prefix1.ld_100kb  > 11_LD/$prefix1.runLog.txt 2>&1     

prefix1=H_samples
vcftools  --vcf 8_selectSNPs/$prefix1.recode.vcf   --hap-r2     --ld-window-bp 100000   --out 11_LD/$prefix1.ld_100kb  > 11_LD/$prefix1.runLog.txt 2>&1     

prefix1=T_samples
vcftools  --vcf 8_selectSNPs/$prefix1.recode.vcf   --hap-r2     --ld-window-bp 100000   --out 11_LD/$prefix1.ld_100kb  > 11_LD/$prefix1.runLog.txt 2>&1     



