library(ggplot2)
library(gridExtra)
library(dplyr)


df1 <- read.table(file="TPM.txt", header = TRUE, sep = "\t", quote = "\"'",   dec = "." )
dim(df1)
df1[1:5,]

df2 = data.frame( celltype=c(df1$celltype, df1$celltype),  rep=c(df1$rep, df1$rep ), value=log2( c(df1$Rny1, df1$Rny3) +1), RNAtype=c( rep("Rny1", nrow(df1)), rep("Rny3", nrow(df1)) ) )
dim(df2)
df2

se <- function(x){sd(x)/sqrt(length(x))}

my_dat <- summarise( group_by(df2, celltype, RNAtype),  my_mean = mean(value),  my_se = se(value)  )

p1 <- ggplot() + 
  geom_bar(data = my_dat, aes(y = my_mean, x = celltype, color=RNAtype, fill=RNAtype ), position = position_dodge(width = .9), stat="identity", width=0.75) + 
  geom_errorbar(data = my_dat,  aes(y = my_mean, x = celltype, color=RNAtype,   ymin = my_mean - my_se, ymax = my_mean + my_se), position = position_dodge(width = .9), stat="identity", width=0.2) + 
  geom_point(data = df2, aes(y = value, x = celltype, color=RNAtype, fill=RNAtype, alpha=0.5), position = position_dodge(width = .9) ) +
  scale_color_manual(values=c("black", "black" )) + theme_classic()  


pdf("TPM.txt.pdf")
p1
dev.off()

