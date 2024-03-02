library(ggplot2)
library(ggbreak)


MySaveGgplot2_1_g <- function(ggplot2Figure1,  path1, fileName1,  height1, width1) {
  SVG1 <- paste(path1,  "/",  "SVG",  sep = "",  collapse = NULL)
  PDF1 <- paste(path1,  "/",  "PDF",  sep = "",  collapse = NULL)
  EPS1 <- paste(path1,  "/",  "EPS",  sep = "",  collapse = NULL)
  if( ! file.exists(SVG1) ) { dir.create(SVG1) }
  if( ! file.exists(PDF1) ) { dir.create(PDF1) }
  if( ! file.exists(EPS1) ) { dir.create(EPS1) }
  ggplot2::ggsave(filename=paste(SVG1, "/", fileName1, ".svg", sep=""),  plot = last_plot(), device = "svg",   path = NULL, scale = 1, width = width1, height = height1, units = "in", dpi = 3000, limitsize = FALSE)
  ggplot2::ggsave(filename=paste(PDF1, "/", fileName1, ".pdf", sep=""),  plot = last_plot(), device = "pdf",   path = NULL, scale = 1, width = width1, height = height1, units = "in", dpi = 3000, limitsize = FALSE)
  ggplot2::ggsave(filename=paste(EPS1, "/", fileName1, ".eps", sep=""),  plot = last_plot(), device =cairo_ps, path = NULL, scale = 1, width = width1, height = height1, units = "in", dpi = 3000, limitsize = FALSE)
}





###################
df1 <- read.table("1_BA9.hyper.txt", header=TRUE, sep="\t", quote = "", comment.char = "") 
df2 <- read.table("2_BA24.hyper.txt", header=TRUE, sep="\t", quote = "", comment.char = "") 
df3 <- read.table("3_C.hyper.txt", header=TRUE, sep="\t", quote = "", comment.char = "") 
df4 <- read.table("4_H.hyper.txt", header=TRUE, sep="\t", quote = "", comment.char = "") 
df5 <- read.table("5_T.hyper.txt", header=TRUE, sep="\t", quote = "", comment.char = "") 


dim(df1)
dim(df2)
dim(df3)
dim(df4)
dim(df5)





outDir_g = "peaks_times_on_transcript.hyper"
if( ! file.exists(outDir_g)   ) { dir.create(outDir_g, recursive = TRUE) }

write.table(x= table(df1[ ,1 ]) , file = paste(outDir_g, "1.num-peaks.txt" , sep="/"),  append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,  col.names = TRUE )
write.table(x= table(df2[ ,1 ]) , file = paste(outDir_g, "2.num-peaks.txt" , sep="/"),  append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,  col.names = TRUE )
write.table(x= table(df3[ ,1 ]) , file = paste(outDir_g, "3.num-peaks.txt" , sep="/"),  append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,  col.names = TRUE )
write.table(x= table(df4[ ,1 ]) , file = paste(outDir_g, "4.num-peaks.txt" , sep="/"),  append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,  col.names = TRUE )
write.table(x= table(df5[ ,1 ]) , file = paste(outDir_g, "5.num-peaks.txt" , sep="/"),  append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,  col.names = TRUE )


times_1 <- as.vector( table(df1[ ,1 ]) )
times_2 <- as.vector( table(df2[ ,1 ]) )
times_3 <- as.vector( table(df3[ ,1 ]) )
times_4 <- as.vector( table(df4[ ,1 ]) )
times_5 <- as.vector( table(df5[ ,1 ]) )
 
 
times_1[times_1>8] = 8
times_2[times_2>8] = 8
times_3[times_3>8] = 8
times_4[times_4>8] = 8
times_5[times_5>8] = 8
 


numVec = times_1
x1 = names( table(numVec) )
y1 = as.vector( table(numVec) )
x1
y1

df3 = data.frame(x=x1, y=y1)
p1 <- ggplot(df3, aes(x=x, y=y, color="red4" , fill="red4")) + geom_bar(stat="identity", color="red4", position=position_dodge( )) +
        labs(x = "number of peaks", y="number of genes") + scale_y_break( c(180, 1200) ) + 
        geom_text(aes(label = y1), vjust = 1.5 , color="black") + theme(legend.position="none")

MySaveGgplot2_1_g(ggplot2Figure1=p1, path1=outDir_g, fileName1="1.barPlot", height1=4, width1=5)

 




numVec = times_2
x1 = names( table(numVec) )
y1 = as.vector( table(numVec) )
x1
y1
 
df3 = data.frame(x=x1, y=y1)
p2 <- ggplot(df3, aes(x=x, y=y, color="red4" , fill="red4")) + geom_bar(stat="identity", color="red4", position=position_dodge( )) +
  labs(x = "number of peaks", y="number of genes") + scale_y_break( c(160, 1100) ) + 
  geom_text(aes(label = y1), vjust = 1.5 , color="black") + theme(legend.position="none")

MySaveGgplot2_1_g(ggplot2Figure1=p2, path1=outDir_g, fileName1="2.barPlot", height1=4, width1=5)






numVec = times_3
x1 = names( table(numVec) )
y1 = as.vector( table(numVec) )
x1
y1

df3 = data.frame(x=x1, y=y1)
p3 <- ggplot(df3, aes(x=x, y=y, color="red4" , fill="red4")) + geom_bar(stat="identity", color="red4", position=position_dodge( )) +
  labs(x = "number of peaks", y="number of genes") + scale_y_break( c(100, 800) ) + 
  geom_text(aes(label = y1), vjust = 1.5 , color="black") + theme(legend.position="none")

MySaveGgplot2_1_g(ggplot2Figure1=p3, path1=outDir_g, fileName1="3.barPlot", height1=4, width1=5)











numVec = times_4
x1 = names( table(numVec) )
y1 = as.vector( table(numVec) )
x1
y1

df3 = data.frame(x=x1, y=y1)
p4 <- ggplot(df3, aes(x=x, y=y, color="red4" , fill="red4")) + geom_bar(stat="identity", color="red4", position=position_dodge( )) +
  labs(x = "number of peaks", y="number of genes") + scale_y_break( c(90, 950) ) + 
  geom_text(aes(label = y1), vjust = 1.5 , color="black") + theme(legend.position="none")

MySaveGgplot2_1_g(ggplot2Figure1=p4, path1=outDir_g, fileName1="4.barPlot", height1=4, width1=5)










numVec = times_5
x1 = names( table(numVec) )
y1 = as.vector( table(numVec) )
x1
y1

df3 = data.frame(x=x1, y=y1)
p5 <- ggplot(df3, aes(x=x, y=y, color="red4" , fill="red4")) + geom_bar(stat="identity", color="red4", position=position_dodge( )) +
  labs(x = "number of peaks", y="number of genes") + scale_y_break( c(130, 1000) ) + 
  geom_text(aes(label = y1), vjust = 1.5 , color="black") + theme(legend.position="none")

MySaveGgplot2_1_g(ggplot2Figure1=p5, path1=outDir_g, fileName1="5.barPlot", height1=4, width1=5)








 
library(ggpubr)

pdf( "figures.pdf"    )
ggarrange( p1, p2, p3, p4, p5,     ncol = 5, nrow = 1)
dev.off()





