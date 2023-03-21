
library(mediation)



###########################################################################################################################################################################
DF_1 <- read.table("common.txt", header=F,   sep="\t" , comment.char = "" )  
dim( DF_1 )
#  DF_1[1:10,] 

numSamples = 21


DF_phe_1 = DF_1[,24:(24+numSamples-1)] 
DF_gno_1 = DF_1[,(24+numSamples+9):(24+numSamples+8+numSamples)] 
dim( DF_phe_1 )
dim( DF_gno_1 )
#  DF_phe_1[1:10,] 
#  DF_gno_1[1:10,]

DF_phe_1 = as.matrix(DF_phe_1)
DF_gno_1 = as.matrix(DF_gno_1)
dim(DF_phe_1 )
dim(DF_gno_1 )


N = ncol(DF_1)/2
N
DF_phe_2 = DF_1[,(24+N):(24+N+numSamples-1)] 
DF_gno_2 = DF_1[,(24+numSamples+9+N):(24+numSamples+8+numSamples+N)] 
dim( DF_phe_2 )
dim( DF_gno_2 )
#  DF_phe_2[1:10,] 
#  DF_gno_2[1:10,]

DF_phe_2 = as.matrix(DF_phe_2)
DF_gno_2 = as.matrix(DF_gno_2)
dim(DF_phe_2 )
dim(DF_gno_2 )

DF_gno_2 == DF_gno_1

 

DF_gno_3 = DF_gno_2
dim(DF_gno_3)
DF_gno_3[ DF_gno_3=="0|0" ] = 0
DF_gno_3[ DF_gno_3=="0|1" ] = 1
DF_gno_3[ DF_gno_3=="1|0" ] = 1
DF_gno_3[ DF_gno_3=="1|1" ] = 2
DF_gno_3[1:10,]
table(DF_gno_3)
DF_gno_3 <- matrix(as.numeric(unlist(DF_gno_3)), ncol = ncol(DF_gno_3))   # Convert to numeric matrix 

 


sink("1.txt")
for( i  in  c(1:nrow(DF_phe_2)) ) {
    cat("###############################################################")
    myData = data.frame("X" = DF_gno_3[i,] ,  "M" = DF_phe_2[i,] , "Y" = DF_phe_1[i,]  )
    model.0 <- lm(Y ~ X, myData)
    model.M <- lm(M ~ X, myData)
    model.Y <- lm(Y ~ X + M, myData)
    results <- mediate(model.M, model.Y, treat='X', mediator='M', boot=TRUE, sims=500)
    print( summary(results) )
    cat("###############################################################\n\n\n\n\n")
}
sink()









sink("2.txt")
for( i  in  c(1:nrow(DF_phe_2)) ) {
  cat("###############################################################")
  myData = data.frame("X" = DF_gno_3[i,] ,  "M" = DF_phe_1[i,] , "Y" = DF_phe_2[i,]  )
  model.0 <- lm(Y ~ X, myData)
  model.M <- lm(M ~ X, myData)
  model.Y <- lm(Y ~ X + M, myData)
  results <- mediate(model.M, model.Y, treat='X', mediator='M', boot=TRUE, sims=500)
  print( summary(results) )
  cat("###############################################################\n\n\n\n\n")
}
sink()




 