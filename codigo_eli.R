require(gridExtra)
require(runjags)
require(ggmcmc)
library(coda)
library(Rcpp)
library(maps)
library(mapproj)
library(ggplot2)
library(shiny)
library(R2jags)
library(rjags)
library(psych)
library(caret)
library(bnclassify)
library(klaR)
library(rocc)

setwd("~/proyectos/violencia_sexual/Bayesian-Variable-Selection")
tabla<-read.csv("dataxy_tot.csv",header=TRUE)

tabla_datos<-tabla
names(tabla)
proporcion_entrena<-1
vector_variables<-names(tabla)[2:ncol(tabla_datos)]
iteraciones_jags<-10000
calentamiento_jags<-1000


tabla_entrena1<-tabla_datos
inTraining <- createDataPartition(tabla_entrena1$y_tot, p = proporcion_entrena, list = FALSE)
tabla_entrena2 <- tabla_entrena1[ inTraining,]
n<-nrow(tabla_entrena2)
var_expl<-ncol(tabla_entrena2)-1

#-Defining data-
data<-list("n"=n,"var_expl"=var_expl,"y"=tabla_entrena2$y_tot)
for(i in 1:var_expl){
  #i<-1
  data[[i+3]]<-as.array(tabla_entrena2[,i+1])
  
}

for( j in 1:var_expl){
  #j<-1
  names(data)[j+3]<-sprintf("x%i",j)
  
}

x<-matrix(nrow=n,ncol=var_expl)
for( j in 1:var_expl){
  
  x[,j]<-data[[j+3]]
  
}



data2<-list("n"=n,"var_expl"=var_expl,"y"=tabla_entrena2$y_tot,"x"=x)
#-Defining inits-
inits<-function(){list(alpha=0,sdBeta=.5,
                       IndA=c(rep(1,var_expl)),yest=rep(0,n))}

#-Selecting parameters to monitor-
parameters<-c("alpha","sdBeta","Ind","beta","tauBeta","TauM","yest")

###################
#     RUN.JAGS
##################
out3 <- run.jags("ssvs_04.txt", parameters, data=data2, n.chains=3,inits=inits,
                 method="parallel", adapt=5000, burnin=5000)
outdf <- ggs(as.mcmc.list(out3))
ncov<-var_expl
probs <- out3$summary$statistics[((3):(2+ncov)), 1]

###################
#       JAGS
##################

out_jags20<-jags(data2, inits, parameters, model.file="ssvs_04.txt", n.iter=iteraciones_jags,
                 n.chains=1 , n.burnin=calentamiento_jags)

out<-out_jags20$BUGSoutput$sims.list
out.sum<-out_jags20$BUGSoutput$summary
out.dic<-out_jags20$BUGSoutput$DIC

headTail(out.sum,60,20)

