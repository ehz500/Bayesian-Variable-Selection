cat("model{
   for(i in 1:n){
    y[i]~dbin(pi[i],1)
    #Funcion Liga
    logit(pi[i])<-alpha+x[i,]%*%beta
    
    }
    #A priori's
    
    
    alpha ~ dnorm(0,1.0E-6)
    tau ~ dgamma(1.0E-4,1.0E-4)
    tauBeta <-pow(sdBeta,-2);   sdBeta ~ dunif(0,20)
    TauM[1] <- tauBeta*1000;      TauM[2] <- tauBeta
    PInd[2] <- 20/127;      PInd[1] <- 1-PInd[2]

    for(i in 1:var_expl){
    
    IndA[i] ~ dcat(PInd[])
    Ind [i] <- IndA[i]-1
    beta[i] ~ dnorm(0, TauM[IndA[i]])
    
    }
    #Estimaciones de Ys conocidas con
    #La posterior que genera Jags
    for(i in 1:n){
    
    yest[i]~dbin(pi[i],1)
    
    }
    }"
    , file="ssvs_02.txt")