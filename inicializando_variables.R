setwd("~/Dropbox/01_ITAM_Ciencia_de_Datos/2do_semestre/Analisis_Multivariado/Bayesian-Variable-Selection")
tabla<-read.csv("datos_falsos_para_pruebas.csv",header=TRUE)

tabla_datos<-tabla
Entidad<-"DISTRITO FEDERAL"
Concurrente=1
proporcion_entrena<-.02
vector_variables<-"Porcentaje_Sustituciones"
iteraciones_jags<-1000
calentamiento_jags<-200
modelo_jags1<-1

data2<-list("n"=n,"var_expl"=var_expl,"y"=tabla_entrena2$Ausentismo2,"x"=x)
#-Defining inits-
inits<-function(){list(alpha=0,sdBeta=.5,
                      IndA=c(rep(1,var_expl)),yest=rep(0,n))}
inits<-function(){list(alpha=0,sdBeta=.5,
                       IndA=c(rep(1,var_expl)))}

#-Selecting parameters to monitor-
parameters<-c("alpha","sdBeta","Ind","beta","tauBeta","TauM")#,"yest")
out3 <- run.jags("ssvs_03.txt", parameters, data=data2, n.chains=3,inits=inits,
                 method="parallel", adapt=5000, burnin=5000)
outdf <- ggs(as.mcmc.list(out3))
probs <- out3$summary$statistics


jags(data2,inits,
     parameters,
     model.file=modelo_binomial_logit,
     n.iter=iteraciones_jags,
     n.chains=1,n.burnin=calentamiento_jags)


dat <- list(Y=Y, X=X, nobs=nobs, ncov=ncov)
vars <- c("alpha", "sd_bet", "gamma", "beta", "tau_in", "sd_y")
out2 <- run.jags("ssvs.txt", vars, data=dat, n.chains=3,
                 method="parallel", adapt=5000, burnin=5000)


out2 <- run.jags("ssvs.txt", vars, data=dat, n.chains=3,
                 method="parallel", adapt=5000, burnin=5000)



out2 <- run.jags("ssvs_02.txt", parameters,inits=inits, data=data2, 
                 n.chains=3,
                 adapt=5000, burnin=500)
outdf <- ggs(as.mcmc.list(out2))
