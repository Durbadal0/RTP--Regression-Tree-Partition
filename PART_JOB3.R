library(nimble)
solveLeastSquares <- nimbleFunction(
  run = function(X = double(2), y = double(1)) { # type declarations
    ans <- inverse(t(X) %*% X) %*% (t(X) %*% y)
    return(ans)
    returnType(double(2))  # return type declaration
  } )
fb1=nimbleFunction(run=function(a=double(1),nn=double(0)){
  return(a[nn])
  returnType(double(0))})
fb1(c(2,3,4),2)
X <- matrix(rnorm(400), nrow = 100)
y <- rnorm(100)
solveLeastSquares(X, y)
CsolveLeastSquares <- compileNimble(solveLeastSquares)
CsolveLeastSquares(X, y)

foo <- nimbleFunction( run = function(x = double(1)) {return(sum(x)); returnType(double())})
cfoo <- compileNimble(foo)
cfoo(1:10)

log_lk_R3=function(x1,partitions,radius,cov1,b1,a1,W1,lambda1,death1) { # type declarations
  N1=length(partitions[,1])
  p=length(partitions[1,])-1
  cov2=c(x1,cov1)
  partitions1=partitions[order(partitions[,1],decreasing = TRUE),]
  partitions2=partitions1+radius
  ind=matrix(0,ncol=p+1,nrow=N1)
  for(i in 1:p+1){
    for(j in 1:N1){
      if(cov2>=part){}
    }
  }
  xb<-rep(1,L)
  for (l in 1:L) {
    
    
    
    index2<-l*L2^p+int_cov
    
    xb[l]<- exp(W1+lambda1+b1[index2])
  }
  
  
  
  cum_haz <- 1
  
  
  L1<-L+1
  
  
  
  delhaz=rep(0,L1)
  
  
  for(s in 2:L1){
    delhaz[s]<-(a1[s]-a1[(s-1)])*xb[(s-1)];
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  cum_haz<-sum(delhaz[1:interval])+(x1-a1[interval])*xb[interval]
  
  if(death1==0){
    p1 <- -cum_haz
  } else {
    p1<-log(xb[interval]) - cum_haz
  }
  
  
  
  return( p1)
  
  
  # return type declaration
}







bart_fn_r3=function(cov1,partitions,radius,b1,b2,N){
  
  p <- length(partitions[1,])
  
  partitions2 <- partitions+radius
  count <- 0
  part_ind <- 0
  for(i in 1:N){
    a <- 0
    for(j in 1:p){
      if(cov1[j] >= partitions[i,j] & cov1[j] < partitions2[i,j]){a <- (a+1)} 
      
    }
    if(a==p){
      part_ind <- b1[i]+part_ind
      count<- count +1
    }
    
  }
  
  if(part_ind==0){
    jjj <- b2
  }else{jjj <- part_ind/count}
  
  return(jjj)
  
}

bart_fn_nim23 = nimbleFunction(run=function(cov1=double(1),partitions=double(2),radius=double(2),
                                            b1=double(1),b2=double(0),N=integer(0)){
  p <- length(partitions[1,])
  
  partitions2 <- partitions+radius
  count<-0
  part_ind <- 0
  for(i in 1:N){
    a <- 0
    for(j in 1:p){
      if(cov1[j] >= partitions[i,j] & cov1[j] < partitions2[i,j]){a <- (a+1)} 
      
    }
    if(a==p){
      part_ind <- b1[i]+part_ind
      count <- count+1
    }
    
  }
  
  if(part_ind==0){
    jjj <- b2
  }else{jjj <- part_ind/count}
  
  return(jjj)
  returnType(double(0)) 
  
})

bart_fn_nim13=nimbleRcall(function(cov1=double(1),partitions=double(2),radius=double(2),
                                   b1=double(1),b2=double(0),N=integer(0)){},Rfun='bart_fn_r3',returnType=double(1))


Cbart_fn_nim13=compileNimble(bart_fn_nim13)
partitions=matrix(runif(2000,0,1),ncol=2)
radius=matrix(runif(2000,0,.1),ncol=2)
cov1=c(.2,3)
b1=seq(1,1000,1)
b2=99999
cov1=c(.39,.19)
bart_fn_r3(cov1,partitions,radius,b1,b2,150)

bart_fn_nim13(cov1,partitions,radius,b1,b2,N=150)
Cbart_fn_nim13(cov1,partitions,radius,b1,b2,N=150)
bart_fn_nim23(cov1,partitions,radius,b1,b2,N=150)

bart_nimcode3 <- nimbleCode({
  prob ~ dunif(0,1)
  sigma ~ dunif(0, 100)
  sigma2 ~ dunif(0, 100)
  b21~ dnorm(0,sigma2)
  N2~dbin(prob,N3)
  for(i in 1:N3){
    b11[i]~dnorm(0,sigma2)
    for(j in 1:p){
      parti[i,j]~dunif(0,1)
      radi[i,j] ~ dunif(0,.5)
      
    }
  }
  for(k in 1:n) {
    
    predicted.y[k] <- bart_fn_nim23(cov2[k,1:p],parti[1:N3,1:p],radi[1:N3,1:p],b11[1:N3],b21,N2)
    y[k] ~ dnorm(predicted.y[k], sd = sigma)
  }
})












PART_MCMC_results=function(cov_mat,y2,N31=1000,n.iter =5000,
                           n.burnin = 0,n.chains =1){
  p1=length(cov_mat[1,])
  n1=length(y2)
  nim_con=list(cov2=cov_mat,
               p=p1,
               n=n1,
               N3=N31)
  nim_data=list(y=y2)
  inits1=list(sigma=1,b21=0,b11=rep(0,N31),sigma2=1,radi=matrix(0.1,ncol=p1,nrow = N31),N2=2,parti=matrix(0.5,ncol=p1,nrow = N31),prob=.5)
  bart_model4=nimbleModel(bart_nimcode3, data = nim_data, constants = nim_con,inits = inits1)
  
  
  # compile model
  
  CBARTmodel <- compileNimble(bart_model4,showCompilerOutput = TRUE)
  
  # create a MCMC configuration
  
  BARTmodelConf <- configureMCMC(bart_model4)
  
  # add lifespan to list of parameters to monitor
  
  BARTmodelConf$addMonitors(c("N2","b21","b11[]", "predicted.y[]" ))
  # create a MCMC function and compile it
  
  BARTmodelMCMC <- buildMCMC(BARTmodelConf)
  
  CBARTMCMC <- compileNimble(BARTmodelMCMC, project = bart_model4,
                             showCompilerOutput = TRUE)
  
  
  BART_W_samples <- runMCMC(mcmc = CBARTMCMC,
                            
                            niter = n.iter,
                            
                            nburnin = n.burnin,
                            
                            nchain = n.chains)
  
  
  library(MCMCvis)
  summary=MCMCsummary(BART_W_samples)
  
  
  y.pred=summary$mean[(3+N31*(p1+1)):(2+N31*(p1+1)+n1)]
  rmse=sqrt(mean((y.pred-y2)^2))
  
  
  linear=lm(y2~cov_mat)
  
  rmse_lin=sqrt(mean((y2-linear$fitted.values)^2))
  
  results=list(MCMC_samples=BART_W_samples,summary1=summary,RMSE=rmse,RMSE_lin=rmse_lin,y1=y2,cov1=cov_mat,pred.y=y.pred)
  
  return(results)
}






#data1

cov2=matrix(runif(1000,0,1),ncol=10)
y1=c()
for(i in 1:length(cov2[,1])){
  y1[i]=10*sin(pi*cov2[i,1]*cov2[i,2])+20*((cov2[i,3]-.5)^2)+10*cov2[i,4]+5*cov2[i,5]+rnorm(1)
}




job=PART_MCMC_results(cov_mat=cov2,y2=y1,n.iter=5)
rlist::list.save(job,file="Part_job3_data1.rds")
