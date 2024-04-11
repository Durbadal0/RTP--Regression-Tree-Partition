library(rstan)
options(mc.cores = parallel::detectCores()-1)
rstan_options(auto_write = TRUE)
rstan_options(threads_per_chain = 1)


fn=function(n=n,y=y,cov2=cov2,K=K,N=N,p=p,n.chain=1,n.iter=100){
schools_dat <- list(n=n,y=y,cov2=cov2,K=K,N=N,p=p)



fit <- stan(file = "~/Desktop/PART/SPART.stan", data = schools_dat,chains = n.chain, iter = n.iter)
fit
}

cov2=runif(2)
K=100
N=1000
b11=runif(N)
b21=999999
p=2
pointers=runif(N)
fn(cov2,K,pointers,b11,b21,N,p,n.iter = 2000)

a=(123)
K=5
p=7
fn(a,p,K)



n=100
K=10
N=50
p=1
cov2=matrix(runif(n*p),ncol=p)
y1=cov2[,1]
for(i in 1:length(cov2[,1])){
  if(cov2[i,1]>.6){y1[i]=7}else if(cov2[i,2]<.2){ y1[i]=-6} else{y1[i]=20}
}
for(i in 1:length(cov2[,1])){
  y1[i]=10*sin(pi*cov2[i,1]*cov2[i,2])+20*((cov2[i,3]-.5)^2)+10*cov2[i,4]+5*cov2[i,5]+rnorm(1)
}


library(tictoc)
tic()
fit=fn(n,y1,cov2,K,N,p,n.chain =1,n.iter=1000)
toc()
fit_ss=extract(fit)
y.pred=colMeans(fit_ss$y_pred)
sqrt(mean((y.pred-y1)^2))
View(fit_ss$y_pred)
View(fit_ss$b21)
