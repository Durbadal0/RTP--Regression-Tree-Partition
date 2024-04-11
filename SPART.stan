

functions {
  vector Hilberspace(real a, int p, int K){
    vector[p] z;
    real b;
  
    
    b=floor(a*(pow(K,p)));
    
    for(i in 1:p){
      
      
        z[p+1-i]= (b-K*floor(b/K))/K;
        b=floor(b/K);
      
     
    }
    
    
    return z;
  }
  
  real stan_fn(vector cov2, int K, vector pointers, vector radi, vector b11, real b21, int N, int p){
    real jjj;
    jjj= b21;
    vector[p] Z;
    vector[p] r;
    int part_ind;
    part_ind = 0;
  
     for(i in 1:N){
      real a;
        a = 0;
        Z=Hilberspace(pointers[i],p,K);
        r=Hilberspace(radi[i],p,K);
      for(j in 1:p){
        
      
      
      
      if(cov2[j] >= Z[j] && cov2[j] < Z[j]+r[j]){
        a = a+1;
        } 
      
      
    }
    
    
    if(a==p){
      jjj=b11[i];
      
    }
    
    
    }
    
   
    
    
    return jjj;
  }

}

data {
  int<lower=1> n;         
   real y[n];              
   
   int<lower=1> p; 
   int<lower=1> K; 
   int<lower=1> N; 
   real<lower=0,upper=1> cov2[n,p];
   
   
   
}

transformed data {
  
  
}

parameters {
  real<lower=0.1,upper=100> sigma; 
  real<lower=0.1, upper=100> sigma2; 
  real b21;
  vector[N] b11;
  vector<lower=0,upper= 1>[N]  pointers;
  vector<lower=0,upper= 1>[N]  radi;
 
  
}



transformed parameters {
  vector[n] y_pred;
 
  for(i in 1:n){
    
    y_pred[i]=stan_fn(to_vector(cov2[i]),K,pointers,radi,b11,b21,N,p);
  }
}


model {
  sigma2 ~ uniform(0.1,100);
  sigma2 ~ uniform (0.1, 100);
  b21 ~ normal(0, sigma);
for(i in 1:N){
    b11[i]~ normal(0, sigma);
  pointers[i] ~ uniform(0,1);
  radi[i]~uniform(0,1);
  }
  for(i in 1:n){
    y[i] ~ normal(y_pred[i],sigma2);
  }
}