
#linear regression return response
mylin=function(X,Y, Xpred){
  Xpred1=cbind(1,Xpred)
  X=cbind(1,X)
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  Res = Xpred1%*%beta
  return(Res)
}

#my cv function 
myCV=function(X,Y,Nfolds){
  n=length(Y)
  p=ncol(X)
  set.seed(12345)
  ind=sample(n,n)
  X1=X[ind,]
  Y1=Y[ind]
  sF=floor(n/Nfolds)
  MSE=numeric(2^p-1)
  Nfeat=numeric(2^p-1)
  Features=list()
  curr=0

  #we assume 5 features.
  for (f1 in 0:1)
    for (f2 in 0:1)
      for(f3 in 0:1)
        for(f4 in 0:1)
          for(f5 in 0:1){
            model= c(f1,f2,f3,f4,f5)
            if (sum(model)==0) next()
            SSE=0
  
            # generating sequence
            lower_index_seq <- seq(1,n,sF)
            upper_index_seq <- seq(0,n,sF)
            
            current_selected_feature <- which(model == 1)
            X2<- X1[,current_selected_feature,drop=F] #apply k fold
            
            for (k in 1:Nfolds)
            {
              i <- lower_index_seq[k]
              j <- upper_index_seq[k+1]
              
              k_fold_ind <- ind[i:j] # calculating indexes
              
              Xpred <- X2[k_fold_ind,]
              Xt <- X2[-k_fold_ind,]
              
              Yp <- Y1[k_fold_ind]
              Yt <- Y1[-k_fold_ind]
              
              Ypred <- mylin(Xt,Yt, Xpred)
              SSE=SSE+sum((Ypred-Yp)^2)
            }
            curr=curr+1
            MSE[curr]=SSE/n
            Nfeat[curr]=sum(model)
            Features[[curr]]=model
  
          }
    
      plot(x=Nfeat,y=MSE,  main = "MSE against their features", xlab = "Number of features", ylab ="MSE" , 
       col = "red")
      i=which.min(MSE)
    return(list(CV=MSE[i], Features=Features[[i]]))
}

myCV(as.matrix(swiss[,2:6]), swiss[[1]], 5)
