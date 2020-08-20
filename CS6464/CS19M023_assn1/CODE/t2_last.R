lam = 0.3
samp1 = 100

#KL divergence
kl = function(original , app){

  return(original%*%log(original/app))
}

# fisher information
fish_info = function(ori_lam, n){
  return((1/ori_lam)^2*n)
}

 samples = rexp(samp1,0.3)

 eps1 <- rnorm(100,mean = 2,sd = 0.4)
 
 y = lam*(exp(lam*samples))        + eps1 
 data1 = data.frame(samples,y)
 fit1 = lm(log(y)~ samples, data = data1)
 c = coef(fit1)[1]
 a = coef(fit1)[2]
 predicted_y = exp(c + a*samples)
 #KL value
 kl_value = kl(y,predicted_y)
 print(kl_value)
 
 fish_value = fish_info(lam,40)
 print(fish_value)
 
 plot(samples,y,col = "blue" ,main = "KL",xlab = "samples",ylab = "output",lwd = 3) 
 points(samples,predicted_y, col='red',pch = 20,lwd = 0)
 legend("topleft", legend=c("main data", "pred. data"),
        col=c("blue", "red"), cex=0.6,lty = 1:2)
 
 
