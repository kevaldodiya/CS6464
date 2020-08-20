library(dplyr)

f1 = function(x){
  return(0.3 * cos(3 * pi * x)) - (0.4 * cos(4 * pi * x)) - (10 / ((x ^ 2) + 1))
}

data_generation = function(sig1,k){
  samp <-runif(100,min = -10,max = 10)
  eps1 <- rnorm(length(samp),mean = 0,sd = sig1)
  f1_y <- k(samp) + eps1
  data1 = data.frame(samp,f1_y)
  return (data1)
}



find_bias = function(tst, original){
  return ( mean(tst)-original )
}

find_var = function(tst){
  return (mean((tst - mean(tst))^2))
}
  
find_mse = function(tst,original){
  return(mean((tst - original) ^ 2))
}
  set.seed(1)
  sig1 = 0.4
  samp =  data.frame(samp = runif(n=100, min=-10, max=10))
  tot_points = 100
  simul = 10
  d = vector()
  d = c(d,1,2,3,10,13,15,17,20,22)
  mat1  = array(0,dim = c(simul,length(d),tot_points))

  for (var1 in 1:simul) {
    data2 <- data_generation(sig1,f1)
    
    fit_m1 <- lm(f1_y ~ poly(samp,degree = d[1]),data = data2)
    fit_m2 <- lm(f1_y ~ poly(samp,degree = d[2]),data = data2)
    fit_m3 <- lm(f1_y ~ poly(samp,degree = d[3]),data = data2)
    fit_m4 <- lm(f1_y ~ poly(samp,degree = d[4]),data = data2)
    fit_m5 <- lm(f1_y ~ poly(samp,degree = d[5]),data = data2)
    fit_m6 <- lm(f1_y ~ poly(samp,degree = d[6]),data = data2)
    fit_m7 <- lm(f1_y ~ poly(samp,degree = d[7]),data = data2)
    fit_m8 <- lm(f1_y ~ poly(samp,degree = d[8]),data = data2)
    fit_m9 <- lm(f1_y ~ poly(samp,degree = d[9]),data = data2)
      
      mat1[var1,1,] = predict(fit_m1,data.frame(samp = samp$samp))
      mat1[var1,2,] = predict(fit_m2,data.frame(samp = samp$samp))
      mat1[var1,3,] = predict(fit_m3,data.frame(samp = samp$samp))
      mat1[var1,4,] = predict(fit_m4,data.frame(samp = samp$samp))
      mat1[var1,5,] = predict(fit_m5,data.frame(samp = samp$samp))
      mat1[var1,6,] = predict(fit_m6,data.frame(samp = samp$samp))
      mat1[var1,7,] = predict(fit_m7,data.frame(samp = samp$samp))
      mat1[var1,8,] = predict(fit_m8,data.frame(samp = samp$samp))
      mat1[var1,9,] = predict(fit_m9,data.frame(samp = samp$samp))
      
  }
  
  bias_arr = array(0,c(tot_points,length(d)))
  var_arr = array(0,c(tot_points,length(d)))
  mse_arr = array(0,c(tot_points,length(d)))
  
  for (var1 in 1:length(samp$samp)) {
    mat2 = mat1[,,var1]
    bias_arr[var1,] = apply(mat2,2,find_bias,original=f1(samp$samp[var1]))
    var_arr[var1,] = apply(mat2,2,find_var)
    mse_arr[var1,] = apply(mat2,2,find_mse,original = f1(samp$samp[var1]))
    
  }
  bias = sqrt(colMeans((abs(bias_arr))^2))
  variance = colMeans(var_arr)
  mse = colMeans(mse_arr)
  print(bias^2)
  print(variance)
  print(mse)
  miniv = min(variance)
  minib = min(bias)
  maxiv = max(variance)
  maxib = max(bias)
  if(maxiv > maxib){
    maxi = maxiv
  } else {
    maxi = maxib
  }
  
  if(miniv > minib){
    mini = minib
  } else {
    mini = miniv
  }
  plot(d,bias, lty = 1,type = "b", ylim= c(mini, maxi+0.1),col = "green" ,main = "bias/variance Vs model comp(sigma)",xlab = "degree",ylab = "bias/variance") 
  lines(d,variance, type='b', col='red',pch=18,lty = 2)
  legend("topleft", legend=c("bias", "variance"),
         col=c("green", "red"), cex=0.7,lty = 1:2) 


