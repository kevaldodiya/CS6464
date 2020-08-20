library(dplyr)

f1 = function(x){
  return(0.3 * cos(3 * pi * x)) - (0.4 * cos(4 * pi * x)) - (10 / ((x ^ 2) + 1))
}

data_generation = function(sig1,k){
  samples <-runif(100,min = -10,max = 10)
  eps1 <- rnorm(length(samples),mean = 0,sd = sig1)
  f1_y <- k(samples) + eps1
  samp = samples
  data1 = data.frame(samp,f1_y)
  return (data1)
}



find_bias = function(tst, original){
  return (mean(tst)-original)
}

find_var = function(tst){
  return (mean((tst - mean(tst))^2))
}

find_mse = function(tst,original){
  return(mean((tst - original) ^ 2))
}

sig1 = seq(0.5,1.4,by = 0.1)
samp =  data.frame(samp = runif(n=100, min=-10, max=10))
tot_points = 100
simul = 10

mat1  = array(0,dim = c(simul,length(sig1),tot_points))

for (var1 in 1:simul) {
  
  data2 <- data_generation(sig1[1],f1)
  fit_m1 <- lm(f1_y ~ poly(samp,degree = 23,row = TRUE),data = data2)
  
  data2 <- data_generation(sig1[2],f1)
  fit_m2 <- lm(f1_y ~ poly(samp,degree = 23,row = TRUE),data = data2)
  
  data2 <- data_generation(sig1[3],f1)
  fit_m3 <- lm(f1_y ~ poly(samp,degree = 23,row = TRUE),data = data2)
  
  data2 <- data_generation(sig1[4],f1)
  fit_m4 <- lm(f1_y ~ poly(samp,degree = 23,row = TRUE),data = data2)
  
  data2 <- data_generation(sig1[5],f1)
  fit_m5 <- lm(f1_y ~ poly(samp,degree = 23,row = TRUE),data = data2)
  
  data2 <- data_generation(sig1[6],f1)
  fit_m6 <- lm(f1_y ~ poly(samp,degree = 23,row = TRUE),data = data2)
  
  data2 <- data_generation(sig1[7],f1)
  fit_m7 <- lm(f1_y ~ poly(samp,degree = 23,row = TRUE),data = data2)
  
  data2 <- data_generation(sig1[8],f1)
  fit_m8 <- lm(f1_y ~ poly(samp,degree = 23,row = TRUE),data = data2)
  
  data2 <- data_generation(sig1[9],f1)
  fit_m9 <- lm(f1_y ~ poly(samp,degree = 23,row = TRUE),data = data2)
  
  data2 <- data_generation(sig1[10],f1)
  fit_m10 <- lm(f1_y ~ poly(samp,degree = 23,row = TRUE),data = data2)
  
  mat1[var1,1,] = predict(fit_m1,data.frame(samp=samp$samp))
  mat1[var1,2,] = predict(fit_m2,data.frame(samp=samp$samp))
  
  mat1[var1,3,] = predict(fit_m3,data.frame(samp=samp$samp))
  mat1[var1,4,] = predict(fit_m4,data.frame(samp=samp$samp))
  
  mat1[var1,5,] = predict(fit_m5,data.frame(samp=samp$samp))
  mat1[var1,6,] = predict(fit_m6,data.frame(samp=samp$samp))
  
  mat1[var1,7,] = predict(fit_m7,data.frame(samp=samp$samp))
  mat1[var1,8,] = predict(fit_m8,data.frame(samp=samp$samp))
  
  mat1[var1,9,] = predict(fit_m9,data.frame(samp=samp$samp))
  mat1[var1,10,] = predict(fit_m10,data.frame(samp=samp$samp))
  
}

bias_arr = array(0,c(tot_points,length(sig1)))
var_arr = array(0,c(tot_points,length(sig1)))
mse_arr = array(0,c(tot_points,length(sig1)))

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
plot(sig1,bias, lty = 1,type = "b", ylim= c(mini, maxi+0.1),col = "green" ,main = "bias/variance Vs sigma",xlab = "sigma",ylab = "bias/variance") 
lines(sig1,variance, type='b', col='red',pch=18,lty = 2)
legend("topleft", legend=c("bias", "variance"),
       col=c("green", "red"), cex=0.5,lty = 1:2) 


