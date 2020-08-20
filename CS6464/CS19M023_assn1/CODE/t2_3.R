lam = 0.3
simul = 1000
samp = 40
mat1 = array(0,c(samp,simul))
for (var1 in 1:simul) {
  mat1[,var1] = rexp(samp,lam)
}
means = colMeans(mat1)
m1 = 1/lam
s1 = sd(means)

#large collection of random exponentials(1000) 
den_g = rexp(simul,lam)
hist(den_g,freq = FALSE ,main = "histogram of large collection of expo.",breaks = 20 )
lines(density(den_g),col = 'red',lwd = 2)

#large collection of 40 exponential average
 samples = seq(min(means),max(means),length = simul/2)
 samp_y = dnorm(samples,mean = m1,sd = s1)
 hist(means,freq = FALSE ,main = "histogram of avg of exponentials",breaks = 30 )
 lines(samples,samp_y,col = 'red',lwd = 2)
