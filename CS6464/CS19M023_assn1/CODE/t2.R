lambada = 0.3
simul = 1000
samp = 40
mat1 = array(0,c(samp,simul))
for (var1 in 1:simul) {
  mat1[,var1] = rexp(samp,lambada)
}
means = colMeans(mat1)

th_mean = 1/lambada
s_mean = mean(means)
th_var = ((th_mean)^2)*(1/samp)
s_var = var(means)
print("theoritical mean")
print(th_mean)
print("sample mean")
print(s_mean)
print("theoritical variance")
print(th_var)
print("sample variance")
print(s_var)

hist(means, main = "histogram of avg of exponentials",breaks = 40)
abline(v=th_mean,col ="blue")
abline(v=s_mean,col ="red")
legend("topright", legend=c("th mean", "smp mean"),
       col=c("blue", "red"), lty=1:1, cex=0.5)



