########Question 1#########
W1=integer(length=100)
W2=integer(length=100)
for(l in 1:100)
{
  vector1 <- rexp(10,1/4) 
  vector2 <- rexp(150,1/4)
  #plot(vector)
  W1[l]=mean(vector1)
  W2[l]=mean(vector2)
}
hist(W1,breaks = 30)        #Looks somewhat like Normal Distribution
hist(W2,breaks = 30)        #Looks Better
#Histogram for n=150 has less observed variance about 4.
#Central Limit Theorem is observed
##########################

########Question 2#########
k=0
W=integer(length=100)
for(i in 1:100)  
{  
  n<-50                               #Sample Size
  x<-rnorm(n, mean = 0, sd = 1)       #Sample from Gaussian Distribution
  mn<-mean(x)                         #Value of Test Statistic for mean Estimation
  #print(mn)                          #95% values in standard normal in (-1.96,1.96)
  confidence<-0.95                    #confidence interval
  a<-qnorm((confidence+1)/2,0,1)      #Construction of 95% confidence interval            
  if(mn>-1*(a/sqrt(n)) && mn<a/sqrt(n))   #sigma_test_statistic = sigma/(sqrt(n))  
  {
    k<-k+1
  }
}
print(k)
############################

########Question 3########### 
#(Gamma Confidence Interval)
r=20
lambda=1/4
low <-qgamma(0.025,shape =20,rate = 5,lower.tail = TRUE,log.p = FALSE)
up <-qgamma(0.975,shape =20,rate = 5,lower.tail = TRUE,log.p = FALSE)
mu0=4
count <- 0
for(l in 1:100)
{
  vector <- rexp(20,1/4)
  mn <- mean(vector) #Test Statistic following Gamma(r,r*lambda)
  if(mn>low && mn<up )
    count<-count+1
}
print(count)
########Question 3########### (CLT Confidence Interval)
#uva<-0
#for(i in 1:100)
#{
vector <- rexp(20,1/4) #Degree of Freedom for T-Test is 19
mn <- mean(vector)#alpha=0.5 critical region is (-inf,-talpha/2) U (talpha/2,inf)
n <-20
ve <- var(vector)*n/(n-1)
ts=((mn-4)*(sqrt(20)))/(sqrt(ve))  #Test Statistic
L = qt(0.025, df=19, lower.tail=F) #Percent point for T Distribution
print(ts)
if(ts > -L && ts < L)
{
  print("Fail To Reject Hypothesis mu=4")
  uva<-uva+1
} else
{
  print("Rejected the Hypothesis mu=4")
}
#Out of every 100, 90-95 times it fails to reject the hypothesis.
#The test is Robust.
#print(uva)
#}
###########################

########Question 4#########
n<-55                              #Sample Size
xbar <-22.7                        #Sample Mean
mp <- sqrt(n)/sqrt(n-1)
ss <- 5.4                          #Sample Standard Deviation
s <- ss * mp                       #Unbiased Point estimator for Standard Deviation
mu0 <- 20                          #Test mu=mu0
test_stat <- (xbar-mu0)*sqrt(55)/s #Follows t-distribution with 54 degrees
ts<-qt(0.025, df=54, lower.tail=F) #of freedom under null hypothesis mu=mu0
print(ts)
print(test_stat)           #Test Results: NUll hypothesis Rejected (5% significance)
ts<-qt(0.005, df=54, lower.tail=F)
print(ts)                  #Test Results: NUll hypothesis Rejected (1% significance)
###########################

########Question 5#########
xbara <- 124.3                   #Sample a mean
xbarb <- 80.5                    #Sample b mean
vra <- 13.4                      #Sample a standard deviation
vrb <- 16.7                      #Sample b standard deviation
na <- 15                         #Subsample size for a
nb <- 15                         #Subsample size for b
sa <- 13.4*15/14                 #Unbiased point estimator for variance of a
sb <- 16.7*15/14                 #Unbiased point estimator for variance of b
sab <- sa/sb  #Unbiased point estimator for variance of a/variance of b (Not sure?)
lb <- sab * qf(.025,14,14,log.p = F) #lower bound of Confidence interval
ub <- sab * qf(.975,14,14,log.p = F) #upper bound of Confidence interval
#95% confidence interval (0.2693878,2.390004)

sa <- 13.4*150/149               #Unbiased point estimator for variance of a
sb <- 16.7*150/149               #Unbiased point estimator for variance of b
sab <- sa/sb  #Unbiased point estimator for variance of a/variance of b (Not sure?)
lb <- sab * qf(.025,149,149,log.p = F) #lower bound of Confidence interval
ub <- sab * qf(.975,149,149,log.p = F) #upper bound of Confidence interval
#95% confidence interval (0.5812822,1.107627)
###########################

########Question 6#########
#Approach 1: Normal Approximation
n <- 20                     #Sample Size
x <- rexp(20,1/4)           #Random sample following exponential distribution
mn <- mean(x)               #Mean of the Random sample
vr <- var(x)                #Variance of Random sample
pvr <- vr*n/(n-1)           #Unbiased point estimator for variance 
lb <- mn - sqrt(vr) * qnorm(0.975) / sqrt(n)
ub <- mn + sqrt(vr) * qnorm(0.975) / sqrt(n)
print(lb)
print(ub)
print(mn)
print((lb+ub)/2)
#Approach 2: Using the fact that sum of n independent exponential distribution is Gamma
n <- 20                     #Sample Size
x <- rexp(20,1/4)           #Random sample following exponential distribution
mn <- mean(x)               #Mean of the Random sample
vr <- var(x)                #Variance of Random sample
low <-qgamma(0.025,shape =20,rate = 20/mn,lower.tail = TRUE,log.p = FALSE)
up <-qgamma(0.975,shape =20,rate = 20/mn,lower.tail = TRUE,log.p = FALSE)
print(low)
print(up)

#I will prefer approach 2 because by simulation, I can see more number of times,
#4 is contained in the 95% confidence interval for approach 2 than approach 1.
#The added information of distribution being exponential actually helps us in 
#strenthening our confidence interval.
###########################

########Question 7#########
w=integer(length=30)
w[0]<-290
w[1]<-610
w[2]<-790
w[3]<-670
w[4]<-770
w[5]<-420
w[6]<-600
w[7]<-350
w[8]<-800
w[9]<-920
w[10]<-410
w[11]<-810
w[12]<-620
w[13]<-560
w[14]<-550
w[15]<-610
w[16]<-510
w[17]<-390
w[18]<-480
w[19]<-630
w[20]<-470
w[21]<-380
w[22]<-550
w[23]<-570
w[24]<-730
w[25]<-680
w[26]<-530
w[27]<-650
w[28]<-1000
w[29]<-720
mn<-mean(w)                        #Sample mean
vr<-var(w)                         #Sample Variance
s<-vr*30/(30-1)                    #Unbiased point estimator for variance
ts<-qt(0.005, df=29, lower.tail=F) #Percent Point for T Distribution
l<-mn-(ts*sqrt(s))/sqrt(30)        #lower bound of confidence interval
u<-mn+(ts*sqrt(s))/sqrt(30)        #upper bound of confidence interval
print(l)
print(u)
#99% Confidence Interval is (493.3526,691.9808)
###########################

#########Question 8########
#Variance of two populations are not taken to be same. Though an assumption based on experience. 
u=integer(length=13)
v=integer(length=13)
u[0]<-1.79
u[1]<-1.75
u[2]<-1.67
u[3]<-1.65
u[4]<-1.87
u[5]<-1.74
u[6]<-1.94
u[7]<-1.62
u[8]<-2.06
u[9]<-1.33
u[10]<-1.96
u[11]<-1.69
u[12]<-1.70
v[0]<-2.39
v[1]<-2.51
v[2]<-2.86
v[3]<-2.14
v[4]<-2.56
v[5]<-2.29
v[6]<-2.49
v[7]<-2.36
v[8]<-2.58
v[9]<-2.33
v[10]<-2.62
v[11]<-2.41
v[12]<-1.94
n1 <- 13
n2 <- 13
umn <- mean(u)
vmn <- mean(v)
uvar <- var(u)
vvar <- var(v)
s1 <- sqrt(uvar*n1/(n1-1))
s2 <- sqrt(vvar*n2/(n2-1))
dfnumerator <- (s1*s1/n1+s2*s2/n2)**2 
dfdenominator <- ((s1*s1/n1)**2)/(n1+1)+((s2*s2/n2)**2)/(n2+1) 
dff <- dfnumerator/dfdenominator -2
equi_var <- sqrt(s1*s1/n1+s2*s2/n2)
lb <- umn-vmn - equi_var*qt(0.05, df=dff, lower.tail=F) 
ub <- umn-vmn + equi_var*qt(0.05, df=dff, lower.tail=F) 
#90% confidence interval (-1.058486,-0.1892059)
###########################
