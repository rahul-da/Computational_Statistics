#COMPUTATIONAL STATISTICS Assignment 2
#############Question 1###################
df<-read.csv("a2q1.csv")

l<-aov(df$Conductivity ~ df$Coating_Type)
summary(l)  #ANOVA TABLE FOR THE DATASET


print(mean(df$Conductivity))
#Estimates of Overall Mean and Treatment Effects are summarised below:
#Overall Mean Estimate = 139.25
#Treatment effect 1 (tau1) estimate = 5.75
#Treatment effect 2 (tau2) estimate = 6
#Treatment effect 3 (tau3) estimate = -7.75
#Treatment effect 4 (tau4) estimate = -10
#Treatment effect 5 (tau1) estimate = 6
#Please note that sum of estimators of Treatment Effects is 0

#Data for Coating Type 1 is 143, 141, 150, 146
#Unbiased Point Estimator of Variance is Mean Square Error ~ chiSq(15 dof) and equals 16.22
#Therefore, the t-test for 95% CI estimate of coating type 1 has 15 dof
print(qt(0.025, df=15, lower.tail=F))  
# 95 % Confidence Interval is (5.75-2.13145*sqrt(16.22/4),5.75+2.13145*sqrt(16.22/4))  

#Unbiased point estimator for difference of types 2 and 4 is y4.bar - y2.bar
#99% Confidence Interval is (129.25-145.25-2.946713*sqrt(2*16.22/4),129.25-145.25+2.946713*sqrt(2*16.22/4))

#RESIDUAL ANALYSIS
eruption.lm = lm(df$Conductivity ~ df$Coating_Type, data=df) 
eruption.res = resid(eruption.lm) 
plot.default(df$Coating_Type, eruption.res,ylab="Residuals", xlab="Coating Type")
abline(0,0)
#plot(df$Coating_Type, eruption.res)
#plot.default(df$Coating_Type, eruption.res)
#According to me, levels are independent of each other and the expetiment is performed well.

#Design of Tukey's Test for checking pairwise variance


#Since the point estimate of level 4 is the least, we should say the manufacturer should stick to level 4 in manufacturing



###########Question 2##############
df<- read.csv("a2q2.csv")

l<-aov(df$CalciumContent ~ df$Batch)
summary(l)  #ANOVA TABLE FOR THE DATASET
#Critical Region for the test is (2.886,inf). Test Statistic value is 5.641.
#Therefore we reject the hypothesis "Calcium Content does not depend on Batches".
#Hence calcium content depends on the batch process. 

#RESIDUAL ANALYSIS
eruption.lm = lm(df$CalciumContent ~ df$Batch, data=df) 
eruption.res = resid(eruption.lm) 
plot.default(df$Batch, eruption.res,ylab="Residuals", xlab="Batch")
abline(0,0)
#Since Residual Analysis is a qualititative test, looking at the residual plot, it seems that 
#normality and independence assumption holds true for the data.

#Analysis for equality of variances
#Bartlett test of homogeneity of variances
#data:  df$CalciumContent and df$Batch
#Bartlett's K-squared = 0.039349, df = 4, p-value = 0.9998
#Clearly from the bartlett test we cannot reject the hypothesis of equality of variances.
#Infact such a high p value makes us think variances among levels is the same.
#Therefore all assumptions of Model for One Way ANOVA Hold true.

#Confidence Interval for Batch 3


#Condidence Interval for Batch 5



#############Question 3##############
#Proof see text and solution by me.

############Question 4###############
df<-read.csv("a2q4.csv")

boxplot(df$Noise~df$CircuitDesign,data=df, main="Noise box plot",xlab="Circuit Design", ylab="NOISE") 

l<-aov(df$Noise ~ df$CircuitDesign)
print(summary(l))  #ANOVA TABLE FOR THE DATASET
#Hypothesis is Rejected of Equality of means for noise in different Circuit Designs therefore 
#the amount of noise is not the same for all four designs.

#RESIDUAL ANALYSIS
eruption.lm = lm(df$Noise ~ df$CircuitDesign, data=df) 
eruption.res = resid(eruption.lm) 
plot.default(df$CircuitDesign, eruption.res,ylab="Residuals", xlab="Circuit Design")
abline(0,0)
#Since Residual Analysis is a qualititative test, looking at the residual plot, it seems that 
#normality and independence assumption holds true for the data. One outlier is present clearly
#for Circuit Design 4.

#From the modelling, I conclude that Circuit Design 1 has lowest amount of noise 
#Therefore I recommend Circuit Design 1.


#Tukey Test (Special Attention needed)
l<-aov(df$Conductivity ~ df$Coating_Type)
#a1 <- aov(chocolate$Sabor ~ chocolate$Tipo + chocolate$Provador)
#posthoc <- TukeyHSD(x=a1, 'chocolate$Tipo', conf.level=0.95)
posthoc <- TukeyHSD(x=l, 'df$Coating_Type', conf.level=0.95)
#plot(l)


l<-aov(df$CalciumContent ~ df$Batch)
posthoc <- TukeyHSD(x=l, 'df$Batch', conf.level=0.95)
