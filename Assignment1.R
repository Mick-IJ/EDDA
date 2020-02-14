## ASSIGNMENT 1
#EXERCISE 1
nuseq=seq(175,185,by=0.25)

#a)
n=m=30; mu=180; sd=5
A = numeric(length(nuseq))
for (i in 1:length(A)){
  D = numeric(1000)
  nu = nuseq[i]
  for (j in 1:1000){
    x = rnorm(n, mu, sd); y = rnorm(m, nu, sd)
    D[j] = t.test(x, y, var.equal=TRUE)[[3]]
  }
  A[i] = mean(D < 0.05)
}

plot(nuseq,A,main="Exercise 1",xlab="nu",ylab="Power",type='l',lwd=2,col="blue")

#b)
n=m=100; mu=180; sd=5
B = numeric(length(nuseq))
for (i in 1:length(B)){
  D = numeric(1000)
  nu = nuseq[i]
  for (j in 1:1000){
    x = rnorm(n, mu, sd); y = rnorm(m, nu, sd)
    D[j] = t.test(x, y, var.equal=TRUE)[[3]]
  }
  B[i] = mean(D < 0.05)
}

lines(nuseq,B,type='l',lwd=2,col="red")

#c)
n=m=30; mu=180; sd=15
C = numeric(length(nuseq))
for (i in 1:length(C)){
  D = numeric(1000)
  nu = nuseq[i]
  for (j in 1:1000){
    x = rnorm(n, mu, sd); y = rnorm(m, nu, sd)
    D[j] = t.test(x, y, var.equal=TRUE)[[3]]
  }
  C[i] = mean(D < 0.05)
}

lines(nuseq,C,type='l',lwd=2,col="green")

legend(178.7, 0.9, c('a) n = 30, sd = 5', 'b) n = 100, sd=5', 'c) n = 30, sd = 15'), 
       lwd=c(5,3), col=c("blue", "red", "green"))


#EXERCISE 2
#load data and compute to km/sec from the same scale
light1879= read.table("light1879.txt",header=TRUE)
all1879 = c(light1879$X850, light1879$X740, light1879$X900, light1879$X1070, light1879$X930) +299000
light1882 <- read.table("light1882.txt", header = FALSE, fill=TRUE)
all1882 = c(light1882$V1, light1882$V2, light1882$V3, light1882$V4, light1882$V5) +299000
all1882 <- all1882[!is.na(all1882)] #remove NA
light = read.table('light.txt', header=TRUE)
light = 7442000/(24.8+(light$X28/1000))

#plot histograms, boxplot, qqplot
par(mfrow=c(3,3))
hist(all1879, xlim = c(299400, 300500))
hist(all1882, xlim = c(299400, 300500))
hist(light, xlim = c(299400, 300500))

boxplot(all1879,ylim = c(299400, 300500))
boxplot(all1882, ylim = c(299400, 300500))
boxplot(light, ylim = c(299400, 300500))

qqnorm(all1879)
qqnorm(all1882)
qqnorm(light)

#compute confidence intervals, use population means
# current speed of light: 299,792 kilometers per second-> compare
all1879quant25 = quantile(all1879, 0.025)
all1879quant975 = quantile(all1879, 0.975)
ci1879 = c(2*mean(all1879) - all1879quant975, 2*mean(all1879) - all1879quant25)

all1882quant25 = quantile(all1882, 0.025)
all1882quant975 = quantile(all1882, 0.975)
ci1882 = c(2*mean(all1882) - all1882quant975, 2*mean(all1882) - all1882quant25)

light25 = quantile(light, 0.025)
light975 = quantile(light, 0.975)
ciLight = c(2*mean(light) - light975, 2*mean(light) - light25)



# EXERCISE 3
telephone = read.table('telephone.txt', header=T)
med = median(telephone$Bills)
par(mfrow = c(1,3))
hist(telephone$Bills) # inconsistency: weird shape
boxplot(telephone$Bills)
qqnorm(telephone$Bills)

#b)
par(mfrow = c(1,1))
lamdaseq = seq(0.01, 0.1, by = 0.002)
A = numeric(length(lamdaseq))

for (i in 1:length(lamdaseq)){
  B = numeric(1000)
  lambda = lamdaseq[i]
  for (j in 1:length(B)){
    sample = rexp(1000, lambda)
    B[j] = median(sample)
  }
  
  A[i] = (length(B[B<med])/length(B))
}

plot(lamdaseq, c(diff(A), 0), type='l')


# Construct a 95% bootstrap confidence interval for the population median of the sample.
#computing surrogate dataset, compute T for surrogate dataset
B =1000 
Tstar=numeric(B) 
for(i in 1:B) {
  Xstar=sample(telephone$Bills,replace=TRUE) 
  Tstar[i]=median(Xstar) 
}
#95% confidence interval
Tstar25=quantile(Tstar,0.025)
Tstar975=quantile(Tstar,0.975)
ciTstar = c(2*median(Tstar)-Tstar975, median(Tstar), 2*median(Tstar)-Tstar25)

# construct 95% confidence interval from exp. distr. lambda=0.26
B = numeric(1000)
for (j in 1:length(B)){
  sample = rexp(200, 0.026)
  B[j] = median(sample)
}

B25 = quantile(B, 0.025)
B975 = quantile(B, 0.975)
med = median(B)

ciB = c(2*med - B975, 2*med - B25)


# test median bill >40 from surrogate dataset
pl = sum(Tstar<40)/B
pr= sum(Tstar>=40)/B
p = 2*(min(pl,pr))

# test percentage bill <10 at most 25% from surrogate dataset
B =1000 
Tstar=numeric(B) 
for(i in 1:B) {
  Xstar=sample(telephone$Bills,replace=TRUE) 
  Tstar[i]=mean(Xstar < 10) 
}

# test median bill <10 
pl = sum(Tstar<0.25)/B
pr= sum(Tstar>=0.25)/B
p = 2*(min(pl,pr))
mean(Tstar)

## exercise 4
run = read.table('run.txt', header=TRUE)
#a)
cor.test(run$before,run$after) #Conclusion: there is significant correlation, if normality is assumed.
par(mfrow=c(1, 2))
qqnorm(run$before, main = "qq plot before") #Check the normality assumption on the two samples
qqnorm(run$after, main = "qq plot after") #conclusion: normality assumed

#b)
par(mfrow=c(1, 1))
t.test(run$before[1:12], run$after[1:12], paired = T) #softdrink
qqnorm(run$before[1:12]-run$after[1:12]) #Check the normality assumption on the differences, normal?
#Conclusion: p = 0.4373, so no difference in the soft drink condition
t.test(run$before[13:24], run$after[13:24], paired = T) #energydrink
qqnorm(run$before[13:24]-run$after[13:24]) #not normal?
#Conclusion: p = 0.1264, also no difference in energy drink condition

#c)
difference = run$before - run$after #positive values mean faster times
t.test(difference[1:12], difference[13:24])
#Conclusion: p = 0.1586, so the drinks do not affect difference in times

#d)?


## EXERCISE 5

#a)
t.test(chickwts$weight[which(chickwts$feed=='sunflower')], 
       chickwts$weight[which(chickwts$feed=='meatmeal')])
#Conclusion: p = 0.044, so there is a significant difference --> meatmeal lower weight

wilcox.test(chickwts$weight[which(chickwts$feed=='sunflower')], 
            chickwts$weight[which(chickwts$feed=='meatmeal')])
#Conclusion: p = 0.069, so there is no significant difference

ks.test(chickwts$weight[which(chickwts$feed=='sunflower')], 
        chickwts$weight[which(chickwts$feed=='meatmeal')])
#Conclusion: p = 0.109, so there is no significant difference

#plot for KS
par(mfrow=c(1,2))
hist(chickwts$weight[which(chickwts$feed=='sunflower')])
hist(chickwts$weight[which(chickwts$feed=='meatmeal')])
par(mfrow=c(1,1))
plot(ecdf(chickwts$weight[which(chickwts$feed=='sunflower')]), col='red', 
     main='Cumulative density of feed types', xlab='Weight', ylab='Cumulative Probability')
lines(ecdf(chickwts$weight[which(chickwts$feed=='meatmeal')]), col='blue')
legend(400, 0.4, c('sunflower', 'meatmeal'), 
       lwd=c(4,3), col=c("red", "blue"))

#b)
feedtypes = c('sunflower', 'meatmeal', 'horsebean', 'linseed', 'soybean', 'casein')
contrasts(chickwts$feed) = contr.sum
chickwtsaov = lm(weight~feed, data=chickwts)
anova(chickwtsaov)
summary(chickwtsaov)

par(mfrow=c(2,3))
for (i in 1:6){
  data = chickwts$weight[which(chickwts$feed==feedtypes[i])]
  boxplot(data, main=feedtypes[i], ylim=c(150,400))
  }

#c)

