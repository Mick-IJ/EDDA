data = read.table('psi.txt', header=TRUE)
data$passed = as.factor(data$passed)
data$psi = as.factor(data$psi)
attach(data)

#a)
boxplot(gpa~passed, xlab='Pass', main='Mean GPA')
round(xtabs(~psi+passed) / c(sum(psi==0), sum(psi==1)), 2)
xtabs(~round(gpa)+passed+psi,data=data)
hist(gpa, breaks=7)

#b)
aov = glm(passed~gpa+psi, family=binomial)
summary(aov)

#The effect of psi is significant, p = 0.025. 
#The direction of this effect is that psi increases 
#the probability of getting a passing grade.
#Thus psi seems to work.

#c)
predict(aov, newdata=data.frame(psi=factor(c(0, 1)), gpa=c(3,3)), type="response")

#The probability of passing for a student with gpa 3 and no psi is 8.2%.
#For a student who did receive psi the probability is 48.2%.

#d)
exp(2.338)

#The relative change in odds when receiving psi is 10.36.
#This is not dependent on gpa.

#e)
fisher.test(matrix(c(3,15,8,6),2,2))

#The 15 are the number of students who did not receive psi and did not improve.
#The 8 are the number of students who did receive psi, but did not improve.
#The conclusion is that p1 != p2 (p = 0.027).
#The odds ratio is not equal to 1.
#The odds ratio for the people who dont receive psi is given by 95%BI[0.02, 0.96].
#Students who don't receive psi have a lower chance to improve.

#f)

#The fisher test is the wrong test to use given the experimental design.
#Contingency tables could be used when (a) a random sample is drawn from a population,
#(b) when a random sample is drawn for each level of the first factor, or
#(c) when a random sample is drawn for each level of the second factor.
#The current design doesn't include a random sample, since all students either do
#or do not receive psi. Therefore, a logistic regression is the correct analysis.


#g)

