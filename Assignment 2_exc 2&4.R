data = read.table(file="search.txt",header=TRUE)
data$skill = factor(data$skill)
data$interface = factor(data$interface)

#a) Show code and output
rbind(c(replicate(5, sample(1:3))),rep(1:5,each=3), rep(1:15)) 

#A randomizied block design was used to distribute the 15 students over the 3 possible interfaces. 
#Because both the student ID and the skill level are fixed, only the interface had to be evenly distributed over the students. 
#The code below/above shows that step. There are three students in each skill-level category, 
#and since there are 3 possible interfaces, every one of those three students should be assigned a different interface. 
#In the output the randomized assignment of students to each interface is shown. 
#The first row indicates the interface, the second row shows the skill level of the students. 
#And the third row represents the ids of the students. Each column corresponds to a student.


#b) Show only the output
attach(data)
par(mfrow=c(1,2)); hist(time); qqnorm(time)

par(mfrow=c(2,2))
boxplot(time~skill); boxplot(time~interface)
interaction.plot(skill, interface, time, col = c('red', 'green', 'blue'))
interaction.plot(interface, skill, time, col = c('red', 'green', 'blue', 'yellow', 'black'))

# First the distribution of the dependent variable is shown in a histogram and qq-plot.
# The data looks normally distributed. Next two boxplots are created to visualize the effect that the indpendent variables
# skill and interface have on the time required to finish the task. Overall the students with a lower skill level (better skills)
# are quicker than students with a high skill rank. Furthermore, the use of interface 1 seems to result in lower times.
# Lastly two interaction plots are created. These show that there are no obvious interaction effects. 
# However, the previous statements are made based on the visualization of the data. No test have been used to underline these
# statements. Lastly, any possible interaction effects cannot be reliably tested, because there is only one observation
# for each combination of factors.


#c) Show only the output of anova
contrasts(data$skill) = contr.sum
contrasts(data$interface) = contr.sum

dataaov = lm(time~interface+skill,data=data)  # balanced so order doesnt matter
anova(dataaov)
summary(dataaov)

# To test whether interface has a main effect on the time it takes to complete the task an anova was carried out with
# both interface and skill as independent variables. No interaction effect was taken into account. Results show that both
# interface, F(2,8)=7.82, p = 0.013, and skill, F(4,8)=6.21, p = 0.014, have a significant main effect. 
# Therefore the search time is not equal for all of the interfaces.To compute the estimated time it takes for a user with 
# skill level 3 who uses interface 2 the summary of the anova was used. This results in an estimated time of 
# (20.5467 + 0.3133 + -0.1133) 20.7467.


#d) Show only the plots
par(mfrow=c(1,2)); qqnorm(residuals(dataaov)); plot(fitted(dataaov), residuals(dataaov))
shapiro.test(residuals(dataaov))

# The model assumptions were checked by looking at both the normality of the residuals and a plot of 
# the fitted values against the residuals. The residuals seem to be normally distributed. This was confirmed by
# a Shapiro-Wilk normality test, W=0.93, p = 0.282. The plot of the fitted values against the residuals shows that
# there is no systematic error.

#e) Only text
friedman.test(time, interface, skill) # interface first because that is the effect we want

# A non-parametric Friedman test was carried out to test whether there is an effect of interface. Results show that
# interface has a significant influence on the time it takes to complete the task, X^2(2)=6.4, p = 0.041.

#f)
dataaov2 = lm(time~interface, data=data)
anova(dataaov2)
summary(dataaov2)

# When carrying out an analysis of variance with only the interface as independent variable, the results indicate
# that there is no significant effect of interface, F(2,12)=2.86, p = 0.096 on the time it takes to complete the task.
# It could make sense to carry out this test, however in the current context it is wrong. This is because earlier tests,
# as well as the visualization of the data indicate an significant effect of skill. Therefore, it is unwise to remove this
# factor from the analysis. For this test to be valid, tests and visualization should have indicated that there is no
# effect of skill on the dependent variable. If there is no effect of skill it would not matter if it was used as a factor
# in the analysis. Since this is no the case, it is unwise to carry out and interpret the results of this analysis.


## Question 4
nausea = read.table(file="nauseatable.txt",header=TRUE)

#a) show code data.frame and output xtabs
nauseadf = data.frame(naus=c(rep('no', each=180), rep('yes', each=124)), 
           medicin=c(rep('chlor', each=100), rep('pent100', each=32), rep('pent150', each=48),
                     rep('chlor', each=52), rep('pent100', 35), rep('pent150', 37)))

attach(nauseadf)
xtabs(~medicin+naus)

# The dataframe is created by first creating a column with the number of patients with nausea (180) and without (124).
# Next the column for medicin is created. This is done by repeating the type of medicin a specified number of times. The
# number of repititions is equal to the number of patients that took a certain medicin and did not have nausea.


#b) Show The code for permutation test
B = numeric(1000)
for (i in 1:length(B)){
  nauseadf2 = transform(nauseadf, medicin=sample(medicin))
  B[i] = chisq.test(xtabs(~nauseadf2$medicin+nauseadf2$naus))[[1]]
}

chi_contingency = chisq.test(xtabs(~medicin+naus))[[1]]
p_permutation = mean(B>chi_contingency)

# To perform a permutation test, the labels were shuffled 1.000 times. After every shuffle the chi value was computed
# and saved. If the null-hypothesis is true, then the chi value of the original dataset would be a probable outcome.
# Therefore the chi value from the original dataset was compared to the chi-values of the 1.000 permutations. The original
# chi value was 6.62. Only 3.6% of the permuted chi values were larger than 6.62. Therefore the null-hypothesis can be rejected,
# and the conclusion is that medicin and nausea are not independent.


#c) Show the histogram

par(mfrow=c(1,1)); hist(B)
p_contingency = chisq.test(xtabs(~medicin+naus))[[3]]

# When performing a chi-square test for contingency tables, the outcome is similar. Medicin and reported nausea seem to be
# related, X^2(2)=6.62, p = 0.036. This makes sense, because the resulting chi-values of the permutations is more or less
# equal to the true chi-distribution. This is illustrated by the histogram. With the permutation test, chi-values are computed
# under the assumption that the null-hypothesis is true. This results in the actual chi-distribution. Therefore, the p-values
# from permutation and contingency tables are very similar.
