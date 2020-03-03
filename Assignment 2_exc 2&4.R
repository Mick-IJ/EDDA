data = read.table(file="search.txt",header=TRUE)
data$skill = factor(data$skill)
data$interface = factor(data$interface)

#a)
rbind(c(replicate(5, sample(1:3))),rep(1:5,each=3), rep(1:15)) 

#Interface, Skill, ID

#b)
attach(data)
boxplot(time~skill)
boxplot(time~interface)

interaction.plot(skill, interface, time, col = c('red', 'green', 'blue'))
interaction.plot(interface, skill, time, col = c('red', 'green', 'blue', 'yellow', 'black'))

# There doesnt seem to be any interactions. Furthermore, they can't be tested with this data 
# due to the fact that there is only one observation per factor-combination.

#c)
contrasts(data$skill) = contr.sum
contrasts(data$interface) = contr.sum

dataaov = lm(time~interface+skill,data=data)
anova(dataaov)
summary(dataaov)

# Search time is significantly lower for the first interface.
# skill 3 interface 2: 20.5467 + 0.3133 - 0.1133 = 

#d)
qqnorm(residuals(dataaov)) 
shapiro.test(residuals(dataaov))
plot(fitted(dataaov), residuals(dataaov))

# Looks kinda normal. Shapiro says also normal.

#e)
friedman.test(time, interface, skill) # interface first because that is the effect we want

# p < 0.05 so there is a significant effect of interface.

#f)
dataaov2 = lm(time~interface, data=data)
anova(dataaov2)
summary(dataaov2)

# There seems to be no effect from the interface in that case. The question itself is not useful 
# because we have seen that skill has a significant effect on time. Therefore, it is unwise to remove 
# this factor in the analysis. If skill would have a nonsignificant effect it would have been fine 
# to not take skill into account.


## Question 4
nausea = read.table(file="nauseatable.txt",header=TRUE)
nauseadf = data.frame(naus=c(rep('no', each=180), rep('yes', each=124)), 
           medicin=c(rep('chlor', each=100), rep('pent100', each=32), rep('pent150', each=48),
                     rep('chlor', each=52), rep('pent100', 35), rep('pent150', 37)))

attach(nauseadf)
xtabs(~medicin+naus)


B = numeric(1000)
for (i in 1:length(B)){
  nauseadf2 = transform(nauseadf, medicin=sample(medicin))
  chi = chisq.test(xtabs(~nauseadf2$medicin+nauseadf2$naus))[[1]]
  B[i] = chi
}

chi_contingency = chisq.test(xtabs(~medicin+naus))[[1]]
p_contingency = chisq.test(xtabs(~medicin+naus))[[3]]

p_permutation = mean(B>chi_contingency)


