fruitdata = read.table('fruitflies.txt', header=TRUE)
fruitdata$loglongevity = log(fruitdata$longevity)
attach(fruitdata)

#a)
boxplot(loglongevity~activity)
contrasts(activity) = contr.sum
loglongaov = lm(loglongevity~activity)
anova(loglongaov)
summary(loglongaov)

#activity heeft een significant effect F(2, 72)=19.42, p < .000.
#high conditie leeft korter (3.60212), 
#en isolated leeft langer (4.11935), 
#en low leeft ergens er tussen in (3.81437).

#b)
loglongaov2 = lm(loglongevity~thorax+activity)
anova(loglongaov2)
summary(loglongaov2)

#The effect of activity is significant F(2, 71)=25.71, p < .000.
#high conditie leeft korter, isolated langer, en low ergens er tussen in
#estimates:
#high = 3.675217
#isolated = 4.085197
#low = 3.960907

#c)
loglongaov3 = lm(loglongevity~activity*thorax)
anova(loglongaov3)
summary(loglongaov3)
plot(loglongevity~thorax, pch=unclass(activity))
abline(lm(loglongevity~thorax, data=fruitdata[fruitdata$activity == 'high', ]), col='blue')
abline(lm(loglongevity~thorax, data=fruitdata[fruitdata$activity == 'low', ]), col='red')
abline(lm(loglongevity~thorax, data=fruitdata[fruitdata$activity == 'isolated', ]), col='green')
legend(0.85, 3.5, legend=c('high', 'low', 'isolated'), col=c('blue', 'red', 'green'), lty=1)

#eerst met interactie ancova. Interactie is niet significant (p = 0.154) dus ancova zonder interactie:
loglongaov4 = lm(loglongevity~activity+thorax)
anova(loglongaov4)
summary(loglongaov4)

#thorax heeft een significant effect, F(1, 71) = 94.37, p < .000. Waarbij een longer thorax betekent langer leven.

#d)
#met thorax is beter verklaarde variantie (R2) zonder (33.2%) is veel lager dat met thorax (70.9%)
#Als je van te voren weet (literatuur) dat thorax invloed heeft dan is de eerste analyse fout
#als je dat niet weet en je wilt de invloed van sexual activity weten dan is het prieemmm

#e)
qqnorm(residuals(loglongaov4))
shapiro.test(residuals(loglongaov4))
hist(residuals(loglongaov4))
plot(fitted(loglongaov4), residuals(loglongaov4))

#niet heteroscedastisch, aangezien er geen patroon is in residuals vs fitted

#f)
longaov = lm(longevity~activity+thorax)
anova(longaov)
summary(longaov)

qqnorm(residuals(longaov))
shapiro.test(residuals(longaov))
hist(residuals(longaov))
plot(fitted(longaov), residuals(longaov))

#verdeling van residuals ziet er priem uit. Plot fitted vs residuals laat iets heteroscedastisch zien.