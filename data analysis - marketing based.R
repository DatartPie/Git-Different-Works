
#data analysis - marketing analytics based

metadat=read.csv("variables lifestyle.csv", ,header = TRUE)
crosstab1 = read.csv("lifestyle.csv", ,header = TRUE)
summary(crosstab1)
summary(metadat)
View(crosstab1)
crosstab2=t(table(crosstab1[,2], crosstab1[,17]))
View(crosstab2)
#crosstab analysis using gmodels

library(gmodels)
net_dome=crosstab1[,2]
emplm=crosstab1[,17]

net_dome[net_dome==1] = "Definetely Diagree"
net_dome[net_dome==2] = "Generally Diagree"
net_dome[net_dome==3] = "Moderately Diagree"
net_dome[net_dome==4] = "Moderately Agree"
net_dome[net_dome==5] = "Generally Agree"
net_dome[net_dome==6] = "Definetely Agree"

emplm[emplm==1] = "Full time or Self-Employed"
emplm[emplm==2] = "Part-time"
emplm[emplm==3] = "Retired"
emplm[emplm==4] = "Not Employed"

#dimnames(net_dome)<-list(c("Definetely Diagree", "Generally Disagree", "Moderately Disagree", "Moderately Agree", "Generally Agree", "Definetely Agree"))
#dimnames(emplm)<-list(c("Full time", "Self-Employed", "Part-time", "Retired", "Not Employed"))
CrossTable(net_dome, emplm, chisq = TRUE, expected = TRUE,  dnn=c("net_dome","emplm"))
chisq.test(crosstab2)

#deathpen - age

View(crosstab1)
death_pen<-crosstab1[,6];
res_age<-crosstab1[,15];

cor(death_pen,res_age)
#corr using package hmisc
#install.packages(Hmisc)
install.packages('Hmisc', repos='http://cran.us.r-project.org')
library(Hmisc)
rcorr(cbind(death_pen, res_age), type="pearson")

#bivariate regression
Y=crosstab1[,2]
#net_dome=as.numeric(crosstab1[,2])
res_age=as.numeric(crosstab1[,15])

bi_mod<-lm(Y ~ res_age)
summary(bi_mod)

1.185 + (70 * 0.042)
#gun and age
Y=crosstab1[,8]
res_age=as.numeric(crosstab1[,15])
bi_mod01<-lm(Y ~ res_age)
summary(bi_mod01)

#gun and gender
Y=crosstab1[,8]
gen=as.numeric(crosstab1[,19])
bi_mod1<-lm(Y ~ gen)
summary(bi_mod1)

#liberal - gun, age, politics
Y=crosstab1[,14]
gun=as.numeric(crosstab1[,8])
res_age=as.numeric(crosstab1[,15])
poli=as.numeric(crosstab1[,11])
bi_mod3<-lm(Y ~ gun + res_age + poli)
summary(bi_mod3)
2.93 + (-0.0057*30) + (-0.045*1) + (0.0214*1)

newdata = read.csv("experiment_price_ad.csv", ,header = TRUE)
View(newdata)

#sales and adv
Y=newdata[,1]
ad=as.numeric(newdata[,3])
bi_mod4<-lm(Y~ad)
summary(bi_mod4)

#sales, adv and price
Y=newdata[,1]
ad=as.numeric(newdata[,3])
price=as.numeric(newdata[,2])
bi_mod5<-lm(Y~ad+price)
summary(bi_mod5)

#sales, adv, price and store size
Y=newdata[,1]
ad=as.numeric(newdata[,3])
price=as.numeric(newdata[,2])
store_size=as.numeric(newdata[,4])
bi_mod6<-lm(Y~ad+price+store_size)
summary(bi_mod6)

207.082 + (74.219) + (24* (-10.645)) + (32*7.884)

#dummy var as price model
newdata$newprice<-factor(newdata$Price)
View(newdata)
Y=newdata[,1]
ad=as.numeric(newdata[,3])

store_size=as.numeric(newdata[,4])

bi_mod7<-lm(Y~ad+newdata$newprice+store_size)
summary(bi_mod7)




