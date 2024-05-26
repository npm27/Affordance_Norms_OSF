##Read in data
dat = read.csv("Animacy.csv")

library(psych)

options(scipen = 999)

cor.test(dat$QSS, dat$Living) #.14
cor.test(dat$AFSS, dat$Person) #.15
cor.test(dat$AFSS, dat$Move) #.15

mean(dat$AFSS)

#living
dat.high = subset(dat,
             dat$Living > 400)
dat.low = subset(dat,
                 dat$Living < 400)

mean(dat.high$AFSS)
mean(dat.low$AFSS)

temp = t.test(dat.high$AFSS, dat.low$AFSS, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Signficant

#person
dat.high2 = subset(dat,
                  dat$Person > 400)
dat.low2 = subset(dat,
                 dat$Person < 400)

mean(dat.high2$AFSS)
mean(dat.low2$AFSS)

#movement
dat.high3 = subset(dat,
                   dat$Move > 400)
dat.low3 = subset(dat,
                  dat$Move < 400)

mean(dat.high3$AFSS)
mean(dat.low3$AFSS)

##overall
mean(dat$Living, na.rm = T); sd(dat$Living, na.rm = T)
