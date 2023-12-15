##set up
dat = read.csv("Data/Affordance Norms_fsg.csv")

fsg = dat[ , -7]
fsg = na.omit(fsg)

cos = dat[ , -6]
cos = na.omit(cos)

library(psych)

options(scipen = 999)

#get overlapping pair percentages
nrow(fsg)/nrow(dat) * 100

nrow(cos)/nrow(dat) * 100

corr.test(fsg[ , c(3,4,6)])
corr.test(cos[ , c(3,4,6)])

cor.test(fsg$AFS, fsg$FSG)
cor.test(cos$AFS, cos$COS)

cor.test(fsg$AFP, fsg$FSG)
cor.test(cos$AFP, cos$COS)

