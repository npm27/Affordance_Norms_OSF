####Load the data####
dat = read.csv("Data/Affordance Norms Final.csv")
ugrad = read.csv("Data/ugrad_afss.csv")
prolific = read.csv("Data/prolific_afss.csv")

mean(dat$AFS); sd(dat$AFS); min(dat$AFS); max(dat$AFS)
mean(dat$AFP); sd(dat$AFP); min(dat$AFP); max(dat$AFP)
mean(dat$AFSS); sd(dat$AFSS); min(dat$AFSS); max(dat$AFSS)
