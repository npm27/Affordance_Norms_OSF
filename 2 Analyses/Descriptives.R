####Load the data####
dat = read.csv("Data/Affordance Norms Final.csv")
ugrad = read.csv("Data/ugrad_afss.csv")
prolific = read.csv("Data/prolific_afss.csv")

mean(dat$AFS); sd(dat$AFS); min(dat$AFS); max(dat$AFS)
mean(dat$AFP); sd(dat$AFP); min(dat$AFP); max(dat$AFP)
mean(dat$AFSS); sd(dat$AFSS); min(dat$AFSS); max(dat$AFSS)

####Make example for talk####
temp = subset(dat,
             dat$cue == "string")
max(temp$AFP)

string = temp$AFS
hist(string)
max(string)

temp = subset(dat,
             dat$cue == "acorn")
max(temp$AFP)

acorn = temp$AFS
hist(acorn)
max(acorn)

temp = subset(dat,
              dat$cue == "fabric")
max(temp$AFP)

fabric = temp$AFS
hist(fabric)
max(fabric)
