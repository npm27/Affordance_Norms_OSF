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
             dat$cue == "bird")
max(temp$AFP)

bird = temp$AFS
hist(bird)
max(bird)

temp = subset(dat,
              dat$cue == "dog")
max(temp$AFP)

dog = temp$AFS
hist(dog)
max(dog)
