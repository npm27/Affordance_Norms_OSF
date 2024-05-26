####Set up####
##read in data
affs = read.csv("Data/Affordance Norms_fsg.csv")
cue_table = read.csv("Data/Cue Table.csv")
elp = read.csv("Data/ELP_Z.csv")

colnames(cue_table)[1] = "Cue"

##load libraries
library(psych)
library(dplyr)

##scientific notation
options(scipen = 999)

####Correlations####
##make the subsets
fsg = subset(affs,
             is.na(affs$FSG) == F )

cos = subset(affs,
             is.na(affs$COS) == F )

###Correlations w/ double word norms
##FSG
cor.test(fsg$AFS, fsg$FSG) #.18
cor.test(fsg$AFSS, fsg$FSG) #-.08

##COS
cor.test(cos$AFS, cos$COS) #.11
cor.test(cos$AFSS, cos$COS) #-.07

####Set up for Single Word Norms and strongest AFS Pairs####
##get the strongest affordance pairing
temp = data.frame()

cuelist = unique(affs$Cue)

for(i in cuelist){
  
  temp2 = subset(affs,
                 affs$Cue == i)
  
  temp2 = temp2[order(temp2$AFS, decreasing = TRUE),]  
  
  temp2 = temp2[1, ]
  
  temp = rbind(temp2, temp)
  
}

aff2 = temp

####Strongest AFS Pairs####
##build subsets
fsg2 = subset(aff2,
              is.na(aff2$FSG) == F)

cos2 = subset(aff2,
              is.na(aff2$COS) == F)

##merge w/ lexical variables

###Semantic Correlations
##FSG
cor.test(fsg2$AFS, fsg2$FSG) #.19
cor.test(fsg2$AFSS, fsg2$FSG) #-.04

##COS
cor.test(cos2$AFS, cos2$COS) #-.10
cor.test(cos2$AFSS, cos2$COS) #-.08

###Lexical Correlations
##strongest affordance
cue_table$Cue = tolower(cue_table$Cue)
aff2$Cue = tolower(aff2$Cue)

combined = merge(aff2, cue_table, by.x = "Cue", by.y = "Cue")

combined = combined[ , -8]
colnames(combined)[5] = "AFSS"

##set size
cor.test(combined$AFSS, combined$BOI) #.11 #BOI
cor.test(combined$AFSS, combined$Concrete) #.01 #CONCRETE
cor.test(combined$AFSS, combined$SUBTLEX) #.33 #SUBTLEX
cor.test(combined$AFSS, combined$AoA) #-.21 #AoA 
cor.test(combined$AFSS, combined$QSS) #.13 #QSS
cor.test(combined$AFSS, combined$Animacy) #.13 #QSS

#Strongest AFS
cor.test(combined$AFS, combined$BOI) #.17 #BOI
cor.test(combined$AFS, combined$Concrete) #.13 #CONCRETE
cor.test(combined$AFS, combined$SUBTLEX) #-.09 #SUBTLEX
cor.test(combined$AFS, combined$AoA) #.01 #AOA #Non-sig
cor.test(combined$AFS, combined$AFSS) #-.47
cor.test(combined$AFS, combined$QSS) #-.09
cor.test(combined$AFS, combined$Animacy) #-.30

mean(combined$AFS); sd(combined$AFS)
mean(combined$AFS); sd(combined$AFS)

##Strongest AFP
cor.test(combined$AFP, combined$BOI) #.33 #BOI
cor.test(combined$AFP, combined$Concrete) #.25 #CONCRETE
cor.test(combined$AFP, combined$SUBTLEX) #.08 #SUBTLEX
cor.test(combined$AFP, combined$AoA) #-.21 #AOA #Non-sig
cor.test(combined$AFP, combined$AFSS) #-.09
cor.test(combined$AFP, combined$QSS) #.03
cor.test(combined$AFP, combined$Animacy) #-.31

corr.test(combined[ , c(5,3,4,9,8,10,11,13)])

####Regressions####
###AFSS
##going to try w/ AOA and SUBLTEX First
#main effects only
model1 = lm(scale(AFSS) ~ scale(SUBTLEX) + scale(AoA), data = combined)

summary(model1)

#interaction
model2 = lm(scale(AFSS) ~ scale(SUBTLEX) * scale(AoA), data = combined)

summary(model2) #no interaction (not really surprising)

#what about BOI?
model3 = lm(scale(AFSS) ~ scale(BOI), data = combined)
summary(model3) #signficant, but a very small effect

#throw all of the variables in the soup and see what happens
model4 = lm(scale(AFS) ~ scale(SUBTLEX) + scale(AoA) + scale(Concrete) * scale(BOI), data = combined)
summary(model4) #SUTBLEX & AoA again are sig... BOI also comes out

#Any BOI interactions?
model5 = lm(scale(AFSS) ~ scale(SUBTLEX) * scale(AoA) * scale(BOI), data = combined) #No interactions w/ BOI
summary(model5) 

###AFS
##FSG
model6 = lm(AFS ~ FSG, data = fsg)
summary(model6) #significant, but very small. Likely because of sample size?

model7 = lm(AFS ~ FSG, data = fsg2)
summary(model7) #again, tiny effect

##COS
model8 = lm(AFS ~ COS, data = cos)
summary(model8)

model9 = lm(AFS ~ COS, data = cos2) #tiny effect
summary(model9) #non-sig

####UPDATED CORRELATIONS####
cor.test(cue_table$AoA, cue_table$BOI) #.13 #BOI
cor.test(cue_table$AoA, cue_table$Concrete) #.03 #CONCRETE
cor.test(cue_table$AoA, cue_table$SUBTLEX) #.33 #SUBTLEX
cor.test(cue_table$BOI, cue_table$AoA) #-.22 #AoA
cor.test(cue_table$BOI, cue_table$AoA) #-.22 #AoA

mean(cue_table$AoA, na.rm = T)
sd(cue_table$AoA, na.rm = T)

####LDT stuff####
elp$Word = tolower(elp$Word)
elp$I_Zscore = as.numeric(elp$I_Zscore)

elp2 = merge(cue_table, elp, by.x = "Cue", by.y = "Word")

##variables to include
#length and frequency (step 1)
#semantic variables AOA, concreteness, BOI, AFSS (Step 2)

elp2$z_length = scale(elp2$Length.x)
elp2$z_aoa = scale(elp2$AoA)
elp2$z_BOI = scale(elp2$BOI)
elp2$z_con = scale(elp2$Concrete)
elp2$z_SUBTLEX = scale(elp2$SUBTLEX)
elp2$z_AFSS = scale(elp2$AFSS)

#step 1
ldt.1 = lm(I_Zscore ~ z_length + z_SUBTLEX, data = elp2)
summary(ldt.1)

#step 2
ldt.2 = lm(I_Zscore ~ z_length + z_SUBTLEX +
             z_aoa + z_BOI + z_con + z_AFSS, data = elp2)
summary(ldt.2)

##AFSS not a good predictor

####What about overlap measures?####
elp3 = merge(elp2, fsg2, by.x = "Cue", by.y = "Cue")

elp4 = na.omit(elp3)
