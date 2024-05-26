##set up
dat = read.csv("Cue Table.csv")

dat2 = read.csv("Animacy/VanArsdall_Blunt_NormData.csv")

dat3 = merge(dat, dat2, by.x = "Cue", by.y = "Word", all.x = T, all.y = F)

#write.csv(dat3, file = "Cue Table 2.csv", row.names = F)
