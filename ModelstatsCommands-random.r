#MODEL STATISTICS

### Ind v. eCnj
ii.ive.observed.my.rand <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my.rand <- ifelse(as.vector(fitted(ii.ive.glmer.rand))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my.rand,ii.ive.predicted.my.rand)
ii.ive.p.values.my.rand <- cbind(as.vector(fitted(ii.ive.glmer.rand)), 1-as.vector(fitted(ii.ive.glmer.rand)))
colnames(ii.ive.p.values.my.rand) <- c("Ind","eCnj")
ii.ive.ModelStats.my.rand<- model.statistics(ii.ive.observed.my.rand, ii.ive.predicted.my.rand, ii.ive.p.values.my.rand)

ai.ive.observed.my.rand <- ifelse(subset(AWive, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.ive.predicted.my.rand <- ifelse(as.vector(fitted(ai.ive.glmer.rand))>0.5, "Ind", "eCnj")
table(ai.ive.observed.my.rand,ai.ive.predicted.my.rand)
ai.ive.p.values.my.rand <- cbind(as.vector(fitted(ai.ive.glmer.rand)), 1-as.vector(fitted(ai.ive.glmer.rand)))
colnames(ai.ive.p.values.my.rand) <- c("Ind","eCnj")
ai.ive.ModelStats.my.rand<- model.statistics(ai.ive.observed.my.rand, ai.ive.predicted.my.rand, ai.ive.p.values.my.rand)

ti.ive.observed.my.rand <- ifelse(subset(AWive, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.ive.predicted.my.rand <- ifelse(as.vector(fitted(ti.ive.glmer.rand))>0.5, "Ind", "eCnj")
table(ti.ive.observed.my.rand,ti.ive.predicted.my.rand)
ti.ive.p.values.my.rand <- cbind(as.vector(fitted(ti.ive.glmer.rand)), 1-as.vector(fitted(ti.ive.glmer.rand)))
colnames(ti.ive.p.values.my.rand) <- c("Ind","eCnj")
ti.ive.ModelStats.my.rand<- model.statistics(ti.ive.observed.my.rand, ti.ive.predicted.my.rand, ti.ive.p.values.my.rand)

ta.ive.observed.my.rand <- ifelse(subset(AWive, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.ive.predicted.my.rand <- ifelse(as.vector(fitted(ta.ive.glmer.rand))>0.5, "Ind", "eCnj")
table(ta.ive.observed.my.rand,ta.ive.predicted.my.rand)
ta.ive.p.values.my.rand <- cbind(as.vector(fitted(ta.ive.glmer.rand)), 1-as.vector(fitted(ta.ive.glmer.rand)))
colnames(ta.ive.p.values.my.rand) <- c("Ind","eCnj")
ta.ive.ModelStats.my.rand<- model.statistics(ta.ive.observed.my.rand, ta.ive.predicted.my.rand, ta.ive.p.values.my.rand)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-ive-ModelStats.my.rand.txt')
print(ii.ive.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-ive-ModelStats.my.rand.txt')
print(ai.ive.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-ive-ModelStats.my.rand.txt')
print(ti.ive.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-ive-ModelStats.my.rand.txt')
print(ta.ive.ModelStats.my.rand, max.print = NA)
sink(NULL)








#================================================
### eCnj v. kaaCnj v. OtherCnj
#================================================

ii.ive.observed.my.rand <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my.rand <- ifelse(as.vector(fitted(ii.ive.glmer.rand))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my.rand,ii.ive.predicted.my.rand)
ii.ive.p.values.my.rand <- cbind(as.vector(fitted(ii.ive.glmer.rand)), 1-as.vector(fitted(ii.ive.glmer.rand)))
colnames(ii.ive.p.values.my.rand) <- c("Ind","eCnj")
ii.ive.ModelStats.my.rand<- model.statistics(ii.ive.observed.my.rand, ii.ive.predicted.my.rand, ii.ive.p.values.my.rand)

ii.eCnj.cnjtypes.observed.my.rand <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ii.eCnj.cnjtypes.predicted.my.rand <- ifelse(as.vector(fitted(ii.e.cnjtype.glmer.rand))>0.5, "eCnj", "otherCnj")
table(ii.eCnj.cnjtypes.observed.my.rand,ii.eCnj.cnjtypes.predicted.my.rand)
ii.eCnj.cnjtypes.p.values.my.rand <- cbind(as.vector(fitted(ii.e.cnjtype.glmer.rand)), 1-as.vector(fitted(ii.e.cnjtype.glmer.rand)))
colnames(ii.eCnj.cnjtypes.p.values.my.rand) <- c("eCnj","otherCnj")
ii.eCnj.cnjtypes.ModelStats.my.rand<- model.statistics(ii.eCnj.cnjtypes.observed.my.rand, ii.eCnj.cnjtypes.predicted.my.rand, ii.eCnj.cnjtypes.p.values.my.rand)

ii.kaaCnj.cnjtypes.observed.my.rand <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ii.kaaCnj.cnjtypes.predicted.my.rand <- ifelse(as.vector(fitted(ii.kaa.cnjtype.glmer.rand))>0.5, "kaaCnj", "otherCnj")
table(ii.kaaCnj.cnjtypes.observed.my.rand,ii.kaaCnj.cnjtypes.predicted.my.rand)
ii.kaaCnj.cnjtypes.p.values.my.rand <- cbind(as.vector(fitted(ii.kaa.cnjtype.glmer.rand)), 1-as.vector(fitted(ii.kaa.cnjtype.glmer.rand)))
colnames(ii.kaaCnj.cnjtypes.p.values.my.rand) <- c("kaaCnj","otherCnj")
ii.kaaCnj.cnjtypes.ModelStats.my.rand<- model.statistics(ii.kaaCnj.cnjtypes.observed.my.rand, ii.kaaCnj.cnjtypes.predicted.my.rand, ii.kaaCnj.cnjtypes.p.values.my.rand)

ii.OtherCnj.cnjtypes.observed.my.rand <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ii.OtherCnj.cnjtypes.predicted.my.rand <- ifelse(as.vector(fitted(ii.other.cnjtype.glmer.rand))>0.5, "OtherCnj", "otherCnj")
table(ii.OtherCnj.cnjtypes.observed.my.rand,ii.OtherCnj.cnjtypes.predicted.my.rand)
ii.OtherCnj.cnjtypes.p.values.my.rand <- cbind(as.vector(fitted(ii.other.cnjtype.glmer.rand)), 1-as.vector(fitted(ii.other.cnjtype.glmer.rand)))
colnames(ii.OtherCnj.cnjtypes.p.values.my.rand) <- c("OtherCnj","otherCnj")
ii.OtherCnj.cnjtypes.ModelStats.my.rand<- model.statistics(ii.OtherCnj.cnjtypes.observed.my.rand, ii.OtherCnj.cnjtypes.predicted.my.rand, ii.OtherCnj.cnjtypes.p.values.my.rand)

ai.eCnj.cnjtypes.observed.my.rand <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ai.eCnj.cnjtypes.predicted.my.rand <- ifelse(as.vector(fitted(ai.e.cnjtype.glmer.rand))>0.5, "eCnj", "otherCnj")
table(ai.eCnj.cnjtypes.observed.my.rand,ai.eCnj.cnjtypes.predicted.my.rand)
ai.eCnj.cnjtypes.p.values.my.rand <- cbind(as.vector(fitted(ai.e.cnjtype.glmer.rand)), 1-as.vector(fitted(ai.e.cnjtype.glmer.rand)))
colnames(ai.eCnj.cnjtypes.p.values.my.rand) <- c("eCnj","otherCnj")
ai.eCnj.cnjtypes.ModelStats.my.rand<- model.statistics(ai.eCnj.cnjtypes.observed.my.rand, ai.eCnj.cnjtypes.predicted.my.rand, ai.eCnj.cnjtypes.p.values.my.rand)

ai.kaaCnj.cnjtypes.observed.my.rand <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ai.kaaCnj.cnjtypes.predicted.my.rand <- ifelse(as.vector(fitted(ai.kaa.cnjtype.glmer.rand))>0.5, "kaaCnj", "otherCnj")
table(ai.kaaCnj.cnjtypes.observed.my.rand,ai.kaaCnj.cnjtypes.predicted.my.rand)
ai.kaaCnj.cnjtypes.p.values.my.rand <- cbind(as.vector(fitted(ai.kaa.cnjtype.glmer.rand)), 1-as.vector(fitted(ai.kaa.cnjtype.glmer.rand)))
colnames(ai.kaaCnj.cnjtypes.p.values.my.rand) <- c("kaaCnj","otherCnj")
ai.kaaCnj.cnjtypes.ModelStats.my.rand<- model.statistics(ai.kaaCnj.cnjtypes.observed.my.rand, ai.kaaCnj.cnjtypes.predicted.my.rand, ai.kaaCnj.cnjtypes.p.values.my.rand)

ai.OtherCnj.cnjtypes.observed.my.rand <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ai.OtherCnj.cnjtypes.predicted.my.rand <- ifelse(as.vector(fitted(ai.other.cnjtype.glmer.rand))>0.5, "OtherCnj", "otherCnj")
table(ai.OtherCnj.cnjtypes.observed.my.rand,ai.OtherCnj.cnjtypes.predicted.my.rand)
ai.OtherCnj.cnjtypes.p.values.my.rand <- cbind(as.vector(fitted(ai.other.cnjtype.glmer.rand)), 1-as.vector(fitted(ai.other.cnjtype.glmer.rand)))
colnames(ai.OtherCnj.cnjtypes.p.values.my.rand) <- c("OtherCnj","otherCnj")
ai.OtherCnj.cnjtypes.ModelStats.my.rand<- model.statistics(ai.OtherCnj.cnjtypes.observed.my.rand, ai.OtherCnj.cnjtypes.predicted.my.rand, ai.OtherCnj.cnjtypes.p.values.my.rand)

ti.eCnj.cnjtypes.observed.my.rand <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ti.eCnj.cnjtypes.predicted.my.rand <- ifelse(as.vector(fitted(ti.e.cnjtype.glmer.rand))>0.5, "eCnj", "otherCnj")
table(ti.eCnj.cnjtypes.observed.my.rand,ti.eCnj.cnjtypes.predicted.my.rand)
ti.eCnj.cnjtypes.p.values.my.rand <- cbind(as.vector(fitted(ti.e.cnjtype.glmer.rand)), 1-as.vector(fitted(ti.e.cnjtype.glmer.rand)))
colnames(ti.eCnj.cnjtypes.p.values.my.rand) <- c("eCnj","otherCnj")
ti.eCnj.cnjtypes.ModelStats.my.rand<- model.statistics(ti.eCnj.cnjtypes.observed.my.rand, ti.eCnj.cnjtypes.predicted.my.rand, ti.eCnj.cnjtypes.p.values.my.rand)

ti.kaaCnj.cnjtypes.observed.my.rand <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ti.kaaCnj.cnjtypes.predicted.my.rand <- ifelse(as.vector(fitted(ti.kaa.cnjtype.glmer.rand))>0.5, "kaaCnj", "otherCnj")
table(ti.kaaCnj.cnjtypes.observed.my.rand,ti.kaaCnj.cnjtypes.predicted.my.rand)
ti.kaaCnj.cnjtypes.p.values.my.rand <- cbind(as.vector(fitted(ti.kaa.cnjtype.glmer.rand)), 1-as.vector(fitted(ti.kaa.cnjtype.glmer.rand)))
colnames(ti.kaaCnj.cnjtypes.p.values.my.rand) <- c("kaaCnj","otherCnj")
ti.kaaCnj.cnjtypes.ModelStats.my.rand<- model.statistics(ti.kaaCnj.cnjtypes.observed.my.rand, ti.kaaCnj.cnjtypes.predicted.my.rand, ti.kaaCnj.cnjtypes.p.values.my.rand)

ti.OtherCnj.cnjtypes.observed.my.rand <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ti.OtherCnj.cnjtypes.predicted.my.rand <- ifelse(as.vector(fitted(ti.other.cnjtype.glmer.rand))>0.5, "OtherCnj", "otherCnj")
table(ti.OtherCnj.cnjtypes.observed.my.rand,ti.OtherCnj.cnjtypes.predicted.my.rand)
ti.OtherCnj.cnjtypes.p.values.my.rand <- cbind(as.vector(fitted(ti.other.cnjtype.glmer.rand)), 1-as.vector(fitted(ti.other.cnjtype.glmer.rand)))
colnames(ti.OtherCnj.cnjtypes.p.values.my.rand) <- c("OtherCnj","otherCnj")
ti.OtherCnj.cnjtypes.ModelStats.my.rand<- model.statistics(ti.OtherCnj.cnjtypes.observed.my.rand, ti.OtherCnj.cnjtypes.predicted.my.rand, ti.OtherCnj.cnjtypes.p.values.my.rand)

ta.eCnj.cnjtypes.observed.my.rand <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ta.eCnj.cnjtypes.predicted.my.rand <- ifelse(as.vector(fitted(ta.e.cnjtype.glmer.rand))>0.5, "eCnj", "otherCnj")
table(ta.eCnj.cnjtypes.observed.my.rand,ta.eCnj.cnjtypes.predicted.my.rand)
ta.eCnj.cnjtypes.p.values.my.rand <- cbind(as.vector(fitted(ta.e.cnjtype.glmer.rand)), 1-as.vector(fitted(ta.e.cnjtype.glmer.rand)))
colnames(ta.eCnj.cnjtypes.p.values.my.rand) <- c("eCnj","otherCnj")
ta.eCnj.cnjtypes.ModelStats.my.rand<- model.statistics(ta.eCnj.cnjtypes.observed.my.rand, ta.eCnj.cnjtypes.predicted.my.rand, ta.eCnj.cnjtypes.p.values.my.rand)

ta.kaaCnj.cnjtypes.observed.my.rand <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ta.kaaCnj.cnjtypes.predicted.my.rand <- ifelse(as.vector(fitted(ta.kaa.cnjtype.glmer.rand))>0.5, "kaaCnj", "otherCnj")
table(ta.kaaCnj.cnjtypes.observed.my.rand,ta.kaaCnj.cnjtypes.predicted.my.rand)
ta.kaaCnj.cnjtypes.p.values.my.rand <- cbind(as.vector(fitted(ta.kaa.cnjtype.glmer.rand)), 1-as.vector(fitted(ta.kaa.cnjtype.glmer.rand)))
colnames(ta.kaaCnj.cnjtypes.p.values.my.rand) <- c("kaaCnj","otherCnj")
ta.kaaCnj.cnjtypes.ModelStats.my.rand<- model.statistics(ta.kaaCnj.cnjtypes.observed.my.rand, ta.kaaCnj.cnjtypes.predicted.my.rand, ta.kaaCnj.cnjtypes.p.values.my.rand)

ta.OtherCnj.cnjtypes.observed.my.rand <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ta.OtherCnj.cnjtypes.predicted.my.rand <- ifelse(as.vector(fitted(ta.other.cnjtype.glmer.rand))>0.5, "OtherCnj", "otherCnj")
table(ta.OtherCnj.cnjtypes.observed.my.rand,ta.OtherCnj.cnjtypes.predicted.my.rand)
ta.OtherCnj.cnjtypes.p.values.my.rand <- cbind(as.vector(fitted(ta.other.cnjtype.glmer.rand)), 1-as.vector(fitted(ta.other.cnjtype.glmer.rand)))
colnames(ta.OtherCnj.cnjtypes.p.values.my.rand) <- c("OtherCnj","otherCnj")
ta.OtherCnj.cnjtypes.ModelStats.my.rand<- model.statistics(ta.OtherCnj.cnjtypes.observed.my.rand, ta.OtherCnj.cnjtypes.predicted.my.rand, ta.OtherCnj.cnjtypes.p.values.my.rand)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-ecnj-cnjtypes-ModelStats.my.rand.txt')
print(ii.eCnj.cnjtypes.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-ecnj-cnjtypes-ModelStats.my.rand.txt')
print(ai.eCnj.cnjtypes.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-ecnj-cnjtypes-ModelStats.my.rand.txt')
print(ti.eCnj.cnjtypes.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-ecnj-cnjtypes-ModelStats.my.rand.txt')
print(ta.eCnj.cnjtypes.ModelStats.my.rand, max.print = NA)
sink(NULL)

sink(file = 'modelstats/Manual/ii-kaacnj-cnjtypes-ModelStats.my.rand.txt')
print(ii.kaaCnj.cnjtypes.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-kaacnj-cnjtypes-ModelStats.my.rand.txt')
print(ai.kaaCnj.cnjtypes.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-kaacnj-cnjtypes-ModelStats.my.rand.txt')
print(ti.kaaCnj.cnjtypes.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-kaacnj-cnjtypes-ModelStats.my.rand.txt')
print(ta.kaaCnj.cnjtypes.ModelStats.my.rand, max.print = NA)
sink(NULL)

sink(file = 'modelstats/Manual/ii-othercnj-cnjtypes-ModelStats.my.rand.txt')
print(ii.OtherCnj.cnjtypes.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-othercnj-cnjtypes-ModelStats.my.rand.txt')
print(ai.OtherCnj.cnjtypes.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-othercnj-cnjtypes-ModelStats.my.rand.txt')
print(ti.OtherCnj.cnjtypes.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-othercnj-cnjtypes-ModelStats.my.rand.txt')
print(ta.OtherCnj.cnjtypes.ModelStats.my.rand, max.print = NA)
sink(NULL)



#================================================
### Ind v. Cnj
#================================================

ii.indcnj.observed.my.rand <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.indcnj.predicted.my.rand <- ifelse(as.vector(fitted(ii.indcnj.glmer.rand))>0.5, "Ind", "eCnj")
table(ii.indcnj.observed.my.rand,ii.indcnj.predicted.my.rand)
ii.indcnj.p.values.my.rand <- cbind(as.vector(fitted(ii.indcnj.glmer.rand)), 1-as.vector(fitted(ii.indcnj.glmer.rand)))
colnames(ii.indcnj.p.values.my.rand) <- c("Ind","eCnj")
ii.indcnj.ModelStats.my.rand<- model.statistics(ii.indcnj.observed.my.rand, ii.indcnj.predicted.my.rand, ii.indcnj.p.values.my.rand)

ai.indcnj.observed.my.rand <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.indcnj.predicted.my.rand <- ifelse(as.vector(fitted(ai.indcnj.glmer.rand))>0.5, "Ind", "eCnj")
table(ai.indcnj.observed.my.rand,ai.indcnj.predicted.my.rand)
ai.indcnj.p.values.my.rand <- cbind(as.vector(fitted(ai.indcnj.glmer.rand)), 1-as.vector(fitted(ai.indcnj.glmer.rand)))
colnames(ai.indcnj.p.values.my.rand) <- c("Ind","eCnj")
ai.indcnj.ModelStats.my.rand<- model.statistics(ai.indcnj.observed.my.rand, ai.indcnj.predicted.my.rand, ai.indcnj.p.values.my.rand)

ti.indcnj.observed.my.rand <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.indcnj.predicted.my.rand <- ifelse(as.vector(fitted(ti.indcnj.glmer.rand))>0.5, "Ind", "eCnj")
table(ti.indcnj.observed.my.rand,ti.indcnj.predicted.my.rand)
ti.indcnj.p.values.my.rand <- cbind(as.vector(fitted(ti.indcnj.glmer.rand)), 1-as.vector(fitted(ti.indcnj.glmer.rand)))
colnames(ti.indcnj.p.values.my.rand) <- c("Ind","eCnj")
ti.indcnj.ModelStats.my.rand<- model.statistics(ti.indcnj.observed.my.rand, ti.indcnj.predicted.my.rand, ti.indcnj.p.values.my.rand)

ta.indcnj.observed.my.rand <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.indcnj.predicted.my.rand <- ifelse(as.vector(fitted(ta.indcnj.glmer.rand))>0.5, "Ind", "eCnj")
table(ta.indcnj.observed.my.rand,ta.indcnj.predicted.my.rand)
ta.indcnj.p.values.my.rand <- cbind(as.vector(fitted(ta.indcnj.glmer.rand)), 1-as.vector(fitted(ta.indcnj.glmer.rand)))
colnames(ta.indcnj.p.values.my.rand) <- c("Ind","eCnj")
ta.indcnj.ModelStats.my.rand<- model.statistics(ta.indcnj.observed.my.rand, ta.indcnj.predicted.my.rand, ta.indcnj.p.values.my.rand)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-indcnj-ModelStats.my.rand.txt')
print(ii.indcnj.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-indcnj-ModelStats.my.rand.txt')
print(ai.indcnj.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-indcnj-ModelStats.my.rand.txt')
print(ti.indcnj.ModelStats.my.rand, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-indcnj-ModelStats.my.rand.txt')
print(ta.indcnj.ModelStats.my.rand, max.print = NA)
sink(NULL)

