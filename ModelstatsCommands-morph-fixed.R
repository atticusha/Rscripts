#MODEL STATISTICS

### Ind v. eCnj
#ii.ive.observed.my.morph.fixed <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
#ii.ive.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ii.ive.morph.fixed.glmer))>0.5, "Ind", "eCnj")
#table(ii.ive.observed.my.morph.fixed,ii.ive.predicted.my.morph.fixed)
#ii.ive.p.values.my.morph.fixed <- cbind(as.vector(fitted(ii.ive.morph.fixed.glmer)), 1-as.vector(fitted(ii.ive.morph.fixed.glmer)))
#colnames(ii.ive.p.values.my.morph.fixed) <- c("Ind","eCnj")
#ii.ive.ModelStats.my.morph.fixed<- model.statistics(ii.ive.observed.my.morph.fixed, ii.ive.predicted.my.morph.fixed, ii.ive.p.values.my.morph.fixed)

ai.ive.observed.my.morph.fixed <- ifelse(subset(AWive, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.ive.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ai.ive.morph.fixed.glmer))>0.5, "Ind", "eCnj")
table(ai.ive.observed.my.morph.fixed,ai.ive.predicted.my.morph.fixed)
ai.ive.p.values.my.morph.fixed <- cbind(as.vector(fitted(ai.ive.morph.fixed.glmer)), 1-as.vector(fitted(ai.ive.morph.fixed.glmer)))
colnames(ai.ive.p.values.my.morph.fixed) <- c("Ind","eCnj")
ai.ive.ModelStats.my.morph.fixed<- model.statistics(ai.ive.observed.my.morph.fixed, ai.ive.predicted.my.morph.fixed, ai.ive.p.values.my.morph.fixed)

ti.ive.observed.my.morph.fixed <- ifelse(subset(AWive, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.ive.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ti.ive.morph.fixed.glmer))>0.5, "Ind", "eCnj")
table(ti.ive.observed.my.morph.fixed,ti.ive.predicted.my.morph.fixed)
ti.ive.p.values.my.morph.fixed <- cbind(as.vector(fitted(ti.ive.morph.fixed.glmer)), 1-as.vector(fitted(ti.ive.morph.fixed.glmer)))
colnames(ti.ive.p.values.my.morph.fixed) <- c("Ind","eCnj")
ti.ive.ModelStats.my.morph.fixed<- model.statistics(ti.ive.observed.my.morph.fixed, ti.ive.predicted.my.morph.fixed, ti.ive.p.values.my.morph.fixed)

ta.ive.observed.my.morph.fixed <- ifelse(subset(AWive, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.ive.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ta.ive.morph.fixed.glmer))>0.5, "Ind", "eCnj")
table(ta.ive.observed.my.morph.fixed,ta.ive.predicted.my.morph.fixed)
ta.ive.p.values.my.morph.fixed <- cbind(as.vector(fitted(ta.ive.morph.fixed.glmer)), 1-as.vector(fitted(ta.ive.morph.fixed.glmer)))
colnames(ta.ive.p.values.my.morph.fixed) <- c("Ind","eCnj")
ta.ive.ModelStats.my.morph.fixed<- model.statistics(ta.ive.observed.my.morph.fixed, ta.ive.predicted.my.morph.fixed, ta.ive.p.values.my.morph.fixed)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-ive-ModelStats.my.morph.fixed.txt')
print(ii.ive.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-ive-ModelStats.my.morph.fixed.txt')
print(ai.ive.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-ive-ModelStats.my.morph.fixed.txt')
print(ti.ive.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-ive-ModelStats.my.morph.fixed.txt')
print(ta.ive.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)








#================================================
### eCnj v. kaaCnj v. OtherCnj
#================================================

#ii.ive.observed.my.morph.fixed <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
#ii.ive.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ii.ive.morph.fixed.glmer))>0.5, "Ind", "eCnj")
#table(ii.ive.observed.my.morph.fixed,ii.ive.predicted.my.morph.fixed)
#ii.ive.p.values.my.morph.fixed <- cbind(as.vector(fitted(ii.ive.morph.fixed.glmer)), 1-as.vector(fitted(ii.ive.morph.fixed.glmer)))
#colnames(ii.ive.p.values.my.morph.fixed) <- c("Ind","eCnj")
#ii.ive.ModelStats.my.morph.fixed<- model.statistics(ii.ive.observed.my.morph.fixed, ii.ive.predicted.my.morph.fixed, ii.ive.p.values.my.morph.fixed)

#ii.eCnj.cnjtypes.observed.my.morph.fixed <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
#ii.eCnj.cnjtypes.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ii.e.cnjtype.morph.fixed.glmer))>0.5, "eCnj", "otherCnj")
#table(ii.eCnj.cnjtypes.observed.my.morph.fixed,ii.eCnj.cnjtypes.predicted.my.morph.fixed)
#ii.eCnj.cnjtypes.p.values.my.morph.fixed <- cbind(as.vector(fitted(ii.e.cnjtype.morph.fixed.glmer)), 1-as.vector(fitted(ii.e.cnjtype.morph.fixed.glmer)))
#colnames(ii.eCnj.cnjtypes.p.values.my.morph.fixed) <- c("eCnj","otherCnj")
#ii.eCnj.cnjtypes.ModelStats.my.morph.fixed<- model.statistics(ii.eCnj.cnjtypes.observed.my.morph.fixed, ii.eCnj.cnjtypes.predicted.my.morph.fixed, ii.eCnj.cnjtypes.p.values.my.morph.fixed)

#ii.kaaCnj.cnjtypes.observed.my.morph.fixed <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
#ii.kaaCnj.cnjtypes.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ii.kaa.cnjtype.morph.fixed.glmer))>0.5, "kaaCnj", "otherCnj")
#table(ii.kaaCnj.cnjtypes.observed.my.morph.fixed,ii.kaaCnj.cnjtypes.predicted.my.morph.fixed)
#ii.kaaCnj.cnjtypes.p.values.my.morph.fixed <- cbind(as.vector(fitted(ii.kaa.cnjtype.morph.fixed.glmer)), 1-as.vector(fitted(ii.kaa.cnjtype.morph.fixed.glmer)))
#colnames(ii.kaaCnj.cnjtypes.p.values.my.morph.fixed) <- c("kaaCnj","otherCnj")
#ii.kaaCnj.cnjtypes.ModelStats.my.morph.fixed<- model.statistics(ii.kaaCnj.cnjtypes.observed.my.morph.fixed, ii.kaaCnj.cnjtypes.predicted.my.morph.fixed, ii.kaaCnj.cnjtypes.p.values.my.morph.fixed)

#ii.OtherCnj.cnjtypes.observed.my.morph.fixed <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
#ii.OtherCnj.cnjtypes.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ii.other.cnjtype.morph.fixed.glmer))>0.5, "OtherCnj", "otherCnj")
#table(ii.OtherCnj.cnjtypes.observed.my.morph.fixed,ii.OtherCnj.cnjtypes.predicted.my.morph.fixed)
#ii.OtherCnj.cnjtypes.p.values.my.morph.fixed <- cbind(as.vector(fitted(ii.other.cnjtype.morph.fixed.glmer)), 1-as.vector(fitted(ii.other.cnjtype.morph.fixed.glmer)))
#colnames(ii.OtherCnj.cnjtypes.p.values.my.morph.fixed) <- c("OtherCnj","otherCnj")
#ii.OtherCnj.cnjtypes.ModelStats.my.morph.fixed<- model.statistics(ii.OtherCnj.cnjtypes.observed.my.morph.fixed, ii.OtherCnj.cnjtypes.predicted.my.morph.fixed, ii.OtherCnj.cnjtypes.p.values.my.morph.fixed)

ai.eCnj.cnjtypes.observed.my.morph.fixed <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ai.eCnj.cnjtypes.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ai.e.cnjtype.morph.fixed.glmer))>0.5, "eCnj", "otherCnj")
table(ai.eCnj.cnjtypes.observed.my.morph.fixed,ai.eCnj.cnjtypes.predicted.my.morph.fixed)
ai.eCnj.cnjtypes.p.values.my.morph.fixed <- cbind(as.vector(fitted(ai.e.cnjtype.morph.fixed.glmer)), 1-as.vector(fitted(ai.e.cnjtype.morph.fixed.glmer)))
colnames(ai.eCnj.cnjtypes.p.values.my.morph.fixed) <- c("eCnj","otherCnj")
ai.eCnj.cnjtypes.ModelStats.my.morph.fixed<- model.statistics(ai.eCnj.cnjtypes.observed.my.morph.fixed, ai.eCnj.cnjtypes.predicted.my.morph.fixed, ai.eCnj.cnjtypes.p.values.my.morph.fixed)

ai.kaaCnj.cnjtypes.observed.my.morph.fixed <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ai.kaaCnj.cnjtypes.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ai.kaa.cnjtype.morph.fixed.glmer))>0.5, "kaaCnj", "otherCnj")
table(ai.kaaCnj.cnjtypes.observed.my.morph.fixed,ai.kaaCnj.cnjtypes.predicted.my.morph.fixed)
ai.kaaCnj.cnjtypes.p.values.my.morph.fixed <- cbind(as.vector(fitted(ai.kaa.cnjtype.morph.fixed.glmer)), 1-as.vector(fitted(ai.kaa.cnjtype.morph.fixed.glmer)))
colnames(ai.kaaCnj.cnjtypes.p.values.my.morph.fixed) <- c("kaaCnj","otherCnj")
ai.kaaCnj.cnjtypes.ModelStats.my.morph.fixed<- model.statistics(ai.kaaCnj.cnjtypes.observed.my.morph.fixed, ai.kaaCnj.cnjtypes.predicted.my.morph.fixed, ai.kaaCnj.cnjtypes.p.values.my.morph.fixed)

ai.OtherCnj.cnjtypes.observed.my.morph.fixed <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ai.OtherCnj.cnjtypes.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ai.other.cnjtype.morph.fixed.glmer))>0.5, "OtherCnj", "otherCnj")
table(ai.OtherCnj.cnjtypes.observed.my.morph.fixed,ai.OtherCnj.cnjtypes.predicted.my.morph.fixed)
ai.OtherCnj.cnjtypes.p.values.my.morph.fixed <- cbind(as.vector(fitted(ai.other.cnjtype.morph.fixed.glmer)), 1-as.vector(fitted(ai.other.cnjtype.morph.fixed.glmer)))
colnames(ai.OtherCnj.cnjtypes.p.values.my.morph.fixed) <- c("OtherCnj","otherCnj")
ai.OtherCnj.cnjtypes.ModelStats.my.morph.fixed<- model.statistics(ai.OtherCnj.cnjtypes.observed.my.morph.fixed, ai.OtherCnj.cnjtypes.predicted.my.morph.fixed, ai.OtherCnj.cnjtypes.p.values.my.morph.fixed)

ti.eCnj.cnjtypes.observed.my.morph.fixed <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ti.eCnj.cnjtypes.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ti.e.cnjtype.morph.fixed.glmer))>0.5, "eCnj", "otherCnj")
table(ti.eCnj.cnjtypes.observed.my.morph.fixed,ti.eCnj.cnjtypes.predicted.my.morph.fixed)
ti.eCnj.cnjtypes.p.values.my.morph.fixed <- cbind(as.vector(fitted(ti.e.cnjtype.morph.fixed.glmer)), 1-as.vector(fitted(ti.e.cnjtype.morph.fixed.glmer)))
colnames(ti.eCnj.cnjtypes.p.values.my.morph.fixed) <- c("eCnj","otherCnj")
ti.eCnj.cnjtypes.ModelStats.my.morph.fixed<- model.statistics(ti.eCnj.cnjtypes.observed.my.morph.fixed, ti.eCnj.cnjtypes.predicted.my.morph.fixed, ti.eCnj.cnjtypes.p.values.my.morph.fixed)

ti.kaaCnj.cnjtypes.observed.my.morph.fixed <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ti.kaaCnj.cnjtypes.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ti.kaa.cnjtype.morph.fixed.glmer))>0.5, "kaaCnj", "otherCnj")
table(ti.kaaCnj.cnjtypes.observed.my.morph.fixed,ti.kaaCnj.cnjtypes.predicted.my.morph.fixed)
ti.kaaCnj.cnjtypes.p.values.my.morph.fixed <- cbind(as.vector(fitted(ti.kaa.cnjtype.morph.fixed.glmer)), 1-as.vector(fitted(ti.kaa.cnjtype.morph.fixed.glmer)))
colnames(ti.kaaCnj.cnjtypes.p.values.my.morph.fixed) <- c("kaaCnj","otherCnj")
ti.kaaCnj.cnjtypes.ModelStats.my.morph.fixed<- model.statistics(ti.kaaCnj.cnjtypes.observed.my.morph.fixed, ti.kaaCnj.cnjtypes.predicted.my.morph.fixed, ti.kaaCnj.cnjtypes.p.values.my.morph.fixed)

ti.OtherCnj.cnjtypes.observed.my.morph.fixed <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ti.OtherCnj.cnjtypes.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ti.other.cnjtype.morph.fixed.glmer))>0.5, "OtherCnj", "otherCnj")
table(ti.OtherCnj.cnjtypes.observed.my.morph.fixed,ti.OtherCnj.cnjtypes.predicted.my.morph.fixed)
ti.OtherCnj.cnjtypes.p.values.my.morph.fixed <- cbind(as.vector(fitted(ti.other.cnjtype.morph.fixed.glmer)), 1-as.vector(fitted(ti.other.cnjtype.morph.fixed.glmer)))
colnames(ti.OtherCnj.cnjtypes.p.values.my.morph.fixed) <- c("OtherCnj","otherCnj")
ti.OtherCnj.cnjtypes.ModelStats.my.morph.fixed<- model.statistics(ti.OtherCnj.cnjtypes.observed.my.morph.fixed, ti.OtherCnj.cnjtypes.predicted.my.morph.fixed, ti.OtherCnj.cnjtypes.p.values.my.morph.fixed)

ta.eCnj.cnjtypes.observed.my.morph.fixed <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ta.eCnj.cnjtypes.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ta.e.cnjtype.morph.fixed.glmer))>0.5, "eCnj", "otherCnj")
table(ta.eCnj.cnjtypes.observed.my.morph.fixed,ta.eCnj.cnjtypes.predicted.my.morph.fixed)
ta.eCnj.cnjtypes.p.values.my.morph.fixed <- cbind(as.vector(fitted(ta.e.cnjtype.morph.fixed.glmer)), 1-as.vector(fitted(ta.e.cnjtype.morph.fixed.glmer)))
colnames(ta.eCnj.cnjtypes.p.values.my.morph.fixed) <- c("eCnj","otherCnj")
ta.eCnj.cnjtypes.ModelStats.my.morph.fixed<- model.statistics(ta.eCnj.cnjtypes.observed.my.morph.fixed, ta.eCnj.cnjtypes.predicted.my.morph.fixed, ta.eCnj.cnjtypes.p.values.my.morph.fixed)

ta.kaaCnj.cnjtypes.observed.my.morph.fixed <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ta.kaaCnj.cnjtypes.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ta.kaa.cnjtype.morph.fixed.glmer))>0.5, "kaaCnj", "otherCnj")
table(ta.kaaCnj.cnjtypes.observed.my.morph.fixed,ta.kaaCnj.cnjtypes.predicted.my.morph.fixed)
ta.kaaCnj.cnjtypes.p.values.my.morph.fixed <- cbind(as.vector(fitted(ta.kaa.cnjtype.morph.fixed.glmer)), 1-as.vector(fitted(ta.kaa.cnjtype.morph.fixed.glmer)))
colnames(ta.kaaCnj.cnjtypes.p.values.my.morph.fixed) <- c("kaaCnj","otherCnj")
ta.kaaCnj.cnjtypes.ModelStats.my.morph.fixed<- model.statistics(ta.kaaCnj.cnjtypes.observed.my.morph.fixed, ta.kaaCnj.cnjtypes.predicted.my.morph.fixed, ta.kaaCnj.cnjtypes.p.values.my.morph.fixed)

ta.OtherCnj.cnjtypes.observed.my.morph.fixed <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ta.OtherCnj.cnjtypes.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ta.other.cnjtype.morph.fixed.glmer))>0.5, "OtherCnj", "otherCnj")
table(ta.OtherCnj.cnjtypes.observed.my.morph.fixed,ta.OtherCnj.cnjtypes.predicted.my.morph.fixed)
ta.OtherCnj.cnjtypes.p.values.my.morph.fixed <- cbind(as.vector(fitted(ta.other.cnjtype.morph.fixed.glmer)), 1-as.vector(fitted(ta.other.cnjtype.morph.fixed.glmer)))
colnames(ta.OtherCnj.cnjtypes.p.values.my.morph.fixed) <- c("OtherCnj","otherCnj")
ta.OtherCnj.cnjtypes.ModelStats.my.morph.fixed<- model.statistics(ta.OtherCnj.cnjtypes.observed.my.morph.fixed, ta.OtherCnj.cnjtypes.predicted.my.morph.fixed, ta.OtherCnj.cnjtypes.p.values.my.morph.fixed)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-ecnj-cnjtypes-ModelStats.my.morph.fixed.txt')
print(ii.eCnj.cnjtypes.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-ecnj-cnjtypes-ModelStats.my.morph.fixed.txt')
print(ai.eCnj.cnjtypes.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-ecnj-cnjtypes-ModelStats.my.morph.fixed.txt')
print(ti.eCnj.cnjtypes.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-ecnj-cnjtypes-ModelStats.my.morph.fixed.txt')
print(ta.eCnj.cnjtypes.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)

sink(file = 'modelstats/Manual/ii-kaacnj-cnjtypes-ModelStats.my.morph.fixed.txt')
print(ii.kaaCnj.cnjtypes.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-kaacnj-cnjtypes-ModelStats.my.morph.fixed.txt')
print(ai.kaaCnj.cnjtypes.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-kaacnj-cnjtypes-ModelStats.my.morph.fixed.txt')
print(ti.kaaCnj.cnjtypes.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-kaacnj-cnjtypes-ModelStats.my.morph.fixed.txt')
print(ta.kaaCnj.cnjtypes.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)

sink(file = 'modelstats/Manual/ii-othercnj-cnjtypes-ModelStats.my.morph.fixed.txt')
print(ii.OtherCnj.cnjtypes.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-othercnj-cnjtypes-ModelStats.my.morph.fixed.txt')
print(ai.OtherCnj.cnjtypes.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-othercnj-cnjtypes-ModelStats.my.morph.fixed.txt')
print(ti.OtherCnj.cnjtypes.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-othercnj-cnjtypes-ModelStats.my.morph.fixed.txt')
print(ta.OtherCnj.cnjtypes.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)



#================================================
### Ind v. Cnj
#================================================

ii.indcnj.observed.my.morph.fixed <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.indcnj.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ii.indcnj.morph.fixed.glmer))>0.5, "Ind", "eCnj")
table(ii.indcnj.observed.my.morph.fixed,ii.indcnj.predicted.my.morph.fixed)
ii.indcnj.p.values.my.morph.fixed <- cbind(as.vector(fitted(ii.indcnj.morph.fixed.glmer)), 1-as.vector(fitted(ii.indcnj.morph.fixed.glmer)))
colnames(ii.indcnj.p.values.my.morph.fixed) <- c("Ind","eCnj")
ii.indcnj.ModelStats.my.morph.fixed<- model.statistics(ii.indcnj.observed.my.morph.fixed, ii.indcnj.predicted.my.morph.fixed, ii.indcnj.p.values.my.morph.fixed)

ai.indcnj.observed.my.morph.fixed <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.indcnj.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ai.indcnj.morph.fixed.glmer))>0.5, "Ind", "eCnj")
table(ai.indcnj.observed.my.morph.fixed,ai.indcnj.predicted.my.morph.fixed)
ai.indcnj.p.values.my.morph.fixed <- cbind(as.vector(fitted(ai.indcnj.morph.fixed.glmer)), 1-as.vector(fitted(ai.indcnj.morph.fixed.glmer)))
colnames(ai.indcnj.p.values.my.morph.fixed) <- c("Ind","eCnj")
ai.indcnj.ModelStats.my.morph.fixed<- model.statistics(ai.indcnj.observed.my.morph.fixed, ai.indcnj.predicted.my.morph.fixed, ai.indcnj.p.values.my.morph.fixed)

ti.indcnj.observed.my.morph.fixed <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.indcnj.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ti.indcnj.morph.fixed.glmer))>0.5, "Ind", "eCnj")
table(ti.indcnj.observed.my.morph.fixed,ti.indcnj.predicted.my.morph.fixed)
ti.indcnj.p.values.my.morph.fixed <- cbind(as.vector(fitted(ti.indcnj.morph.fixed.glmer)), 1-as.vector(fitted(ti.indcnj.morph.fixed.glmer)))
colnames(ti.indcnj.p.values.my.morph.fixed) <- c("Ind","eCnj")
ti.indcnj.ModelStats.my.morph.fixed<- model.statistics(ti.indcnj.observed.my.morph.fixed, ti.indcnj.predicted.my.morph.fixed, ti.indcnj.p.values.my.morph.fixed)

ta.indcnj.observed.my.morph.fixed <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.indcnj.predicted.my.morph.fixed <- ifelse(as.vector(fitted(ta.indcnj.morph.fixed.glmer))>0.5, "Ind", "eCnj")
table(ta.indcnj.observed.my.morph.fixed,ta.indcnj.predicted.my.morph.fixed)
ta.indcnj.p.values.my.morph.fixed <- cbind(as.vector(fitted(ta.indcnj.morph.fixed.glmer)), 1-as.vector(fitted(ta.indcnj.morph.fixed.glmer)))
colnames(ta.indcnj.p.values.my.morph.fixed) <- c("Ind","eCnj")
ta.indcnj.ModelStats.my.morph.fixed<- model.statistics(ta.indcnj.observed.my.morph.fixed, ta.indcnj.predicted.my.morph.fixed, ta.indcnj.p.values.my.morph.fixed)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-indcnj-ModelStats.my.morph.fixed.txt')
print(ii.indcnj.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-indcnj-ModelStats.my.morph.fixed.txt')
print(ai.indcnj.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-indcnj-ModelStats.my.morph.fixed.txt')
print(ti.indcnj.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-indcnj-ModelStats.my.morph.fixed.txt')
print(ta.indcnj.ModelStats.my.morph.fixed, max.print = NA)
sink(NULL)
s
