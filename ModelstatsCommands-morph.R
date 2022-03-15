#MODEL STATISTICS

### Ind v. eCnj
ii.ive.observed.my.morph <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my.morph <- ifelse(as.vector(fitted(ii.ive.glmer.morph))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my.morph,ii.ive.predicted.my.morph)
ii.ive.p.values.my.morph <- cbind(as.vector(fitted(ii.ive.glmer.morph)), 1-as.vector(fitted(ii.ive.glmer.morph)))
colnames(ii.ive.p.values.my.morph) <- c("Ind","eCnj")
ii.ive.ModelStats.my.morph<- model.statistics(ii.ive.observed.my.morph, ii.ive.predicted.my.morph, ii.ive.p.values.my.morph)

ai.ive.observed.my.morph <- ifelse(subset(AWive, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.ive.predicted.my.morph <- ifelse(as.vector(fitted(ai.ive.glmer.morph))>0.5, "Ind", "eCnj")
table(ai.ive.observed.my.morph,ai.ive.predicted.my.morph)
ai.ive.p.values.my.morph <- cbind(as.vector(fitted(ai.ive.glmer.morph)), 1-as.vector(fitted(ai.ive.glmer.morph)))
colnames(ai.ive.p.values.my.morph) <- c("Ind","eCnj")
ai.ive.ModelStats.my.morph<- model.statistics(ai.ive.observed.my.morph, ai.ive.predicted.my.morph, ai.ive.p.values.my.morph)

ti.ive.observed.my.morph <- ifelse(subset(AWive, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.ive.predicted.my.morph <- ifelse(as.vector(fitted(ti.ive.glmer.morph))>0.5, "Ind", "eCnj")
table(ti.ive.observed.my.morph,ti.ive.predicted.my.morph)
ti.ive.p.values.my.morph <- cbind(as.vector(fitted(ti.ive.glmer.morph)), 1-as.vector(fitted(ti.ive.glmer.morph)))
colnames(ti.ive.p.values.my.morph) <- c("Ind","eCnj")
ti.ive.ModelStats.my.morph<- model.statistics(ti.ive.observed.my.morph, ti.ive.predicted.my.morph, ti.ive.p.values.my.morph)

ta.ive.observed.my.morph <- ifelse(subset(AWive, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.ive.predicted.my.morph <- ifelse(as.vector(fitted(ta.ive.glmer.morph))>0.5, "Ind", "eCnj")
table(ta.ive.observed.my.morph,ta.ive.predicted.my.morph)
ta.ive.p.values.my.morph <- cbind(as.vector(fitted(ta.ive.glmer.morph)), 1-as.vector(fitted(ta.ive.glmer.morph)))
colnames(ta.ive.p.values.my.morph) <- c("Ind","eCnj")
ta.ive.ModelStats.my.morph<- model.statistics(ta.ive.observed.my.morph, ta.ive.predicted.my.morph, ta.ive.p.values.my.morph)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-ive-ModelStats.my.morph.txt')
print(ii.ive.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-ive-ModelStats.my.morph.txt')
print(ai.ive.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-ive-ModelStats.my.morph.txt')
print(ti.ive.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-ive-ModelStats.my.morph.txt')
print(ta.ive.ModelStats.my.morph, max.print = NA)
sink(NULL)








#================================================
### eCnj v. kaaCnj v. OtherCnj
#================================================

ii.ive.observed.my.morph <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my.morph <- ifelse(as.vector(fitted(ii.ive.glmer.morph))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my.morph,ii.ive.predicted.my.morph)
ii.ive.p.values.my.morph <- cbind(as.vector(fitted(ii.ive.glmer.morph)), 1-as.vector(fitted(ii.ive.glmer.morph)))
colnames(ii.ive.p.values.my.morph) <- c("Ind","eCnj")
ii.ive.ModelStats.my.morph<- model.statistics(ii.ive.observed.my.morph, ii.ive.predicted.my.morph, ii.ive.p.values.my.morph)

ii.eCnj.cnjtypes.observed.my.morph <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ii.eCnj.cnjtypes.predicted.my.morph <- ifelse(as.vector(fitted(ii.e.cnjtype.glmer.morph))>0.5, "eCnj", "otherCnj")
table(ii.eCnj.cnjtypes.observed.my.morph,ii.eCnj.cnjtypes.predicted.my.morph)
ii.eCnj.cnjtypes.p.values.my.morph <- cbind(as.vector(fitted(ii.e.cnjtype.glmer.morph)), 1-as.vector(fitted(ii.e.cnjtype.glmer.morph)))
colnames(ii.eCnj.cnjtypes.p.values.my.morph) <- c("eCnj","otherCnj")
ii.eCnj.cnjtypes.ModelStats.my.morph<- model.statistics(ii.eCnj.cnjtypes.observed.my.morph, ii.eCnj.cnjtypes.predicted.my.morph, ii.eCnj.cnjtypes.p.values.my.morph)

ii.kaaCnj.cnjtypes.observed.my.morph <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ii.kaaCnj.cnjtypes.predicted.my.morph <- ifelse(as.vector(fitted(ii.kaa.cnjtype.glmer.morph))>0.5, "kaaCnj", "otherCnj")
table(ii.kaaCnj.cnjtypes.observed.my.morph,ii.kaaCnj.cnjtypes.predicted.my.morph)
ii.kaaCnj.cnjtypes.p.values.my.morph <- cbind(as.vector(fitted(ii.kaa.cnjtype.glmer.morph)), 1-as.vector(fitted(ii.kaa.cnjtype.glmer.morph)))
colnames(ii.kaaCnj.cnjtypes.p.values.my.morph) <- c("kaaCnj","otherCnj")
ii.kaaCnj.cnjtypes.ModelStats.my.morph<- model.statistics(ii.kaaCnj.cnjtypes.observed.my.morph, ii.kaaCnj.cnjtypes.predicted.my.morph, ii.kaaCnj.cnjtypes.p.values.my.morph)

ii.OtherCnj.cnjtypes.observed.my.morph <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ii.OtherCnj.cnjtypes.predicted.my.morph <- ifelse(as.vector(fitted(ii.other.cnjtype.glmer.morph))>0.5, "OtherCnj", "otherCnj")
table(ii.OtherCnj.cnjtypes.observed.my.morph,ii.OtherCnj.cnjtypes.predicted.my.morph)
ii.OtherCnj.cnjtypes.p.values.my.morph <- cbind(as.vector(fitted(ii.other.cnjtype.glmer.morph)), 1-as.vector(fitted(ii.other.cnjtype.glmer.morph)))
colnames(ii.OtherCnj.cnjtypes.p.values.my.morph) <- c("OtherCnj","otherCnj")
ii.OtherCnj.cnjtypes.ModelStats.my.morph<- model.statistics(ii.OtherCnj.cnjtypes.observed.my.morph, ii.OtherCnj.cnjtypes.predicted.my.morph, ii.OtherCnj.cnjtypes.p.values.my.morph)

ai.eCnj.cnjtypes.observed.my.morph <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ai.eCnj.cnjtypes.predicted.my.morph <- ifelse(as.vector(fitted(ai.e.cnjtype.glmer.morph))>0.5, "eCnj", "otherCnj")
table(ai.eCnj.cnjtypes.observed.my.morph,ai.eCnj.cnjtypes.predicted.my.morph)
ai.eCnj.cnjtypes.p.values.my.morph <- cbind(as.vector(fitted(ai.e.cnjtype.glmer.morph)), 1-as.vector(fitted(ai.e.cnjtype.glmer.morph)))
colnames(ai.eCnj.cnjtypes.p.values.my.morph) <- c("eCnj","otherCnj")
ai.eCnj.cnjtypes.ModelStats.my.morph<- model.statistics(ai.eCnj.cnjtypes.observed.my.morph, ai.eCnj.cnjtypes.predicted.my.morph, ai.eCnj.cnjtypes.p.values.my.morph)

ai.kaaCnj.cnjtypes.observed.my.morph <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ai.kaaCnj.cnjtypes.predicted.my.morph <- ifelse(as.vector(fitted(ai.kaa.cnjtype.glmer.morph))>0.5, "kaaCnj", "otherCnj")
table(ai.kaaCnj.cnjtypes.observed.my.morph,ai.kaaCnj.cnjtypes.predicted.my.morph)
ai.kaaCnj.cnjtypes.p.values.my.morph <- cbind(as.vector(fitted(ai.kaa.cnjtype.glmer.morph)), 1-as.vector(fitted(ai.kaa.cnjtype.glmer.morph)))
colnames(ai.kaaCnj.cnjtypes.p.values.my.morph) <- c("kaaCnj","otherCnj")
ai.kaaCnj.cnjtypes.ModelStats.my.morph<- model.statistics(ai.kaaCnj.cnjtypes.observed.my.morph, ai.kaaCnj.cnjtypes.predicted.my.morph, ai.kaaCnj.cnjtypes.p.values.my.morph)

ai.OtherCnj.cnjtypes.observed.my.morph <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ai.OtherCnj.cnjtypes.predicted.my.morph <- ifelse(as.vector(fitted(ai.other.cnjtype.glmer.morph))>0.5, "OtherCnj", "otherCnj")
table(ai.OtherCnj.cnjtypes.observed.my.morph,ai.OtherCnj.cnjtypes.predicted.my.morph)
ai.OtherCnj.cnjtypes.p.values.my.morph <- cbind(as.vector(fitted(ai.other.cnjtype.glmer.morph)), 1-as.vector(fitted(ai.other.cnjtype.glmer.morph)))
colnames(ai.OtherCnj.cnjtypes.p.values.my.morph) <- c("OtherCnj","otherCnj")
ai.OtherCnj.cnjtypes.ModelStats.my.morph<- model.statistics(ai.OtherCnj.cnjtypes.observed.my.morph, ai.OtherCnj.cnjtypes.predicted.my.morph, ai.OtherCnj.cnjtypes.p.values.my.morph)

ti.eCnj.cnjtypes.observed.my.morph <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ti.eCnj.cnjtypes.predicted.my.morph <- ifelse(as.vector(fitted(ti.e.cnjtype.glmer.morph))>0.5, "eCnj", "otherCnj")
table(ti.eCnj.cnjtypes.observed.my.morph,ti.eCnj.cnjtypes.predicted.my.morph)
ti.eCnj.cnjtypes.p.values.my.morph <- cbind(as.vector(fitted(ti.e.cnjtype.glmer.morph)), 1-as.vector(fitted(ti.e.cnjtype.glmer.morph)))
colnames(ti.eCnj.cnjtypes.p.values.my.morph) <- c("eCnj","otherCnj")
ti.eCnj.cnjtypes.ModelStats.my.morph<- model.statistics(ti.eCnj.cnjtypes.observed.my.morph, ti.eCnj.cnjtypes.predicted.my.morph, ti.eCnj.cnjtypes.p.values.my.morph)

ti.kaaCnj.cnjtypes.observed.my.morph <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ti.kaaCnj.cnjtypes.predicted.my.morph <- ifelse(as.vector(fitted(ti.kaa.cnjtype.glmer.morph))>0.5, "kaaCnj", "otherCnj")
table(ti.kaaCnj.cnjtypes.observed.my.morph,ti.kaaCnj.cnjtypes.predicted.my.morph)
ti.kaaCnj.cnjtypes.p.values.my.morph <- cbind(as.vector(fitted(ti.kaa.cnjtype.glmer.morph)), 1-as.vector(fitted(ti.kaa.cnjtype.glmer.morph)))
colnames(ti.kaaCnj.cnjtypes.p.values.my.morph) <- c("kaaCnj","otherCnj")
ti.kaaCnj.cnjtypes.ModelStats.my.morph<- model.statistics(ti.kaaCnj.cnjtypes.observed.my.morph, ti.kaaCnj.cnjtypes.predicted.my.morph, ti.kaaCnj.cnjtypes.p.values.my.morph)

ti.OtherCnj.cnjtypes.observed.my.morph <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ti.OtherCnj.cnjtypes.predicted.my.morph <- ifelse(as.vector(fitted(ti.other.cnjtype.glmer.morph))>0.5, "OtherCnj", "otherCnj")
table(ti.OtherCnj.cnjtypes.observed.my.morph,ti.OtherCnj.cnjtypes.predicted.my.morph)
ti.OtherCnj.cnjtypes.p.values.my.morph <- cbind(as.vector(fitted(ti.other.cnjtype.glmer.morph)), 1-as.vector(fitted(ti.other.cnjtype.glmer.morph)))
colnames(ti.OtherCnj.cnjtypes.p.values.my.morph) <- c("OtherCnj","otherCnj")
ti.OtherCnj.cnjtypes.ModelStats.my.morph<- model.statistics(ti.OtherCnj.cnjtypes.observed.my.morph, ti.OtherCnj.cnjtypes.predicted.my.morph, ti.OtherCnj.cnjtypes.p.values.my.morph)

ta.eCnj.cnjtypes.observed.my.morph <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ta.eCnj.cnjtypes.predicted.my.morph <- ifelse(as.vector(fitted(ta.e.cnjtype.glmer.morph))>0.5, "eCnj", "otherCnj")
table(ta.eCnj.cnjtypes.observed.my.morph,ta.eCnj.cnjtypes.predicted.my.morph)
ta.eCnj.cnjtypes.p.values.my.morph <- cbind(as.vector(fitted(ta.e.cnjtype.glmer.morph)), 1-as.vector(fitted(ta.e.cnjtype.glmer.morph)))
colnames(ta.eCnj.cnjtypes.p.values.my.morph) <- c("eCnj","otherCnj")
ta.eCnj.cnjtypes.ModelStats.my.morph<- model.statistics(ta.eCnj.cnjtypes.observed.my.morph, ta.eCnj.cnjtypes.predicted.my.morph, ta.eCnj.cnjtypes.p.values.my.morph)

ta.kaaCnj.cnjtypes.observed.my.morph <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ta.kaaCnj.cnjtypes.predicted.my.morph <- ifelse(as.vector(fitted(ta.kaa.cnjtype.glmer.morph))>0.5, "kaaCnj", "otherCnj")
table(ta.kaaCnj.cnjtypes.observed.my.morph,ta.kaaCnj.cnjtypes.predicted.my.morph)
ta.kaaCnj.cnjtypes.p.values.my.morph <- cbind(as.vector(fitted(ta.kaa.cnjtype.glmer.morph)), 1-as.vector(fitted(ta.kaa.cnjtype.glmer.morph)))
colnames(ta.kaaCnj.cnjtypes.p.values.my.morph) <- c("kaaCnj","otherCnj")
ta.kaaCnj.cnjtypes.ModelStats.my.morph<- model.statistics(ta.kaaCnj.cnjtypes.observed.my.morph, ta.kaaCnj.cnjtypes.predicted.my.morph, ta.kaaCnj.cnjtypes.p.values.my.morph)

ta.OtherCnj.cnjtypes.observed.my.morph <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ta.OtherCnj.cnjtypes.predicted.my.morph <- ifelse(as.vector(fitted(ta.other.cnjtype.glmer.morph))>0.5, "OtherCnj", "otherCnj")
table(ta.OtherCnj.cnjtypes.observed.my.morph,ta.OtherCnj.cnjtypes.predicted.my.morph)
ta.OtherCnj.cnjtypes.p.values.my.morph <- cbind(as.vector(fitted(ta.other.cnjtype.glmer.morph)), 1-as.vector(fitted(ta.other.cnjtype.glmer.morph)))
colnames(ta.OtherCnj.cnjtypes.p.values.my.morph) <- c("OtherCnj","otherCnj")
ta.OtherCnj.cnjtypes.ModelStats.my.morph<- model.statistics(ta.OtherCnj.cnjtypes.observed.my.morph, ta.OtherCnj.cnjtypes.predicted.my.morph, ta.OtherCnj.cnjtypes.p.values.my.morph)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-ecnj-cnjtypes-ModelStats.my.morph.txt')
print(ii.eCnj.cnjtypes.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-ecnj-cnjtypes-ModelStats.my.morph.txt')
print(ai.eCnj.cnjtypes.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-ecnj-cnjtypes-ModelStats.my.morph.txt')
print(ti.eCnj.cnjtypes.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-ecnj-cnjtypes-ModelStats.my.morph.txt')
print(ta.eCnj.cnjtypes.ModelStats.my.morph, max.print = NA)
sink(NULL)

sink(file = 'modelstats/Manual/ii-kaacnj-cnjtypes-ModelStats.my.morph.txt')
print(ii.kaaCnj.cnjtypes.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-kaacnj-cnjtypes-ModelStats.my.morph.txt')
print(ai.kaaCnj.cnjtypes.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-kaacnj-cnjtypes-ModelStats.my.morph.txt')
print(ti.kaaCnj.cnjtypes.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-kaacnj-cnjtypes-ModelStats.my.morph.txt')
print(ta.kaaCnj.cnjtypes.ModelStats.my.morph, max.print = NA)
sink(NULL)

sink(file = 'modelstats/Manual/ii-othercnj-cnjtypes-ModelStats.my.morph.txt')
print(ii.OtherCnj.cnjtypes.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-othercnj-cnjtypes-ModelStats.my.morph.txt')
print(ai.OtherCnj.cnjtypes.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-othercnj-cnjtypes-ModelStats.my.morph.txt')
print(ti.OtherCnj.cnjtypes.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-othercnj-cnjtypes-ModelStats.my.morph.txt')
print(ta.OtherCnj.cnjtypes.ModelStats.my.morph, max.print = NA)
sink(NULL)



#================================================
### Ind v. Cnj
#================================================

ii.indcnj.observed.my.morph <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.indcnj.predicted.my.morph <- ifelse(as.vector(fitted(ii.indcnj.glmer.morph))>0.5, "Ind", "eCnj")
table(ii.indcnj.observed.my.morph,ii.indcnj.predicted.my.morph)
ii.indcnj.p.values.my.morph <- cbind(as.vector(fitted(ii.indcnj.glmer.morph)), 1-as.vector(fitted(ii.indcnj.glmer.morph)))
colnames(ii.indcnj.p.values.my.morph) <- c("Ind","eCnj")
ii.indcnj.ModelStats.my.morph<- model.statistics(ii.indcnj.observed.my.morph, ii.indcnj.predicted.my.morph, ii.indcnj.p.values.my.morph)

ai.indcnj.observed.my.morph <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.indcnj.predicted.my.morph <- ifelse(as.vector(fitted(ai.indcnj.glmer.morph))>0.5, "Ind", "eCnj")
table(ai.indcnj.observed.my.morph,ai.indcnj.predicted.my.morph)
ai.indcnj.p.values.my.morph <- cbind(as.vector(fitted(ai.indcnj.glmer.morph)), 1-as.vector(fitted(ai.indcnj.glmer.morph)))
colnames(ai.indcnj.p.values.my.morph) <- c("Ind","eCnj")
ai.indcnj.ModelStats.my.morph<- model.statistics(ai.indcnj.observed.my.morph, ai.indcnj.predicted.my.morph, ai.indcnj.p.values.my.morph)

ti.indcnj.observed.my.morph <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.indcnj.predicted.my.morph <- ifelse(as.vector(fitted(ti.indcnj.glmer.morph))>0.5, "Ind", "eCnj")
table(ti.indcnj.observed.my.morph,ti.indcnj.predicted.my.morph)
ti.indcnj.p.values.my.morph <- cbind(as.vector(fitted(ti.indcnj.glmer.morph)), 1-as.vector(fitted(ti.indcnj.glmer.morph)))
colnames(ti.indcnj.p.values.my.morph) <- c("Ind","eCnj")
ti.indcnj.ModelStats.my.morph<- model.statistics(ti.indcnj.observed.my.morph, ti.indcnj.predicted.my.morph, ti.indcnj.p.values.my.morph)

ta.indcnj.observed.my.morph <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.indcnj.predicted.my.morph <- ifelse(as.vector(fitted(ta.indcnj.glmer.morph))>0.5, "Ind", "eCnj")
table(ta.indcnj.observed.my.morph,ta.indcnj.predicted.my.morph)
ta.indcnj.p.values.my.morph <- cbind(as.vector(fitted(ta.indcnj.glmer.morph)), 1-as.vector(fitted(ta.indcnj.glmer.morph)))
colnames(ta.indcnj.p.values.my.morph) <- c("Ind","eCnj")
ta.indcnj.ModelStats.my.morph<- model.statistics(ta.indcnj.observed.my.morph, ta.indcnj.predicted.my.morph, ta.indcnj.p.values.my.morph)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-indcnj-ModelStats.my.morph.txt')
print(ii.indcnj.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-indcnj-ModelStats.my.morph.txt')
print(ai.indcnj.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-indcnj-ModelStats.my.morph.txt')
print(ti.indcnj.ModelStats.my.morph, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-indcnj-ModelStats.my.morph.txt')
print(ta.indcnj.ModelStats.my.morph, max.print = NA)
sink(NULL)

