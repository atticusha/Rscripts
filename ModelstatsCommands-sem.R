#MODEL STATISTICS

### Ind v. eCnj
ii.ive.observed.my.sem <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my.sem <- ifelse(as.vector(fitted(ii.ive.glmer.sem))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my.sem,ii.ive.predicted.my.sem)
ii.ive.p.values.my.sem <- cbind(as.vector(fitted(ii.ive.glmer.sem)), 1-as.vector(fitted(ii.ive.glmer.sem)))
colnames(ii.ive.p.values.my.sem) <- c("Ind","eCnj")
ii.ive.ModelStats.my.sem<- model.statistics(ii.ive.observed.my.sem, ii.ive.predicted.my.sem, ii.ive.p.values.my.sem)

ai.ive.observed.my.sem <- ifelse(subset(AWive, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.ive.predicted.my.sem <- ifelse(as.vector(fitted(ai.ive.glmer.sem))>0.5, "Ind", "eCnj")
table(ai.ive.observed.my.sem,ai.ive.predicted.my.sem)
ai.ive.p.values.my.sem <- cbind(as.vector(fitted(ai.ive.glmer.sem)), 1-as.vector(fitted(ai.ive.glmer.sem)))
colnames(ai.ive.p.values.my.sem) <- c("Ind","eCnj")
ai.ive.ModelStats.my.sem<- model.statistics(ai.ive.observed.my.sem, ai.ive.predicted.my.sem, ai.ive.p.values.my.sem)

ti.ive.observed.my.sem <- ifelse(subset(AWive, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.ive.predicted.my.sem <- ifelse(as.vector(fitted(ti.ive.glmer.sem))>0.5, "Ind", "eCnj")
table(ti.ive.observed.my.sem,ti.ive.predicted.my.sem)
ti.ive.p.values.my.sem <- cbind(as.vector(fitted(ti.ive.glmer.sem)), 1-as.vector(fitted(ti.ive.glmer.sem)))
colnames(ti.ive.p.values.my.sem) <- c("Ind","eCnj")
ti.ive.ModelStats.my.sem<- model.statistics(ti.ive.observed.my.sem, ti.ive.predicted.my.sem, ti.ive.p.values.my.sem)

ta.ive.observed.my.sem <- ifelse(subset(AWive, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.ive.predicted.my.sem <- ifelse(as.vector(fitted(ta.ive.glmer.sem))>0.5, "Ind", "eCnj")
table(ta.ive.observed.my.sem,ta.ive.predicted.my.sem)
ta.ive.p.values.my.sem <- cbind(as.vector(fitted(ta.ive.glmer.sem)), 1-as.vector(fitted(ta.ive.glmer.sem)))
colnames(ta.ive.p.values.my.sem) <- c("Ind","eCnj")
ta.ive.ModelStats.my.sem<- model.statistics(ta.ive.observed.my.sem, ta.ive.predicted.my.sem, ta.ive.p.values.my.sem)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-ive-ModelStats.my.sem.txt')
print(ii.ive.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-ive-ModelStats.my.sem.txt')
print(ai.ive.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-ive-ModelStats.my.sem.txt')
print(ti.ive.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-ive-ModelStats.my.sem.txt')
print(ta.ive.ModelStats.my.sem, max.print = NA)
sink(NULL)








#================================================
### eCnj v. kaaCnj v. OtherCnj
#================================================

ii.ive.observed.my.sem <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my.sem <- ifelse(as.vector(fitted(ii.ive.glmer.sem))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my.sem,ii.ive.predicted.my.sem)
ii.ive.p.values.my.sem <- cbind(as.vector(fitted(ii.ive.glmer.sem)), 1-as.vector(fitted(ii.ive.glmer.sem)))
colnames(ii.ive.p.values.my.sem) <- c("Ind","eCnj")
ii.ive.ModelStats.my.sem<- model.statistics(ii.ive.observed.my.sem, ii.ive.predicted.my.sem, ii.ive.p.values.my.sem)

ii.eCnj.cnjtypes.observed.my.sem <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ii.eCnj.cnjtypes.predicted.my.sem <- ifelse(as.vector(fitted(ii.e.cnjtype.glmer.sem))>0.5, "eCnj", "otherCnj")
table(ii.eCnj.cnjtypes.observed.my.sem,ii.eCnj.cnjtypes.predicted.my.sem)
ii.eCnj.cnjtypes.p.values.my.sem <- cbind(as.vector(fitted(ii.e.cnjtype.glmer.sem)), 1-as.vector(fitted(ii.e.cnjtype.glmer.sem)))
colnames(ii.eCnj.cnjtypes.p.values.my.sem) <- c("eCnj","otherCnj")
ii.eCnj.cnjtypes.ModelStats.my.sem<- model.statistics(ii.eCnj.cnjtypes.observed.my.sem, ii.eCnj.cnjtypes.predicted.my.sem, ii.eCnj.cnjtypes.p.values.my.sem)

ii.kaaCnj.cnjtypes.observed.my.sem <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ii.kaaCnj.cnjtypes.predicted.my.sem <- ifelse(as.vector(fitted(ii.kaa.cnjtype.glmer.sem))>0.5, "kaaCnj", "otherCnj")
table(ii.kaaCnj.cnjtypes.observed.my.sem,ii.kaaCnj.cnjtypes.predicted.my.sem)
ii.kaaCnj.cnjtypes.p.values.my.sem <- cbind(as.vector(fitted(ii.kaa.cnjtype.glmer.sem)), 1-as.vector(fitted(ii.kaa.cnjtype.glmer.sem)))
colnames(ii.kaaCnj.cnjtypes.p.values.my.sem) <- c("kaaCnj","otherCnj")
ii.kaaCnj.cnjtypes.ModelStats.my.sem<- model.statistics(ii.kaaCnj.cnjtypes.observed.my.sem, ii.kaaCnj.cnjtypes.predicted.my.sem, ii.kaaCnj.cnjtypes.p.values.my.sem)

ii.OtherCnj.cnjtypes.observed.my.sem <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ii.OtherCnj.cnjtypes.predicted.my.sem <- ifelse(as.vector(fitted(ii.other.cnjtype.glmer.sem))>0.5, "OtherCnj", "otherCnj")
table(ii.OtherCnj.cnjtypes.observed.my.sem,ii.OtherCnj.cnjtypes.predicted.my.sem)
ii.OtherCnj.cnjtypes.p.values.my.sem <- cbind(as.vector(fitted(ii.other.cnjtype.glmer.sem)), 1-as.vector(fitted(ii.other.cnjtype.glmer.sem)))
colnames(ii.OtherCnj.cnjtypes.p.values.my.sem) <- c("OtherCnj","otherCnj")
ii.OtherCnj.cnjtypes.ModelStats.my.sem<- model.statistics(ii.OtherCnj.cnjtypes.observed.my.sem, ii.OtherCnj.cnjtypes.predicted.my.sem, ii.OtherCnj.cnjtypes.p.values.my.sem)

ai.eCnj.cnjtypes.observed.my.sem <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ai.eCnj.cnjtypes.predicted.my.sem <- ifelse(as.vector(fitted(ai.e.cnjtype.glmer.sem))>0.5, "eCnj", "otherCnj")
table(ai.eCnj.cnjtypes.observed.my.sem,ai.eCnj.cnjtypes.predicted.my.sem)
ai.eCnj.cnjtypes.p.values.my.sem <- cbind(as.vector(fitted(ai.e.cnjtype.glmer.sem)), 1-as.vector(fitted(ai.e.cnjtype.glmer.sem)))
colnames(ai.eCnj.cnjtypes.p.values.my.sem) <- c("eCnj","otherCnj")
ai.eCnj.cnjtypes.ModelStats.my.sem<- model.statistics(ai.eCnj.cnjtypes.observed.my.sem, ai.eCnj.cnjtypes.predicted.my.sem, ai.eCnj.cnjtypes.p.values.my.sem)

ai.kaaCnj.cnjtypes.observed.my.sem <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ai.kaaCnj.cnjtypes.predicted.my.sem <- ifelse(as.vector(fitted(ai.kaa.cnjtype.glmer.sem))>0.5, "kaaCnj", "otherCnj")
table(ai.kaaCnj.cnjtypes.observed.my.sem,ai.kaaCnj.cnjtypes.predicted.my.sem)
ai.kaaCnj.cnjtypes.p.values.my.sem <- cbind(as.vector(fitted(ai.kaa.cnjtype.glmer.sem)), 1-as.vector(fitted(ai.kaa.cnjtype.glmer.sem)))
colnames(ai.kaaCnj.cnjtypes.p.values.my.sem) <- c("kaaCnj","otherCnj")
ai.kaaCnj.cnjtypes.ModelStats.my.sem<- model.statistics(ai.kaaCnj.cnjtypes.observed.my.sem, ai.kaaCnj.cnjtypes.predicted.my.sem, ai.kaaCnj.cnjtypes.p.values.my.sem)

ai.OtherCnj.cnjtypes.observed.my.sem <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ai.OtherCnj.cnjtypes.predicted.my.sem <- ifelse(as.vector(fitted(ai.other.cnjtype.glmer.sem))>0.5, "OtherCnj", "otherCnj")
table(ai.OtherCnj.cnjtypes.observed.my.sem,ai.OtherCnj.cnjtypes.predicted.my.sem)
ai.OtherCnj.cnjtypes.p.values.my.sem <- cbind(as.vector(fitted(ai.other.cnjtype.glmer.sem)), 1-as.vector(fitted(ai.other.cnjtype.glmer.sem)))
colnames(ai.OtherCnj.cnjtypes.p.values.my.sem) <- c("OtherCnj","otherCnj")
ai.OtherCnj.cnjtypes.ModelStats.my.sem<- model.statistics(ai.OtherCnj.cnjtypes.observed.my.sem, ai.OtherCnj.cnjtypes.predicted.my.sem, ai.OtherCnj.cnjtypes.p.values.my.sem)

ti.eCnj.cnjtypes.observed.my.sem <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ti.eCnj.cnjtypes.predicted.my.sem <- ifelse(as.vector(fitted(ti.e.cnjtype.glmer.sem))>0.5, "eCnj", "otherCnj")
table(ti.eCnj.cnjtypes.observed.my.sem,ti.eCnj.cnjtypes.predicted.my.sem)
ti.eCnj.cnjtypes.p.values.my.sem <- cbind(as.vector(fitted(ti.e.cnjtype.glmer.sem)), 1-as.vector(fitted(ti.e.cnjtype.glmer.sem)))
colnames(ti.eCnj.cnjtypes.p.values.my.sem) <- c("eCnj","otherCnj")
ti.eCnj.cnjtypes.ModelStats.my.sem<- model.statistics(ti.eCnj.cnjtypes.observed.my.sem, ti.eCnj.cnjtypes.predicted.my.sem, ti.eCnj.cnjtypes.p.values.my.sem)

ti.kaaCnj.cnjtypes.observed.my.sem <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ti.kaaCnj.cnjtypes.predicted.my.sem <- ifelse(as.vector(fitted(ti.kaa.cnjtype.glmer.sem))>0.5, "kaaCnj", "otherCnj")
table(ti.kaaCnj.cnjtypes.observed.my.sem,ti.kaaCnj.cnjtypes.predicted.my.sem)
ti.kaaCnj.cnjtypes.p.values.my.sem <- cbind(as.vector(fitted(ti.kaa.cnjtype.glmer.sem)), 1-as.vector(fitted(ti.kaa.cnjtype.glmer.sem)))
colnames(ti.kaaCnj.cnjtypes.p.values.my.sem) <- c("kaaCnj","otherCnj")
ti.kaaCnj.cnjtypes.ModelStats.my.sem<- model.statistics(ti.kaaCnj.cnjtypes.observed.my.sem, ti.kaaCnj.cnjtypes.predicted.my.sem, ti.kaaCnj.cnjtypes.p.values.my.sem)

ti.OtherCnj.cnjtypes.observed.my.sem <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ti.OtherCnj.cnjtypes.predicted.my.sem <- ifelse(as.vector(fitted(ti.other.cnjtype.glmer.sem))>0.5, "OtherCnj", "otherCnj")
table(ti.OtherCnj.cnjtypes.observed.my.sem,ti.OtherCnj.cnjtypes.predicted.my.sem)
ti.OtherCnj.cnjtypes.p.values.my.sem <- cbind(as.vector(fitted(ti.other.cnjtype.glmer.sem)), 1-as.vector(fitted(ti.other.cnjtype.glmer.sem)))
colnames(ti.OtherCnj.cnjtypes.p.values.my.sem) <- c("OtherCnj","otherCnj")
ti.OtherCnj.cnjtypes.ModelStats.my.sem<- model.statistics(ti.OtherCnj.cnjtypes.observed.my.sem, ti.OtherCnj.cnjtypes.predicted.my.sem, ti.OtherCnj.cnjtypes.p.values.my.sem)

ta.eCnj.cnjtypes.observed.my.sem <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ta.eCnj.cnjtypes.predicted.my.sem <- ifelse(as.vector(fitted(ta.e.cnjtype.glmer.sem))>0.5, "eCnj", "otherCnj")
table(ta.eCnj.cnjtypes.observed.my.sem,ta.eCnj.cnjtypes.predicted.my.sem)
ta.eCnj.cnjtypes.p.values.my.sem <- cbind(as.vector(fitted(ta.e.cnjtype.glmer.sem)), 1-as.vector(fitted(ta.e.cnjtype.glmer.sem)))
colnames(ta.eCnj.cnjtypes.p.values.my.sem) <- c("eCnj","otherCnj")
ta.eCnj.cnjtypes.ModelStats.my.sem<- model.statistics(ta.eCnj.cnjtypes.observed.my.sem, ta.eCnj.cnjtypes.predicted.my.sem, ta.eCnj.cnjtypes.p.values.my.sem)

ta.kaaCnj.cnjtypes.observed.my.sem <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ta.kaaCnj.cnjtypes.predicted.my.sem <- ifelse(as.vector(fitted(ta.kaa.cnjtype.glmer.sem))>0.5, "kaaCnj", "otherCnj")
table(ta.kaaCnj.cnjtypes.observed.my.sem,ta.kaaCnj.cnjtypes.predicted.my.sem)
ta.kaaCnj.cnjtypes.p.values.my.sem <- cbind(as.vector(fitted(ta.kaa.cnjtype.glmer.sem)), 1-as.vector(fitted(ta.kaa.cnjtype.glmer.sem)))
colnames(ta.kaaCnj.cnjtypes.p.values.my.sem) <- c("kaaCnj","otherCnj")
ta.kaaCnj.cnjtypes.ModelStats.my.sem<- model.statistics(ta.kaaCnj.cnjtypes.observed.my.sem, ta.kaaCnj.cnjtypes.predicted.my.sem, ta.kaaCnj.cnjtypes.p.values.my.sem)

ta.OtherCnj.cnjtypes.observed.my.sem <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ta.OtherCnj.cnjtypes.predicted.my.sem <- ifelse(as.vector(fitted(ta.other.cnjtype.glmer.sem))>0.5, "OtherCnj", "otherCnj")
table(ta.OtherCnj.cnjtypes.observed.my.sem,ta.OtherCnj.cnjtypes.predicted.my.sem)
ta.OtherCnj.cnjtypes.p.values.my.sem <- cbind(as.vector(fitted(ta.other.cnjtype.glmer.sem)), 1-as.vector(fitted(ta.other.cnjtype.glmer.sem)))
colnames(ta.OtherCnj.cnjtypes.p.values.my.sem) <- c("OtherCnj","otherCnj")
ta.OtherCnj.cnjtypes.ModelStats.my.sem<- model.statistics(ta.OtherCnj.cnjtypes.observed.my.sem, ta.OtherCnj.cnjtypes.predicted.my.sem, ta.OtherCnj.cnjtypes.p.values.my.sem)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-ecnj-cnjtypes-ModelStats.my.sem.txt')
print(ii.eCnj.cnjtypes.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-ecnj-cnjtypes-ModelStats.my.sem.txt')
print(ai.eCnj.cnjtypes.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-ecnj-cnjtypes-ModelStats.my.sem.txt')
print(ti.eCnj.cnjtypes.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-ecnj-cnjtypes-ModelStats.my.sem.txt')
print(ta.eCnj.cnjtypes.ModelStats.my.sem, max.print = NA)
sink(NULL)

sink(file = 'modelstats/Manual/ii-kaacnj-cnjtypes-ModelStats.my.sem.txt')
print(ii.kaaCnj.cnjtypes.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-kaacnj-cnjtypes-ModelStats.my.sem.txt')
print(ai.kaaCnj.cnjtypes.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-kaacnj-cnjtypes-ModelStats.my.sem.txt')
print(ti.kaaCnj.cnjtypes.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-kaacnj-cnjtypes-ModelStats.my.sem.txt')
print(ta.kaaCnj.cnjtypes.ModelStats.my.sem, max.print = NA)
sink(NULL)

sink(file = 'modelstats/Manual/ii-othercnj-cnjtypes-ModelStats.my.sem.txt')
print(ii.OtherCnj.cnjtypes.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-othercnj-cnjtypes-ModelStats.my.sem.txt')
print(ai.OtherCnj.cnjtypes.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-othercnj-cnjtypes-ModelStats.my.sem.txt')
print(ti.OtherCnj.cnjtypes.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-othercnj-cnjtypes-ModelStats.my.sem.txt')
print(ta.OtherCnj.cnjtypes.ModelStats.my.sem, max.print = NA)
sink(NULL)



#================================================
### Ind v. Cnj
#================================================

ii.indcnj.observed.my.sem <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.indcnj.predicted.my.sem <- ifelse(as.vector(fitted(ii.indcnj.glmer.sem))>0.5, "Ind", "eCnj")
table(ii.indcnj.observed.my.sem,ii.indcnj.predicted.my.sem)
ii.indcnj.p.values.my.sem <- cbind(as.vector(fitted(ii.indcnj.glmer.sem)), 1-as.vector(fitted(ii.indcnj.glmer.sem)))
colnames(ii.indcnj.p.values.my.sem) <- c("Ind","eCnj")
ii.indcnj.ModelStats.my.sem<- model.statistics(ii.indcnj.observed.my.sem, ii.indcnj.predicted.my.sem, ii.indcnj.p.values.my.sem)

ai.indcnj.observed.my.sem <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.indcnj.predicted.my.sem <- ifelse(as.vector(fitted(ai.indcnj.glmer.sem))>0.5, "Ind", "eCnj")
table(ai.indcnj.observed.my.sem,ai.indcnj.predicted.my.sem)
ai.indcnj.p.values.my.sem <- cbind(as.vector(fitted(ai.indcnj.glmer.sem)), 1-as.vector(fitted(ai.indcnj.glmer.sem)))
colnames(ai.indcnj.p.values.my.sem) <- c("Ind","eCnj")
ai.indcnj.ModelStats.my.sem<- model.statistics(ai.indcnj.observed.my.sem, ai.indcnj.predicted.my.sem, ai.indcnj.p.values.my.sem)

ti.indcnj.observed.my.sem <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.indcnj.predicted.my.sem <- ifelse(as.vector(fitted(ti.indcnj.glmer.sem))>0.5, "Ind", "eCnj")
table(ti.indcnj.observed.my.sem,ti.indcnj.predicted.my.sem)
ti.indcnj.p.values.my.sem <- cbind(as.vector(fitted(ti.indcnj.glmer.sem)), 1-as.vector(fitted(ti.indcnj.glmer.sem)))
colnames(ti.indcnj.p.values.my.sem) <- c("Ind","eCnj")
ti.indcnj.ModelStats.my.sem<- model.statistics(ti.indcnj.observed.my.sem, ti.indcnj.predicted.my.sem, ti.indcnj.p.values.my.sem)

ta.indcnj.observed.my.sem <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.indcnj.predicted.my.sem <- ifelse(as.vector(fitted(ta.indcnj.glmer.sem))>0.5, "Ind", "eCnj")
table(ta.indcnj.observed.my.sem,ta.indcnj.predicted.my.sem)
ta.indcnj.p.values.my.sem <- cbind(as.vector(fitted(ta.indcnj.glmer.sem)), 1-as.vector(fitted(ta.indcnj.glmer.sem)))
colnames(ta.indcnj.p.values.my.sem) <- c("Ind","eCnj")
ta.indcnj.ModelStats.my.sem<- model.statistics(ta.indcnj.observed.my.sem, ta.indcnj.predicted.my.sem, ta.indcnj.p.values.my.sem)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-indcnj-ModelStats.my.sem.txt')
print(ii.indcnj.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-indcnj-ModelStats.my.sem.txt')
print(ai.indcnj.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-indcnj-ModelStats.my.sem.txt')
print(ti.indcnj.ModelStats.my.sem, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-indcnj-ModelStats.my.sem.txt')
print(ta.indcnj.ModelStats.my.sem, max.print = NA)
sink(NULL)

