#MODEL STATISTICS

### Ind v. eCnj
ii.ive.observed.my.sem.fixed <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ii.ive.glm.sem.fixed))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my.sem.fixed,ii.ive.predicted.my.sem.fixed)
ii.ive.p.values.my.sem.fixed <- cbind(as.vector(fitted(ii.ive.glm.sem.fixed)), 1-as.vector(fitted(ii.ive.glm.sem.fixed)))
colnames(ii.ive.p.values.my.sem.fixed) <- c("Ind","eCnj")
ii.ive.ModelStats.my.sem.fixed<- model.statistics(ii.ive.observed.my.sem.fixed, ii.ive.predicted.my.sem.fixed, ii.ive.p.values.my.sem.fixed)

ai.ive.observed.my.sem.fixed <- ifelse(subset(AWive, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.ive.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ai.ive.glm.sem.fixed))>0.5, "Ind", "eCnj")
table(ai.ive.observed.my.sem.fixed,ai.ive.predicted.my.sem.fixed)
ai.ive.p.values.my.sem.fixed <- cbind(as.vector(fitted(ai.ive.glm.sem.fixed)), 1-as.vector(fitted(ai.ive.glm.sem.fixed)))
colnames(ai.ive.p.values.my.sem.fixed) <- c("Ind","eCnj")
ai.ive.ModelStats.my.sem.fixed<- model.statistics(ai.ive.observed.my.sem.fixed, ai.ive.predicted.my.sem.fixed, ai.ive.p.values.my.sem.fixed)

ti.ive.observed.my.sem.fixed <- ifelse(subset(AWive, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.ive.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ti.ive.glm.sem.fixed))>0.5, "Ind", "eCnj")
table(ti.ive.observed.my.sem.fixed,ti.ive.predicted.my.sem.fixed)
ti.ive.p.values.my.sem.fixed <- cbind(as.vector(fitted(ti.ive.glm.sem.fixed)), 1-as.vector(fitted(ti.ive.glm.sem.fixed)))
colnames(ti.ive.p.values.my.sem.fixed) <- c("Ind","eCnj")
ti.ive.ModelStats.my.sem.fixed<- model.statistics(ti.ive.observed.my.sem.fixed, ti.ive.predicted.my.sem.fixed, ti.ive.p.values.my.sem.fixed)

ta.ive.observed.my.sem.fixed <- ifelse(subset(AWive, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.ive.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ta.ive.glm.sem.fixed))>0.5, "Ind", "eCnj")
table(ta.ive.observed.my.sem.fixed,ta.ive.predicted.my.sem.fixed)
ta.ive.p.values.my.sem.fixed <- cbind(as.vector(fitted(ta.ive.glm.sem.fixed)), 1-as.vector(fitted(ta.ive.glm.sem.fixed)))
colnames(ta.ive.p.values.my.sem.fixed) <- c("Ind","eCnj")
ta.ive.ModelStats.my.sem.fixed<- model.statistics(ta.ive.observed.my.sem.fixed, ta.ive.predicted.my.sem.fixed, ta.ive.p.values.my.sem.fixed)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-ive-ModelStats.my.sem.fixed.txt')
print(ii.ive.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-ive-ModelStats.my.sem.fixed.txt')
print(ai.ive.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-ive-ModelStats.my.sem.fixed.txt')
print(ti.ive.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-ive-ModelStats.my.sem.fixed.txt')
print(ta.ive.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)








#================================================
### eCnj v. kaaCnj v. OtherCnj
#================================================

ii.ive.observed.my.sem.fixed <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ii.ive.glm.sem.fixed))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my.sem.fixed,ii.ive.predicted.my.sem.fixed)
ii.ive.p.values.my.sem.fixed <- cbind(as.vector(fitted(ii.ive.glm.sem.fixed)), 1-as.vector(fitted(ii.ive.glm.sem.fixed)))
colnames(ii.ive.p.values.my.sem.fixed) <- c("Ind","eCnj")
ii.ive.ModelStats.my.sem.fixed<- model.statistics(ii.ive.observed.my.sem.fixed, ii.ive.predicted.my.sem.fixed, ii.ive.p.values.my.sem.fixed)

ii.eCnj.cnjtypes.observed.my.sem.fixed <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ii.eCnj.cnjtypes.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ii.e.cnjtype.glm.sem.fixed))>0.5, "eCnj", "otherCnj")
table(ii.eCnj.cnjtypes.observed.my.sem.fixed,ii.eCnj.cnjtypes.predicted.my.sem.fixed)
ii.eCnj.cnjtypes.p.values.my.sem.fixed <- cbind(as.vector(fitted(ii.e.cnjtype.glm.sem.fixed)), 1-as.vector(fitted(ii.e.cnjtype.glm.sem.fixed)))
colnames(ii.eCnj.cnjtypes.p.values.my.sem.fixed) <- c("eCnj","otherCnj")
ii.eCnj.cnjtypes.ModelStats.my.sem.fixed<- model.statistics(ii.eCnj.cnjtypes.observed.my.sem.fixed, ii.eCnj.cnjtypes.predicted.my.sem.fixed, ii.eCnj.cnjtypes.p.values.my.sem.fixed)

ii.kaaCnj.cnjtypes.observed.my.sem.fixed <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ii.kaaCnj.cnjtypes.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ii.kaa.cnjtype.glm.sem.fixed))>0.5, "kaaCnj", "otherCnj")
table(ii.kaaCnj.cnjtypes.observed.my.sem.fixed,ii.kaaCnj.cnjtypes.predicted.my.sem.fixed)
ii.kaaCnj.cnjtypes.p.values.my.sem.fixed <- cbind(as.vector(fitted(ii.kaa.cnjtype.glm.sem.fixed)), 1-as.vector(fitted(ii.kaa.cnjtype.glm.sem.fixed)))
colnames(ii.kaaCnj.cnjtypes.p.values.my.sem.fixed) <- c("kaaCnj","otherCnj")
ii.kaaCnj.cnjtypes.ModelStats.my.sem.fixed<- model.statistics(ii.kaaCnj.cnjtypes.observed.my.sem.fixed, ii.kaaCnj.cnjtypes.predicted.my.sem.fixed, ii.kaaCnj.cnjtypes.p.values.my.sem.fixed)

ii.OtherCnj.cnjtypes.observed.my.sem.fixed <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ii.OtherCnj.cnjtypes.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ii.other.cnjtype.glm.sem.fixed))>0.5, "OtherCnj", "otherCnj")
table(ii.OtherCnj.cnjtypes.observed.my.sem.fixed,ii.OtherCnj.cnjtypes.predicted.my.sem.fixed)
ii.OtherCnj.cnjtypes.p.values.my.sem.fixed <- cbind(as.vector(fitted(ii.other.cnjtype.glm.sem.fixed)), 1-as.vector(fitted(ii.other.cnjtype.glm.sem.fixed)))
colnames(ii.OtherCnj.cnjtypes.p.values.my.sem.fixed) <- c("OtherCnj","otherCnj")
ii.OtherCnj.cnjtypes.ModelStats.my.sem.fixed<- model.statistics(ii.OtherCnj.cnjtypes.observed.my.sem.fixed, ii.OtherCnj.cnjtypes.predicted.my.sem.fixed, ii.OtherCnj.cnjtypes.p.values.my.sem.fixed)

ai.eCnj.cnjtypes.observed.my.sem.fixed <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ai.eCnj.cnjtypes.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ai.e.cnjtype.glm.sem.fixed))>0.5, "eCnj", "otherCnj")
table(ai.eCnj.cnjtypes.observed.my.sem.fixed,ai.eCnj.cnjtypes.predicted.my.sem.fixed)
ai.eCnj.cnjtypes.p.values.my.sem.fixed <- cbind(as.vector(fitted(ai.e.cnjtype.glm.sem.fixed)), 1-as.vector(fitted(ai.e.cnjtype.glm.sem.fixed)))
colnames(ai.eCnj.cnjtypes.p.values.my.sem.fixed) <- c("eCnj","otherCnj")
ai.eCnj.cnjtypes.ModelStats.my.sem.fixed<- model.statistics(ai.eCnj.cnjtypes.observed.my.sem.fixed, ai.eCnj.cnjtypes.predicted.my.sem.fixed, ai.eCnj.cnjtypes.p.values.my.sem.fixed)

ai.kaaCnj.cnjtypes.observed.my.sem.fixed <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ai.kaaCnj.cnjtypes.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ai.kaa.cnjtype.glm.sem.fixed))>0.5, "kaaCnj", "otherCnj")
table(ai.kaaCnj.cnjtypes.observed.my.sem.fixed,ai.kaaCnj.cnjtypes.predicted.my.sem.fixed)
ai.kaaCnj.cnjtypes.p.values.my.sem.fixed <- cbind(as.vector(fitted(ai.kaa.cnjtype.glm.sem.fixed)), 1-as.vector(fitted(ai.kaa.cnjtype.glm.sem.fixed)))
colnames(ai.kaaCnj.cnjtypes.p.values.my.sem.fixed) <- c("kaaCnj","otherCnj")
ai.kaaCnj.cnjtypes.ModelStats.my.sem.fixed<- model.statistics(ai.kaaCnj.cnjtypes.observed.my.sem.fixed, ai.kaaCnj.cnjtypes.predicted.my.sem.fixed, ai.kaaCnj.cnjtypes.p.values.my.sem.fixed)

ai.OtherCnj.cnjtypes.observed.my.sem.fixed <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ai.OtherCnj.cnjtypes.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ai.other.cnjtype.glm.sem.fixed))>0.5, "OtherCnj", "otherCnj")
table(ai.OtherCnj.cnjtypes.observed.my.sem.fixed,ai.OtherCnj.cnjtypes.predicted.my.sem.fixed)
ai.OtherCnj.cnjtypes.p.values.my.sem.fixed <- cbind(as.vector(fitted(ai.other.cnjtype.glm.sem.fixed)), 1-as.vector(fitted(ai.other.cnjtype.glm.sem.fixed)))
colnames(ai.OtherCnj.cnjtypes.p.values.my.sem.fixed) <- c("OtherCnj","otherCnj")
ai.OtherCnj.cnjtypes.ModelStats.my.sem.fixed<- model.statistics(ai.OtherCnj.cnjtypes.observed.my.sem.fixed, ai.OtherCnj.cnjtypes.predicted.my.sem.fixed, ai.OtherCnj.cnjtypes.p.values.my.sem.fixed)

ti.eCnj.cnjtypes.observed.my.sem.fixed <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ti.eCnj.cnjtypes.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ti.e.cnjtype.glm.sem.fixed))>0.5, "eCnj", "otherCnj")
table(ti.eCnj.cnjtypes.observed.my.sem.fixed,ti.eCnj.cnjtypes.predicted.my.sem.fixed)
ti.eCnj.cnjtypes.p.values.my.sem.fixed <- cbind(as.vector(fitted(ti.e.cnjtype.glm.sem.fixed)), 1-as.vector(fitted(ti.e.cnjtype.glm.sem.fixed)))
colnames(ti.eCnj.cnjtypes.p.values.my.sem.fixed) <- c("eCnj","otherCnj")
ti.eCnj.cnjtypes.ModelStats.my.sem.fixed<- model.statistics(ti.eCnj.cnjtypes.observed.my.sem.fixed, ti.eCnj.cnjtypes.predicted.my.sem.fixed, ti.eCnj.cnjtypes.p.values.my.sem.fixed)

ti.kaaCnj.cnjtypes.observed.my.sem.fixed <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ti.kaaCnj.cnjtypes.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ti.kaa.cnjtype.glm.sem.fixed))>0.5, "kaaCnj", "otherCnj")
table(ti.kaaCnj.cnjtypes.observed.my.sem.fixed,ti.kaaCnj.cnjtypes.predicted.my.sem.fixed)
ti.kaaCnj.cnjtypes.p.values.my.sem.fixed <- cbind(as.vector(fitted(ti.kaa.cnjtype.glm.sem.fixed)), 1-as.vector(fitted(ti.kaa.cnjtype.glm.sem.fixed)))
colnames(ti.kaaCnj.cnjtypes.p.values.my.sem.fixed) <- c("kaaCnj","otherCnj")
ti.kaaCnj.cnjtypes.ModelStats.my.sem.fixed<- model.statistics(ti.kaaCnj.cnjtypes.observed.my.sem.fixed, ti.kaaCnj.cnjtypes.predicted.my.sem.fixed, ti.kaaCnj.cnjtypes.p.values.my.sem.fixed)

ti.OtherCnj.cnjtypes.observed.my.sem.fixed <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ti.OtherCnj.cnjtypes.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ti.other.cnjtype.glm.sem.fixed))>0.5, "OtherCnj", "otherCnj")
table(ti.OtherCnj.cnjtypes.observed.my.sem.fixed,ti.OtherCnj.cnjtypes.predicted.my.sem.fixed)
ti.OtherCnj.cnjtypes.p.values.my.sem.fixed <- cbind(as.vector(fitted(ti.other.cnjtype.glm.sem.fixed)), 1-as.vector(fitted(ti.other.cnjtype.glm.sem.fixed)))
colnames(ti.OtherCnj.cnjtypes.p.values.my.sem.fixed) <- c("OtherCnj","otherCnj")
ti.OtherCnj.cnjtypes.ModelStats.my.sem.fixed<- model.statistics(ti.OtherCnj.cnjtypes.observed.my.sem.fixed, ti.OtherCnj.cnjtypes.predicted.my.sem.fixed, ti.OtherCnj.cnjtypes.p.values.my.sem.fixed)

ta.eCnj.cnjtypes.observed.my.sem.fixed <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ta.eCnj.cnjtypes.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ta.e.cnjtype.glm.sem.fixed))>0.5, "eCnj", "otherCnj")
table(ta.eCnj.cnjtypes.observed.my.sem.fixed,ta.eCnj.cnjtypes.predicted.my.sem.fixed)
ta.eCnj.cnjtypes.p.values.my.sem.fixed <- cbind(as.vector(fitted(ta.e.cnjtype.glm.sem.fixed)), 1-as.vector(fitted(ta.e.cnjtype.glm.sem.fixed)))
colnames(ta.eCnj.cnjtypes.p.values.my.sem.fixed) <- c("eCnj","otherCnj")
ta.eCnj.cnjtypes.ModelStats.my.sem.fixed<- model.statistics(ta.eCnj.cnjtypes.observed.my.sem.fixed, ta.eCnj.cnjtypes.predicted.my.sem.fixed, ta.eCnj.cnjtypes.p.values.my.sem.fixed)

ta.kaaCnj.cnjtypes.observed.my.sem.fixed <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ta.kaaCnj.cnjtypes.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ta.kaa.cnjtype.glm.sem.fixed))>0.5, "kaaCnj", "otherCnj")
table(ta.kaaCnj.cnjtypes.observed.my.sem.fixed,ta.kaaCnj.cnjtypes.predicted.my.sem.fixed)
ta.kaaCnj.cnjtypes.p.values.my.sem.fixed <- cbind(as.vector(fitted(ta.kaa.cnjtype.glm.sem.fixed)), 1-as.vector(fitted(ta.kaa.cnjtype.glm.sem.fixed)))
colnames(ta.kaaCnj.cnjtypes.p.values.my.sem.fixed) <- c("kaaCnj","otherCnj")
ta.kaaCnj.cnjtypes.ModelStats.my.sem.fixed<- model.statistics(ta.kaaCnj.cnjtypes.observed.my.sem.fixed, ta.kaaCnj.cnjtypes.predicted.my.sem.fixed, ta.kaaCnj.cnjtypes.p.values.my.sem.fixed)

ta.OtherCnj.cnjtypes.observed.my.sem.fixed <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ta.OtherCnj.cnjtypes.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ta.other.cnjtype.glm.sem.fixed))>0.5, "OtherCnj", "otherCnj")
table(ta.OtherCnj.cnjtypes.observed.my.sem.fixed,ta.OtherCnj.cnjtypes.predicted.my.sem.fixed)
ta.OtherCnj.cnjtypes.p.values.my.sem.fixed <- cbind(as.vector(fitted(ta.other.cnjtype.glm.sem.fixed)), 1-as.vector(fitted(ta.other.cnjtype.glm.sem.fixed)))
colnames(ta.OtherCnj.cnjtypes.p.values.my.sem.fixed) <- c("OtherCnj","otherCnj")
ta.OtherCnj.cnjtypes.ModelStats.my.sem.fixed<- model.statistics(ta.OtherCnj.cnjtypes.observed.my.sem.fixed, ta.OtherCnj.cnjtypes.predicted.my.sem.fixed, ta.OtherCnj.cnjtypes.p.values.my.sem.fixed)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-ecnj-cnjtypes-ModelStats.my.sem.fixed.txt')
print(ii.eCnj.cnjtypes.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-ecnj-cnjtypes-ModelStats.my.sem.fixed.txt')
print(ai.eCnj.cnjtypes.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-ecnj-cnjtypes-ModelStats.my.sem.fixed.txt')
print(ti.eCnj.cnjtypes.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-ecnj-cnjtypes-ModelStats.my.sem.fixed.txt')
print(ta.eCnj.cnjtypes.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)

sink(file = 'modelstats/Manual/ii-kaacnj-cnjtypes-ModelStats.my.sem.fixed.txt')
print(ii.kaaCnj.cnjtypes.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-kaacnj-cnjtypes-ModelStats.my.sem.fixed.txt')
print(ai.kaaCnj.cnjtypes.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-kaacnj-cnjtypes-ModelStats.my.sem.fixed.txt')
print(ti.kaaCnj.cnjtypes.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-kaacnj-cnjtypes-ModelStats.my.sem.fixed.txt')
print(ta.kaaCnj.cnjtypes.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)

sink(file = 'modelstats/Manual/ii-othercnj-cnjtypes-ModelStats.my.sem.fixed.txt')
print(ii.OtherCnj.cnjtypes.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-othercnj-cnjtypes-ModelStats.my.sem.fixed.txt')
print(ai.OtherCnj.cnjtypes.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-othercnj-cnjtypes-ModelStats.my.sem.fixed.txt')
print(ti.OtherCnj.cnjtypes.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-othercnj-cnjtypes-ModelStats.my.sem.fixed.txt')
print(ta.OtherCnj.cnjtypes.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)



#================================================
### Ind v. Cnj
#================================================

ii.indcnj.observed.my.sem.fixed <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.indcnj.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ii.indcnj.glm.sem.fixed))>0.5, "Ind", "eCnj")
table(ii.indcnj.observed.my.sem.fixed,ii.indcnj.predicted.my.sem.fixed)
ii.indcnj.p.values.my.sem.fixed <- cbind(as.vector(fitted(ii.indcnj.glm.sem.fixed)), 1-as.vector(fitted(ii.indcnj.glm.sem.fixed)))
colnames(ii.indcnj.p.values.my.sem.fixed) <- c("Ind","eCnj")
ii.indcnj.ModelStats.my.sem.fixed<- model.statistics(ii.indcnj.observed.my.sem.fixed, ii.indcnj.predicted.my.sem.fixed, ii.indcnj.p.values.my.sem.fixed)

ai.indcnj.observed.my.sem.fixed <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.indcnj.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ai.indcnj.glm.sem.fixed))>0.5, "Ind", "eCnj")
table(ai.indcnj.observed.my.sem.fixed,ai.indcnj.predicted.my.sem.fixed)
ai.indcnj.p.values.my.sem.fixed <- cbind(as.vector(fitted(ai.indcnj.glm.sem.fixed)), 1-as.vector(fitted(ai.indcnj.glm.sem.fixed)))
colnames(ai.indcnj.p.values.my.sem.fixed) <- c("Ind","eCnj")
ai.indcnj.ModelStats.my.sem.fixed<- model.statistics(ai.indcnj.observed.my.sem.fixed, ai.indcnj.predicted.my.sem.fixed, ai.indcnj.p.values.my.sem.fixed)

ti.indcnj.observed.my.sem.fixed <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.indcnj.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ti.indcnj.glm.sem.fixed))>0.5, "Ind", "eCnj")
table(ti.indcnj.observed.my.sem.fixed,ti.indcnj.predicted.my.sem.fixed)
ti.indcnj.p.values.my.sem.fixed <- cbind(as.vector(fitted(ti.indcnj.glm.sem.fixed)), 1-as.vector(fitted(ti.indcnj.glm.sem.fixed)))
colnames(ti.indcnj.p.values.my.sem.fixed) <- c("Ind","eCnj")
ti.indcnj.ModelStats.my.sem.fixed<- model.statistics(ti.indcnj.observed.my.sem.fixed, ti.indcnj.predicted.my.sem.fixed, ti.indcnj.p.values.my.sem.fixed)

ta.indcnj.observed.my.sem.fixed <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.indcnj.predicted.my.sem.fixed <- ifelse(as.vector(fitted(ta.indcnj.glm.sem.fixed))>0.5, "Ind", "eCnj")
table(ta.indcnj.observed.my.sem.fixed,ta.indcnj.predicted.my.sem.fixed)
ta.indcnj.p.values.my.sem.fixed <- cbind(as.vector(fitted(ta.indcnj.glm.sem.fixed)), 1-as.vector(fitted(ta.indcnj.glm.sem.fixed)))
colnames(ta.indcnj.p.values.my.sem.fixed) <- c("Ind","eCnj")
ta.indcnj.ModelStats.my.sem.fixed<- model.statistics(ta.indcnj.observed.my.sem.fixed, ta.indcnj.predicted.my.sem.fixed, ta.indcnj.p.values.my.sem.fixed)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-indcnj-ModelStats.my.sem.fixed.txt')
print(ii.indcnj.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-indcnj-ModelStats.my.sem.fixed.txt')
print(ai.indcnj.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-indcnj-ModelStats.my.sem.fixed.txt')
print(ti.indcnj.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-indcnj-ModelStats.my.sem.fixed.txt')
print(ta.indcnj.ModelStats.my.sem.fixed, max.print = NA)
sink(NULL)

