#MODEL STATISTICS

### Ind v. eCnj
ii.ive.observed.my <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my <- ifelse(as.vector(fitted(ii.ive.glmer))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my,ii.ive.predicted.my)
ii.ive.p.values.my <- cbind(as.vector(fitted(ii.ive.glmer)), 1-as.vector(fitted(ii.ive.glmer)))
colnames(ii.ive.p.values.my) <- c("Ind","eCnj")
ii.ive.ModelStats.my<- model.statistics(ii.ive.observed.my, ii.ive.predicted.my, ii.ive.p.values.my)

ai.ive.observed.my <- ifelse(subset(AWive, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.ive.predicted.my <- ifelse(as.vector(fitted(ai.ive.glmer))>0.5, "Ind", "eCnj")
table(ai.ive.observed.my,ai.ive.predicted.my)
ai.ive.p.values.my <- cbind(as.vector(fitted(ai.ive.glmer)), 1-as.vector(fitted(ai.ive.glmer)))
colnames(ai.ive.p.values.my) <- c("Ind","eCnj")
ai.ive.ModelStats.my<- model.statistics(ai.ive.observed.my, ai.ive.predicted.my, ai.ive.p.values.my)

ti.ive.observed.my <- ifelse(subset(AWive, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.ive.predicted.my <- ifelse(as.vector(fitted(ti.ive.glmer))>0.5, "Ind", "eCnj")
table(ti.ive.observed.my,ti.ive.predicted.my)
ti.ive.p.values.my <- cbind(as.vector(fitted(ti.ive.glmer)), 1-as.vector(fitted(ti.ive.glmer)))
colnames(ti.ive.p.values.my) <- c("Ind","eCnj")
ti.ive.ModelStats.my<- model.statistics(ti.ive.observed.my, ti.ive.predicted.my, ti.ive.p.values.my)

ta.ive.observed.my <- ifelse(subset(AWive, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.ive.predicted.my <- ifelse(as.vector(fitted(ta.ive.glmer))>0.5, "Ind", "eCnj")
table(ta.ive.observed.my,ta.ive.predicted.my)
ta.ive.p.values.my <- cbind(as.vector(fitted(ta.ive.glmer)), 1-as.vector(fitted(ta.ive.glmer)))
colnames(ta.ive.p.values.my) <- c("Ind","eCnj")
ta.ive.ModelStats.my<- model.statistics(ta.ive.observed.my, ta.ive.predicted.my, ta.ive.p.values.my)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-ive-ModelStats-my.txt')
print(ii.ive.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-ive-ModelStats-my.txt')
print(ai.ive.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-ive-ModelStats-my.txt')
print(ti.ive.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-ive-ModelStats-my.txt')
print(ta.ive.ModelStats.my, max.print = NA)
sink(NULL)








#================================================
### eCnj v. kaaCnj v. OtherCnj
#================================================

ii.ive.observed.my <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my <- ifelse(as.vector(fitted(ii.ive.glmer))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my,ii.ive.predicted.my)
ii.ive.p.values.my <- cbind(as.vector(fitted(ii.ive.glmer)), 1-as.vector(fitted(ii.ive.glmer)))
colnames(ii.ive.p.values.my) <- c("Ind","eCnj")
ii.ive.ModelStats.my<- model.statistics(ii.ive.observed.my, ii.ive.predicted.my, ii.ive.p.values.my)

ii.eCnj.cnjtypes.observed.my <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ii.eCnj.cnjtypes.predicted.my <- ifelse(as.vector(fitted(ii.e.cnjtype.glmer))>0.5, "eCnj", "otherCnj")
table(ii.eCnj.cnjtypes.observed.my,ii.eCnj.cnjtypes.predicted.my)
ii.eCnj.cnjtypes.p.values.my <- cbind(as.vector(fitted(ii.e.cnjtype.glmer)), 1-as.vector(fitted(ii.e.cnjtype.glmer)))
colnames(ii.eCnj.cnjtypes.p.values.my) <- c("eCnj","otherCnj")
ii.eCnj.cnjtypes.ModelStats.my<- model.statistics(ii.eCnj.cnjtypes.observed.my, ii.eCnj.cnjtypes.predicted.my, ii.eCnj.cnjtypes.p.values.my)

ii.kaaCnj.cnjtypes.observed.my <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ii.kaaCnj.cnjtypes.predicted.my <- ifelse(as.vector(fitted(ii.kaa.cnjtype.glmer))>0.5, "kaaCnj", "otherCnj")
table(ii.kaaCnj.cnjtypes.observed.my,ii.kaaCnj.cnjtypes.predicted.my)
ii.kaaCnj.cnjtypes.p.values.my <- cbind(as.vector(fitted(ii.kaa.cnjtype.glmer)), 1-as.vector(fitted(ii.kaa.cnjtype.glmer)))
colnames(ii.kaaCnj.cnjtypes.p.values.my) <- c("kaaCnj","otherCnj")
ii.kaaCnj.cnjtypes.ModelStats.my<- model.statistics(ii.kaaCnj.cnjtypes.observed.my, ii.kaaCnj.cnjtypes.predicted.my, ii.kaaCnj.cnjtypes.p.values.my)

ii.OtherCnj.cnjtypes.observed.my <- ifelse(subset(AWCnj, II, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ii.OtherCnj.cnjtypes.predicted.my <- ifelse(as.vector(fitted(ii.other.cnjtype.glmer))>0.5, "OtherCnj", "otherCnj")
table(ii.OtherCnj.cnjtypes.observed.my,ii.OtherCnj.cnjtypes.predicted.my)
ii.OtherCnj.cnjtypes.p.values.my <- cbind(as.vector(fitted(ii.other.cnjtype.glmer)), 1-as.vector(fitted(ii.other.cnjtype.glmer)))
colnames(ii.OtherCnj.cnjtypes.p.values.my) <- c("OtherCnj","otherCnj")
ii.OtherCnj.cnjtypes.ModelStats.my<- model.statistics(ii.OtherCnj.cnjtypes.observed.my, ii.OtherCnj.cnjtypes.predicted.my, ii.OtherCnj.cnjtypes.p.values.my)

ai.eCnj.cnjtypes.observed.my <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ai.eCnj.cnjtypes.predicted.my <- ifelse(as.vector(fitted(ai.e.cnjtype.glmer))>0.5, "eCnj", "otherCnj")
table(ai.eCnj.cnjtypes.observed.my,ai.eCnj.cnjtypes.predicted.my)
ai.eCnj.cnjtypes.p.values.my <- cbind(as.vector(fitted(ai.e.cnjtype.glmer)), 1-as.vector(fitted(ai.e.cnjtype.glmer)))
colnames(ai.eCnj.cnjtypes.p.values.my) <- c("eCnj","otherCnj")
ai.eCnj.cnjtypes.ModelStats.my<- model.statistics(ai.eCnj.cnjtypes.observed.my, ai.eCnj.cnjtypes.predicted.my, ai.eCnj.cnjtypes.p.values.my)

ai.kaaCnj.cnjtypes.observed.my <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ai.kaaCnj.cnjtypes.predicted.my <- ifelse(as.vector(fitted(ai.kaa.cnjtype.glmer))>0.5, "kaaCnj", "otherCnj")
table(ai.kaaCnj.cnjtypes.observed.my,ai.kaaCnj.cnjtypes.predicted.my)
ai.kaaCnj.cnjtypes.p.values.my <- cbind(as.vector(fitted(ai.kaa.cnjtype.glmer)), 1-as.vector(fitted(ai.kaa.cnjtype.glmer)))
colnames(ai.kaaCnj.cnjtypes.p.values.my) <- c("kaaCnj","otherCnj")
ai.kaaCnj.cnjtypes.ModelStats.my<- model.statistics(ai.kaaCnj.cnjtypes.observed.my, ai.kaaCnj.cnjtypes.predicted.my, ai.kaaCnj.cnjtypes.p.values.my)

ai.OtherCnj.cnjtypes.observed.my <- ifelse(subset(AWCnj, AI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ai.OtherCnj.cnjtypes.predicted.my <- ifelse(as.vector(fitted(ai.other.cnjtype.glmer))>0.5, "OtherCnj", "otherCnj")
table(ai.OtherCnj.cnjtypes.observed.my,ai.OtherCnj.cnjtypes.predicted.my)
ai.OtherCnj.cnjtypes.p.values.my <- cbind(as.vector(fitted(ai.other.cnjtype.glmer)), 1-as.vector(fitted(ai.other.cnjtype.glmer)))
colnames(ai.OtherCnj.cnjtypes.p.values.my) <- c("OtherCnj","otherCnj")
ai.OtherCnj.cnjtypes.ModelStats.my<- model.statistics(ai.OtherCnj.cnjtypes.observed.my, ai.OtherCnj.cnjtypes.predicted.my, ai.OtherCnj.cnjtypes.p.values.my)

ti.eCnj.cnjtypes.observed.my <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ti.eCnj.cnjtypes.predicted.my <- ifelse(as.vector(fitted(ti.e.cnjtype.glmer))>0.5, "eCnj", "otherCnj")
table(ti.eCnj.cnjtypes.observed.my,ti.eCnj.cnjtypes.predicted.my)
ti.eCnj.cnjtypes.p.values.my <- cbind(as.vector(fitted(ti.e.cnjtype.glmer)), 1-as.vector(fitted(ti.e.cnjtype.glmer)))
colnames(ti.eCnj.cnjtypes.p.values.my) <- c("eCnj","otherCnj")
ti.eCnj.cnjtypes.ModelStats.my<- model.statistics(ti.eCnj.cnjtypes.observed.my, ti.eCnj.cnjtypes.predicted.my, ti.eCnj.cnjtypes.p.values.my)

ti.kaaCnj.cnjtypes.observed.my <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ti.kaaCnj.cnjtypes.predicted.my <- ifelse(as.vector(fitted(ti.kaa.cnjtype.glmer))>0.5, "kaaCnj", "otherCnj")
table(ti.kaaCnj.cnjtypes.observed.my,ti.kaaCnj.cnjtypes.predicted.my)
ti.kaaCnj.cnjtypes.p.values.my <- cbind(as.vector(fitted(ti.kaa.cnjtype.glmer)), 1-as.vector(fitted(ti.kaa.cnjtype.glmer)))
colnames(ti.kaaCnj.cnjtypes.p.values.my) <- c("kaaCnj","otherCnj")
ti.kaaCnj.cnjtypes.ModelStats.my<- model.statistics(ti.kaaCnj.cnjtypes.observed.my, ti.kaaCnj.cnjtypes.predicted.my, ti.kaaCnj.cnjtypes.p.values.my)

ti.OtherCnj.cnjtypes.observed.my <- ifelse(subset(AWCnj, TI, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ti.OtherCnj.cnjtypes.predicted.my <- ifelse(as.vector(fitted(ti.other.cnjtype.glmer))>0.5, "OtherCnj", "otherCnj")
table(ti.OtherCnj.cnjtypes.observed.my,ti.OtherCnj.cnjtypes.predicted.my)
ti.OtherCnj.cnjtypes.p.values.my <- cbind(as.vector(fitted(ti.other.cnjtype.glmer)), 1-as.vector(fitted(ti.other.cnjtype.glmer)))
colnames(ti.OtherCnj.cnjtypes.p.values.my) <- c("OtherCnj","otherCnj")
ti.OtherCnj.cnjtypes.ModelStats.my<- model.statistics(ti.OtherCnj.cnjtypes.observed.my, ti.OtherCnj.cnjtypes.predicted.my, ti.OtherCnj.cnjtypes.p.values.my)

ta.eCnj.cnjtypes.observed.my <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "eCnj", "eCnj", "otherCnj")
ta.eCnj.cnjtypes.predicted.my <- ifelse(as.vector(fitted(ta.e.cnjtype.glmer))>0.5, "eCnj", "otherCnj")
table(ta.eCnj.cnjtypes.observed.my,ta.eCnj.cnjtypes.predicted.my)
ta.eCnj.cnjtypes.p.values.my <- cbind(as.vector(fitted(ta.e.cnjtype.glmer)), 1-as.vector(fitted(ta.e.cnjtype.glmer)))
colnames(ta.eCnj.cnjtypes.p.values.my) <- c("eCnj","otherCnj")
ta.eCnj.cnjtypes.ModelStats.my<- model.statistics(ta.eCnj.cnjtypes.observed.my, ta.eCnj.cnjtypes.predicted.my, ta.eCnj.cnjtypes.p.values.my)

ta.kaaCnj.cnjtypes.observed.my <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "kaaCnj", "kaaCnj", "otherCnj")
ta.kaaCnj.cnjtypes.predicted.my <- ifelse(as.vector(fitted(ta.kaa.cnjtype.glmer))>0.5, "kaaCnj", "otherCnj")
table(ta.kaaCnj.cnjtypes.observed.my,ta.kaaCnj.cnjtypes.predicted.my)
ta.kaaCnj.cnjtypes.p.values.my <- cbind(as.vector(fitted(ta.kaa.cnjtype.glmer)), 1-as.vector(fitted(ta.kaa.cnjtype.glmer)))
colnames(ta.kaaCnj.cnjtypes.p.values.my) <- c("kaaCnj","otherCnj")
ta.kaaCnj.cnjtypes.ModelStats.my<- model.statistics(ta.kaaCnj.cnjtypes.observed.my, ta.kaaCnj.cnjtypes.predicted.my, ta.kaaCnj.cnjtypes.p.values.my)

ta.OtherCnj.cnjtypes.observed.my <- ifelse(subset(AWCnj, TA, OrderType, drop=TRUE) == "Other", "OtherCnj", "otherCnj")
ta.OtherCnj.cnjtypes.predicted.my <- ifelse(as.vector(fitted(ta.other.cnjtype.glmer))>0.5, "OtherCnj", "otherCnj")
table(ta.OtherCnj.cnjtypes.observed.my,ta.OtherCnj.cnjtypes.predicted.my)
ta.OtherCnj.cnjtypes.p.values.my <- cbind(as.vector(fitted(ta.other.cnjtype.glmer)), 1-as.vector(fitted(ta.other.cnjtype.glmer)))
colnames(ta.OtherCnj.cnjtypes.p.values.my) <- c("OtherCnj","otherCnj")
ta.OtherCnj.cnjtypes.ModelStats.my<- model.statistics(ta.OtherCnj.cnjtypes.observed.my, ta.OtherCnj.cnjtypes.predicted.my, ta.OtherCnj.cnjtypes.p.values.my)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-ecnj-cnjtypes-ModelStats-my.txt')
print(ii.eCnj.cnjtypes.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-ecnj-cnjtypes-ModelStats-my.txt')
print(ai.eCnj.cnjtypes.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-ecnj-cnjtypes-ModelStats-my.txt')
print(ti.eCnj.cnjtypes.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-ecnj-cnjtypes-ModelStats-my.txt')
print(ta.eCnj.cnjtypes.ModelStats.my, max.print = NA)
sink(NULL)

sink(file = 'modelstats/Manual/ii-kaacnj-cnjtypes-ModelStats-my.txt')
print(ii.kaaCnj.cnjtypes.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-kaacnj-cnjtypes-ModelStats-my.txt')
print(ai.kaaCnj.cnjtypes.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-kaacnj-cnjtypes-ModelStats-my.txt')
print(ti.kaaCnj.cnjtypes.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-kaacnj-cnjtypes-ModelStats-my.txt')
print(ta.kaaCnj.cnjtypes.ModelStats.my, max.print = NA)
sink(NULL)

sink(file = 'modelstats/Manual/ii-othercnj-cnjtypes-ModelStats-my.txt')
print(ii.OtherCnj.cnjtypes.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-othercnj-cnjtypes-ModelStats-my.txt')
print(ai.OtherCnj.cnjtypes.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-othercnj-cnjtypes-ModelStats-my.txt')
print(ti.OtherCnj.cnjtypes.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-othercnj-cnjtypes-ModelStats-my.txt')
print(ta.OtherCnj.cnjtypes.ModelStats.my, max.print = NA)
sink(NULL)



#================================================
### Ind v. Cnj
#================================================

ii.indcnj.observed.my <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.indcnj.predicted.my <- ifelse(as.vector(fitted(ii.indcnj.glmer))>0.5, "Ind", "eCnj")
table(ii.indcnj.observed.my,ii.indcnj.predicted.my)
ii.indcnj.p.values.my <- cbind(as.vector(fitted(ii.indcnj.glmer)), 1-as.vector(fitted(ii.indcnj.glmer)))
colnames(ii.indcnj.p.values.my) <- c("Ind","eCnj")
ii.indcnj.ModelStats.my<- model.statistics(ii.indcnj.observed.my, ii.indcnj.predicted.my, ii.indcnj.p.values.my)

ai.indcnj.observed.my <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.indcnj.predicted.my <- ifelse(as.vector(fitted(ai.indcnj.glmer))>0.5, "Ind", "eCnj")
table(ai.indcnj.observed.my,ai.indcnj.predicted.my)
ai.indcnj.p.values.my <- cbind(as.vector(fitted(ai.indcnj.glmer)), 1-as.vector(fitted(ai.indcnj.glmer)))
colnames(ai.indcnj.p.values.my) <- c("Ind","eCnj")
ai.indcnj.ModelStats.my<- model.statistics(ai.indcnj.observed.my, ai.indcnj.predicted.my, ai.indcnj.p.values.my)

ti.indcnj.observed.my <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.indcnj.predicted.my <- ifelse(as.vector(fitted(ti.indcnj.glmer))>0.5, "Ind", "eCnj")
table(ti.indcnj.observed.my,ti.indcnj.predicted.my)
ti.indcnj.p.values.my <- cbind(as.vector(fitted(ti.indcnj.glmer)), 1-as.vector(fitted(ti.indcnj.glmer)))
colnames(ti.indcnj.p.values.my) <- c("Ind","eCnj")
ti.indcnj.ModelStats.my<- model.statistics(ti.indcnj.observed.my, ti.indcnj.predicted.my, ti.indcnj.p.values.my)

ta.indcnj.observed.my <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.indcnj.predicted.my <- ifelse(as.vector(fitted(ta.indcnj.glmer))>0.5, "Ind", "eCnj")
table(ta.indcnj.observed.my,ta.indcnj.predicted.my)
ta.indcnj.p.values.my <- cbind(as.vector(fitted(ta.indcnj.glmer)), 1-as.vector(fitted(ta.indcnj.glmer)))
colnames(ta.indcnj.p.values.my) <- c("Ind","eCnj")
ta.indcnj.ModelStats.my<- model.statistics(ta.indcnj.observed.my, ta.indcnj.predicted.my, ta.indcnj.p.values.my)

## Save Model stats##
sink(file = 'modelstats/Manual/ii-indcnj-ModelStats-my.txt')
print(ii.indcnj.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ai-indcnj-ModelStats-my.txt')
print(ai.indcnj.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ti-indcnj-ModelStats-my.txt')
print(ti.indcnj.ModelStats.my, max.print = NA)
sink(NULL)
sink(file = 'modelstats/Manual/ta-indcnj-ModelStats-my.txt')
print(ta.indcnj.ModelStats.my, max.print = NA)
sink(NULL)

