#Ind vs eCnj
#============================================================================================================
### VII
ii.ive.glmer.sem <- glmer(Ind ~ II.sense + II.weather + NI.object.actor + (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/ive-ii-sem.txt')
round(print(summary(ii.ive.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.ive.glmer.sem <- glmer(Ind ~ PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Time + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.plural + NA.persons.actor + NDA.Relations.actor + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/ive-ai-sem.txt')
round(print(summary(ai.ive.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.ive.glmer.sem <- glmer(Ind ~ PV.Time + PV.Qual + PV.Discourse + TI.cognitive + TI.speech + NA.persons.actor + NI.object.goal + NI.natural.force.goal + NI.nature.plants.goal + NI.place.goal + NDI.Body.goal + (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/ive-ti-sem.txt')
round(print(summary(ti.ive.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.ive.glmer.sem <- glmer(Ind ~ PV.Move + PV.Discourse + PV.Position + PV.Qual + PV.Time + TA.speech + TA.do + TA.cognitive + TA.food + D.actor + Pers.actor + NA.persons.goal + NA.beast.of.burden.goal + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/ive-ta-sem.txt')
round(print(summary(ta.ive.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)


#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
ii.indcnj.glmer.sem <- glmer(Ind ~ II.sense + II.weather + NI.nominal.actor + (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/ivc-ii-sem.txt')
round(print(summary(ii.indcnj.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.indcnj.glmer.sem <- glmer(Ind ~ PV.Move + PV.Time + PV.Position + PV.Qual + PV.Discourse + PV.StartFin + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.pray + NA.persons.actor + Pers.actor + (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/ivc-ai-sem.txt')
round(print(summary(ai.indcnj.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.indcnj.glmer.sem <- glmer(Ind ~ PV.Time + PV.Discourse + PV.Qual + TI.cognitive + TI.money.count + NI.object.goal + NA.persons.actor + NI.nominal.goal + NI.natural.force.goal + NI.nature.plants.goal + NI.place.goal + NDI.Body.goal + (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/ivc-ti-sem.txt')
round(print(summary(ti.indcnj.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.indcnj.glmer.sem <- glmer(Ind ~ PV.Time + PV.Move + PV.Position + PV.Discourse + PV.Qual + TA.speech + TA.do + TA.cognitive + TA.food + TA.money.count + NA.persons.goal + (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/ivc-ta-sem.txt')
round(print(summary(ta.indcnj.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
ii.e.cnjtype.glmer.sem <- glmer(PV.e ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/e-cnj-ii-sem.txt')
round(print(summary(ii.e.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.e.cnjtype.glmer.sem <- glmer(PV.e ~ PV.Time + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + NDA.Relations.actor + Dem.actor + Pers.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/e-cnj-ai-sem.txt')
round(print(summary(ai.e.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.e.cnjtype.glmer.sem <- glmer(PV.e ~ PV.Time + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + TI.cognitive + TI.speech + NA.persons.actor + Dem.actor + NI.object.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/e-cnj-ti-sem.txt')
round(print(summary(ti.e.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.e.cnjtype.glmer.sem <- glmer(PV.e ~PV.Time + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + D.actor + Prox.actor + NDA.Relations.goal + Prox.goal + NA.beast.of.burden.goal + Pers.goal + NA.food.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/e-cnj-ta-sem.txt')
round(print(summary(ta.e.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

#-------------------------------------------------------
### VII
### VII
ii.kaa.cnjtype.glmer.sem <- glmer(PV.kaa ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/kaa-cnj-ii-sem.txt')
round(print(summary(ii.kaa.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.kaa.cnjtype.glmer.sem <- glmer(PV.kaa ~ PV.Time + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + NDA.Relations.actor + Dem.actor + Pers.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/kaa-cnj-ai-sem.txt')
round(print(summary(ai.kaa.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.kaa.cnjtype.glmer.sem <- glmer(PV.kaa ~ PV.Time + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + TI.cognitive + TI.speech + NA.persons.actor + Dem.actor + NI.object.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/kaa-cnj-ti-sem.txt')
round(print(summary(ti.kaa.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.kaa.cnjtype.glmer.sem <- glmer(PV.kaa ~PV.Time + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + D.actor + Prox.actor + NDA.Relations.goal + Prox.goal + NA.beast.of.burden.goal + Pers.goal + NA.food.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/kaa-cnj-ta-sem.txt')
round(print(summary(ta.kaa.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)
#-------------------------------------------------------
### VII
ii.other.cnjtype.glmer.sem <- glmer(OtherCnj ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/other-cnj-ii-sem.txt')
round(print(summary(ii.other.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.other.cnjtype.glmer.sem <- glmer(OtherCnj ~ PV.Time + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + NDA.Relations.actor + Pers.actor + Prox.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/other-cnj-ai-sem.txt')
round(print(summary(ai.other.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.other.cnjtype.glmer.sem <- glmer(OtherCnj ~ PV.Time + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + TI.cognitive + TI.speech + NA.persons.actor + Dem.actor + NI.object.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/other-cnj-ti-sem.txt')
round(print(summary(ti.other.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.other.cnjtype.glmer.sem <- glmer(OtherCnj ~ PV.Time + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + D.actor + Prox.actor + NDA.Relations.goal + Dem.goal + NA.beast.of.burden.goal + Pers.goal + NA.food.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/sem/other-cnj-ta-sem.txt')
round(print(summary(ta.other.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

