#Ind vs eCnj
#============================================================================================================
### VII
ii.ive.glmer.sem <- glmer(Ind ~ PV.Time + II.sense + NI.object.actor + Dem.actor + (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ii.sem.txt')
round(print(summary(ii.ive.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.ive.glmer.sem <- glmer(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + NA.persons.actor + NA.beast.of.burden.actor + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ai.sem.txt')
round(print(summary(ai.ive.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.ive.glmer.sem <- glmer(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + NI.natural.force.goal + NI.place.goal  + (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ti.sem.txt')
round(print(summary(ti.ive.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.ive.glmer.sem <- glmer(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + NDA.Relations.actor   + NA.persons.goal  + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ta.sem.txt')
round(print(summary(ta.ive.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)


#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
ii.indcnj.glmer.sem <- glmer(Ind ~ PV.Time + II.sense + NI.object.actor + Med.actor + (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ii.sem.txt')
round(print(summary(ii.indcnj.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.indcnj.glmer.sem <- glmer(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + NA.persons.actor + NA.beast.of.burden.actor + NA.food.actor + (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ai.sem.txt')
round(print(summary(ai.indcnj.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.indcnj.glmer.sem <- glmer(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + NI.nominal.goal + NI.natural.force.goal + NDI.Body.goal + NI.place.goal + Der.Dim.goal  + (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ti.sem.txt')
round(print(summary(ti.indcnj.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.indcnj.glmer.sem <- glmer(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + NA.persons.actor + NDA.Relations.actor + NA.persons.goal  + (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ta.sem.txt')
round(print(summary(ta.indcnj.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
ii.e.cnjtype.glmer.sem <- glmer(PV.e ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ii.sem.txt')
round(print(summary(ii.e.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.e.cnjtype.glmer.sem <- glmer(PV.e ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ai.sem.txt')
round(print(summary(ai.e.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.e.cnjtype.glmer.sem <- glmer(PV.e ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ti.sem.txt')
round(print(summary(ti.e.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.e.cnjtype.glmer.sem <- glmer(PV.e ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ta.sem.txt')
round(print(summary(ta.e.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

#-------------------------------------------------------
### VII
ii.kaa.cnjtype.glmer.sem <- glmer(PV.kaa ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ii.sem.txt')
round(print(summary(ii.kaa.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.kaa.cnjtype.glmer.sem <- glmer(PV.kaa ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ai.sem.txt')
round(print(summary(ai.kaa.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.kaa.cnjtype.glmer.sem <- glmer(PV.kaa ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ti.sem.txt')
round(print(summary(ti.kaa.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.kaa.cnjtype.glmer.sem <- glmer(PV.kaa ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ta.sem.txt')
round(print(summary(ta.kaa.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)
#-------------------------------------------------------
### VII
ii.other.cnjtype.glmer.sem <- glmer(OtherCnj ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ii.sem.txt')
round(print(summary(ii.other.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.other.cnjtype.glmer.sem <- glmer(OtherCnj ~  PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ai.sem.txt')
round(print(summary(ai.other.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.other.cnjtype.glmer.sem <- glmer(OtherCnj ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ti.sem.txt')
round(print(summary(ti.other.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.other.cnjtype.glmer.sem <- glmer(OtherCnj ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ta.sem.txt')
round(print(summary(ta.other.cnjtype.glmer.sem)$coefficients, max.print = NA), 3)
sink(NULL)

