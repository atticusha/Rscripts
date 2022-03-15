#Ind vs eCnj
#============================================================================================================
### VII
ii.ive.glmer.time <- glmer(Ind ~ II.sense + II.weather + NI.object.actor + (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ii-time.txt')
print(summary(ii.ive.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.ive.glmer.time <- glmer(Ind ~ PV.Move + PV.Qual + PV.StartFin + PV.Discourse + Fut + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.plural + RdplW + NA.persons.actor + Pl.actor + NDA.Relations.actor + actor.1 + actor.2 + actor.4 + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ai-time-time.txt')
print(summary(ai.ive.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VTI -- + fut wont work
ti.ive.glmer.time <- glmer(Ind ~  PV.Qual + PV.Discourse + TI.cognitive + TI.speech + NA.persons.actor + actor.1 + actor.2 + actor.4 + Sg.goal + NI.object.goal + Pl.goal + NI.natural.force.goal + NI.nature.plants.goal + NI.place.goal + NDI.Body.goal + Px1Sg.goal + Px3Pl.goal + (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ti-time.txt')
print(summary(ti.ive.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.ive.glmer.time <- glmer(Ind ~ PV.Move + PV.Discourse + PV.Position + PV.Qual + Fut + TA.speech + TA.do + TA.cognitive + TA.food +  Sg.actor + D.actor + Pers.actor + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + NA.persons.goal + NA.beast.of.burden.goal + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ta-time.txt')
print(summary(ta.ive.glmer.time)$coefficients, max.print = NA)
sink(NULL)


#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
ii.indcnj.glmer.time <- glmer(Ind ~ II.sense + II.weather + NI.nominal.actor + (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ii-time.txt')
print(summary(ii.indcnj.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.indcnj.glmer.time <- glmer(Ind ~ PV.Move + Fut + PV.Position + PV.Qual + PV.Discourse + PV.StartFin + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.pray + RdplW + NA.persons.actor + Pl.actor + Pers.actor + actor.1 + actor.2 + actor.4 + (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ai-time.txt')
print(summary(ai.indcnj.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.indcnj.glmer.time <- glmer(Ind ~ Fut + PV.Discourse + PV.Qual + TI.cognitive + TI.money.count + Pron.actor + actor.1 + actor.2 + actor.4 + Sg.goal + NI.object.goal + Pl.goal + NI.nominal.goal + NI.natural.force.goal + NI.nature.plants.goal + NI.place.goal + NDI.Body.goal + (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ti-time.txt')
print(summary(ti.indcnj.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.indcnj.glmer.time <- glmer(Ind ~ Fut + PV.Move + PV.Position + PV.Discourse + PV.StartFinWantCan + PV.Qual + TA.speech + TA.do + TA.cognitive + TA.food + TA.money.count + Sg.actor + actor.1 + actor.2 + actor.4 + goal.3 + goal.4 + goal.2 + NA.persons.goal + (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ta-time.txt')
print(summary(ta.indcnj.glmer.time)$coefficients, max.print = NA)
sink(NULL)

#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
ii.e.cnjtype.glmer.time <- glmer(PV.e ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ii-time.txt')
print(summary(ii.e.cnjtype.glmer.time)$coefficients, max.print = NA)
sink(NULL)


### VAI
ai.e.cnjtype.glmer.time <- glmer(PV.e ~ Fut + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + Pers.actor + Prox.actor + Indef.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ai-time.txt')
print(summary(ai.e.cnjtype.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.e.cnjtype.glmer.time <- glmer(PV.e ~ Fut + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + TI.cognitive + TI.speech + NA.persons.actor + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + NI.object.goal + Pl.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ti-time.txt')
print(summary(ti.e.cnjtype.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.e.cnjtype.glmer.time <- glmer(PV.e ~Fut + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + RdplS + Sg.actor + D.actor + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + NDA.Relations.goal + Dem.goal + NA.beast.of.burden.goal + Pers.goal + NA.food.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ta-time.txt')
print(summary(ta.e.cnjtype.glmer.time)$coefficients, max.print = NA)
sink(NULL)

#-------------------------------------------------------
### VII
ii.kaa.cnjtype.glmer.time <- glmer(PV.kaa ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ii-time.txt')
print(summary(ii.kaa.cnjtype.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.kaa.cnjtype.glmer.time <- glmer(PV.kaa ~ Fut + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + Pers.actor + Prox.actor + Indef.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ai-time.txt')
print(summary(ai.kaa.cnjtype.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.kaa.cnjtype.glmer.time <- glmer(PV.kaa ~ Fut + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + TI.cognitive + TI.speech + NA.persons.actor + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + NI.object.goal + Pl.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ti-time.txt')
print(summary(ti.kaa.cnjtype.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.kaa.cnjtype.glmer.time <- glmer(PV.kaa ~ Fut + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + RdplS + Sg.actor + D.actor + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + NDA.Relations.goal + Dem.goal + NA.beast.of.burden.goal + Pers.goal + NA.food.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ta-time.txt')
print(summary(ta.kaa.cnjtype.glmer.time)$coefficients, max.print = NA)
sink(NULL)

#-------------------------------------------------------
### VII
ii.other.cnjtype.glmer.time <- glmer(OtherCnj ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ii-time.txt')
print(summary(ii.other.cnjtype.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.other.cnjtype.glmer.time <- glmer(OtherCnj ~  Fut + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + Pers.actor + Prox.actor + Indef.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ai-time.txt')
print(summary(ai.other.cnjtype.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.other.cnjtype.glmer.time <- glmer(OtherCnj ~ Fut + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + TI.cognitive + TI.speech + NA.persons.actor + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + NI.object.goal + Pl.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ti-time.txt')
print(summary(ti.other.cnjtype.glmer.time)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.other.cnjtype.glmer.time <- glmer(OtherCnj ~ Fut + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + RdplS + Sg.actor + D.actor + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + NDA.Relations.goal + Dem.goal + NA.beast.of.burden.goal + Pers.goal + NA.food.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ta-time.txt')
print(summary(ta.other.cnjtype.glmer.time)$coefficients, max.print = NA)
sink(NULL)
