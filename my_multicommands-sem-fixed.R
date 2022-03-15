#Ind vs eCnj
#============================================================================================================
### VII
ii.ive.glm.sem.fixed <- glm(Ind ~ PV.Time + II.sense + NI.object.actor + Dem.actor, data=subset(AWive, II), family=binomial)

sink(file = 'SummaryTables/man/ive-ii.sem.fixed.txt')
round(print(summary(ii.ive.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.ive.glm.sem.fixed <- glm(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + NA.persons.actor + NA.beast.of.burden.actor, data=subset(AWive, AI), family=binomial)

sink(file = 'SummaryTables/man/ive-ai.sem.fixed.txt')
round(print(summary(ai.ive.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.ive.glm.sem.fixed <- glm(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + NI.natural.force.goal + NI.place.goal, data=subset(AWive, TI), family=binomial)

sink(file = 'SummaryTables/man/ive-ti.sem.fixed.txt')
round(print(summary(ti.ive.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.ive.glm.sem.fixed <- glm(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + NDA.Relations.actor + NA.persons.goal, data=subset(AWive, TA), family=binomial)

sink(file = 'SummaryTables/man/ive-ta.sem.fixed.txt')
round(print(summary(ta.ive.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)


#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
ii.indcnj.glm.sem.fixed <- glm(Ind ~ PV.Time + II.sense + NI.object.actor + Med.actor, data=subset(AWnimp, II), family=binomial)

sink(file = 'SummaryTables/man/ivc-ii.sem.fixed.txt')
round(print(summary(ii.indcnj.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.indcnj.glm.sem.fixed <- glm(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + NA.persons.actor + NA.beast.of.burden.actor + NA.food.actor, data=subset(AWnimp, AI), family=binomial)

sink(file = 'SummaryTables/man/ivc-ai.sem.fixed.txt')
round(print(summary(ai.indcnj.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.indcnj.glm.sem.fixed <- glm(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + NI.nominal.goal + NI.natural.force.goal + NDI.Body.goal + NI.place.goal, data=subset(AWnimp, TI), family=binomial)

sink(file = 'SummaryTables/man/ivc-ti.sem.fixed.txt')
round(print(summary(ti.indcnj.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.indcnj.glm.sem.fixed <- glm(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + NA.persons.actor + NDA.Relations.actor + NA.persons.goal, data=subset(AWnimp, TA), family=binomial)

sink(file = 'SummaryTables/man/ivc-ta.sem.fixed.txt')
round(print(summary(ta.indcnj.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
ii.e.cnjtype.glm.sem.fixed <- glm(PV.e ~ II.sense + II.natural.land + II.weather + NI.object.actor, data=subset(AWCnj, II), family=binomial)

sink(file = 'SummaryTables/man/e-cnj-ii.sem.fixed.txt')
round(print(summary(ii.e.cnjtype.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.e.cnjtype.glm.sem.fixed <- glm(PV.e ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor, data=subset(AWCnj, AI), family=binomial)

sink(file = 'SummaryTables/man/e-cnj-ai.sem.fixed.txt')
round(print(summary(ai.e.cnjtype.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.e.cnjtype.glm.sem.fixed <- glm(PV.e ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = 'SummaryTables/man/e-cnj-ti.sem.fixed.txt')
round(print(summary(ti.e.cnjtype.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.e.cnjtype.glm.sem.fixed <- glm(PV.e ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = 'SummaryTables/man/e-cnj-ta.sem.fixed.txt')
round(print(summary(ta.e.cnjtype.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

#-------------------------------------------------------
### VII
ii.kaa.cnjtype.glm.sem.fixed <- glm(PV.kaa ~ II.sense + II.natural.land + II.weather + NI.object.actor, data=subset(AWCnj, II), family=binomial)

sink(file = 'SummaryTables/man/kaa-cnj-ii.sem.fixed.txt')
round(print(summary(ii.kaa.cnjtype.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.kaa.cnjtype.glm.sem.fixed <- glm(PV.kaa ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor, data=subset(AWCnj, AI), family=binomial)

sink(file = 'SummaryTables/man/kaa-cnj-ai.sem.fixed.txt')
round(print(summary(ai.kaa.cnjtype.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.kaa.cnjtype.glm.sem.fixed <- glm(PV.kaa ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = 'SummaryTables/man/kaa-cnj-ti.sem.fixed.txt')
round(print(summary(ti.kaa.cnjtype.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.kaa.cnjtype.glm.sem.fixed <- glm(PV.kaa ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = 'SummaryTables/man/kaa-cnj-ta.sem.fixed.txt')
round(print(summary(ta.kaa.cnjtype.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)
#-------------------------------------------------------
### VII
ii.other.cnjtype.glm.sem.fixed <- glm(OtherCnj ~ II.sense + II.natural.land + II.weather + NI.object.actor, data=subset(AWCnj, II), family=binomial)

sink(file = 'SummaryTables/man/other-cnj-ii.sem.fixed.txt')
round(print(summary(ii.other.cnjtype.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.other.cnjtype.glm.sem.fixed <- glm(OtherCnj ~  PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor, data=subset(AWCnj, AI), family=binomial)

sink(file = 'SummaryTables/man/other-cnj-ai.sem.fixed.txt')
round(print(summary(ai.other.cnjtype.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.other.cnjtype.glm.sem.fixed <- glm(OtherCnj ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = 'SummaryTables/man/other-cnj-ti.sem.fixed.txt')
round(print(summary(ti.other.cnjtype.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.other.cnjtype.glm.sem.fixed <- glm(OtherCnj ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = 'SummaryTables/man/other-cnj-ta.sem.fixed.txt')
round(print(summary(ta.other.cnjtype.glm.sem.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

