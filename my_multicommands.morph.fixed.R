#Ind vs eCnj
#============================================================================================================
### VII
ii.ive.glmer <- glmer(Ind ~ PV.Time + II.sense + NI.object.actor + Dem.actor + (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ii.txt')
round(print(summary(ii.ive.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.ive.glmer <- glmer(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + RdplW + NA.persons.actor + Pl.actor + NA.beast.of.burden.actor + actor.3 + actor.1 + actor.4 + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ai.txt')
round(print(summary(ai.ive.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.ive.glmer <- glmer(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + actor.3 + actor.2 + Pl.goal + NI.natural.force.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal + (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ti.txt')
round(print(summary(ti.ive.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.ive.glmer <- glmer(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + Sg.actor + NDA.Relations.actor + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + NA.persons.goal + Px3Sg.goal + Px3Pl.goal + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ta.txt')
round(print(summary(ta.ive.glmer)$coefficients, max.print = NA), 3)
sink(NULL)


#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
ii.indcnj.glmer <- glmer(Ind ~ PV.Time + II.sense + NI.object.actor + Med.actor + (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ii.txt')
round(print(summary(ii.indcnj.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.indcnj.glmer <- glmer(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NA.beast.of.burden.actor + NA.food.actor + actor.3 + actor.2 + actor.4 + (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ai.txt')
round(print(summary(ai.indcnj.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.indcnj.glmer <- glmer(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + actor.3 + actor.2 + Sg.goal + Pl.goal + NI.nominal.goal + NI.natural.force.goal + NDI.Body.goal + Px3Sg.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal + (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ti.txt')
round(print(summary(ti.indcnj.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.indcnj.glmer <- glmer(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + NA.persons.actor + Sg.actor + NDA.Relations.actor + actor.1 + actor.4 + goal.3 + goal.4 + goal.2 + NA.persons.goal + Px3Pl.goal + (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ta.txt')
round(print(summary(ta.indcnj.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
ii.e.cnjtype.glmer <- glmer(PV.e ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ii.txt')
round(print(summary(ii.e.cnjtype.glmer)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.e.cnjtype.glmer <- glmer(PV.e ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ai.txt')
round(print(summary(ai.e.cnjtype.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.e.cnjtype.glmer <- glmer(PV.e ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + Sg.actor + actor.3 + actor.2 + Sg.goal + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ti.txt')
round(print(summary(ti.e.cnjtype.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.e.cnjtype.glmer <- glmer(PV.e ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ta.txt')
round(print(summary(ta.e.cnjtype.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

#-------------------------------------------------------
### VII
ii.kaa.cnjtype.glmer <- glmer(PV.kaa ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ii.txt')
round(print(summary(ii.kaa.cnjtype.glmer)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.kaa.cnjtype.glmer <- glmer(PV.kaa ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ai.txt')
round(print(summary(ai.kaa.cnjtype.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.kaa.cnjtype.glmer <- glmer(PV.kaa ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + Sg.actor + actor.3 + actor.2 + Sg.goal + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ti.txt')
round(print(summary(ti.kaa.cnjtype.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.kaa.cnjtype.glmer <- glmer(PV.kaa ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ta.txt')
round(print(summary(ta.kaa.cnjtype.glmer)$coefficients, max.print = NA), 3)
sink(NULL)
#-------------------------------------------------------
### VII
ii.other.cnjtype.glmer <- glmer(OtherCnj ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ii.txt')
round(print(summary(ii.other.cnjtype.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.other.cnjtype.glmer <- glmer(OtherCnj ~  PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ai.txt')
round(print(summary(ai.other.cnjtype.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.other.cnjtype.glmer <- glmer(OtherCnj ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + Sg.actor + actor.3 + actor.2 + Sg.goal + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ti.txt')
round(print(summary(ti.other.cnjtype.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.other.cnjtype.glmer <- glmer(OtherCnj ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ta.txt')
round(print(summary(ta.other.cnjtype.glmer)$coefficients, max.print = NA), 3)
sink(NULL)
