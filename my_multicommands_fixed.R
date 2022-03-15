#Ind vs eCnj
#============================================================================================================
### VII
ii.ive.glmer.fixed <- glm(Ind ~ II.sense + II.weather + NI.object.actor, data=subset(AWive, II), family=binomial)

sink(file = 'SummaryTables/man/fixed/ive-ii.txt')
round(print(summary(ii.ive.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.ive.glmer.fixed <- glm(Ind ~ PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Time + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.plural + RdplW + NA.persons.actor + Pl.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.2 + actor.4 , data=subset(AWive, AI),family=binomial)

sink(file = 'SummaryTables/man/fixed/ive-ai.txt')
round(print(summary(ai.ive.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.ive.glmer.fixed <- glm(Ind ~ PV.Time + PV.Qual + PV.Discourse + TI.cognitive + TI.speech + NA.persons.actor + actor.1 + actor.2 + actor.4 + Sg.goal + NI.object.goal + Pl.goal + NI.natural.force.goal + NI.nature.plants.goal + NI.place.goal + NDI.Body.goal + Px1Sg.goal + Px3Pl.goal , data=subset(AWive, TI),family=binomial)

sink(file = 'SummaryTables/man/fixed/ive-ti.txt')
round(print(summary(ti.ive.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.ive.glmer.fixed <- glm(Ind ~ PV.Move + PV.Discourse + PV.Position + PV.Qual + PV.Time + TA.speech + TA.do + TA.cognitive + TA.food +  Sg.actor + D.actor + Pers.actor + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + NA.persons.goal + NA.beast.of.burden.goal , data=subset(AWive, TA),family=binomial)

sink(file = 'SummaryTables/man/fixed/ive-ta.txt')
round(print(summary(ta.ive.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)


#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
ii.indcnj.glmer.fixed <- glm(Ind ~ II.sense + II.weather + NI.nominal.actor , data=subset(AWnimp, II),family=binomial)

sink(file = 'SummaryTables/man/fixed/ivc-ii.txt')
round(print(summary(ii.indcnj.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.indcnj.glmer.fixed <- glm(Ind ~ PV.Move + PV.Time + PV.Position + PV.Qual + PV.Discourse + PV.StartFin + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.pray + RdplW + NA.persons.actor + Pl.actor + Pers.actor + actor.1 + actor.2 + actor.4 , data=subset(AWnimp, AI),family=binomial)

sink(file = 'SummaryTables/man/fixed/ivc-ai.txt')
round(print(summary(ai.indcnj.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.indcnj.glmer.fixed <- glm(Ind ~ PV.Time + PV.Discourse + PV.Qual + TI.cognitive + TI.money.count + actor.1 + actor.2 + actor.4 + Sg.goal + NI.object.goal + Pl.goal + NA.persons.actor + NI.nominal.goal + NI.natural.force.goal + NI.nature.plants.goal + NI.place.goal + NDI.Body.goal , data=subset(AWnimp, TI),family=binomial)

sink(file = 'SummaryTables/man/fixed/ivc-ti.txt')
round(print(summary(ti.indcnj.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.indcnj.glmer.fixed <- glm(Ind ~ PV.Time + PV.Move + PV.Position + PV.Discourse + PV.Qual + TA.speech + TA.do + TA.cognitive + TA.food + TA.money.count + Sg.actor + actor.1 + actor.2 + actor.4 + goal.3 + goal.4 + goal.2 + NA.persons.goal , data=subset(AWnimp, TA),family=binomial)

sink(file = 'SummaryTables/man/fixed/ivc-ta.txt')
round(print(summary(ta.indcnj.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
ii.e.cnjtype.glmer.fixed <- glm(PV.e ~ II.sense + II.natural.land + II.weather + NI.object.actor , data=subset(AWCnj, II),family=binomial)

sink(file = 'SummaryTables/man/fixed/e-cnj-ii.txt')
round(print(summary(ii.e.cnjtype.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.e.cnjtype.glmer.fixed <- glm(PV.e ~ PV.Time + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + Dem.actor + Pers.actor + Indef.actor + actor.3 + actor.1 + actor.2 , data=subset(AWCnj, AI),family=binomial)

sink(file = 'SummaryTables/man/fixed/e-cnj-ai.txt')
round(print(summary(ai.e.cnjtype.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.e.cnjtype.glmer.fixed <- glm(PV.e ~ PV.Time + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + TI.cognitive + TI.speech + NA.persons.actor + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + NI.object.goal + Pl.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal , data=subset(AWCnj, TI),family=binomial)

sink(file = 'SummaryTables/man/fixed/e-cnj-ti.txt')
round(print(summary(ti.e.cnjtype.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.e.cnjtype.glmer.fixed <- glm(PV.e ~PV.Time + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + RdplS + Sg.actor + D.actor + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + NDA.Relations.goal + Prox.goal + NA.beast.of.burden.goal + Pers.goal + NA.food.goal , data=subset(AWCnj, TA),family=binomial)

sink(file = 'SummaryTables/man/fixed/e-cnj-ta.txt')
round(print(summary(ta.e.cnjtype.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

#-------------------------------------------------------
### VII
### VII
ii.kaa.cnjtype.glmer.fixed <- glm(PV.kaa ~ II.sense + II.natural.land + II.weather + NI.object.actor , data=subset(AWCnj, II),family=binomial)

sink(file = 'SummaryTables/man/fixed/kaa-cnj-ii.txt')
round(print(summary(ii.kaa.cnjtype.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.kaa.cnjtype.glmer.fixed <- glm(PV.kaa ~ PV.Time + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + Dem.actor + Pers.actor + Indef.actor + actor.3 + actor.1 + actor.2 , data=subset(AWCnj, AI),family=binomial)

sink(file = 'SummaryTables/man/fixed/kaa-cnj-ai.txt')
round(print(summary(ai.kaa.cnjtype.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.kaa.cnjtype.glmer.fixed <- glm(PV.kaa ~ PV.Time + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + TI.cognitive + TI.speech + NA.persons.actor + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + NI.object.goal + Pl.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal , data=subset(AWCnj, TI),family=binomial)

sink(file = 'SummaryTables/man/fixed/kaa-cnj-ti.txt')
round(print(summary(ti.kaa.cnjtype.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.kaa.cnjtype.glmer.fixed <- glm(PV.kaa ~PV.Time + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + RdplS + Sg.actor + D.actor + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + NDA.Relations.goal + Prox.goal + NA.beast.of.burden.goal + Pers.goal + NA.food.goal , data=subset(AWCnj, TA),family=binomial)

sink(file = 'SummaryTables/man/fixed/kaa-cnj-ta.txt')
round(print(summary(ta.kaa.cnjtype.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)
#-------------------------------------------------------
### VII
ii.other.cnjtype.glmer.fixed <- glm(OtherCnj ~ II.sense + II.natural.land + II.weather + NI.object.actor , data=subset(AWCnj, II),family=binomial)

sink(file = 'SummaryTables/man/fixed/other-cnj-ii.txt')
round(print(summary(ii.other.cnjtype.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.other.cnjtype.glmer.fixed <- glm(OtherCnj ~  PV.Time + PV.Position + PV.Qual + PV.WantCan + AI.state + AI.speech + AI.cooking + AI.health + AI.pray + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + Pers.actor + Prox.actor + Indef.actor + actor.3 + actor.1 + actor.2 , data=subset(AWCnj, AI),family=binomial)

sink(file = 'SummaryTables/man/fixed/other-cnj-ai.txt')
round(print(summary(ai.other.cnjtype.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.other.cnjtype.glmer.fixed <- glm(OtherCnj ~ PV.Time + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + TI.cognitive + TI.speech + NA.persons.actor + Sg.actor + Dem.actor + actor.3 + actor.2 + goal.3 + Sg.goal + NI.object.goal + Pl.goal + Prox.goal + NI.nominal.goal + NI.natural.force.goal , data=subset(AWCnj, TI),family=binomial)

sink(file = 'SummaryTables/man/fixed/other-cnj-ti.txt')
round(print(summary(ti.other.cnjtype.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.other.cnjtype.glmer.fixed <- glm(OtherCnj ~ PV.Time + PV.Position + PV.Discourse + TA.do + TA.speech + TA.cognitive + TA.allow + RdplS + Sg.actor + D.actor + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + NDA.Relations.goal + Dem.goal + NA.beast.of.burden.goal + Pers.goal + NA.food.goal , data=subset(AWCnj, TA),family=binomial)

sink(file = 'SummaryTables/man/fixed/other-cnj-ta.txt')
round(print(summary(ta.other.cnjtype.glmer.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

