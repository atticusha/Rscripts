#Ind vs eCnj
#============================================================================================================
### VII
ii.ive.morph.glmer <- glmer(Ind ~ (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ii.morph.txt')
round(print(summary(ii.ive.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.ive.morph.glmer <- glmer(Ind ~ RdplW + Pl.actor + actor.3 + actor.1 + actor.4 + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ai.morph.txt')
round(print(summary(ai.ive.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.ive.morph.glmer <- glmer(Ind ~ actor.3 + actor.2 + Pl.goal + Der.Dim.goal + Px1Sg.goal + (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ti.morph.txt')
round(print(summary(ti.ive.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.ive.morph.glmer <- glmer(Ind ~ actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + Px3Sg.goal + Px3Pl.goal + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ta.morph.txt')
round(print(summary(ta.ive.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)


#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
ii.indcnj.morph.glmer <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ii.morph.txt')
round(print(summary(ii.indcnj.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.indcnj.morph.glmer <- glmer(Ind ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.2 + actor.4 + (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ai.morph.txt')
round(print(summary(ai.indcnj.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.indcnj.morph.glmer <- glmer(Ind ~ actor.3 + actor.2 + Sg.goal + Pl.goal + Px3Sg.goal + Der.Dim.goal + Px1Sg.goal + (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ti.morph.txt')
round(print(summary(ti.indcnj.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.indcnj.morph.glmer <- glmer(Ind ~ Sg.actor + actor.1 + actor.4 + goal.3 + goal.4 + goal.2 + Px3Pl.goal + (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ta.morph.txt')
round(print(summary(ta.indcnj.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
ii.e.cnjtype.morph.glmer <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ii.morph.txt')
round(print(summary(ii.e.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.e.cnjtype.morph.glmer <- glmer(PV.e ~  RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ai.morph.txt')
round(print(summary(ai.e.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.e.cnjtype.morph.glmer <- glmer(PV.e ~ Sg.actor + actor.3 + actor.2 + Sg.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ti.morph.txt')
round(print(summary(ti.e.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.e.cnjtype.morph.glmer <- glmer(PV.e ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ta.morph.txt')
round(print(summary(ta.e.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

#-------------------------------------------------------
### VII
ii.kaa.cnjtype.morph.glmer <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ii.morph.txt')
round(print(summary(ii.kaa.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.kaa.cnjtype.morph.glmer <- glmer(PV.kaa ~  RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ai.morph.txt')
round(print(summary(ai.kaa.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.kaa.cnjtype.morph.glmer <- glmer(PV.kaa ~ Sg.actor + actor.3 + actor.2 + Sg.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ti.morph.txt')
round(print(summary(ti.kaa.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.kaa.cnjtype.morph.glmer <- glmer(PV.kaa ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ta.morph.txt')
round(print(summary(ta.kaa.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)
#-------------------------------------------------------
### VII
ii.other.cnjtype.morph.glmer <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ii.morph.txt')
round(print(summary(ii.other.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.other.cnjtype.morph.glmer <- glmer(OtherCnj ~   RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ai.morph.txt')
round(print(summary(ai.other.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.other.cnjtype.morph.glmer <- glmer(OtherCnj ~ Sg.actor + actor.3 + actor.2 + Sg.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ti.morph.txt')
round(print(summary(ti.other.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.other.cnjtype.morph.glmer <- glmer(OtherCnj ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ta.morph.txt')
round(print(summary(ta.other.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

