#Ind vs eCnj
#============================================================================================================
### VII
ii.ive.glmer.morph <- glmer(Ind  ~ (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/ive-ii-morph.txt')
round(print(summary(ii.ive.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.ive.glmer.morph <- glmer(Ind  ~ RdplW + Pl.actor + actor.3 + actor.1 + actor.2 + actor.4 + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/ive-ai-morph.txt')
round(print(summary(ai.ive.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.ive.glmer.morph <- glmer(Ind  ~ actor.1 + actor.2 + actor.4 + Sg.goal + Pl.goal + Px1Sg.goal + Px3Pl.goal + (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/ive-ti-morph.txt')
round(print(summary(ti.ive.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.ive.glmer.morph <- glmer(Ind  ~ Sg.actor + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/ive-ta-morph.txt')
round(print(summary(ta.ive.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)


#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
ii.indcnj.glmer.morph <- glmer(Ind  ~ (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/ivc-ii-morph.txt')
round(print(summary(ii.indcnj.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.indcnj.glmer.morph <- glmer(Ind  ~ RdplW + Pl.actor + actor.1 + actor.2 + actor.4 + (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/ivc-ai-morph.txt')
round(print(summary(ai.indcnj.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.indcnj.glmer.morph <- glmer(Ind  ~ actor.1 + actor.2 + actor.4 + Sg.goal + Pl.goal + (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/ivc-ti-morph.txt')
round(print(summary(ti.indcnj.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.indcnj.glmer.morph <- glmer(Ind  ~ Sg.actor + actor.1 + actor.2 + actor.4 + goal.3 + goal.4 + goal.2 + (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/ivc-ta-morph.txt')
round(print(summary(ta.indcnj.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
ii.e.cnjtype.glmer.morph <- glmer(PV.e  ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/e-cnj-ii-morph.txt')
round(print(summary(ii.e.cnjtype.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.e.cnjtype.glmer.morph <- glmer(PV.e  ~ Sg.actor + Pl.actor + Indef.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/e-cnj-ai-morph.txt')
round(print(summary(ai.e.cnjtype.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.e.cnjtype.glmer.morph <- glmer(PV.e  ~ Sg.actor + actor.3 + actor.2 + goal.3 + Sg.goal + Pl.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/e-cnj-ti-morph.txt')
round(print(summary(ti.e.cnjtype.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.e.cnjtype.glmer.morph <- glmer(PV.e  ~ RdplS + Sg.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/e-cnj-ta-morph.txt')
round(print(summary(ta.e.cnjtype.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

#-------------------------------------------------------
### VII
### VII
ii.kaa.cnjtype.glmer.morph <- glmer(PV.kaa  ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/kaa-cnj-ii-morph.txt')
round(print(summary(ii.kaa.cnjtype.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.kaa.cnjtype.glmer.morph <- glmer(PV.kaa  ~ Sg.actor + Pl.actor + Indef.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/kaa-cnj-ai-morph.txt')
round(print(summary(ai.kaa.cnjtype.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.kaa.cnjtype.glmer.morph <- glmer(PV.kaa  ~ Sg.actor + actor.3 + actor.2 + goal.3 + Sg.goal + Pl.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/kaa-cnj-ti-morph.txt')
round(print(summary(ti.kaa.cnjtype.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.kaa.cnjtype.glmer.morph <- glmer(PV.kaa  ~ RdplS + Sg.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/kaa-cnj-ta-morph.txt')
round(print(summary(ta.kaa.cnjtype.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)
#-------------------------------------------------------
### VII
ii.other.cnjtype.glmer.morph <- glmer(OtherCnj  ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/other-cnj-ii-morph.txt')
round(print(summary(ii.other.cnjtype.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.other.cnjtype.glmer.morph <- glmer(OtherCnj  ~ Sg.actor + Pl.actor + Indef.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/other-cnj-ai-morph.txt')
round(print(summary(ai.other.cnjtype.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.other.cnjtype.glmer.morph <- glmer(OtherCnj  ~ Sg.actor + actor.3 + actor.2 + goal.3 + Sg.goal + Pl.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/other-cnj-ti-morph.txt')
round(print(summary(ti.other.cnjtype.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.other.cnjtype.glmer.morph <- glmer(OtherCnj  ~ RdplS + Sg.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/morph/other-cnj-ta-morph.txt')
round(print(summary(ta.other.cnjtype.glmer.morph)$coefficients, max.print = NA), 3)
sink(NULL)
