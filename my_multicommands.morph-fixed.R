#Ind vs eCnj
#============================================================================================================
### VII
#ii.ive.morph.fixed.glmer <- glm(Ind ~ (1|Lemma), data=subset(AWive, II), family=binomial)

#sink(file = 'SummaryTables/man/ive-ii.morph.txt')
#round(print(summary(ii.ive.morph.fixed.glmer)$coefficients, max.print = NA), 3)
#sink(NULL)

### VAI
ai.ive.morph.fixed.glmer <- glm(Ind ~ RdplW + Pl.actor + actor.3 + actor.1 + actor.4, data=subset(AWive, AI), family=binomial)

sink(file = 'SummaryTables/man/ive-ai.morph.txt')
round(print(summary(ai.ive.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.ive.morph.fixed.glmer <- glm(Ind ~ actor.3 + actor.2 + Pl.goal + Der.Dim.goal + Px1Sg.goal, data=subset(AWive, TI), family=binomial)

sink(file = 'SummaryTables/man/ive-ti.morph.txt')
round(print(summary(ti.ive.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.ive.morph.fixed.glmer <- glm(Ind ~ actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + Px3Sg.goal + Px3Pl.goal, data=subset(AWive, TA), family=binomial)

sink(file = 'SummaryTables/man/ive-ta.morph.txt')
round(print(summary(ta.ive.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)


#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
#ii.indcnj.morph.fixed.glmer <- glm(Ind ~ (1|Lemma), data=subset(AWnimp, II), family=binomial)

#sink(file = 'SummaryTables/man/ivc-ii.morph.txt')
#round(print(summary(ii.indcnj.morph.fixed.glmer)$coefficients, max.print = NA), 3)
#sink(NULL)

### VAI
ai.indcnj.morph.fixed.glmer <- glm(Ind ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.2 + actor.4, data=subset(AWnimp, AI), family=binomial)

sink(file = 'SummaryTables/man/ivc-ai.morph.txt')
round(print(summary(ai.indcnj.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.indcnj.morph.fixed.glmer <- glm(Ind ~ actor.3 + actor.2 + Sg.goal + Pl.goal + Px3Sg.goal + Der.Dim.goal + Px1Sg.goal, data=subset(AWnimp, TI), family=binomial)

sink(file = 'SummaryTables/man/ivc-ti.morph.txt')
round(print(summary(ti.indcnj.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.indcnj.morph.fixed.glmer <- glm(Ind ~ Sg.actor + actor.1 + actor.4 + goal.3 + goal.4 + goal.2 + Px3Pl.goal, data=subset(AWnimp, TA), family=binomial)

sink(file = 'SummaryTables/man/ivc-ta.morph.txt')
round(print(summary(ta.indcnj.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
#ii.e.cnjtype.morph.fixed.glmer <- glm(PV.e ~ (1|Lemma), data=subset(AWCnj, II), family=binomial)

#sink(file = 'SummaryTables/man/e-cnj-ii.morph.txt')
#round(print(summary(ii.e.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
#sink(NULL)


### VAI
ai.e.cnjtype.morph.fixed.glmer <- glm(PV.e ~  RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2, data=subset(AWCnj, AI), family=binomial)

sink(file = 'SummaryTables/man/e-cnj-ai.morph.txt')
round(print(summary(ai.e.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.e.cnjtype.morph.fixed.glmer <- glm(PV.e ~ Sg.actor + actor.3 + actor.2 + Sg.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = 'SummaryTables/man/e-cnj-ti.morph.txt')
round(print(summary(ti.e.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.e.cnjtype.morph.fixed.glmer <- glm(PV.e ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = 'SummaryTables/man/e-cnj-ta.morph.txt')
round(print(summary(ta.e.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

#-------------------------------------------------------
### VII
#ii.kaa.cnjtype.morph.fixed.glmer <- glm(PV.kaa ~ (1|Lemma), data=subset(AWCnj, II), family=binomial)

#sink(file = 'SummaryTables/man/kaa-cnj-ii.morph.txt')
#round(print(summary(ii.kaa.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
#sink(NULL)


### VAI
ai.kaa.cnjtype.morph.fixed.glmer <- glm(PV.kaa ~  RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2, data=subset(AWCnj, AI), family=binomial)

sink(file = 'SummaryTables/man/kaa-cnj-ai.morph.txt')
round(print(summary(ai.kaa.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.kaa.cnjtype.morph.fixed.glmer <- glm(PV.kaa ~ Sg.actor + actor.3 + actor.2 + Sg.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = 'SummaryTables/man/kaa-cnj-ti.morph.txt')
round(print(summary(ti.kaa.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.kaa.cnjtype.morph.fixed.glmer <- glm(PV.kaa ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = 'SummaryTables/man/kaa-cnj-ta.morph.txt')
round(print(summary(ta.kaa.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)
#-------------------------------------------------------
### VII
#ii.other.cnjtype.morph.fixed.glmer <- glm(OtherCnj ~ (1|Lemma), data=subset(AWCnj, II), family=binomial)

#sink(file = 'SummaryTables/man/other-cnj-ii.morph.txt')
#round(print(summary(ii.other.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
#sink(NULL)

### VAI
ai.other.cnjtype.morph.fixed.glmer <- glm(OtherCnj ~   RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2, data=subset(AWCnj, AI), family=binomial)

sink(file = 'SummaryTables/man/other-cnj-ai.morph.txt')
round(print(summary(ai.other.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.other.cnjtype.morph.fixed.glmer <- glm(OtherCnj ~ Sg.actor + actor.3 + actor.2 + Sg.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = 'SummaryTables/man/other-cnj-ti.morph.txt')
round(print(summary(ti.other.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.other.cnjtype.morph.fixed.glmer <- glm(OtherCnj ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = 'SummaryTables/man/other-cnj-ta.morph.txt')
round(print(summary(ta.other.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

