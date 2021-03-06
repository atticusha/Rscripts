#Ind vs eCnj
#============================================================================================================
### VII
ii.ive.glmer.rand <- glmer(Ind ~  (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/ive-ii.txt')
print(summary(ii.ive.glmer.rand)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/ive-ai.txt')
print(summary(ai.ive.glmer.rand)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/ive-ti.txt')
print(summary(ti.ive.glmer.rand)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/ive-ta.txt')
print(summary(ta.ive.glmer.rand)$coefficients, max.print = NA)
sink(NULL)


#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
ii.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/ivc-ii.txt')
print(summary(ii.indcnj.glmer.rand)$coefficients, max.print = NA)
sink(NULL)

### VAI
ai.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/ivc-ai.txt')
print(summary(ai.indcnj.glmer.rand)$coefficients, max.print = NA)
sink(NULL)

### VTI
ti.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/ivc-ti.txt')
print(summary(ti.indcnj.glmer.rand)$coefficients, max.print = NA)
sink(NULL)

### VTA
ta.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/ivc-ta.txt')
print(summary(ta.indcnj.glmer.rand)$coefficients, max.print = NA)
sink(NULL)



#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
ii.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/e-cnj-ii.txt')
round(print(summary(ii.e.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/e-cnj-ai.txt')
round(print(summary(ai.e.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/e-cnj-ti.txt')
round(print(summary(ti.e.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/e-cnj-ta.txt')
round(print(summary(ta.e.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

#-------------------------------------------------------
### VII
### VII
ii.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/kaa-cnj-ii.txt')
round(print(summary(ii.kaa.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/kaa-cnj-ai.txt')
round(print(summary(ai.kaa.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/kaa-cnj-ti.txt')
round(print(summary(ti.kaa.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/kaa-cnj-ta.txt')
round(print(summary(ta.kaa.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)
#-------------------------------------------------------
### VII
ii.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/other-cnj-ii.txt')
round(print(summary(ii.other.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/other-cnj-ai.txt')
round(print(summary(ai.other.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/other-cnj-ti.txt')
round(print(summary(ti.other.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'Multi/SummaryTables/man/rand/other-cnj-ta.txt')
round(print(summary(ta.other.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

