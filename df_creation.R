
library(readr)

AW <- read_delim("~/df.tsv", "\t")

#==========================================================================================================
#drop AI and TA:
AW<-AW[AW$Index!='29147',]
#drop items that are AI with TI classes
AW2<-AW[AW$Lemma != 'ayâw',]



#Making CnjType Column
AW2$CnjType<-ifelse(AW2$IC,'IC',
                    ifelse(AW2$`PV.e`,'eCnj',
                           ifelse(AW2$`PV.kaa`,'kaaCnj',
                                  ifelse(AW2$`PV.ka` | AW2$`PV.ta`,'kaCnj',
                                         ifelse(AW2$IC & AW2$Cond,'TimeCond',
                                                ifelse(AW2$Cond & !AW2$IC,'FutCond',
                                                       NA
                                                ))))))

#==============================================================================================================
#Making Ordertype Column
#-------------------------
#Make TopOrder column

AW2$TopOrder<-ifelse(AW2$IC | AW2$Cnj | AW2$Fut | AW2$Cond,'Cnj',
                     ifelse(AW2$Ind,'Ind',
                            ifelse(AW2$Imp,'Imp', NA
                            )))



AW2$OrderType<-ifelse(AW2$IC,'OtherCnj',
                      ifelse(AW2$`PV.e`,'eCnj',
                             ifelse(AW2$`PV.kaa`,'kaaCnj',
                                    ifelse(AW2$`PV.ka` | AW2$`PV.ta`,'OtherCnj',
                                           ifelse(AW2$IC & AW2$Cond,'OtherCnj',
                                                  ifelse(AW2$Cond & !AW2$IC,'OtherCnj',
                                                         ifelse(AW2$Ind,'Ind',
                                                                NA 
                                                         )))))))

AW2<-AW2[!is.na(AW2$OrderType),] #just check to make sure nothign got missed

#==============================================================================================================
# Multinomial v.class

AW2$v.class<-ifelse(AW2$AI.canine,'AI.canine',
                    ifelse(AW2$AI.childcare,'AI.childcare',
                           ifelse(AW2$AI.cooking,'AI.cooking',
                                  ifelse(AW2$AI.do,'AI.do',
                                         ifelse(AW2$AI.health,'AI.health',
                                                ifelse(AW2$AI.heat.fire,'AI.heat.fire',
                                                       ifelse(AW2$AI.money.count,'AI.money.count',
                                                              ifelse(AW2$AI.plural,'AI.plural',
                                                                     ifelse(AW2$AI.pray,'AI.pray',
                                                                            ifelse(AW2$AI.reflexive,'AI.reflexive',
                                                                                   ifelse(AW2$AI.speech,'AI.speech',
                                                                                          ifelse(AW2$AI.state,'AI.state',
                                                                                                 ifelse(AW2$II.move,'II.move',
                                                                                                        ifelse(AW2$II.natural.land,'II.natural.land',
                                                                                                               ifelse(AW2$II.plural,'II.plural',
                                                                                                                      ifelse(AW2$II.sense,'II.sense.attitude',
                                                                                                                             ifelse(AW2$II.weather,'II.weather.time',
                                                                                                                                    ifelse(AW2$TA.allow,'TA.allow',
                                                                                                                                           ifelse(AW2$TA.cognitive,'TA.cognitive',
                                                                                                                                                  ifelse(AW2$TA.do,'TA.do',
                                                                                                                                                         ifelse(AW2$TA.food,'TA.food',
                                                                                                                                                                ifelse(AW2$TA.money.count,'TA.money.count',
                                                                                                                                                                       ifelse(AW2$TA.religion,'TA.religion',
                                                                                                                                                                              ifelse(AW2$TA.speech,'TA.speech',
                                                                                                                                                                                     ifelse(AW2$TI.cognitive,'TI.cognitive',
                                                                                                                                                                                            ifelse(AW2$TI.do,'TI.do',
                                                                                                                                                                                                   ifelse(AW2$TI.fit,'TI.fit',
                                                                                                                                                                                                          ifelse(AW2$TI.money.count,'TI.money.count',
                                                                                                                                                                                                                 ifelse(AW2$TI.speech,'TI.speech',
                                                                                                                                                                                                                        NA
                                                                                                                                                                                                                 )))))))))))))))))))))))))))))


AW2$n.class<-ifelse(AW2$NA.beast.of.burden.actor,'NA.beast.of.burden.actor',
                    ifelse(AW2$NA.beast.of.burden.goal,'NA.beast.of.burden.goal',
                           ifelse(AW2$NA.body.part.goal,'NA.body.part.goal',
                                  ifelse(AW2$NA.celestial.actor,'NA.celestial.actor',
                                         ifelse(AW2$NA.celestial.goal,'NA.celestial.goal',
                                                ifelse(AW2$NA.food.actor,'NA.food.actor',
                                                       ifelse(AW2$NA.food.goal,'NA.food.goal',
                                                              ifelse(AW2$NA.money.count.actor,'NA.money.count.actor',
                                                                     ifelse(AW2$NA.money.count.goal,'NA.money.count.goal',
                                                                            ifelse(AW2$NA.persons.actor,'NA.persons.actor',
                                                                                   ifelse(AW2$NA.persons.goal,'NA.persons.goal',
                                                                                          ifelse(AW2$NA.religious.actor,'NA.religious.actor',
                                                                                                 ifelse(AW2$NA.religious.goal,'NA.religious.goal',
                                                                                                        ifelse(AW2$NA.shield.goal,'NA.shield.goal',
                                                                                                               ifelse(AW2$NDA.Relations.actor,'NDA.Relations.actor',
                                                                                                                      ifelse(AW2$NDA.Relations.goal,'NDA.Relations.goal',
                                                                                                                             ifelse(AW2$NDI.Body.actor,'NDI.Body.actor',
                                                                                                                                    ifelse(AW2$NDI.Body.goal,'NDI.Body.goal',
                                                                                                                                           ifelse(AW2$NDI.House.goal,'NDI.House.goal',
                                                                                                                                                  ifelse(AW2$NI.alter.goal,'NI.alter.goal',
                                                                                                                                                         ifelse(AW2$NI.animal.product.actor,'NI.animal.product.actor',
                                                                                                                                                                ifelse(AW2$NI.animal.product.goal,'NI.animal.product.goal',
                                                                                                                                                                       ifelse(AW2$NI.hunt.trap.actor,'NI.hunt.trap.actor',
                                                                                                                                                                              ifelse(AW2$NI.hunt.trap.goal,'NI.hunt.trap.goal',
                                                                                                                                                                                     ifelse(AW2$NI.natural.force.actor,'NI.natural.force.actor',
                                                                                                                                                                                            ifelse(AW2$NI.natural.force.goal,'NI.natural.force.goal',
                                                                                                                                                                                                   ifelse(AW2$NI.nature.plants.actor,'NI.nature.plants.actor',
                                                                                                                                                                                                          ifelse(AW2$NI.nature.plants.goal,'NI.nature.plants.goal',
                                                                                                                                                                                                                 ifelse(AW2$NI.nominal.actor,'NI.nominal.actor',
                                                                                                                                                                                                                        ifelse(AW2$NI.nominal.goal,'NI.nominal.goal',
                                                                                                                                                                                                                               ifelse(AW2$NI.object.actor,'NI.object.actor',
                                                                                                                                                                                                                                      ifelse(AW2$NI.object.goal,'NI.object.goal',
                                                                                                                                                                                                                                             ifelse(AW2$NI.place.actor,'NI.place.actor',
                                                                                                                                                                                                                                                    ifelse(AW2$NI.place.goal,'NI.place.goal',
                                                                                                                                                                                                                                                                  ifelse(AW2$NI.religious.goal,'NI.religious.goal',
                                                                                                                                                                                                                                                                         NA
                                                                                                                                                                                                                                                                  )))))))))))))))))))))))))))))))))))

test <- AW2[is.na(AW2$v.class),]
unique(test$Lemma)
#[1] "âyiman"            "miywâsin"          "pipon"             "tipiskâw"          "wâpan"            
#[6] "kaskipitêw"        "manitowi.kîsikâw"  "ayamihêwi.kîsikâw" "sîkwan"            "misi.paskwâw"      

AW2<-AW2[!(AW2$Lemma=="iyiniw" | AW2$Lemma=="oskinîkiw"),]

AW2[AW2$Lemma=='miywâsin',]$II.sense <- TRUE
AW2[AW2$Lemma=='âyiman',]$II.sense <- TRUE
AW2[AW2$Lemma=='wâpan',]$II.weather <- TRUE
AW2[AW2$Lemma=='manitowi.kîsikâw',]$II.weather <- TRUE
AW2[AW2$Lemma=='ayamihêwi.kîsikâw',]$II.weather <- TRUE
AW2[AW2$Lemma=='sîkwan',]$II.weather <- TRUE
AW2[AW2$Lemma=='pipon',]$II.weather <- TRUE
AW2[AW2$Lemma=='tipiskâw',]$II.weather <- TRUE
AW2[AW2$Lemma=='misi.paskwâw',]$II.natural.land <- TRUE
AW2[AW2$Lemma=='kaskipitêw',]$II.move <- TRUE
AW2[AW2$Lemma=='apahkwêw',]$AI.do <- TRUE


#===========================================================================================================


AW2$PV.Discourse<-ifelse(AW2$PV.ayi| AW2$PV.ata| AW2$PV.isi| AW2$PV.isko,TRUE, FALSE)
AW2$PV.Position<-ifelse(AW2$PV.ase | AW2$PV.ahci | AW2$PV.kiwe | AW2$PV.kwahci | AW2$PV.pasiko | AW2$PV.pasci | AW2$PV.pihci | AW2$PV.mamawi | AW2$PV.sasakici | AW2$PV.sipwe | AW2$PV.kwayaski | AW2$PV.wayawi | AW2$PV.ohci | AW2$PV.papami,TRUE, FALSE)
AW2$PV.Qual<-ifelse(AW2$PV.papasi | AW2$PV.ahkami | AW2$PV.kakwahyaki | AW2$PV.kakwayaki | AW2$PV.kisiska | AW2$PV.kisiwi | AW2$PV.kisi | AW2$PV.kisiwi | AW2$PV.kimoci | AW2$PV.maci | AW2$PV.kawi | AW2$PV.mana | AW2$PV.kwayaci | AW2$PV.matwe | AW2$PV.mayi | AW2$PV.misi | AW2$PV.miyo | AW2$PV.mosci | AW2$PV.nahi | AW2$PV.wisaki | AW2$PV.tepi | AW2$PV.takahki | AW2$PV.sohki | AW2$PV.soniyawi | AW2$PV.pimi | AW2$PV.nipahi | AW2$PV.nehiyawi | AW2$PV.wani | AW2$PV.pisci,TRUE, FALSE)
AW2$PV.Quant<-ifelse(AW2$PV.apihci | AW2$PV.apihci | AW2$PV.kihci | AW2$PV.misakame | AW2$PV.tahto | AW2$PV.tako | AW2$PV.peyako,TRUE, FALSE)
AW2$PV.Time<-ifelse(AW2$PV.kita | AW2$PV.kape | AW2$PV.asowi | AW2$PV.ka | AW2$PV.kah | AW2$PV.ki | AW2$PV.mêkwa | AW2$PV.nipa | AW2$PV.niso | AW2$PV.ocihci | AW2$PV.oh | AW2$PV.tasi | AW2$PV.tah | AW2$PV.tita | AW2$PV.wa | AW2$PV.wah | AW2$PV.wapani | AW2$PV.wi,TRUE, FALSE)
AW2$PV.Move<-ifelse(AW2$PV.nitawi | AW2$PV.papa | AW2$PV.pimi | AW2$PV.pe, TRUE, FALSE)
AW2$PV.StartFin<-ifelse(AW2$PV.nihta | AW2$PV.ati | AW2$PV.kisi | AW2$PV.maaci | AW2$PV.poni,TRUE, FALSE)
AW2$PV.WantCan<-ifelse(AW2$PV.nohte | AW2$PV.ta.ki | AW2$PV.ka.ki | AW2$PV.kakwe,TRUE, FALSE)
AW2$PV.StartFinWantCan<-ifelse(AW2$PV.nihta | AW2$PV.ati | AW2$PV.kisi | AW2$PV.maaci | AW2$PV.poni | AW2$PV.nohte | AW2$PV.ta.ki | AW2$PV.ka.ki | AW2$PV.kakwe,TRUE, FALSE)

AW2$actor.1<-ifelse(AW2$`1Pl` | AW2$`1Sg`, TRUE, FALSE)
AW2$actor.2<-ifelse(AW2$`12Pl` | AW2$`2Sg` | AW2$`2Pl`, TRUE, FALSE)
AW2$actor.3<-ifelse(AW2$`3Sg` | AW2$`3Pl`, TRUE, FALSE)
AW2$actor.4<-ifelse(AW2$`4Sg.Pl` | AW2$`4Sg` | AW2$`4Pl`, TRUE, FALSE)

AW2$goal.1<-ifelse(AW2$`1SgO` | AW2$`1PlO`, TRUE, FALSE)
AW2$goal.2<-ifelse(AW2$`12PlO` | AW2$`2SgO` | AW2$`2PlO`, TRUE, FALSE)
AW2$goal.3<-ifelse(AW2$`3SgO` | AW2$`3PlO`, TRUE, FALSE)
AW2$goal.4<-ifelse(AW2$`4Sg.PlO`, TRUE, FALSE)
AW2$goal.5<-ifelse(AW2$`5Sg.PlO`, TRUE, FALSE)
#------------------------------------------------------------------------------------
AW2$CnjType<-ifelse(AW2$IC,'IC',
                    ifelse(AW2$`PV.e`,'eCnj',
                           ifelse(AW2$`PV.kaa`,'kaaCnj',
                                  ifelse(AW2$`PV.ka` | AW2$`PV.ta`,'kaCnj',
                                         ifelse(AW2$IC & AW2$Cond,'TimeCond',
                                                ifelse(AW2$Cond & !AW2$IC,'FutCond',
                                                       NA
                                                ))))))


#MAke deps

AW2$NDA<-ifelse(!AW2$V & AW2$D.actor & (AW2$A.actor | AW2$A.goal), TRUE, 
                ifelse(!AW2$V & AW2$D.goal & (AW2$A.actor | AW2$A.goal), TRUE, FALSE))


AW2$NDI<-ifelse(!AW2$V & AW2$D.actor & (AW2$I.actor | AW2$I.goal), TRUE, 
                ifelse(!AW2$V & AW2$D.goal & (AW2$I.actor | AW2$I.goal), TRUE, FALSE))

# make NA/NI

AW2$N.A<-ifelse(!AW2$V & (AW2$A.actor | AW2$A.goal), TRUE, FALSE)


AW2$N.I<-ifelse(!AW2$V & AW2$D.actor & (AW2$I.actor | AW2$I.goal), TRUE, FALSE)

#Making Ordertype Column
#-------------------------
AW2$OrderType<-ifelse(AW2$IC,'Other',
                      ifelse(AW2$`PV.e`,'eCnj',
                             ifelse(AW2$`PV.kaa`,'kaaCnj',
                                    ifelse(AW2$`PV.ka` | AW2$`PV.ta`,'Other',
                                           ifelse(AW2$IC & AW2$Cond,'Other',
                                                  ifelse(AW2$Cond & !AW2$IC,'Other',
                                                         ifelse(AW2$Ind,'Ind',
                                                                ifelse(AW2$Imp,'Imp',
                                                                       NA
                                                                ))))))))



#==========================================================================================================
#Remove imps
AWnimp <- subset(AW2, !Imp)

AWnimp$OrderType<-ifelse(AWnimp$IC,'OtherCnj',
                         ifelse(AWnimp$`PV.e`,'eCnj',
                                ifelse(AWnimp$`PV.kaa`,'kaaCnj',
                                       ifelse(AWnimp$`PV.ka` | AWnimp$`PV.ta`,'OtherCnj',
                                              ifelse(AWnimp$IC & AWnimp$Cond,'OtherCnj',
                                                     ifelse(AWnimp$Cond & !AWnimp$IC,'OtherCnj',
                                                            ifelse(AWnimp$Ind,'Ind',
                                                                   NA 
                                                            )))))))

AWCnj <- subset(AW2, (Cnj | Cond))

AWCnj$OtherCnj<-ifelse(AWCnj$IC,TRUE,
                       ifelse(AWCnj$`PV.e`,FALSE,
                              ifelse(AWCnj$`PV.kaa`,FALSE,
                                     ifelse(AWCnj$`PV.ka` | AWCnj$`PV.ta`,TRUE,
                                            ifelse(AWCnj$IC & AWCnj$Cond,TRUE,
                                                   ifelse(AWCnj$Cond & !AWCnj$IC,TRUE,
                                                          ifelse(AWCnj$Ind,FALSE,
                                                                 NA
                                                          )))))))


AWnimp$OtherCnj<-ifelse(AWnimp$IC,TRUE,
                        ifelse(AWnimp$`PV.e`,FALSE,
                               ifelse(AWnimp$`PV.kaa`,FALSE,
                                      ifelse(AWnimp$`PV.ka` | AWnimp$`PV.ta`,TRUE,
                                             ifelse(AWnimp$IC & AWnimp$Cond,TRUE,
                                                    ifelse(AWnimp$Cond & !AWnimp$IC,TRUE,
                                                           ifelse(AWnimp$Ind,FALSE,NA)))))))



AWnimp$OrderType<-ifelse(AWnimp$IC,'OtherCnj',
                         ifelse(AWnimp$`PV.e`,'eCnj',
                                ifelse(AWnimp$`PV.kaa`,'kaaCnj',
                                       ifelse(AWnimp$`PV.ka` | AWnimp$`PV.ta`,'OtherCnj',
                                              ifelse(AWnimp$IC & AWnimp$Cond,'OtherCnj',
                                                     ifelse(AWnimp$Cond & !AWnimp$IC,'OtherCnj',
                                                            ifelse(AWnimp$Ind,'Ind',
                                                                   NA 
                                                            )))))))




#Make ind v e column
#==========================================================================================================
#make ind v ecnj  

AWive <- subset(AW2, Ind | PV.e)

#Make order type column
AWive$OrderType<-ifelse(AWive$Ind, 'Ind',
                        ifelse(AWive$`PV.e`,'eCnj',
                               NA
                        ))
#==========================================================================================================

#Crosstaulation of class and Cnj type
#----------------

AWnimp$ConjClass <- apply(AWnimp[c("II","AI","TI","TA")], 1, function(x) names(which(x))[1])


table(AWnimp$ConjClass, AWnimp$CnjType)
table(AWnimp$ConjClass, AWnimp$OrderType)

#Crosstaulation of class and typw AWnimp
# ----------------

AW2$ConjClass <- apply(AW2[c("II","AI","TI","TA")], 1, function(x) names(which(x))[1])

table(AWnimp$ConjClass, AWnimp$CnjType)
table(AWnimp$ConjClass, AWnimp$OrderType)

#==========================================================================================================


# get rid of ayamahewi-kisikaw and make cosine columns







AWCnj$OtherCnj<-ifelse(AWCnj$IC, TRUE,
                       ifelse(AWCnj$`PV.ka` | AWCnj$`PV.ta`, TRUE,
                              ifelse(AWCnj$IC & AWCnj$Cond, TRUE,
                                     ifelse(AWCnj$Cond & !AWCnj$IC, TRUE,
                                            FALSE    ))))


# Install packages
----------------
  installed.packages(lem4)
install.packages("~/Downloads/polytomous_0.1.6.tar.gz", repos = NULL, type="source")
library(polytomous)
#  ==========================================================================================================

#Associations
----------------
  
  table(AWnimp$ConjClass, AWnimp$OrderType)

