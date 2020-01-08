### Test social behavior meta-analysis  ###

soc <- read.csv("Shark Species Profile 2.0_test.csv")
head(soc)
class(soc)
nrow(soc)

soc2 <- soc[!(soc$Researcher.Initials==""),] #removing sharks that haven't been done yet
nrow(soc2)

##############################
##### Brief data explore #####
##############################
## Separate passive aggregation vs. social behavior
summary(soc2$Type.of.interaction.documented)
na <- sum(soc2$Type.of.interaction.documented=="")
sp <- sum(soc2$Type.of.interaction.documented=="both")
pa <- sum(soc2$Type.of.interaction.documented=="Passive agg")
solpass <- sum(soc2$Type.of.interaction.documented=="solitary + passive agg")
unknown <- sum(soc2$Type.of.interaction.documented=="unknown")

barplot(c(sp, pa, solpass,unknown, na), names.arg=c("Both", "Passive Agg", "Sol + PA", "Unknown", "NA"))

# separating all of dataframes
predictors <- soc2[,c(4,5,6,7,9,10,11,12,14:20, 94, 95,96,97,98,99,100,101, 103,104,106, 108:111)]
sociality <- soc2[,c(21,23,24,28,29,33,34,38,39,43,44,48,49, 53,54, 58,59, 63,64, 68,69, 73,74,78,80,81,83,84,86, 88,90,92, 116)]
samplesize<-soc2[,c(113,114)]
socinfo <- soc2[,c(23,28,33, 38, 43, 48, 53, 58, 63, 68, 73)]
colnames(socinfo)

# gives us idea as to how many individuals are giving us this information
hist(as.numeric(samplesize$TOTAL.SAMPLE.SIZE..sum.of.individuals.in.all.articles.used.to.compile.this.profile.))
# and how many articles in our search
hist(as.numeric(samplesize$Number.of.Articles.sampled))
sum(as.numeric(samplesize$Number.of.Articles.sampled), na.rm=T)

################ Summary of what we are finding so far in terms of behavior ####################
SummaryBeh <- data.frame(matrix(ncol = ncol(socinfo), nrow = 10), row.names=c("Yes", "NA", "Spec", "Spec.F", "Spec.M", "Spec.Juv", "No", "F", "M", "Juv"))
colnames(SummaryBeh) <- colnames(socinfo)
head(SummaryBeh)

for(j in 1:ncol(SummaryBeh)){
  SummaryBeh[1,j] <- sum(socinfo[j]=="Yes", na.rm=T)
  SummaryBeh[2,j] <- sum(socinfo[j]=="")
  SummaryBeh[3,j] <- sum(socinfo[j]=="speculated only", na.rm=T)
  SummaryBeh[4,j] <- sum(socinfo[j]=="speculated (F)", na.rm=T)
  SummaryBeh[5,j] <- sum(socinfo[j]=="speculated (M)", na.rm=T)
  SummaryBeh[6,j] <- sum(socinfo[j]=="speculated (Juv)", na.rm=T)
  SummaryBeh[7,j] <- sum(socinfo[j]=="No", na.rm=T)
  SummaryBeh[8,j] <- sum(socinfo[j]=="F only", na.rm=T)
  SummaryBeh[9,j] <- sum(socinfo[j]=="M only", na.rm=T)
  SummaryBeh[10,j] <- sum(socinfo[j]=="Juv only", na.rm=T)
}

par(mfrow=c(6,2))

barplot(SummaryBeh[,1], names.arg=row.names(SummaryBeh), main="Social Foraging")
barplot(SummaryBeh[,2], names.arg=row.names(SummaryBeh), main="Social Migration")
barplot(SummaryBeh[,3], names.arg=row.names(SummaryBeh), main="Thermoregulation")
barplot(SummaryBeh[,4], names.arg=row.names(SummaryBeh), main="Schooling/Shoaling")
barplot(SummaryBeh[,5], names.arg=row.names(SummaryBeh), main="Breaching")
barplot(SummaryBeh[,6], names.arg=row.names(SummaryBeh), main="Size Assortment")
barplot(SummaryBeh[,7], names.arg=row.names(SummaryBeh), main="Sex Assortment")
barplot(SummaryBeh[,8], names.arg=row.names(SummaryBeh), main="Paired swimming")
barplot(SummaryBeh[,9], names.arg=row.names(SummaryBeh), main="Swimming in formation")
barplot(SummaryBeh[,10], names.arg=row.names(SummaryBeh), main="Breeding Aggregations")
barplot(SummaryBeh[,11], names.arg=row.names(SummaryBeh), main="Fission/Fusion")


############### Classification of behaviors #######################
## consider Y, N, Speculated, etc. Need to divide datasets to either consider "speculated" as a yes, or not

#### Types of social behavior we are considering:
# Low level
## Hunting/foraging
## Breeding aggregation
## Refuging
## Migration
## Thermoregulation

# High level
## Breaching
## Swimming in formation
## Size assortment/sex assortment
## Paired swimming
## Fission-fusion
## Schooling/shoaling

soc2$HighLevel <- ifelse(soc2$Evidence.of.breaching.=="Yes"|soc2$Evidence.of.swimming.in.formation.=="Yes"|soc2$Evidence.of.size.assortment.=="Yes"|soc2$Evidence.of.sex.assortment.=="Yes"|soc2$Evidence.of.paired.swimming.=="Yes"|soc2$Evidence.of.fission.fusion.=="Yes"|soc2$Evidence.of.schooling.shoaling.=="Yes"|soc2$Evidence.of.dominance.hierarchies.in.groups.=="Yes", 1, 0)

soc2$SpecHighLevel <- ifelse(soc2$Evidence.of.breaching.=="speculated only"|soc2$Evidence.of.swimming.in.formation.=="speculated only"|soc2$Evidence.of.size.assortment.=="speculated only"|soc2$Evidence.of.sex.assortment.=="speculated only"|soc2$Evidence.of.paired.swimming.=="speculated only"|soc2$Evidence.of.fission.fusion.=="speculated only"|soc2$Evidence.of.schooling.shoaling.=="speculated only"|soc2$Evidence.of.dominance.hierarchies.in.groups.=="speculated only", 1, 0)

soc2$FemHighLevel <- ifelse(soc2$Evidence.of.breaching.=="F only"|soc2$Evidence.of.swimming.in.formation.=="F only"|soc2$Evidence.of.size.assortment.=="F only"|soc2$Evidence.of.sex.assortment.=="F only"|soc2$Evidence.of.paired.swimming.=="F only"|soc2$Evidence.of.fission.fusion.=="F only"|soc2$Evidence.of.schooling.shoaling.=="F only"|soc2$Evidence.of.breaching.=="speculated (F)"|soc2$Evidence.of.swimming.in.formation.=="speculated (F)"|soc2$Evidence.of.size.assortment.=="speculated (F)"|soc2$Evidence.of.sex.assortment.=="speculated (F)"|soc2$Evidence.of.paired.swimming.=="speculated (F)"|soc2$Evidence.of.fission.fusion.=="speculated (F)"|soc2$Evidence.of.schooling.shoaling.=="speculated (F)"|soc2$Evidence.of.dominance.hierarchies.in.groups.=="speculated (F)"|soc2$Evidence.of.dominance.hierarchies.in.groups.=="F only",1, 0)

soc2$MalHighLevel <- ifelse(soc2$Evidence.of.breaching.=="M only"|soc2$Evidence.of.swimming.in.formation.=="M only"|soc2$Evidence.of.size.assortment.=="M only"|soc2$Evidence.of.sex.assortment.=="M only"|soc2$Evidence.of.paired.swimming.=="M only"|soc2$Evidence.of.fission.fusion.=="M only"|soc2$Evidence.of.schooling.shoaling.=="M only"|soc2$Evidence.of.breaching.=="speculated (M)"|soc2$Evidence.of.swimming.in.formation.=="speculated (M)"|soc2$Evidence.of.size.assortment.=="speculated (M)"|soc2$Evidence.of.sex.assortment.=="speculated (M)"|soc2$Evidence.of.paired.swimming.=="speculated (M)"|soc2$Evidence.of.fission.fusion.=="speculated (M)"|soc2$Evidence.of.schooling.shoaling.=="speculated (M)"|soc2$Evidence.of.dominance.hierarchies.in.groups.=="M only"|soc2$Evidence.of.dominance.hierarchies.in.groups.=="speculated (M)",1, 0)

soc2$JuvHighLevel <- ifelse(soc2$Evidence.of.breaching.=="Juv only"|soc2$Evidence.of.swimming.in.formation.=="Juv only"|soc2$Evidence.of.size.assortment.=="Juv only"|soc2$Evidence.of.sex.assortment.=="Juv only"|soc2$Evidence.of.paired.swimming.=="Juv only"|soc2$Evidence.of.fission.fusion.=="Juv only"|soc2$Evidence.of.schooling.shoaling.=="Juv only"|soc2$Evidence.of.breaching.=="speculated (Juv)"|soc2$Evidence.of.swimming.in.formation.=="speculated (Juv)"|soc2$Evidence.of.size.assortment.=="speculated (Juv)"|soc2$Evidence.of.sex.assortment.=="speculated (Juv)"|soc2$Evidence.of.paired.swimming.=="speculated (Juv)"|soc2$Evidence.of.fission.fusion.=="speculated (Juv)"|soc2$Evidence.of.schooling.shoaling.=="speculated (Juv)"|soc2$Evidence.of.dominance.hierarchies.in.groups.=="Juv only"|soc2$Evidence.of.dominance.hierarchies.in.groups.=="speculated (Juv)",1, 0)

############################# Low level behaviors #################################
soc2$LowLevel <- ifelse(soc2$Evidence.of.social.hunting.foraging.=="Yes"|soc2$Evidence.of.breeding.aggregation.=="Yes"|soc2$Evidence.of.social.migration.=="Yes"|soc2$Evidence.of.thermoregulation.=="Yes", 1, 0)

soc2$SpecLowLevel <- ifelse(soc2$Evidence.of.social.hunting.foraging.=="speculated only"|soc2$Evidence.of.breeding.aggregation.=="speculated only"|soc2$Evidence.of.social.migration.=="speculated only"|soc2$Evidence.of.thermoregulation.=="speculated only", 1, 0)

soc2$FemLowLevel <- ifelse(soc2$Evidence.of.social.hunting.foraging.=="F only"|soc2$Evidence.of.breeding.aggregation.=="F only"|soc2$Evidence.of.social.migration.=="F only"|soc2$Evidence.of.thermoregulation.=="F only"|soc2$Evidence.of.social.hunting.foraging.=="speculated (F)"|soc2$Evidence.of.breeding.aggregation.=="speculated (F)"|soc2$Evidence.of.social.migration.=="speculated (F)"|soc2$Evidence.of.thermoregulation.=="speculated (F)", 1, 0)

soc2$MalLowLevel <- ifelse(soc2$Evidence.of.social.hunting.foraging.=="M only"|soc2$Evidence.of.breeding.aggregation.=="M only"|soc2$Evidence.of.social.migration.=="M only"|soc2$Evidence.of.thermoregulation.=="M only"|soc2$Evidence.of.social.hunting.foraging.=="speculated (M)"|soc2$Evidence.of.breeding.aggregation.=="speculated (M)"|soc2$Evidence.of.social.migration.=="speculated (M)"|soc2$Evidence.of.thermoregulation.=="speculated (M)", 1, 0)

soc2$JuvLowLevel <- ifelse(soc2$Evidence.of.social.hunting.foraging.=="Juv only"|soc2$Evidence.of.breeding.aggregation.=="Juv only"|soc2$Evidence.of.social.migration.=="Juv only"|soc2$Evidence.of.thermoregulation.=="Juv only"|soc2$Evidence.of.social.hunting.foraging.=="speculated (Juv)"|soc2$Evidence.of.breeding.aggregation.=="speculated (Juv)"|soc2$Evidence.of.social.migration.=="speculated (Juv)"|soc2$Evidence.of.thermoregulation.=="speculated (Juv)", 1, 0)

##################################################
##################################################
##################################################
summary(soc2)

# next step - make each of these into a dataframe 
highdf <- soc2[c(which(soc2$HighLevel==1), which(soc2$FemHighLevel==1), which(soc2$MalHighLevel==1), which(soc2$JuvHighLevel==1), which(soc2$SpecHighLevel==1)),]
nrow(highdf) #includes speculated
lowdf <- soc2[c(which(soc2$LowLevel==1), which(soc2$FemLowLevel==1), which(soc2$MalLowLevel==1), which(soc2$JuvLowLevel==1),which(soc2$SpecLowLevel==1)),]
nrow(lowdf) #includes speculated
