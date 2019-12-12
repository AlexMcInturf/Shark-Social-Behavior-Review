### Test social behavior meta-analysis  ###

soc <- read.csv("Shark Species Profile 2.0_test.csv")
head(soc)
class(soc)
nrow(soc)

soc2 <- soc[!(soc$Researcher.Initials==""),] #removing sharks that haven't been done yet
nrow(soc2)

## Separate passive aggregation vs. social behavior
soc2$Sociality 



## consider Y, N, Speculated, etc. 


# classify degrees of sociality (for 1 or 0)
soc$DegSociality <- ifelse(soc2$Evidence.of.social.hunting.foraging. == "Yes"|soc2$Evidence.of.social.migration. == "Yes"| soc2$Evidence.of.thermoregulation. == "Yes"| soc2$Evidence.of.breeding.aggregation. == "Yes", "Low", "High")


predictors <- soc2[,c(4,5,6,7,9,10,11,12,14:20, 94, 95,96,97,98,99,100,101, 103,104,106, 108:111)]
sociality <- soc2[,c(21,23,24,28,29,33,34,38,39,43,44,48,49, 53,54, 58,59, 63,64, 68,69, 73,74,78,80,81,83,84,86, 88,90,92, 116)]
samplesize<-soc2[,c(113,114)]
hist(as.numeric(samplesize$TOTAL.SAMPLE.SIZE..sum.of.individuals.in.all.articles.used.to.compile.this.profile.))
hist(as.numeric(samplesize$Number.of.Articles.sampled))

nrow(soc2)


# response variable: Passive vs. social 
# variables of interest: Conservation.status, FB_Hab, FB_Hab2, Atlantic, Pacific, Arctic, Antarctic, India, Brackish.Freshwater


