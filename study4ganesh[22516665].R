
########################## data prep
setwd("/Users/randrew1/Study 4/") 
# define working folder containing data etc.; exported data etc. with go here too

### read spss file and save as .csv
library(foreign)
data0 = read.spss("Study4R.sav", to.data.frame=TRUE)
str(data0)
write.csv(data0,"Study4.R.csv",row.names=F) # save as .csv file
sink("Study4.R.txt"); attributes(data0)$variable.labels; sink() #save variable labels

### create a subset for ANOVA etc.
names(data0)[1:10]
head(data0[,c(1,4,5)]); tail(data0[,c(1,4,5)])
# "ID.Numeric" & "ParticipantID" match, so choose "ID.Numeric"

## choose general, response, covariate variables etc.
generalVars = c("ID.Numeric","Age","Groupallocation","General.Health")
respVars = c("PRE.TEST.Symptom.total","POST.TEST.Symptom.Total","PreNegativeEmotions",
             "PostNegativeEmotions","PreLonliness","PostLonliness","PrePositiveEmotions",
             "PostPositiveEmotions","PreDRKAnxiety","PostDRKAnxiety","PreDepression",
             "PostDepression","PrePosAffect","PostPosAffect","Pre.ClusterA","Post.ClusterA",
             "Pre.ClusterB","Post.ClusterB","Pre.ClusterC","Post.ClusterC",
             "Pre.ClusterD","Post.ClusterD","Pre.ClusterE","Post.ClusterE")
covVars = c("NEO.Score","Continuous.MBSS.Monitoring.Score","Coded.Employment","Coded.Living.Status",
            "Coded.generalhealth","Coded.Relationship.Status","Coded.Medical.Conditions")
resppredVars = c("Pre.Health.Awareness.Score","Post.Health.Awareness.Score",
                 "Pre.Health.Anxiety.Score","Post.Health.Anxiety.Score",
                 "Pre.Help.Seeking.Score","Post.Help.Seeking.Score",
                 "Pre.Communication.Score","Post.Communication.Score",
                 "Pre.Decision.Making.Score","Post.Decision.Making.Score",
                 "Pre.Goal.Setting.Score","Post.Goal.Setting.Score")
#  create data with just a subset of above variables
dataRA = data0[,c(generalVars,covVars,respVars,resppredVars)]
str(dataRA)
head(dataRA[,1:5]); tail(dataRA[,1:5])
names(dataRA)
# rename variables for ease of use
newVars = c("pID","Age","Groups","GenHealth","NEOScore","ContMBSSMonitor",
            "Coded.Employment","Coded.Living.Status",
            "Coded.generalhealth","Coded.Relationship.Status","Coded.Medical.Conditions",
            "Pre.SymptomTotal","Post.SymptomTotal","Pre.NegEmotions","Post.NegEmotions",
            "Pre.Loneliness","Post.Loneliness","Pre.PosEmotions","Post.PosEmotions",
            "Pre.DRKAnxiety","Post.DRKAnxiety","Pre.Depression","Post.Depression",
            "Pre.PosAffect","Post.PosAffect","Pre.HealthAwareness","Post.HealthAwareness",
            "Pre.HealthAnxiety","Post.HealthAnxiety","Pre.HelpSeeking","Post.HelpSeeking",
            "Pre.Communication","Post.Communication","Pre.DecisionMaking","Post.DecisionMaking",
            "Pre.GoalSetting","Post.GoalSetting","Pre.ClusterA","Post.ClusterA",
            "Pre.ClusterB","Post.ClusterB","Pre.ClusterC","Post.ClusterC",
            "Pre.ClusterD","Post.ClusterD","Pre.ClusterE","Post.ClusterE")
colnames(dataRA) = newVars # shorter names for variables
dataRA$pID = paste("ID",dataRA$pID,sep="") # create a character ID variab
head(dataRA[,1:8])

## create data with stacked Pre & Post values: data for Rep.Mes.ANOVA
# using the reshape() function
unique(dataRA$pID)
names(dataRA)
# try first for symptom total data
install.packages("data.table")
library("data.table")
Symp = data.frame(reshape(dataRA[,c(1:11,12,13)],idvar=c("pID"),
                          varying=c("Pre.SymptomTotal","Post.SymptomTotal"),
                          v.names="SymptomTotal",direction="long"))
str(Symp)
head(Symp); tail(Symp)
summary(Symp)

## now all responses to go in the same dataset - created via for loop...
rps = c("SymptomTotal","NegEmotions","Loneliness","PosEmotions","DRKAnxiety","Depression",
        "PosAffect","HealthAwareness","HealthAnxiety","HelpSeeking","Communication",
        "DecisionMaking","GoalSetting","ClusterA","ClusterB","ClusterC","ClusterD","ClusterE")
faccov = c("pID","Age","Groups","GenHealth","NEOScore","ContMBSSMonitor","Employment","Living","GeneralHealth","Relationship","Medical")

## initialise long data; stack data for faccov variables above twice...
dataRMaov = rbind(data.frame(dataRA[,faccov],time=1),data.frame(dataRA[,faccov],time=2))
str(dataRMaov)
for (rp in rps) { #rp=rps[2]
  prepst = paste(c("Pre","Post"),rp,sep=".")
  stkdata = data.frame(reshape(dataRA[,c(faccov,prepst)],idvar=c("pID"),varying=list(12:13),
                               v.names=rp,direction="long"))
  #str(stkdata); summary(stkdata)
  # add new columns using 'merge' function data...
  dataRMaov = merge(dataRMaov,stkdata,by=c(faccov,"time"),sort=F) 
}
str(dataRMaov)
colnames(dataRMaov)[7] = "PrePost"
head(dataRMaov[,c(1,2,7,8,17:ncol(dataRMaov))]); tail(dataRMaov[,c(1,2,7,8,17:ncol(dataRMaov))])
dataRMaov$PrePost = as.factor(dataRMaov$PrePost); levels(dataRMaov$PrePost)=c("Pre","Post")

## write to a .csv file
write.csv(dataRMaov,"N100DataAov.csv",row.names=F)





setwd("/Users/randrew1/Study 4/")
# define working folder containing data etc.; exported data etc. with go here too

dataRM = read.csv("N100DataAov.csv")
str(dataRM)
names(dataRM)

### choose responses etc.
respVars = c("SymptomTotal","NegEmotions","PosEmotions","DRKAnxiety","Depression",
             "Loneliness","PosAffect")
covVars = c("NEOScore","ContMBSSMonitor")
resppredVars = c("HealthAwareness","HealthAnxiety","HelpSeeking","Communication",     
                 "DecisionMaking","GoalSetting")
factorvars = c("pID","Groups","PrePost")
for (k in factorvars) { dataRM[,k] = as.factor(dataRM[,k])}
summary(dataRM[,respVars])
table(dataRM$pID)
dataRM$PrePost = ordered(dataRM$PrePost,levels=c("Pre","Post"))
head(dataRM); tail(dataRM)
addmargins(table(dataRM$Groups,dataRM$PrePost,useNA="ifany"))

### repeated measures via LME model with REML
library(doBy); library(nlme); library(car); library(Hmisc); library(shape)
library(predictmeans); library(emmeans); library(multcompView) # for post-hoc tests
# some of these libraries are for post-hoc tests and graphs
emm_options(graphics.engine="lattice") # to use with package emmeans
mycol1 = c("red","blue","green4","purple")

### try one response at a time
respvar = respVars[1] # choose response
# subset data for chosen response 
dataAOV = dataRM[,names(dataRM) %in% c(factorvars,covVars,respvar)]
str(dataAOV)
dataAOV = dataAOV0[complete.cases(dataAOV0),] # remove missing data
#str(dataAOV)
# define new factor variable for group x pre_post combinations - for summary/graphs only
dataAOV$GrpRept = ordered(paste(dataAOV$Groups,dataAOV$PrePost,sep="_"),
                          levels=c("Control_Pre","Control_Post","Monitoring_Pre","Monitoring_Post"))
# lme/aov without covariates
fo1 = formula(paste(respvar, " ~ Groups*PrePost",sep=""))
lme1 = lme(fo1, random=~1|pID, data=dataAOV); anova(lme1)
# lme/aov with covariates
fo2 = formula(paste(respvar, " ~ NEOScore+ContMBSSMonitor + Groups*PrePost",sep=""))
lme2 = lme(fo2, random=~1|pID, data=dataAOV); anova(lme2)

## check assumptions (for lme1 & lme2 models)
#dev.off()
par(mfrow=c(2,2), mar=c(3,3,2,1), mgp=c(2,1,0), cex=1, cex.axis=1, cex.lab=1, cex.main=1)
plot(scale(lme1$residuals[,2])~lme1$fitted[,2], main=respvar, xlab="Fitted", ylab="Std.Residuals",  
     col=mycol1[dataAOV$GrpRept], pch=19); abline(h=0)
qqnorm(scale(lme1$residuals[,2]),pch=19); qqline(scale(lme1$residuals[,2]), col="red")
shapiro.test(lme1$residuals[,2]) # normality test
leveneTest(lme1$residuals[,2] ~ Groups*PrePost, data=dataAOV) # homogeneity of variances
plot(scale(lme2$residuals[,2])~lme2$fitted[,2], main=respvar, xlab="Fitted", ylab="Std.Residuals",  
     col=mycol1[dataAOV$GrpRept], pch=19); abline(h=0)
qqnorm(scale(lme2$residuals[,2]),pch=19); qqline(scale(lme2$residuals[,2]), col="red")
shapiro.test(lme2$residuals[,2]) # normality test
leveneTest(lme2$residuals[,2] ~ Groups*PrePost, data=dataAOV) # homogeneity of variances
# if satisfactory, no need for transformations or randomisation tests...

help("predictmeans")
## randomisation test with package predictmeans - this may take a few minutes!
permlme = permmodels(model=lme1, data=dataAOV, fo=fo1, type=1, nperm=2999, seed=2022, ncore=3, prt=F)
permsol = permlme$ANOVA; colnames(permsol)[5] = "Perm.p"; print(permsol,na.print="")
permlme = permmodels(model=lme2,data=dataAOV,fo=fo2,type=1,nperm=2999,seed=2022,ncore=3,prt=F)
permsol = permlme$ANOVA; colnames(permsol)[5] = "Perm.p"; print(permsol,na.print="")

## post-hoc tests - use predictmeans package
# explore interaction term (adj="none" or adj="BH")
predictmeans(model=lme1,modelterm="Groups:PrePost",adj="none",plot=T,plotord=c(2,1),
             pairwise=T,pplot=F,bkplot=F,newwd=F,ncore=3,ndecimal=4,basesz=15)
predictmeans(model=lme1,modelterm="Groups:PrePost",adj="BH",plot=T,plotord=c(2,1),
             pairwise=T,pplot=F,bkplot=F,newwd=F,ncore=3,ndecimal=4,basesz=15)
# this produces a meals plot with LSD bar...
# you may plot points with conf.intervals (bkplot=T) & p-value plot (pplot=T) etc.
# main effects of Groups and PrePost
predictmeans(model=lme1,modelterm="Groups",adj="BH",plot=T,pairwise=T,pplot=F,bkplot=F,
             newwd=F,ncore=3,ndecimal=4,basesz=15)
predictmeans(model=lme1,modelterm="PrePost",adj="BH",plot=T,pairwise=T,pplot=F,bkplot=F,
             newwd=F,ncore=3,ndecimal=4,basesz=15)

## post-hoc tests - use emmeans package
foemm = formula(paste("pairwise ~","Groups:PrePost"))
emm1 = emmeans(lme1, foemm, adjust="none", df=anova(lme1)["Groups:PrePost","denDF"])
#emm1 = emmeans(lme1, foemm, adjust="BH", df=anova(lme1)["Groups:PrePost","denDF"])
plot(emm1$emmeans,comparisons=T,alpha=0.05,horizontal=T,plotit=T,adjust="none")
dev.off()
emm1a = emmeans(lme1, pairwise ~ Groups|PrePost)
plot(emm1a,main="(a)",comparisons=T,alpha=0.05,adjust="none",horizontal=F,xlab="Groups",ylab="",cex=2)
emm1b = emmeans(lme1, pairwise ~ PrePost|Groups)
plot(emm1b,main="(b)",comparisons=T,alpha=0.05,adjust="none",horizontal=F,xlab="PrePost",ylab="",cex=2)
emm2a = emmeans(lme1, ~ Groups)
plot(emm2a,main="(c)",comparisons=T,alpha=0.05,adjust="none",horizontal=F,xlab="",ylab="",cex=2)
emm2b = emmeans(lme1, ~ PrePost)
plot(emm2b,main="(d)",comparisons=T,alpha=0.05,adjust="none",horizontal=F,xlab="",ylab="",cex=2)





