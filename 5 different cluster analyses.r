library(psych)
library(GPArotation)
library(xtable)
library(kableExtra)
library(reshape2)
library(dplyr)
library(corrplot)
library(grDevices)
library(graphics)

a<-read.csv("~/Health and Her/final.csv")
names(a) <-c("Sleeping Problems","Loss of Sex Drive",
             "Painful Sex","Vaginal Dryness","Urinary Changes",
             "Period Changes","Stress/Anxiety","Skin Changes",
             "Joint Aches","Low Energy","Brain Fog","Headaches",
             "Bloating","Poor Conception","Palpitations","Dizziness",
             "Digestive Issues","Hairloss","Itchy Skin","Brittle Nails",
             "Breastpain","Burning Tongue","Muscle Tension","Irregular Heartbeat",
             "Body Odour","Allergies","Electric Shock Syndrome","Hot Flushes",
             "Weight Gain","Low Mood","Nightsweats","Memory Loss","Total Symptoms",
             "Severity Score","Outlier","Outlier_A")

# How many components are optimal using independant methods

# 1 Scree plot from correlation

VSS.scree(cor(a[,1:32]))

# 2 Cluster analysis and calculate Residual Least Squares error vs number of clusters

ic<-list()
for (i in 1:8){
  ic[i] <-iclust(a[,1:32],nclusters = i)$fit$clusterrmse
}

# Residual error plot i.e. Scree plot equivalent
plot(c(1:8),ic,ylab="Root Means Squared Error",
     xlab="Number of Clusters", 
     main="Number of Clusters vs RMSE")

# Both show 6 optimial compnents / clusters

# Loading threshold - Our sample size is greater than 350 so can use 0.3. We need a higher threshold for smaller datasets
# Hair JF, Tatham RL, Anderson RE and Black W (1998) Multivariate data analysis. (Fifth Ed.) Prentice-Hall:London.

loading_threshold <- 0.3
ncomps <- 6 # number of optimal compnents
# you can change this to a different number and re-run all code to produce a new correlation plot with 
# the number of factors you choose.
########################################### Method 1. PCA ################################################


# run the `principle`` function (pca), with 6 components and oblique rotation i.e. oblimin or simplimax
principal(a[,1:32], nfactors = ncomps, residuals = FALSE, rotate="oblimin")
principal(a[,1:32], nfactors = ncomps, residuals = FALSE, rotate="simplimax")

# Neither show the principal components are correlated, so we can use an orthoganol rotation, varimax.
pc<-principal(a[,1:32], nfactors = ncomps, residuals = FALSE,rotate="varimax")

# View various parts of the pca result using the $ notation, i.e. examine the rotated loadings
pc$loadings
# or the eigenvalues 
pc$values

# Scree plot using eigenvalues to verify we agree with independant methods
plot(pc$values, type="b")

# store the loadings in temp object pc_loadings for plotting in correlation plot later
pc_loadings<-pc$loadings
# Set any loading less than 0.3 to 0 (we don't want to see them on the correlation plot later on)
pc_loadings[pc_loadings < loading_threshold] <- 0

# Cronbach Alpha for each component 
# Select columns from the original data according to features in each component, then calculate alpha.

# Discard loadings below threshold entirely by setting to NA, or NULL
pc$loadings[abs(pc$loadings ) < loading_threshold] <- NA
pc.rota<-unclass(pc$loadings) # this just removes the summary part of the loadings so we are left with the matrix

# For each component, if a loading is NA, remove it from the matrix and store the feature name in a list
pc.features<-lapply(seq_along(colnames(pc.rota)), function(x) names(which(!is.na(pc.rota[,x]))))
# Calculate Aplha
pc.groupings.alpha<-sapply(seq_along(pc.features), function(x) alpha(a[, unlist(pc.features[[x]])]))
pc.groupings.alpha # shows all the objects that store various stats from the alpha calculationg
pc.groupings.alpha[1,1] # shows the stats for the first component
pc.stats <- pc.groupings.alpha[1,1:6] # stores stats for all components in pc.stats
pc.stats.drop <- pc.groupings.alpha[2,1:6]  # stats if item dropped




########################################### Method 2. Principal Axis ################################################

pa<-fa(a[,1:32], nfactors = ncomps,fm="pa" ,rotate="varimax")
pa_loadings<-pa$loadings
pa_loadings[pa_loadings < loading_threshold] <- 0
pa$loadings[abs(pa$loadings ) < loading_threshold] <- NA
pa.rota<-unclass(pa$loadings)
pa.features<-lapply(seq_along(colnames(pa.rota)), function(x) names(which(!is.na(pa.rota[,x]))))
# Cronbach Alpha
pa.groupings.alpha<-sapply(seq_along(pa.features), function(x) alpha(a[, unlist(pa.features[[x]])]))
pa.stats <- pa.groupings.alpha[1,1:6]
pa.stats.drop <- pa.groupings.alpha[2,1:6]


########################################### Method 3. Unweighted Least Squares #####################################


uls<-fa(a[,1:32], nfactors = ncomps,fm="uls" ,rotate="varimax")
plot(pa$values, type="b") # scree
uls_loadings<-uls$loadings
uls_loadings[uls_loadings < loading_threshold] <- 0
uls$loadings[abs(uls$loadings ) < loading_threshold] <- NA
uls.rota<-unclass(uls$loadings)
uls.features<-lapply(seq_along(colnames(uls.rota)), function(x) names(which(!is.na(uls.rota[,x]))))
#Cronbach Alpha
uls.groupings.alpha<-sapply(seq_along(uls.features), function(x) alpha(a[, unlist(uls.features[[x]])]))
uls.stats <- uls.groupings.alpha[1,1:6]
uls.stats.drop <- uls.groupings.alpha[2,1:6]


########################################### Method 4. Maximum Likelihood ###########################################

ml<-fa(a[,1:32], nfactors = ncomps,fm="ml" ,rotate="varimax")
ml_loadings<-ml$loadings
ml_loadings[ml_loadings < loading_threshold] <- 0
ml$loadings[abs(ml$loadings ) < loading_threshold] <- NA
ml.rota<-unclass(ml$loadings)
ml.features<-lapply(seq_along(colnames(ml.rota)), function(x) names(which(!is.na(ml.rota[,x]))))
#Cronbach Alpha
ml.groupings.alpha<-sapply(seq_along(ml.features), function(x) alpha(a[, unlist(ml.features[[x]])]))
ml.stats <- ml.groupings.alpha[1,1:6]
ml.stats.drop <- ml.groupings.alpha[2,1:6]


########################################### Method 5. Mcdonald's Alpha ############################################

om<-omega(a[,1:32],nfactors = ncomps)
om_loadings<-om$schmid$orthog
om$loadings<-om$schmid$orthog
om_loadings[om_loadings < loading_threshold] <- 0
om$loadings[abs(om$loadings ) < loading_threshold] <- NA
om.rota<-unclass(om$loadings)
om.features<-lapply(seq_along(colnames(om.rota)), function(x) names(which(!is.na(om.rota[,x]))))
#Cronbach Alpha
om.groupings.alpha<-sapply(seq_along(om.features), function(x) alpha(a[, unlist(om.features[[x]])]))
om.stats <- om.groupings.alpha[1,1:6]
om.stats.drop <- om.groupings.alpha[2,1:6]




##### Rest of the code is just for generating the 5 correlation plots. You have all the stats you need up to this point.

# specify Correlation plot colour schemes 
# #values are actually just a colour i.e. try putting in 7F0000 here https://www.rapidtables.com/web/color/RGB_Color.html 
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "royalblue2", "royalblue4"))
col2 <- colorRampPalette(c("#470116","#67001F", "#B2182B", "#D6604D", "white",
                           "yellow","#89FF00", "#007F1A", "#015813"))
col3 <- colorRampPalette(c("yellow","orange" , "white", "orchid" ,"navyblue")) 
col4 <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "white",
                           "yellow", "#FF7F00", "red", "#7F0000"))
col5 <- colorRampPalette(c("yellow4","yellow","orange" , "white", "pink" ,"red","#580101"))
#Combine into 1 object
model_colors<-c(col5,col2,col3,col4,col1)

# Create one list storing all loadings from each model so we can loop through it an plot them all in one go

models <- list(pc_loadings)
models <- append(models,list(pa_loadings))
models <- append(models,list(uls_loadings))
models <- append(models,list(ml_loadings))
models <- append(models,list(om_loadings))


# This is the plotting method 
# Look at correlation plot for PCA i.e. models[[1]]

png(file="pca_corr.png", res=300, width=5500, height=3000)
corrplot(pc_loadings, is.corr=FALSE, 
         tl.cex = 1.5,               # text size
         tl.col = "black",           # text colour
         method = "color",           # use colour (i.e. not greyscale)
         outline = T,                # use outline
         addCoef.col = "black",      # coefficients in black
         number.digits = 2,          # only have two decimal places in loadings
         number.cex = 1,             # number size
         cl.pos = 'n',               # remove coefficient scale (the numebrs are enough)
         col=model_colors[[1]](200)) # use colour theme 1    
dev.off()


# Would be nice to append Cronbach alpha to the factor labels in the correlation plot
# Put stats containing alpha in one list
stats <- list(pc.stats)
stats <- append(stats,list(pa.stats))
stats <- append(stats,list(uls.stats))
stats <- append(stats,list(ml.stats))
stats <- append(stats,list(om.stats))

# And loop through adding the alpha value to the compnent names
for (i in 1:length(models)){
colnames(models[[i]])<-paste(colnames(models[[i]]),":alpha",round(sapply(stats[[i]], "[[", 2),2))
}


# Let's loop through our 5 models (and their corresponding colour themes) and plot 5 correlation matrices
# Plot and create PNG fole "corr.png"
# png() tells R to start writing to file.
png(file="corr.png", res=300, width=5500, height=3000)
par(mfrow=c(1,5),oma = c(2, 0, 0, 2)) #five columns,  margin size in inches - left, top, right, bottom 
for (i in 1:length(models)){
corrplot(models[[i]], is.corr=FALSE, tl.cex = 1.5, tl.col = "black", method = "color", 
         outline = T,
         addCoef.col = "black", number.digits = 2, number.cex = 1, 
         cl.pos = 'n',col=model_colors[[i]](200))
}
par(xpd=NA) # don't let graph get in the way of legend
# legend position in x,y co-ordinates
legend(-55,-1,c("PCA", "Prinicpal Axis","Uweighted Least Squares","Maximum Likelihood","Omega"),
       # legend colours
       fill = c("red","green3","orchid3","orangered2","royalblue2"),ncol=5,cex=1.3)
dev.off() # tells r we have finished writing to file.


### Compare Scree plots

png(file="scree.png", res=300, width=5500, height=3000)
par(mfrow=c(2,3),oma = c(2, 0, 0, 2)) # 2 rows, 3 columns
plot(pc$values, ylab = "Eigenvalue of components",xlab="component number", type="b",main="PCA")
plot(pa$values, ylab = "Eigenvalue of components",xlab="component number",type="b",main="Principal Axis")
plot(uls$values, ylab = "Eigenvalue of components",xlab="component number",type="b",main="Unweighted Least Squares")
plot(ml$values, ylab = "Eigenvalues of components",xlab="component number",type="b",main="Maximum Likelihood")
VSS.scree(cor(a[,1:32]),main="Correlation Scree")
plot(c(1:8),ic,ylab="Root Means Squared Error",
          xlab="Number of Clusters", 
          type="b",main="IClust")
dev.off()




















