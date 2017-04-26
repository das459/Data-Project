##Load data
load("springs.Rdata")

options(contrasts = rep("contr.treatment", 2))


##Perform Correlation Analysis
springs.corr.variables = springs.data[c("Total.Area", "Geomorphic.Diversity",  "Latitude", "Elevation", "Water.Temperature", "Percent.Solar.Radiation.Summer",  "Structural.Diversity", "Functional.Diversity", "Species.Richness", "Flow", "Specific.Conductance", "Alkalinity")]


springs.correlations = cor(springs.corr.variables) #Make correlation matrix

library(corrgram)

source("corrgram.R") #Source nice function for display

correlogram = corrgram(springs.corr.variables, type = "data", lower.panel=panel.shadeNtext, upper.panel=NULL, cex.labels = 1.1, labels = c("Area", "Geom. Div.", "Latitude", "Elevation", "Water Temp.", "Summer Rad.", "Struct. Div.", "Func. Div.", "Sp. Richness", "Flow", "Spec. Cond.", "Alkalinity")) ##Make correlogram

##Remove missing data with means
regression.vars = c("Geomorphic.Diversity", "Flow",  "Latitude",  "Elevation", "pH", "Alkalinity", "Percent.Solar.Radiation.Summer",  "Water.Temperature", "Specific.Conductance", "Polygon.Count", "Total.Area", "Percent.North")

library(gtools)

for(j in 1:nrow(springs.data)){ 
for(i in 1:length(regression.vars)){
  var = regression.vars[i]
  
  if(is.na(springs.data[j, var])==T) {springs.data[j, var]= mean(springs.data[,var], na.rm = T) }
 
}}


##Perform MLR on Species Richness

richness.init.lm = lm(sqrt(Species.Richness) ~ log(Total.Area) + Geomorphic.Diversity, data = springs.data)

small = lm(sqrt(Species.Richness) ~ 1, data = springs.data)
big = lm(sqrt(Species.Richness) ~ log(Total.Area) + Geomorphic.Diversity + Flow + Latitude + Elevation + pH + Alkalinity + Percent.Solar.Radiation.Summer + Water.Temperature + Specific.Conductance + Percent.North  , data = springs.data)

summary(big)

##Create best model for all springs without incorporating spheres of discharge
springs.richness.lm = step(object = richness.init.lm, scope = list(lower = small, upper =  big), direction = "both")

summary(springs.richness.lm)


#Allow different intercepts for each sphere
springs.richness.type.lm = update(springs.richness.lm, ~ . + Spring.Type)
summary(springs.richness.type.lm)
#Allow different intercepts and interactions with geomorphic diversity
springs.richness.type.interaction.lm = update(springs.richness.lm, ~ . + Spring.Type+ Geomorphic.Diversity:Spring.Type)

summary(springs.richness.type.interaction.lm)
#Perform F tests between models
richness.anova = anova(springs.richness.lm, springs.richness.type.lm, springs.richness.type.interaction.lm)

plot(springs.richness.type.lm, which = 1)
richness.formula = springs.richness.lm$call

load("springs.Rdata")
levels(springs.data$Spring.Type) = c("Hanging Garden", "Helocrene", "Hillslope", "Rheocrene")
springs.data = springs.data[-c(173,290, 284), ]

springs.richness.lm = lm(formula = sqrt(Species.Richness) ~ log(Total.Area) + Geomorphic.Diversity + Latitude + Specific.Conductance + Percent.Solar.Radiation.Summer +  Water.Temperature + Alkalinity, data = springs.data)

summary(springs.richness.lm)



#Allow different intercepts for each sphere
springs.richness.type.lm = update(springs.richness.lm, ~ . + Spring.Type)
summary(springs.richness.type.lm)
#Allow different intercepts and interactions with geomorphic diversity
springs.richness.type.interaction.lm = update(springs.richness.lm, ~ . + Spring.Type+ Geomorphic.Diversity:Spring.Type)

summary(springs.richness.type.interaction.lm)
#Perform F tests between models
richness.anova = anova(springs.richness.lm, springs.richness.type.lm, springs.richness.type.interaction.lm)

plot(springs.richness.type.lm)

library(ggplot2)


richness.mlr.plot = ggplot() + geom_point(aes(y = springs.richness.type.lm$model$`sqrt(Species.Richness)`^2 , x = springs.richness.type.lm$fitted.values^2, color = springs.richness.type.lm$model$Spring.Type)) + labs( x = "Fitted Value (Species Richness)", y = "Species Richness", color = "Sphere of Discharge") + annotate("text", x = 40, y = 75, label = "Formula: sqrt(Richness) = log(Area) + Geomorphic Diversity + Latitude + Alkalinity + Temperature + Solar Radiation Summer + Spring Type", size = 3) + geom_abline(slope = 1, color = "black") + annotate("text", x = 15, y = 70, label = "R-Squared = 0.67", size = 3) + theme_classic()



##Try again with slight differences

springs.richness.lm = lm(formula = sqrt(Species.Richness) ~ log(Total.Area) + Geomorphic.Diversity + Latitude + log(Specific.Conductance) + Percent.Solar.Radiation.Summer +  Water.Temperature + Alkalinity, data = springs.data)

summary(springs.richness.lm)


#Allow different intercepts for each sphere
options(contrasts = rep("contr.sum", 2))
getOption("contrasts")
springs.richness.type.lm = update(springs.richness.lm, ~ . + Spring.Type)
summary(springs.richness.type.lm)
#Allow different intercepts and interactions with geomorphic diversity
springs.richness.type.interaction.lm = update(springs.richness.lm, ~ . + Spring.Type+ Geomorphic.Diversity:Spring.Type)



summary(springs.richness.type.interaction.lm)
#Perform F tests between models
richness.anova = anova(springs.richness.lm, springs.richness.type.lm, springs.richness.type.interaction.lm);richness.anova



library(ggplot2)


richness.mlr.plot = ggplot() + geom_point(aes(y = springs.richness.type.lm$model$`sqrt(Species.Richness)`^2 , x = springs.richness.type.lm$fitted.values^2, color = springs.richness.type.lm$model$Spring.Type, shape = springs.richness.type.lm$model$Spring.Type )) + labs( x = "Fitted Value (Species Richness)", y = "Species Richness", color = "Sphere of Discharge") + annotate("text", x = 40, y = 75, label = "Formula: sqrt(Richness) = log(Area) + Geomorphic Diversity + Latitude + log(Specific Conductance) +  Alkalinity + Temperature + Solar Radiation Summer + Spring Type", size = 5) + geom_abline(slope = 1, color = "black") + annotate("text", x = 15, y = 70, label = "R-Squared = 0.67", size = 5) + theme_classic() + scale_shape_manual(name = "Spring Type", values = c(1, 2, 0, 3)) + scale_colour_manual(values = c("red", "blue", "black", "green"), name = "Spring Type")

library(gmodels)

length(springs.richness.type.lm$coefficients)
cm = rbind(c(0,0,0,0,0,0,0,0,1,0,0), c(0,0,0,0,0,0,0,0,0,1,0), c(0,0,0,0,0,0,0,0,0,0,1), c(0,0,0,0,0,0,0,0,-1,-1,-1)); rownames(cm) = levels(Spring.Type)
levels = estimable(springs.richness.type.lm, cm, conf.int = 0.95)

ggplot() + geom_crossbar(data = levels, aes(x = 1:4, y = Estimate, ymin = Lower.CI, ymax = Upper.CI, color = rownames(levels)), fatten = 3.1) + scale_x_continuous(breaks = 1:4, labels = rownames(levels)) + scale_color_manual(values = c("red", "black", "blue", "green")) + labs(x = "Spring Type", color = "Spring Type", y = "Estimate of Effect on Species Richness")





plot(springs.richness.lm, which = 1)
MASS::boxcox(springs.richness.lm)


##Let's explore a simpler model
options(contrasts = rep("contr.sum", 2))
richness.null = lm(Species.Richness ~ 1, data = springs.data)
richness.type = lm(Species.Richness ~ Spring.Type + 0, data = springs.data)
anova(richness.null, richness.type)
summary(richness.type)
library(gmodels)
cm = diag(1, 4); rownames(cm) = levels(springs.data$Spring.Type)
estimable(richness.type, cm, conf.int = 0.95)



##Let's Do Mann-Whitney Rank Sum Tests
vars = c(16,17,18,23,24,25,27,28,29,30,31,32,33,34,40,49)
rank.var.names = colnames(springs.data[,vars])

rank.ps = matrix(NA, 16, 4)
colnames(rank.ps) = levels(springs.data$Spring.Type)
rownames(rank.ps) = rank.var.names
attach(springs.data)


plots = vector("list", 16)



log.vars = c(vars[c(1,7,9,12)])
p=list()

for(i in 1:length(rank.var.names)) 
  { 
    i = i
  variable = springs.data[,rank.var.names[i]]
  if(i == 1 | i == 7 | i ==9 | i ==12) {plots = ggplot(springs.data) + geom_boxplot(aes_string(x = Spring.Type, y = variable, color = Spring.Type, outlier.shape = Spring.Type), show.legend = F) + labs(x = "Spring Type", y = rank.var.names[i]) + scale_y_log10()} else{plots = ggplot(springs.data) + geom_boxplot(aes_string(Spring.Type, variable, color = Spring.Type, outlier.shape = Spring.Type), show.legend = F) + labs(x = "Spring Type", y = rank.var.names[i]) }
 
  for(j in 1:length(levels(Spring.Type))){ 
   sphere = levels(Spring.Type)[j] 
   test = wilcox.test(variable[Spring.Type==sphere], variable[Spring.Type!=sphere])
   if(test$p.value < 0.05){ rank.ps[i,j] = "*" } else{ rank.ps[i,j] = "NA" }
   if(test$p.value < 0.01){ rank.ps[i,j] = "**" }
   if(test$p.value < 0.001){ rank.ps[i,j] = "***" }
 
 plots =  plots  + scale_shape_manual(name = "Spring Type", values = c(1, 2, 0, 3)) + scale_colour_manual(values = c("red", "blue", "black", "green"), name = "Spring Type") +  if(test$p.value < 0.05){ annotate("text", j, median(variable, na.rm = T), label = paste0("","\n",  rank.ps[i, j]), size = 8 )} 
 
 } 

p[[i]] = plots
}
 
library(gridExtra)
grid.arrange(grobs = p, ncol = 2) 


##Let's explore NMDS
library(vegan)


library(dplyr)
load('springs.Rdata')

functional.data = springs.data %>% select(Wetland.Area:Unknown.Area) %>% na.replace(0)

functional.data = cbind(functional.data, springs.data$Spring.Type)
colnames(functional.data) = c("Wetland.Area","Wetland.Riparian.Area","Riparian.Area","Facultative.Area",         "Upland.Area",   "Aquatic.Area", "Unknown.Area"    ,"Spring.Type")

functional.data = functional.data[-which(rowSums(functional.data[,1:7]) == 0),]
functional.nmds = metaMDS(functional.data[,1:7], trymax = 100)



stressplot(functional.nmds)

ggplot() + geom_point(aes(functional.nmds$points[,1], functional.nmds$points[,2], color = functional.data$Spring.Type, shape = functional.data$Spring.Type), show.legend = F) + scale_shape_manual(name = "Spring Type", values = c(1, 2, 0, 3)) + scale_colour_manual(values = c("red", "blue", "black", "green"), name = "Spring Type") 




ordiplot(functional.nmds,type="p")
ordihull(functional.nmds,groups=functional.data$Spring.Type,draw="lines",col=c("red", "blue", "green", "yellow"),label=F)


functional.anosim = anosim(functional.data[,1:7], functional.data[,8])
plot(functional.anosim)


adonis2(functional.data[,1:7] ~ functional.data[,8], data = functional.data)

library(tidyr)
functional.long = gather(functional.data, key = Level, value = Species, 1:7, factor_key = T  ); levels(functional.long$Level) = c("W", "W-R", "R", "F", "U", "A", "U")

ggplot(functional.long) + geom_boxplot(aes(Level, Species, color = Level)) + facet_grid( . ~ Spring.Type) + labs(y = expression("Area " (m^2)), x = "Functional Group", color = "") + scale_y_continuous(trans="log", breaks=c(1,10,100,1000))

attach(springs.data)
proportion = function(x, area){ 
  prop = x/area
  return(prop)}


functional.data = springs.data %>% select(Wetland.Area:Unknown.Area, Total.Area, Spring.Type, Geomorphic.Diversity) 

functional.data = mutate_each(functional.data, funs(proportion(., Total.Area)), -Spring.Type, -Geomorphic.Diversity) %>% na.replace(0)

functional.data = na.omit(functional.data); functional.data = functional.data[-which(rowSums(functional.data[,1:7])==0),]


functional.nmds = metaMDS(functional.data[,1:7]) 


colnames(functional.data) = c("Wetland.Area","Wetland.Riparian.Area","Riparian.Area","Facultative.Area",         "Upland.Area",   "Aquatic.Area", "Unknown.Area", "Total.Area",  "Spring.Type")

functional.long = gather(functional.data, key = Level, value = Species, 1:7, factor_key = T  ); levels(functional.long$Level) = c("Wet", "W-R", "Rip", "Fac", "Up", "Aq", "Unk")

functional.long = functional.long %>% na.replace(0)
ggplot(functional.long) + geom_boxplot(aes(Level, Species, color = Level)) + facet_grid( . ~ Spring.Type) + labs(y = expression(" Percent Cover" ), x = "Functional Group", color = "") + scale_y_continuous(trans = "sqrt", breaks = c(.01, 0.05, 0.1, .3, .5, .9), labels = c("1%", "5%", "10%", "30%", "50%", "90%"))

##Now structural groups
load("springs.Rdata")
structural.data = springs.data %>% select(Ground.Cover.Species.Count:Tall.Canopy.Species.Count, Spring.Type) %>% na.replace(0)


##structural.data = structural.data[-which(rowSums(structural.data[,1:4]) == 0),]
structural.nmds = metaMDS(structural.data[,1:4], trymax = 100)

stressplot(structural.nmds)

ggplot() + geom_point(aes(structural.nmds$points[,1], structural.nmds$points[,2], color = structural.data$Spring.Type, shape = structural.data$Spring.Type, size = Geomorphic.Diversity), show.legend = F) + geom_text(aes(structural.nmds$species[,1], structural.nmds$species[,2], label = c("GC", "SC", "MC", "TC") )) + scale_shape_manual(name = "Spring Type", values = c(1, 2, 0, 3)) + scale_colour_manual(values = c("red", "blue", "black", "green"), name = "Spring Type") + labs(x = "NMDS 1", y = "NMDS 2") + annotate("text", .7,1, label = "Stress = 0.13")

structural.nmds$stress
functional.nmds$stress
ggplot() + geom_point(aes(functional.nmds$points[,1], functional.nmds$points[,2], color = functional.data$Spring.Type, shape = functional.data$Spring.Type, size = functional.data$Geomorphic.Diversity), show.legend = F) + scale_shape_manual(name = "Spring Type", values = c(1, 2, 0, 3)) + scale_colour_manual(values = c("red", "blue", "black", "green"), name = "Spring Type") + labs(x = "NMDS 1", y = "NMDS 2") + annotate("text", 1,2, label = "Stress = 0.22")



ordiplot(structural.nmds,type="p")
ordihull(structural.nmds,groups=structural.data$`springs.data$Spring.Type`,draw="lines",col=c("red", "blue", "green", "yellow"),label=F) 


structural.anosim = anosim(structural.data[,1:4], structural.data[,5])
plot(structural.anosim)


adonis2(structural.data[,1:4] ~ structural.data[,5], data = structural.data)

library(tidyr)
structural.long = gather(structural.data, key = Level, value = Species, 1:4, factor_key = T  ); levels(structural.long$Level) = c("GC", "SC", "MC", "TC")

ggplot(structural.long) + geom_boxplot(aes(Level, Species, color = Level)) + facet_grid( . ~ springs.data$Spring.Type) + labs(y = "Species Richness", x = "Structural Level") + scale_colour_manual(values = c("red", "blue", "black", "green"), name = "Spring Type") + scale_y_sqrt()

summary(structural.anosim)



##BIO-ENV Data
load("springs.Rdata")

springs.data = filter(springs.data, is.na(Total.Area)==F, is.na(Flow)==F, is.na(Specific.Conductance)==F, is.na(Alkalinity) == F, is.na(Percent.Solar.Radiation.Summer)==F, is.na(Percent.North)==F)
structural.data = springs.data %>% select(Ground.Cover.Species.Count:Tall.Canopy.Species.Count) %>% na.replace(0)

env.data = springs.data %>% select(Total.Area, Specific.Conductance, Alkalinity, Flow, Elevation, Latitude, Percent.North, Percent.Solar.Radiation.Summer)
structural.bioenv = bioenv(comm = structural.data, env = env.data, metric = "euclidean" )
summary(structural.bioenv)



##BIO-ENV Data
load("springs.Rdata")

springs.data = filter(springs.data, is.na(Total.Area)==F, is.na(Flow)==F, is.na(Specific.Conductance)==F, is.na(Alkalinity) == F, is.na(Percent.Solar.Radiation.Summer)==F, is.na(Percent.North)==F)
functional.data = springs.data %>% select(Wetland.Area:Unknown.Area, Total.Area) %>% na.replace(0)
functional.data = mutate_each(functional.data, funs(proportion(., Total.Area)))
functional.data = functional.data %>% select(-Total.Area)

env.data = springs.data %>% select(Total.Area, Specific.Conductance, Alkalinity, Flow, Elevation, Latitude, Percent.North, Percent.Solar.Radiation.Summer)
functional.bioenv = bioenv(comm = functional.data, env = env.data, metric = "euclidean" )
summary(functional.bioenv)


functional.nmds = metaMDS(functional.data)


envfit(functional.nmds, env.data)

ggplot() + geom_point(aes(functional.nmds$points[,1], functional.nmds$points[,2], size = env.data$Specific.Conductance ), shape = 21)

structural.nmds = metaMDS(structural.data)

envfit(structural.nmds, env.data)

ggplot() + geom_point(aes(structural.nmds$points[,1], structural.nmds$points[,2], size = env.data$Latitude ), shape = 21)

ggplot() + geom_point(aes(structural.nmds$points[,1], structural.nmds$points[,2], size = env.data$Specific.Conductance ), shape = 21)

dis = vegdist(structural.data)
clust = hclust(dis, method = "complete")
plot(clust)
rect.hclust(clust, 3)
grp = cutree(clust, 3)

ord = cca(structural.data)

plot(ord, display = "sites", xlim = c(-5,5))
ordihull( ord, grp, lty = 2, col = c("red", "blue", "green"))

load("springs.Rdata")


library(dplyr)
springs.omit = springs.data %>% select(Total.Area, pH, Specific.Conductance, Alkalinity, Flow, Elevation, Latitude, Species.Richness, Functional.Diversity, Structural.Diversity, Polygon.Count, Geomorphic.Diversity, Spring.Type, Water.Temperature, Percent.Solar.Radiation.Summer, Percent.North)
springs.omit = na.omit(springs.omit)

library(MASS)

springs.lda = lda(Spring.Type ~ ., data = springs.omit )
springs.lda.values = predict(springs.lda)


library(ggplot2)
ggplot() + geom_point(aes(springs.lda.values$x[,1], springs.lda.values$x[,2], shape = springs.omit$Spring.Type, color = springs.omit$Spring.Type )) + labs(x = "LDA 1", y = "LDA 2") + scale_shape_manual(name = "Spring Type", values = c(1, 2, 0, 3)) + scale_colour_manual(values = c("red", "blue", "black", "green"), name = "Spring Type")

length(which(springs.lda.values$class == springs.omit$Spring.Type))/94

ggplot() + geom_line(aes(y = springs.lda$scaling[,1], x = 1:15), color = "red") + geom_line(aes(y = springs.lda$scaling[,2], x = 1:15), color = "blue") + geom_line(aes(y = springs.lda$scaling[,3], x = 1:15), color = "green") + scale_x_continuous(breaks = 1:15, labels = rownames(springs.lda$scaling)) + labs(x = "Original Variable", y = "LDA Coefficient") + annotate("text", 2, 2, label = "LD1", color = "red", , size = 7) + annotate("text", 2, 1.6, label = "LD2", color = "blue", size = 7) + annotate("text", 2, 1.2, label = "LD3", color = "green", size = 7)

library(car)
library(caret)

load("springs.Rdata")
attach(springs.data)
scatterplot(Geomorphic.Diversity, Species.Richness, groups = Spring.Type, legend.coords = "topleft", smoother = F, reg.line = lm, col = c("red", "blue", "black", "green"), pch = c(1,2,0,3))

library(caret)
caret::bagEarth()
fit = bagEarth(Spring.Type ~ ., data = springs.omit)


##LDA Confusion Matrix
confusion = confusionMatrix(springs.lda.values$class, springs.omit$Spring.Type)
confusion$table


##Remove missing data for PR Analysis
regression.vars = c("Geomorphic.Diversity", "Flow",  "Latitude",  "Elevation", "pH", "Alkalinity", "Percent.Solar.Radiation.Summer",  "Water.Temperature", "Specific.Conductance", "Polygon.Count", "Total.Area", "Percent.North")

library(gtools)

for(j in 1:nrow(springs.data)){ 
  for(i in 1:length(regression.vars)){
    var = regression.vars[i]
    
    if(is.na(springs.data[j, var])==T) {springs.data[j, var]= mean(springs.data[,var], na.rm = T) }
    
  }}
springs.pr = springs.data[,regression.vars]
pr = princomp(springs.pr, cor = T)

library(ggbiplot)
prpoints = ggbiplot(pr, choices = 1:2, groups = springs.data$Spring.Type, var.axes = F, show.legend = F) + labs(x = "PC1", y = "PC2", col = "Springs Type") + scale_x_continuous(limits = c(-5,2))+ scale_y_continuous(limits = c(-3,3))

praxes = ggbiplot(pr, alpha = 0, varname.adjust = 1, varname.size = 2.5, varname.abbrev = F) + labs(x = "PC1", y = "PC2") + scale_x_continuous(limits = c(-5,2)) + scale_y_continuous(limits = c(-3,3))
library(gridExtra)
grid.arrange(prpoints, praxes, ncol = 2)
