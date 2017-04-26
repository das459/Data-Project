springs.data = read.csv("SpringsData.csv", header = F, skip = 0, nrows = 341)

springs.var.names = c("Survey.ID",	"Survey.Date",	"Site.ID",	"Area.A","Area.B", "Area.C", "Area.D", "Area.E", "Area.F", "Area.G", "Area.H", "Area.I", "Area.J", "Area.K", "Area.L", 	"Total.Area", "Polygon.Count",	"Geomorphic.Diversity",	"Ground.Cover.Species.Count", 	"Shrub.Cover.Species.Count",	"Middle.Canopy.Species.Count",	"Tall.Canopy.Species.Count",	"Species.Richness",	"Latitude",	"Elevation", "Spring.Type",	"Flow", "pH",	"Specific.Conductance", "Water.Temperature", "Alkalinity", "Percent.Solar.Radiation.Summer", 	"Aspect",	"Percent.North",	"Ground.Cover.Percent.Cover", "Shrub.Cover.Percent.Cover",	"Middle.Canopy.Percent.Cover",	"Tall.Canopy.Percent.Cover", 	"Aquatic.Percent.Cover",	"Structural.Diversity", "Nonvascular.Percent.Cover", "Wetland.Area", "Wetland.Riparian.Area",	"Riparian.Area", 	"Facultative.Area", "Upland.Area", "Aquatic.Area", 	"Unknown.Area",	"Functional.Diversity")
colnames(springs.data) = springs.var.names

levels(springs.data$Spring.Type) = c("Hanging Garden", "Helocrene", "Hillslope", "Rheocrene")

save(springs.data, file = 'springs.Rdata')

load('springs.Rdata')




##Lets look at the correlation of our variable


library(lattice)

pairs(~Total.Area + Geomorphic.Diversity + Latitude + Elevation + Flow + pH + Water.Temperature + Aspect + Percent.North + Percent.Solar.Radiation.Summer + Structural.Diversity + Functional.Diversity + Alkalinity, data = springs.data.vars)


springs.corr.variables = springs.data.vars[c("Total.Area", "Geomorphic.Diversity",  "Latitude", "Elevation", "Water.Temperature", "Percent.Solar.Radiation.Summer",  "Structural.Diversity", "Functional.Diversity")]


springs.correlations = cor(springs.corr.variables)



library(corrgram)

source("corrgram.R")

par(cex=0.6)
corrgram(springs.corr.variables, type = "data", lower.panel=panel.shadeNtext, upper.panel=NULL, cex.labels = .6)


library(gtools)


springs.data.vars = na.replace(springs.data.vars, 0)
plot(springs.data.vars[,-c(4,5,6,7,8,11,32,31,30,29,28,27,26,25,23,22,21,20,19)])
springs.correlations = cor(springs.data.vars[,-c(4,5,6,7,8,11,32,31,30,29,28,27,26,25,23,22,21,20,19)])



##Let's make some linear models to predict diversity
springs.lm = lm(Species.Richness ~ Total.Area + Geomorphic.Diversity + Spring.Type, data = springs.data)
summary(springs.lm)

plot(springs.data$Geomorphic.Diversity,springs.data$Species.Richness)
library(ggplot2)

betas.springs.lm = coef(springs.lm)



ggplot() + geom_point(aes(x=springs.lm$fitted.values, y=springs.data$Species.Richness, color=springs.data$Spring.Type)) 

springs.2.lm = lm(Species.Richness ~ Total.Area + Geomorphic.Diversity + Spring.Type + Spring.Type:Geomorphic.Diversity, data = springs.data)
summary(springs.2.lm)

length(springs.data$Species.Richness)
length(springs.lm$fitted.values)
length(springs.data$Total.Area[is.na(springs.data$Species.Richness)==T])

library(gmodels)
library(class)
library(dplyr)
sample()
Training = sample(c(0,1), nrow(springs.data), replace = T, prob = c(2/3,1/3))
vars = c(16,17,18,23,24,25,26,27,28,29,30,31,32,33,34,40,49)
training = springs.data[Training==0,my.vars]
testing = springs.data[Training==1,my.vars] 

springs.knn = apply(springs.data[,vars[-7]], 2, scale)  
training = springs.knn[Training==0,]
testing = springs.knn[Training==1,] 

probs = c()
for(i in 1:50){ 
knn = knn(train = training, test = testing, cl = springs.data[Training==0,26] , i)
knn
springs.data[Training==1,26]
True = knn == springs.data[Training==1,26]
length(True[True==T])/length(True)

 }
barplot(probs)

springs.data %>% group_by(Spring.Type) %>% summarise(n())
samples = sample(levels(springs.data$Spring.Type), 127, replace = T, prob=c(40/341,39/341,160/341,102/341))

True = samples == springs.data[Training==1,26]
length(True[True==T])/length(True)


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

prpoints = ggbiplot(pr, choices = 1:2, groups = springs.data$Spring.Type, var.axes = F, show.legend = F) + labs(x = "PC1", y = "PC2", col = "Springs Type") + scale_x_continuous(limits = c(-5,2))+ scale_y_continuous(limits = c(-3,3))

praxes = ggbiplot(pr, alpha = 0, varname.adjust = 1, varname.size = 2.5, varname.abbrev = F) + labs(x = "PC1", y = "PC2") + scale_x_continuous(limits = c(-5,2)) + scale_y_continuous(limits = c(-3,3))
library(gridExtra)
grid.arrange(prpoints, praxes, ncol = 2)

prloadings = as.data.frame(pr$loadings)
ggplot() + for(i in 1:12){geom_point(aes(y = pr$loadings[,i], x = rownames(pr$loadings)))}
pr$loadings[,1]

princomp()
library(randomForest)
library(caret)
Richness.RF = randomForest(Species.Richness  ~ Elevation + Latitude + Total.Area + Flow + Geomorphic.Diversity + Spring.Type  + Percent.North + Specific.Conductance + Water.Temperature, data = springs.data, na.action = na.roughfix); Richness.RF
importance(Richness.RF)
train(Spring.Type ~ Elevation + Latitude + Total.Area + Flow + Geomorphic.Diversity + Species.Richness  + Percent.North + Specific.Conductance + Water.Temperature, data = springs.data.vars, na.action = na.roughfix)

(Richness.RF = randomForest(Species.Richness ~ Elevation + Latitude + Total.Area + Flow + Geomorphic.Diversity + Spring.Type  + Specific.Conductance + Water.Temperature, data = springs.data.vars, na.action = na.roughfix))
importance(Richness.RF)


(Functional.RF = randomForest(Functional.Diversity ~ Elevation + Latitude + Total.Area + Flow + Geomorphic.Diversity + Spring.Type  + Specific.Conductance + Water.Temperature, data = springs.data.vars, na.action = na.roughfix))
importance(Functional.RF)

(Geomorph.RF = randomForest(Geomorphic.Diversity ~ Elevation + Latitude + Total.Area + Flow + Spring.Type + Specific.Conductance + Water.Temperature, data = springs.data.vars, na.action = na.roughfix))
importance(Geomorph.RF)

##Helocrene
helocrene = springs.data.vars[springs.data.vars$Spring.Type=="helocrene",]
plot(Species.Richness ~ Elevation, data = helocrene)
helocrene.lm = lm(Species.Richness ~ Elevation + Latitude + Total.Area + Flow + Geomorphic.Diversity + Specific.Conductance + Water.Temperature, data = helocrene)

summary(helocrene.lm)

##Rheocrene
rheocrene = springs.data.vars[springs.data.vars$Spring.Type=="rheocrene",]
plot(Species.Richness ~ Elevation, data = rheocrene)
rheocrene.lm = lm(Species.Richness ~ Elevation + Latitude + Total.Area + Flow + Geomorphic.Diversity + Specific.Conductance + Water.Temperature, data = rheocrene)

summary(rheocrene.lm)

##Hillslope
hillslope = springs.data.vars[springs.data.vars$Spring.Type=="hillslope",]
plot(Species.Richness ~ Elevation, data = hillslope)
hillslope.lm = lm(Species.Richness ~ Elevation + Latitude + Total.Area + Flow + Geomorphic.Diversity + Specific.Conductance + Water.Temperature, data = hillslope)

summary(hillslope.lm)


##Hanging Garden
##Hillslope
hanging = springs.data.vars[springs.data.vars$Spring.Type=="hanging garden",]
plot(Species.Richness ~ Elevation, data = hanging)
hanging.lm = lm(Species.Richness ~ Elevation + Latitude + Total.Area + Flow + Geomorphic.Diversity + Specific.Conductance + Water.Temperature, data = hanging)

summary(hanging.lm)


library(ggbiplot)
springs.classes = springs.pr[,7]
print(ggbiplot(pr, obs.scale = 1, var.scale = 1, groups =springs.classes, ellipse = TRUE, circle = TRUE))




density.lm = lm(Species.Richness/log(Total.Area + 1) ~ Geomorphic.Diversity, data = springs.data)
summary(density.lm)

density.type.lm = lm(Species.Richness/log(Total.Area + 1) ~ Spring.Type + Geomorphic.Diversity + Spring.Type:Geomorphic.Diversity, data = springs.data)

summary(density.type.lm)

anova(density.lm, density.type.lm)

ggplot() + geom_point(aes(y = springs.data$Species.Richness/log(springs.data$Total.Area + 1), x = density.type.lm$fitted.values, color = springs.data$Spring.Type))

##Let's look at difference in functional groups by springs type

functional.mean = springs.data %>% group_by(Spring.Type) %>% summarise_at(vars(Wetland.Area:Unknown.Area), mean)

functional.mean = springs.data %>% group_by(Spring.Type) %>% summarise_at(vars(Wetland.Area:Unknown.Area), funs(min,max))



##Let's compare our Spring Types
func.boxplot = ggplot(springs.data) + geom_boxplot(aes(Spring.Type, Functional.Diversity, color = Spring.Type), show.legend = F) + labs(x = "Spring Type", y = "Functional Diversity")

density.boxplot = ggplot(springs.data) + geom_boxplot(aes(Spring.Type, Species.Richness/log(Total.Area), color = Spring.Type), show.legend = F) + labs(x = "Spring Type", y = "Species Density")

richness.boxplot = ggplot(springs.data) + geom_boxplot(aes(Spring.Type, Species.Richness,  color = Spring.Type), show.legend = F) + labs(x = "Spring Type", y = "Species Richness")

struct.boxplot = ggplot(springs.data) + geom_boxplot(aes(Spring.Type, Structural.Diversity, color = Spring.Type), show.legend = F) + labs(x = "Spring Type", y = "Structural Diversity")

geomorphic.boxplot = ggplot(springs.data) + geom_boxplot(aes(Spring.Type, Geomorphic.Diversity, color = Spring.Type), show.legend = F) + labs(x = "Spring Type", y = "Geomorphic Diversity")

habitats.boxplot = ggplot(springs.data) + geom_boxplot(aes(Spring.Type, Polygon.Count, color = Spring.Type), show.legend = F) + labs(x = "Spring Type", y = "Number of Microhabitats")

gridExtra::grid.arrange(func.boxplot, struct.boxplot, geomorphic.boxplot, habitats.boxplot, richness.boxplot, density.boxplot)


install.packages("rJava",type='source')

install.packages("RWeka")

