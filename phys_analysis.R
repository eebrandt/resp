setwd("~/projects/dissertation/uncategorized/physiology/respirometry/data")

vco2data <- read.csv("vco2_data_use.csv", header = TRUE, sep = ",")
vco2data <- subset(vco2data, vco2data$individual != "IE17169")

plot(vco2data$temperature, vco2data$VCO2_corrected)

pairs(vco2data)
coplot(VCO2_raw~species | individual, data = vco2data)

M2 <- lm(vco2data$VCO2_raw~vco2data$temperature)
par(mfrow = c(2,2))
plot(M2, add.smooth = FALSE)
par(op)

plot(vco2data$VCO2_raw~vco2data$temperature)
abline(M2)


M1 <- glm(vco2data$VCO2_raw ~ vco2data$temperature + vco2data$individual + vco2data$species + vco2data$sex)

males <- subset(vco2data, vco2data$sex == "male")
female <- subset(vco2data, vco2data$sex == "female")

plot(males$temperature, males$VCO2_corrected)

par(mfrow=c(1,2))
boxplot(vco2data$VCO2_corrected~ vco2data$temperature, main = "VCO2 by temperature", ylab = "VCO2 (mL/min/mg)", xlab = "Temperature (C)", col = "grey")
boxplot(vco2data$VCO2_raw~ vco2data$temperature, main = "VCO2 by temperature", ylab = "VCO2 (mL/min)", xlab = "Temperature (C)", col = "grey")

par(mfrow=c(1,2))

boxplot(males$VCO2_corrected~ males$temperature, col = "blue", main = "VCO2 by temperature (male)", ylab = "VCO2 (mL/min/mg)", xlab = "Temperature (C)")
boxplot(female$VCO2_corrected~ female$temperature, col = "red", main = "VCO2 by temperature (female)", ylab = "VCO2 (mL/min/mg)", xlab = "Temperature (C)")

all10 <- subset(vco2data, vco2data$temperature =="10")
all15 <- subset(vco2data, vco2data$temperature =="15")
all20 <- subset(vco2data, vco2data$temperature =="20")
all25 <- subset(vco2data, vco2data$temperature =="25")
all30 <- subset(vco2data, vco2data$temperature =="30")
all35 <- subset(vco2data, vco2data$temperature =="35")
all40 <- subset(vco2data, vco2data$temperature =="40")

anova10sp <- aov(all10$VCO2_corrected~all10$species)
anova10sex <- aov(all10$VCO2_corrected~all10$sex)
anova(anova10sp)
anova(anova10sex)

anova15sp <- aov(all15$VCO2_corrected~all15$species)
anova15sex <- aov(all15$VCO2_corrected~all15$sex)
anova(anova15sp)
anova(anova15sex)

anova20sp <- aov(all20$VCO2_corrected~all20$species)
anova20sex <- aov(all20$VCO2_corrected~all20$sex)
anova(anova20sp)
anova(anova20sex)

TukeyHSD(anova20sp)

anova25sp <- aov(all25$VCO2_corrected~all25$species)
anova25sex <- aov(all25$VCO2_corrected~all25$sex)
anova(anova25sp)
anova(anova25sex)

TukeyHSD(anova25sp)

anova30sp <- aov(all30$VCO2_corrected~all30$species)
anova30sex <- aov(all30$VCO2_corrected~all30$sex)
anova(anova30sp)
anova(anova30sex)

TukeyHSD(anova30sp)

anova35sp <- aov(all35$VCO2_corrected~all35$species)
anova35sex <- aov(all35$VCO2_corrected~all35$sex)
anova(anova35sp)
anova(anova35sex)

TukeyHSD(anova35sp)

anova40sp <- aov(all40$VCO2_corrected~all40$species)
anova40sex <- aov(all40$VCO2_corrected~all40$sex)
anova(anova40sp)
anova(anova40sex)

TukeyHSD(anova40sp)

geronimoi <- subset(vco2data, vco2data$species =="geronimoi")
virgulatus <- subset(vco2data, vco2data$species == "virgulatus")
clypeatus <- subset(vco2data, vco2data$species == "clypeatus")
pugillis <- subset(vco2data, vco2data$species == "pugillis")
conjunctus <- subset(vco2data, vco2data$species == "conjunctus")
hallani <- subset(vco2data, vco2data$species == "hallani")

germal <- subset(geronimoi, geronimoi$sex == "male")
gerfem <- subset(geronimoi, geronimoi$sex == "female")

virmal<- subset(virgulatus, virgulatus$sex == "male")
virfem<- subset(virgulatus, virgulatus$sex == "female")

clymal <- subset(clypeatus, clypeatus$sex == "male")
clyfem <- subset(clypeatus, clypeatus$sex == "female")

pugmal <- subset(pugillis, pugillis$sex == "male")
pugfem <- subset(pugillis, pugillis$sex == "female")

conmal <- subset(conjunctus, conjunctus$sex == "male")
confem <- subset(conjunctus, conjunctus$sex == "female")

halmal <- subset(hallani, hallani$sex =="male")
halfem <- subset(hallani, hallani$sex == "female")

par(mfrow=c(1,2))

boxplot(germal$VCO2_corrected~germal$temperature, ylim = c(0,0.00040), main = "Geronimoi Male", col = "orange", ylab = "mL CO2/min/mg", xlab = "Temperature (C)")
boxplot(gerfem$VCO2_corrected~gerfem$temperature, ylim = c(0,0.00040), main = "Geronimoi Female", col = "yellow", ylab = "mL CO2/min/mg", xlab = "Temperature (C)")

boxplot(virmal$VCO2_corrected~virmal$temperature, ylim = c(0,0.00040), main = "Virgulatus Male", col = "dark green", ylab = "mL CO2/min/mg", xlab = "Temperature (C)")
boxplot(virfem$VCO2_corrected~virfem$temperature, ylim = c(0,0.00040), main = "Virgulatus Female", col = "green", ylab = "mL CO2/min/mg", xlab = "Temperature (C)")

boxplot(clymal$VCO2_corrected~clymal$temperature, ylim = c(0,0.00040), main = "Clypeatus Male", col = "red", ylab = "mL CO2/min/mg", xlab = "Temperature (C)")
boxplot(clyfem$VCO2_corrected~clyfem$temperature, ylim = c(0,0.00040), main = "Clypeatus Female", col = "pink", ylab = "mL CO2/min/mg", xlab = "Temperature (C)")

boxplot(pugmal$VCO2_corrected~pugmal$temperature, ylim = c(0,0.00040), main = "Pugillis Male", col = "blue", ylab = "mL CO2/min/mg", xlab = "Temperature (C)")
boxplot(pugfem$VCO2_corrected~pugfem$temperature, ylim = c(0,0.00040), main = "Pugillis Female", col = "light blue", ylab = "mL CO2/min/mg", xlab = "Temperature (C)")

boxplot(conmal$VCO2_corrected~conmal$temperature, ylim = c(0,0.00040), main = "Conjunctus Male", col = "purple", ylab = "mL CO2/min/mg", xlab = "Temperature (C)")
boxplot(confem$VCO2_corrected~confem$temperature, ylim = c(0,0.00040), main = "Conjunctus Female", col = "lavender", ylab = "mL CO2/min/mg", xlab = "Temperature (C)")

boxplot(halmal$VCO2_corrected~halmal$temperature, ylim = c(0,0.00040), main = "Hallani Male", col = "magenta", ylab = "mL CO2/min/mg", xlab = "Temperature (C)")
boxplot(halfem$VCO2_corrected~halfem$temperature, ylim = c(0,0.00040), main = "Hallani Female", col = "pink", ylab = "mL CO2/min/mg", xlab = "Temperature (C)")

par(mfrow=c(2,3))

boxplot(virgulatus$VCO2_corrected~virgulatus$temperature,ylim = c(0,0.00020), main = "Virgulatus", col = "green", ylab = "mL CO2/min/mg", cex.lab =1.5, cex.main =2, cex.axis = 1.4, xlab = expression(paste("Temperature (",degree,"C)")))
boxplot(conjunctus$VCO2_corrected~conjunctus$temperature,ylim = c(0,0.00020), main = "Conjunctus", col = "purple", ylab = "mL CO2/min/mg", cex.lab =1.5, cex.main =2, cex.axis = 1.4, xlab = expression(paste("Temperature (",degree,"C)")))
boxplot(hallani$VCO2_corrected~hallani$temperature,ylim = c(0,0.00020), main= "Hallani", col = "pink", ylab = "mL CO2/min/mg", cex.lab =1.5, cex.main =2, cex.axis = 1.4, xlab = expression(paste("Temperature (",degree,"C)")))
boxplot(clypeatus$VCO2_corrected~clypeatus$temperature,ylim = c(0,0.00020), main = "Clypeatus", col = "red", ylab = "mL CO2/min/mg", cex.lab =1.5, cex.main =2, cex.axis = 1.4, xlab = expression(paste("Temperature (",degree,"C)")))
boxplot(pugillis$VCO2_corrected~pugillis$temperature,ylim = c(0,0.00020), main = "Pugillis", col = "blue", ylab = "mL CO2/min/mg", cex.lab =1.5, cex.main =2, cex.axis = 1.4, xlab = expression(paste("Temperature (",degree,"C)")))
boxplot(geronimoi$VCO2_corrected~geronimoi$temperature, ylim = c(0,0.00020), main = "Geronimoi", col = "orange", ylab = "mL CO2/min/mg", cex.lab =1.5, cex.main =2, cex.axis = 1.4, xlab = expression(paste("Temperature (",degree,"C)")))





par(mfrow=c(1,1))

plot(vco2data$VCO2_raw~vco2data$weight..mg.)
plot(all10$VCO2_raw~all10$weight..mg., type = "p", cex =.75, pch=23, bg = "black",ylim =c(0, 0.003), main = "VCO2 by Weight", ylab = "VCO2 (mL/min/mg)", xlab = "weight (mg)")
lines(all15$VCO2_raw~all15$weight..mg.,type = "p", cex =.75,pch=23, bg = "purple")
lines(all20$VCO2_raw~all20$weight..mg., type = "p", cex =.75,pch=23, bg = "blue")
lines(all25$VCO2_raw~all25$weight..mg., type = "p", cex =.75,pch=23, bg = "green")
lines(all30$VCO2_raw~all30$weight..mg., type = "p", cex =.75,pch=23, bg = "yellow")
lines(all35$VCO2_raw~all35$weight..mg.,  type = "p", cex =.75,pch=23, bg = "orange")
lines(all40$VCO2_raw~all40$weight..mg.,type = "p", cex =.75,pch=23, bg = "red")

par(mfrow=c(1,1))

plot(vco2data$VCO2_raw~vco2data$date)
plot(all10$VCO2_corrected~all10$date, type = "p", cex =.75, pch=23, bg = "black",ylim =c(0, 0.0002), main = "VCO2 by Date", ylab = "VCO2 (mL/min/mg)", xlab = "date")
lines(all15$VCO2_corrected~all15$date,type = "p", cex =.75,pch=23, bg = "purple")
lines(all20$VCO2_corrected~all20$date, type = "p", cex =.75,pch=23, bg = "blue")
lines(all25$VCO2_corrected~all25$date, type = "p", cex =.75,pch=23, bg = "green")
lines(all30$VCO2_corrected~all30$date, type = "p", cex =.75,pch=23, bg = "yellow")
lines(all35$VCO2_corrected~all35$date,  type = "p", cex =.75,pch=23, bg = "orange")
lines(all40$VCO2_corrected~all40$date,type = "p", cex =.75,pch=23, bg = "red")

temps = c("cool", "room", "warm")
rates = c(0, 13, 67)
rateframe = data.frame(temps, rates) 

box()

ger15 <- subset(geronimoi, geronimoi$temperature == "15")
ger30 <- subset(geronimoi, geronimoi$temperature == "30")
gR1 <- mean(ger15$VCO2_raw)
gR2 <- mean(ger30$VCO2_raw)
gQ10 <- (gR2/gR1)^(10/15)

pug15 <- subset(pugillis, pugillis$temperature == "15")
pug30 <- subset(pugillis, pugillis$temperature == "30")
pR1 <- mean(pug15$VCO2_raw)
pR2 <- mean(pug30$VCO2_raw)
pQ10 <- (pR2/pR1)^(10/15)

cly15 <- subset(clypeatus, clypeatus$temperature == "15")
cly30 <- subset(clypeatus, clypeatus$temperature == "30")
cR1 <- mean(cly15$VCO2_raw)
cR2 <- mean(cly30$VCO2_raw)
cQ10 <- (cR2/cR1)^(10/15)

hal15 <- subset(hallani, hallani$temperature == "15")
hal30 <- subset(hallani, hallani$temperature == "30")
hR1 <- mean(hal15$VCO2_raw)
hR2 <- mean(hal30$VCO2_raw)
hQ10 <- (hR2/hR1)^(10/15)

con15 <- subset(conjunctus, conjunctus$temperature == "15")
con30 <- subset(conjunctus, conjunctus$temperature == "30")
conR1 <- mean(con15$VCO2_raw)
conR2 <- mean(con30$VCO2_raw)
conQ10 <- (conR2/conR1)^(10/15)

vir15 <- subset(virgulatus, virgulatus$temperature == "15")
vir30 <- subset(virgulatus, virgulatus$temperature == "30")
vR1 <- mean(vir15$VCO2_raw)
vR2 <- mean(vir30$VCO2_raw)
vQ10 <- (vR2/vR1)^(10/15)

Q10s <- c(vQ10, conQ10, hQ10, cQ10, pQ10, gQ10)

spnames <- c("virgulatus", "conjunctus", "hallani", "clypeatus", "pugillis", "geronimoi")
spcols <- c("green", "pink", "purple", "red", "blue", "orange")

par(mfrow=c(1,1))
plot(Q10s, axes = FALSE, ylim = c(1.5,2.5), ylab = "Q10 Value", xlab = "", pch =21, bg =spcols, cex =2)
axis(side=1,at=c(1, 2, 3, 4, 5, 6),labels=spnames)
axis(side =2,at=c(1.5, 2.0, 2.5))
box()

