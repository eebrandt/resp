setwd("~/projects/dissertation/uncategorized/physiology/data")
CO2in <- read.csv("Stop Flow Preliminary - data.csv")
CO2in$dayssincefed <- as.Date(CO2in$date.fed) - as.Date(CO2in$date)

clypeatus <- subset(CO2in, CO2in$species == "H. clypeatus")

cly10 <- subset(clypeatus, clypeatus$temp == "10")
cly15 <- subset(clypeatus, clypeatus$temp == "15")
cly20 <- subset(clypeatus, clypeatus$temp == "20")
cly25 <- subset(clypeatus, clypeatus$temp == "25")
cly30 <- subset(clypeatus, clypeatus$temp == "30")
cly35 <- subset(clypeatus, clypeatus$temp == "35")
cly40 <- subset(clypeatus, clypeatus$temp == "40")

SD10 <- sd(cly10$Corrected..mL.min.mg.)


virgulatus <- subset(CO2in, CO2in$species == "H. virgulatus")

clyfem <- subset(clypeatus, clypeatus$sex == "female")
clymale <- subset(clypeatus, clypeatus$sex == "male")
plot(clyfem$Corrected..mL.min.~clyfem$Weight..mg., col = "blue", pch = 16)
lines(clymale$Corrected..mL.min.~clymale$Weight..mg., col = "red", type = "p", pch = 16)

all40 <- subset(clypeatus, clypeatus$temp == "40")
line40 <- lm(all40$Weight..mg.~all40$Corrected..mL.min.)
summary(line40)

fem40 = subset(clyfem, clyfem$temp == "40")
male40 = subset(clymale, clymale$temp == "40")

plot(male40$Corrected..mL.min.~male40$Weight..mg., col = "red", type = "p", pch = 16, ylim = c(0, 1.5E-5), xlim = c(4, 20), main = "40 degrees only",
     ylab = "CO2 production (mL 02/min)", xlab = "Weight (mg)")
lines(fem40$Corrected..mL.min.~fem40$Weight..mg., col = "blue", pch = 16, type = "p")
abline(lm(all40$Corrected..mL.min. ~ all40$Weight..mg.))

plot(male40$Corrected..mL.min.mg.~male40$Weight..mg., col = "red", type = "p", pch = 16, ylim = c(0, 1.5E-6), xlim = c(4, 20), main = "40 degrees only (divided by weight)",
     ylab = "CO2 production (mL 02/min)", xlab = "Weight (mg)")
lines(fem40$Corrected..mL.min.mg.~fem40$Weight..mg., col = "blue", pch = 16, type = "p")
abline(lm(all40$Corrected..mL.min.mg. ~ all40$Weight..mg.))

AS16033 <- subset(CO2in, CO2in$individual == "AS16033")
AS16033 <- AS16033[order(AS16033$temp),] 

MA16022 <- subset(CO2in, CO2in$individual == "MA16022")
MA16022 <- MA16022[order(MA16022$temp),] 

JP16095 <- subset(CO2in, CO2in$individual == "JP16095")
JP16095 <- JP16095[order(JP16095$temp),] 

JP16012 <- subset(CO2in, CO2in$individual == "JP16012")
JP16012 <- JP16012[order(JP16012$temp),] 

MA16108 <- subset(CO2in, CO2in$individual == "MA16108")
MA16108 <- AS16033[order(MA16108$temp),] 

EB16141 <- subset(CO2in, CO2in$individual == "EB16141")
EB16141 <- EB16141[order(EB16141$temp),] 

MA16119 <- subset(CO2in, CO2in$individual == "MA16119")
MA16119 <- MA16119[order(MA16119$temp),] 

EB16202 <- subset(CO2in, CO2in$individual == "EB16202")
EB16202 <- EB16202[order(EB16202$temp),] 

CR16008 <- subset(CO2in, CO2in$individual == "CR16008")
CR16008 <- CR16008[order(CR16008$temp),] 

MA16108 <- subset(CO2in, CO2in$individual == "MA16108")
MA16108 <- MA16108[order(MA16108$temp),] 

AS16034 <- subset(CO2in, CO2in$individual == "AS16034")
AS16034 <- AS16034[order(AS16034$temp),] 

MA16086 <- subset(CO2in, CO2in$individual == "MA16086")
MA16086 <- MA16086[order(MA16086$temp),] 

CR16056 <- subset(CO2in, CO2in$individual == "CR16056")
CR16056 <- CR16056[order(CR16056$temp),] 

MA16079 <- subset(CO2in, CO2in$individual == "MA16079")
MA16079 <- MA16079[order(MA16079$temp),] 

EB16083 <- subset(CO2in, CO2in$individual == "EB16083")
EB16083 <- EB16083[order(EB16083$temp),] 

EB16054 <- subset(CO2in, CO2in$individual == "EB16054")
EB16054 <- EB16083[order(EB16054$temp),] 

AS16012 <- subset(CO2in, CO2in$individual == "AS16012")
AS16012 <- AS16012[order(AS16012$temp),] 



par(mfrow=c(1,1))
boxplot(CO2in$Corrected..mL.min.mg.~CO2in$temp, xlab = "temperature  (degrees C)", ylab = "mL CO2/min/mg", main = "overall")


par(mfrow=c(1,3))
boxplot(clymale$Corrected..mL.min.mg.~clymale$temp, xlab = "temperature  (degrees C)", ylab = "mL CO2/min/mg", col = "blue", main = "female")
boxplot(clyfem$Corrected..mL.min.mg.~clyfem$temp, xlab = "temperature  (degrees C)", ylab = "mL CO2/min/mg", col = "red", main = "male")
boxplot(virgulatus$Corrected..mL.min.mg.~virgulatus$temp, xlab = "temperature  (degrees C)", ylab = "mL CO2/min/mg", col = "purple", main = "male virgulatus")
title("Breakdown by Sex and Species", line = -1, outer = TRUE)

par(mfrow=c(2,4))

plot(MA16108$Corrected..mL.min.mg.~MA16108$temp, type = "o", main = "MA16108", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "blue")
plot(MA16022$Corrected..mL.min.mg.~MA16022$temp, type = "o", main = "MA16022", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "blue")
plot(CR16008$Corrected..mL.min.mg.~CR16008$temp, type = "o", main = "CR16008", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "blue")
plot(MA16119$Corrected..mL.min.mg.~MA16119$temp, type = "o", main = "MA16119", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "blue")
plot(JP16012$Corrected..mL.min.mg.~JP16012$temp, type = "o", main = "JP16012", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "blue")
plot(EB16083$Corrected..mL.min.mg.~EB16083$temp, type = "o", main = "EB16083", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "blue")
plot(MA16079$Corrected..mL.min.mg.~MA16079$temp, type = "o", main = "MA16079", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "blue")
plot(AS16012$Corrected..mL.min.mg.~AS16012$temp, type = "o", main = "AS16012", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "blue")
title("Females", line = -1, outer = TRUE)

plot(JP16095$Corrected..mL.min.mg.~JP16095$temp, type = "o", main = "JP16095", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "red")
plot(EB16141$Corrected..mL.min.mg.~EB16141$temp, type = "o", main = "EB16141", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "red")
plot(EB16202$Corrected..mL.min.mg.~EB16202$temp, type = "o", main = "EB16202", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "red")
plot(EB16054$Corrected..mL.min.mg.~EB16054$temp, type = "o", main = "EB16054", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "red")
plot(CR16056$Corrected..mL.min.mg.~CR16056$temp, type = "o", main = "CR16056", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "red")
plot(MA16086$Corrected..mL.min.mg.~MA16086$temp, type = "o", main = "MA16086", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "red")
plot(AS16033$Corrected..mL.min.mg.~AS16033$temp, type = "o", main = "AS16033", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "purple")
plot(AS16034$Corrected..mL.min.mg.~AS16034$temp, type = "o", main = "AS16034", ylab = "mL CO2/min/mg", xlab = "temperature (C)", xlim = c(10, 40), ylim = c(0, 1.5e-6), col = "purple")
title("Males", line = -1, outer = TRUE)




