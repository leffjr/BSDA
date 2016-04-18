# test changes and input data sets
## all variables lowercase, all data sets start with capital letter and then lowercase rest of when read into R

# Script data rework Start 3/24/16
Abbey <- read.csv('abbey.csv')
devtools::use_data(Abbey, overwrite = TRUE) # puts it into RDA, need to do this for all
#
Abilene <- read.csv('Abilene.csv', colClasses = c("factor", "factor", "numeric"))
devtools::use_data(Abilene, overwrite = TRUE)
#
Abc <- read.csv('Abc.csv')
devtools::use_data(Abc, overwrite = TRUE)
# Create Ability
mat <- matrix(data = c(56, 35, 61, 43, 54, 61, 21, 42, 8, 19), nrow = 2)
dimnames(mat) <- list(gender = c("girls", "boys"), 
                      ability = c("hopeless", "belowavg", "average", 
                                  "aboveavg", "superior"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Ability <- vcdExtra::expand.dft(matDF)
Ability$ability <- factor(Ability$ability, 
                          levels = c("hopeless", "belowavg", "average", 
                                     "aboveavg", "superior"))
Ability$gender <- factor(Ability$gender, levels = c("girls", "boys"))
devtools::use_data(Ability, overwrite = TRUE)
#
Abortion <- read.csv('Abortion.csv')
Abortion$rate <- ifelse(Abortion$lowhigh == 1, "Low", "High")
devtools::use_data(Abortion, overwrite = TRUE)
#
Absent <- read.csv('Absent.csv')
devtools::use_data(Absent, overwrite = TRUE)
#
Achieve <- read.csv('Achieve.csv')
Achieve$gender <- as.factor(ifelse(Achieve$gender == 1, "girls", "boys"))
devtools::use_data(Achieve, overwrite = TRUE)
#
Adsales <- read.csv('Adsales.csv', stringsAsFactors = FALSE)
devtools::use_data(Adsales, overwrite = TRUE)
#
Aggress <- read.csv('Aggress.csv')
devtools::use_data(Aggress, overwrite = TRUE)
#
Aid <- read.csv("Aid.csv")
devtools::use_data(Aid, overwrite = TRUE)
#
Aids <- read.csv("Aids.csv")
devtools::use_data(Aids, overwrite = TRUE)
#
Airdisasters <- read.csv("Airdisasters.csv")
# Fix data set
Airdisasters$decade[Airdisasters$year >= 1930 & Airdisasters$year < 1940] <- "1930s"
Airdisasters$decade[Airdisasters$year >= 1940 & Airdisasters$year < 1950] <- "1940s"
Airdisasters$decade[Airdisasters$year >= 1950 & Airdisasters$year < 1960] <- "1950s"
Airdisasters$decade[Airdisasters$year >= 1960 & Airdisasters$year < 1970] <- "1960s"
Airdisasters$decade[Airdisasters$year >= 1970 & Airdisasters$year < 1980] <- "1970s"
Airdisasters$decade[Airdisasters$year >= 1980 & Airdisasters$year < 1990] <- "1980s"
Airdisasters$decade[Airdisasters$year >= 1990 & Airdisasters$year < 2000] <- "1990s"
devtools::use_data(Airdisasters, overwrite = TRUE)
#
Airline <- read.csv("Airline.csv", stringsAsFactors = FALSE)
devtools::use_data(Airline, overwrite = TRUE)
#
Alcohol <- read.csv("Alcohol.csv")
devtools::use_data(Alcohol, overwrite = TRUE)
# Create Allergy
mat <- matrix(data = c(97, 65, 27, 77, 49, 14, 12, 43, 22), nrow = 3)
dimnames(mat) <- list(event = c("insomnia", "headache", "drowsiness"), 
                      medication = c("seldane-d", "pseudoephedrine", "placebo"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Allergy <- vcdExtra::expand.dft(matDF)
Allergy$event <- factor(Allergy$event, 
                          levels = c("insomnia", "headache", "drowsiness"))
Allergy$medication <- factor(Allergy$medication, 
                        levels = c("seldane-d", "pseudoephedrine", "placebo"))
devtools::use_data(Allergy, overwrite = TRUE)
#
# Anesthet
Anesthet <- read.csv("Anesthet.csv")
devtools::use_data(Anesthet, overwrite = TRUE)
#
# Apolip
Apolipop <- read.csv("Apolipop.csv")
devtools::use_data(Apolipop, overwrite = TRUE)
#
# Append
Append <- read.csv("Append.csv")
devtools::use_data(Append, overwrite = TRUE)
#
# Appendec
Appendec <- read.csv("Appendec.csv")
devtools::use_data(Appendec, overwrite = TRUE)
#
# Aptitude
Aptitude <- read.csv("Aptitude.csv")
devtools::use_data(Aptitude, overwrite = TRUE)
#
# Archaeo
Archaeo <- read.csv("Archaeo.csv")
devtools::use_data(Archaeo, overwrite = TRUE)
#
# Arthriti
Arthriti <- read.csv("Arthriti.csv")
str(Arthriti)
Arthriti$treatment <- as.factor(ifelse(Arthriti$treatment == 1, "A", 
                                       ifelse(Arthriti$treatment == 2, "B", "C")))
devtools::use_data(Arthriti, overwrite = TRUE)
#
# Artifici
Artifici <- read.csv("Artifici.csv")
devtools::use_data(Artifici, overwrite = TRUE)
#
#
Asprin <- read.csv("Aspirin.csv")
Asprin$impurity <- factor(Asprin$impurity, 
                        levels = c("1%", "5%", "10%"))
str(Asprin)
devtools::use_data(Asprin, overwrite = TRUE)
#
# Asthmati
Asthmati <- read.csv("Asthmati.csv")
devtools::use_data(Asthmati, overwrite = TRUE)
#
# Attorney
Attorney <- read.csv("Attorney.csv")
devtools::use_data(Attorney, overwrite = TRUE)
#
# Autogear
Autogear <- read.csv("Autogear.csv")
devtools::use_data(Autogear, overwrite = TRUE)
#
# Backtoback
Backtoback <- read.csv("Backtoback.csv")
devtools::use_data(Backtoback, overwrite = TRUE)
#
# Bbsalaries
Bbsalaries <- read.csv("Bbsalaries.csv")
devtools::use_data(Bbsalaries, overwrite = TRUE)
#
# Bigten
Bigten <- read.csv("Bigten.csv")
devtools::use_data(Bigten, overwrite = TRUE)
#
boxplot(rate ~ status, data = subset(Bigten, year = "1993-1994"), 
        horizontal = TRUE, main = "Graduation Rates 1993-1994")
with(data = Bigten,
tapply(rate, list(year, status), mean)
)
#
# Biology
Biology <- read.csv("Biology.csv")
devtools::use_data(Biology, overwrite = TRUE)
#
# Birth
Birth <- read.csv("Birth.csv", colClasses = c("character", "numeric", "factor"))
devtools::use_data(Birth, overwrite = TRUE)
#
rate1998 <- subset(Birth, year == "1998", select = rate, drop = TRUE)
stem(x = rate1998, scale = 2)
hist(rate1998, breaks = seq(10.9, 21.9, 1.0), xlab = "1998 Birth Rate",
     main = "Figure 1.14 in BSDA", col = "pink")
hist(rate1998, breaks = seq(10.9, 21.9, 1.0), xlab = "1998 Birth Rate",
     main = "Figure 1.16 in BSDA", col = "pink", freq = FALSE)
lines(density(rate1998), lwd = 3)
#
# Blackedu
# Create Blackedu
# Create Allergy
mat <- matrix(data = c(486, 496, 659, 530, 691, 435, 208, 134, 96, 65), nrow = 2)
dimnames(mat) <- list(gender = c("Female", "Male"), 
                      education = c("High school dropout", "High school graduate", 
                                    "Some college", "Bachelor's degree", "Graduate degree"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Blackedu <- vcdExtra::expand.dft(matDF)
Blackedu$gender <- factor(Blackedu$gender, 
                        levels = c("Female", "Male"))
Blackedu$education <- factor(Blackedu$education, 
                             levels = c("High school dropout", "High school graduate", 
                                        "Some college", "Bachelor's degree", "Graduate degree"))
# Check
# xtabs(~gender + education, data = Blackedu)  # OK now
devtools::use_data(Blackedu, overwrite = TRUE)
#
# Blood
Blood <- read.csv("Blood.csv")
devtools::use_data(Blood, overwrite = TRUE)
# checks
DIFF <- Blood$machine - Blood$expert
qqnorm(DIFF)
qqline(DIFF)
rm(DIFF)
t.test(Blood$machine, Blood$expert, paired = TRUE)
#
# Board
Board <- read.csv("Board.csv")
devtools::use_data(Board, overwrite = TRUE)
# Checks
boxplot(salary ~ university, data = Board, col = c("red", "blue", "green"),
        ylab = "Income")
tapply(Board$salary, Board$university, summary)
anova(lm(salary ~ university, data = Board))
#
# Bones
Bones <- read.csv("Bones.csv")
devtools::use_data(Bones, overwrite = TRUE)
#
# Books
Books <- read.csv("Books.csv")
devtools::use_data(Books, overwrite = TRUE)
# Examples
plot(spelling ~ book, data = Books)
mod <- lm(spelling ~ book, data = Books)
summary(mod)
abline(mod, col = "blue", lwd = 2)
#
# Bookstor
Bookstor <- read.csv("Bookstor.csv")
devtools::use_data(Bookstor, overwrite = TRUE)
# Checks
boxplot(dollars ~ store, data = Bookstor, 
        col = c("purple", "lightblue", "cyan"))
kruskal.test(dollars ~ store, data = Bookstor)
#
# Brain
Brain <- read.csv("Brain.csv")
devtools::use_data(Brain, overwrite = TRUE)
# Checks
plot(log(brainweight) ~ log(bodyweight), data = Brain,
     pch = 19, col = "blue", main = "Example 2.3")
mod <- lm(log(brainweight) ~ log(bodyweight), data = Brain)
abline(mod, lty = "dashed", col = "blue")
#
# Bumpers
Bumpers <- read.csv("Bumpers.csv")
devtools::use_data(Bumpers, overwrite = TRUE)
# Checks
str(Bumpers)
#
# Bus
# Create Bus
mat <- matrix(data = c(454, 5806, 208, 2112, 491, 3989, 160, 3790, 1599, 10754), nrow = 2)
dimnames(mat) <- list(attendance = c("absent", "present"), 
                      shift = c("am", "noon", "pm", "swing", "split"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Bus <- vcdExtra::expand.dft(matDF)
Bus$attendance <- factor(Bus$attendance, 
                          levels = c("absent", "present"))
Bus$shift <- factor(Bus$shift, levels = c("am", "noon", "pm", "swing", "split"))
# Checks
xtabs(~attendance + shift, data = Bus)
devtools::use_data(Bus, overwrite = TRUE)
#
# Bypass
Bypass <- read.csv("Bypass.csv")
devtools::use_data(Bypass, overwrite = TRUE)
#
# Cabinets
Cabinets <- read.csv("Cabinets.csv")
devtools::use_data(Cabinets, overwrite = TRUE)
# Examples
DIF <- Cabinets$SupplA - Cabinets$SupplB
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
with(data = Cabinets,
t.test(SupplA, SupplB, paired = TRUE)
)
with(data = Cabinets,
wilcox.test(SupplA, SupplB, paired = TRUE)
)
rm(DIF)
#
# Cancer
Cancer <- read.csv("Cancer.csv")
devtools::use_data(Cancer, overwrite = TRUE)
# Examples
str(Cancer)
boxplot(survival ~ type, Cancer)
stomach <- Cancer$survival[Cancer$type == "stomach"]
bronchus <- Cancer$survival[Cancer$type == "bronchus"]
boxplot(stomach, ylab = "Days")
SIGN.test(stomach, md = 100, alternative = "greater")
SIGN.test(bronchus, md = 100, alternative = "greater")
rm(bronchus, stomach)
#
# Carbon
Carbon <- read.csv("Carbon.csv")
devtools::use_data(Carbon, overwrite = TRUE)
# Examples
str(Carbon)
boxplot(CO ~ site, data = Carbon)
kruskal.test(CO ~ site, data = Carbon)
#
# Cat
read.csv("Cat.csv")
devtools::use_data(Cat, overwrite = TRUE)
stem(Cat$score)
fivenum(Cat$score)
boxplot(Cat$score, main = "Problem 1.116", col = "green")
#
# Censored
Censored <- read.csv("Censored.csv")
devtools::use_data(Censored, overwrite = TRUE)
# Examples
str(Censored)
boxplot(survival ~ treatment, data = Censored)
wilcox.test(survival ~ treatment, data = Censored, alternative = "greater")
#
# Challeng
Challeng <- read.csv("Challeng.csv", colClasses = c("character", "character", "numeric", "numeric"))
Challeng$date <- as.Date(Challeng$date, "%m/%d/%y")
devtools::use_data(Challeng, overwrite = TRUE)
# Examples
str(Challeng)
stem(Challeng$temp)
summary(Challeng$temp)
IQR(Challeng$temp)
#
# Chemist
read.csv("Chemist.csv")
devtools::use_data(Chemist, overwrite = TRUE)
#
# Chesapea
read.csv("Chesapea.csv")
devtools::use_data(Chesapea, overwrite = TRUE)
#
# Create Chevy
#
mat <- matrix(data = c(16, 12, 5, 2, 5, 12, 3, 2, 4, 6), nrow = 2)
dimnames(mat) <- list(year = c("1988-90", "1991-93"), 
                      frequency = c("much better than average", "above average", 
                                    "average", "below average", "much worse than average"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Chevy <- vcdExtra::expand.dft(matDF)
Chevy$year <- factor(Chevy$year, 
                         levels = c("1988-90", "1991-93"))
Chevy$frequency <- factor(Chevy$frequency, levels = c("much better than average", "above average", 
                                                     "average", "below average", "much worse than average"))
# Checks
xtabs(~year + frequency, data = Chevy)
devtools::use_data(Chevy, overwrite = TRUE)
#
# Chicken
Chicken <- read.csv("Chicken.csv")
devtools::use_data(Chicken, overwrite = TRUE)
#
# Examples
str(Chicken)
#
# Chipavg
Chipavg <- read.csv("Chipavg.csv")
devtools::use_data(Chipavg, overwrite = TRUE)
#
# Chips
Chips <- read.csv("Chips.csv")
devtools::use_data(Chips, overwrite = TRUE)
#
# Cigar
Cigar <- read.csv("Cigar.csv")
devtools::use_data(Cigar, overwrite = TRUE)
# Examples
#
# Cigarett
Cigarett <- read.csv("Cigarett.csv")
devtools::use_data(Cigarett, overwrite = TRUE)
# 
#
# Citrus
Citrus <- read.csv("Citrus.csv")
devtools::use_data(Citrus, overwrite = TRUE)
#
# Clean
Clean <- read.csv("Clean.csv")
devtools::use_data(Clean, overwrite = TRUE)
# Examples
<<<<<<< HEAD
#
# Darwin
Darwin <- read.csv("Darwin.csv")
devtools::use_data(Darwin, overwrite = TRUE)
=======
str(Clean)
#
# Coaxial
Coaxial <- read.csv("Coaxial.csv")
devtools::use_data(Coaxial, overwrite = TRUE)
# Examples
boxplot(signal ~ cable, data = Coaxial, col = c("red", "green", "yellow"))
kruskal.test(signal ~ cable, data = Coaxial)
#
# Coffee
Coffee <- read.csv("Coffee.csv")
devtools::use_data(Coffee, overwrite = TRUE)
# Examples
differences <- Coffee$with - Coffee$without
qqnorm(differences)
qqline(differences)
shapiro.test(differences)
t.test(Coffee$with, Coffee$without, paired = TRUE, alternative = "greater")
wilcox.test(Coffee$with, Coffee$without, paired = TRUE, alterantive = "greater")
rm(differences)
#
# Coins
Coins <- read.csv("Coins.csv")
devtools::use_data(Coins, overwrite = TRUE)
#
# Commute
Commute <- read.csv("Commute.csv")
devtools::use_data(Commute, overwrite = TRUE)
# Examples
commute <- stack(Commute)
str(commute)
stripplot(ind ~ values, data = commute, jitter = TRUE)
dotplot(ind ~ values, data = commute)
bwplot(ind ~ values, data = commute)
>>>>>>> b00a2bc75db4b07fe2dfe69b95b3e11380c2594b

stripchart(values ~ ind, data = commute, method = "stack", pch = 1, cex = 2, 
           col = c("red", "blue"), group.names = c("1980", "1990"), main = "",
           xlab = "minutes")
title(main = "Commute Time")
boxplot(values ~ ind, data = commute, names=c("1980", "1990"), 
        horizontal = TRUE, las = 1)
rm(commute)
#
# Concept
Concept <- read.csv("Concept.csv")
devtools::use_data(Concept, overwrite = TRUE)
#
str(Concept)
#
# Concrete
Concrete <- read.csv("Concrete.csv")
devtools::use_data(Concrete, overwrite = TRUE)
#
str(Concrete)
#
# Corn
Corn <- read.csv("Corn.csv")
devtools::use_data(Corn, overwrite = TRUE)
#
str(Corn)
#
# Correlat
Correlat <- read.csv("Correlat.csv")
devtools::use_data(Correlat, overwrite = TRUE)
#
#
Counsel <- read.csv("Counsel.csv")
devtools::use_data(Counsel, overwrite = TRUE)
#
str(Counsel)
#
# Cpi
Cpi <- read.csv("Cpi.csv")
devtools::use_data(Cpi, overwrite = TRUE)
#
# Crime
Crime <- read.csv("Crime.csv", colClasses = c("factor", "factor", "numeric"))
devtools::use_data(Crime, overwrite = TRUE)
# 
# Examples
str(Crime)
boxplot(rate ~ year, data = Crime)
#
# Darwin
Darwin <- read.csv("Darwin.csv")
devtools::use_data(Darwin, overwrite = TRUE)



Earthqk <- read.csv("~/BSDA/data-raw/EARTHQK.csv")
devtools::use_data(Earthqk, overwrite = TRUE)
EARTHQK
str(Earthqk)
Earthqk$severity
EDA(Earthqk$severity)
t.test(Earthqk$severity,mu=100,alternative="greater")


EDA
EDA(rnorm(100))
# Produces four graphs for the 100 randomly
# generated standard normal variates.

Educat <- read.csv("~/BSDA/data-raw/EDUCAT.csv")
devtools::use_data(Educat, overwrite = TRUE)
Educat
str(Educat)
plot(Educat$nodegree,Educat$crime,xlab="No Crime",ylab="Violent Crime Rate per 100,000")

Eggs <- read.csv("~/BSDA/data-raw/EGGS.csv")
devtools::use_data(Eggs, overwrite = TRUE)
Eggs
str(Eggs)
plot(Eggs$feed,Eggs$eggs)
model <- lm(Eggs$eggs~Eggs$feed)
abline(model)
summary(model)
remove(model)

Elderly <- read.csv("~/BSDA/data-raw/ELDERLY.csv")
devtools::use_data(Elderly, overwrite = TRUE)
Elderly
str(Elderly)
stripchart(x=list(Elderly$X98percent,Elderly$X85percent),method="stack",pch=19,
           col=c("red","blue"),group.names=c("1998","1985"))
cor(Elderly$X98percent,Elderly$X85percent)

Energy <- read.csv("~/BSDA/data-raw/ENERGY.csv")
devtools::use_data(Energy, overwrite = TRUE)
Energy
str(Energy)
plot(Energy$Size,Energy$kilowatt)
cor(Energy$Size,Energy$kilowatt)
model <- lm(Energy$kilowatt~Energy$Size)
plot(Energy$Size,resid(model))

Engineer <- read.csv("~/BSDA/data-raw/ENGINEER.csv")
devtools::use_data(Engineer, overwrite = TRUE)
Engineer
str(Engineer)
boxplot(Engineer$salary~Engineer$university) #INVALID TYPE (NULL)
kruskal.test(Engineer$salary~as.factor(Engineer$university))

Entrance <- read.csv("~/BSDA/data-raw/ENTRANCE.csv")
devtools::use_data(Entrance, overwrite = TRUE)
Entrance
str(Entrance)
stem(Entrance$score)

Epaminicompact <- read.csv("~/BSDA/data-raw/EPAMINICOMPACT.csv")
devtools::use_data(Epaminicompact, overwrite = TRUE)
Epaminicompact
str(Epaminicompact)
summary(Epaminicompact$cty)

Epatwoseater<- read.csv("~/BSDA/data-raw/EPATWOSEATER.csv")
devtools::use_data(Epatwoseater, overwrite = TRUE)
Epatwoseater
str(Epatwoseater)
boxplot(Epatwoseater$cty)

Executiv <- read.csv("~/BSDA/data-raw/EXECUTIV.csv")
devtools::use_data(Executiv, overwrite = TRUE)
Executiv
str(Executiv)
EDA(Executiv$Age)

Exercise <- read.csv("~/BSDA/data-raw/EXERCISE.csv")
devtools::use_data(Exercise, overwrite = TRUE)
Exercise
str(Exercise)
stem(Exercise$loss)

#cannot computer exact p-value wth ties error for wilcox.test
Fabric <- read.csv("~/BSDA/data-raw/FABRIC.csv")
devtools::use_data(Fabric, overwrite = TRUE)
str(Fabric)
DIF <- Fabric$With - Fabric$Without
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
wilcox.test(Fabric$With,Fabric$Without,paired=TRUE,alternative="greater")

Faithful <- read.csv("~/BSDA/data-raw/FAITHFUL.csv")
devtools::use_data(Faithful, overwrite = TRUE)
Faithful
str(Faithful)
hist(Faithful$Time,prob=TRUE,xlab="Waiting time between eruptions",col="tomato")
lines(density(Faithful$Time),col="red",lwd=3)
t.test(Faithful$Time)$conf

Family <- read.csv("~/BSDA/data-raw/FAMILY.csv")
devtools::use_data(Family, overwrite = TRUE)
Family
str(Family)
plot(Family$Number,Family$Cost)
cor(Family$Number,Family$Cost)
lm(Family$Cost~Family$Number)

Ferraro1 <- read.csv("~/BSDA/data-raw/FERRARO1.csv")
devtools::use_data(Ferraro1, overwrite = TRUE)
Ferraro1
str(Ferraro1)
Ferraro1
chisq.test(Ferraro1[,2:4])

Ferraro2 <- read.csv("~/BSDA/data-raw/FERRARO2.csv")
devtools::use_data(Ferraro2, overwrite = TRUE)
Ferraro2
str(Ferraro2)
Ferraro2
chisq.test(Ferraro2[,2:4])

Fertility <- read.csv("~/BSDA/data-raw/FERTILITY.csv")
devtools::use_data(Fertility, overwrite = TRUE)
Fertility
str(Fertility)
library(lattice)
dotplot(Fertility$State~Fertility$rate)
stem(Fertility$rate)
fivenum(Fertility$rate)
EDA(Fertility$rate)

Firstchi <- read.csv("~/BSDA/data-raw/FIRSTCHI.csv")
devtools::use_data(Firstchi, overwrite = TRUE)
Firstchi #ADD EXAMPLES


Fish <- read.csv("~/BSDA/data-raw/FISH.csv")
devtools::use_data(Fish, overwrite = TRUE)
Fish
str(Fish)
median(Fish$smallmesh,na.rm=TRUE) #is.na applied to null error
median(Fish$largemesh)
IQR(Fish$smallmesh,na.rm=TRUE)
IQR(Fish$largemesh)
SIGN.test(Fish$smallmesh,conf.level=.99)
SIGN.test(Fish$largemesh,conf.level=.99)
t.test(Fish$smallmesh,Fish$largemesh)

Fitness <- read.csv("~/BSDA/data-raw/FITNESS.csv")
devtools::use_data(Fitness, overwrite = TRUE)
str(Fitness)
DIF <- Fitness$After - Fitness$Before
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
t.test(Fitness$After,Fitness$Before,paired=TRUE,alternative="greater")

Florida2000 <- read.csv("~/BSDA/data-raw/FLORIDA2000.csv")
devtools::use_data(Florida2000, overwrite = TRUE)
Florida2000
str(Florida2000)
plot(Florida2000$Total,Florida2000$BUCHANAN,xlab="Total votes cast (in thousands)",
     ylab="Votes for Buchanan")

Fluid <- read.csv("~/BSDA/data-raw/FLUID.csv")
devtools::use_data(Fluid, overwrite = TRUE)
Fluid
str(Fluid)
stem(Fluid$X34kV)
SIGN.test(Fluid$X34kV)

Food <- read.csv("~/BSDA/data-raw/FOOD.csv")
devtools::use_data(Food, overwrite = TRUE)
Food
str(Food)
EDA(Food$food)

Framingh <- read.csv("~/BSDA/data-raw/FRAMINGH.csv")
devtools::use_data(Framingh, overwrite = TRUE)
Framingh
str(Framingh)
stem(Framingh$cholest)
hist(Framingh$cholest,prob=TRUE,ylim=c(0,.012))
lines(density(Framingh$cholest))
boxplot(Framingh$cholest,col="brown")
sum(Framingh$cholest>200&Framingh$cholest<240)/length(Framingh$cholest)

Freshman <- read.csv("~/BSDA/data-raw/FRESHMAN.csv")
devtools::use_data(Freshman, overwrite = TRUE)
Freshman
str(Freshman)
SIGN.test(Freshman$age,md=19)

Funeral <- read.csv("~/BSDA/data-raw/FUNERAL.csv")
devtools::use_data(Funeral, overwrite = TRUE)
Funeral
str(Funeral)
Funeral
chisq.test(Funeral[,2:4])

Galaxie <- read.csv("~/BSDA/data-raw/GALAXIE.csv")
devtools::use_data(Galaxie, overwrite = TRUE)
Galaxie
str(Galaxie)
EDA(Galaxie$velocity)

Gallup <- read.csv("~/BSDA/data-raw/GALLUP.csv")
devtools::use_data(Gallup, overwrite = TRUE)
Gallup
INFO <- c(43,52,5,42,53,5,44,51,5,30,67,3,45,50,5,58,33,9,27,67,6,26,70,4,45,
          52,3,54,39,7,49,47,4,39,55,6)
INFOmat <- matrix(INFO,nrow=12,byrow=TRUE)
INFOmat
rownames(INFOmat) <- c("National","Gender: Male","Gender: Female",
                       "Education: College","Education: High School","Education: Grade School",
                       "Age: 18-24", "Age: 25-29", "Age: 30-49", "Age: 50-older", "Religion: Protestant",
                       "Religion: Catholic")
colnames(INFOmat) <- c("Criminal", "Not.Criminal", "No.Opinion")
INFOmat
barplot(t(INFOmat[2:3,]),beside=TRUE,legend=TRUE,names=c("Male","Female"),
        ylab="Percent of Population Opining")
barplot((INFOmat[2:3,]),beside=TRUE,legend=TRUE,ylab="Percent of Population Opining" )
remove(INFO,INFOmat)

Gasoline<- read.csv("~/BSDA/data-raw/GASOLINE.csv")
devtools::use_data(Gasoline, overwrite = TRUE)
Gasoline
str(Gasoline)
stem(Gasoline$price)

German <- read.csv("~/BSDA/data-raw/GERMAN.csv")
devtools::use_data(German, overwrite = TRUE)
German # same wilcox problem
str(German)
qqnorm(German$differ)
qqline(German$differ)
shapiro.test(German$differ)
wilcox.test(German$Before,German$After,paired=TRUE)

Golf <- read.csv("~/BSDA/data-raw/GOLF.csv")
devtools::use_data(Golf, overwrite = TRUE)
Golf
str(Golf)
stem(Golf$yards)
EDA(Golf$yards)

Governor <- read.csv("~/BSDA/data-raw/GOVERNOR.csv")
devtools::use_data(Governor, overwrite = TRUE)
Governor
str(Governor)
EDA(Governor$X1999salary)

Gpa <- read.csv("~/BSDA/data-raw/GPA.csv")
devtools::use_data(Gpa, overwrite = TRUE)
Gpa
str(Gpa)
plot(Gpa$HSGPA,Gpa$CollGPA)
model <- lm(Gpa$CollGPA~Gpa$HSGPA)
abline(model)
model
r <- resid(model)
yhat <- fitted(model)
Table2.1 <- cbind(Gpa$HSGPA,Gpa$CollGPA,yhat,r)
Table2.1
remove(r,yhat,model,Table2.1)

Grades <- read.csv("~/BSDA/data-raw/GRADES.csv")
devtools::use_data(Grades, overwrite = TRUE)
Grades
str(Grades)
EDA(Grades$grades)

Graduate <- read.csv("~/BSDA/data-raw/GRADUATE.csv")
devtools::use_data(Graduate, overwrite = TRUE)
Graduate
str(Graduate)
names(Graduate$Percent) <- Graduate$School
barplot(Graduate$Percent,las=2,cex.names=.65,col="tomato")

Greenriv <- read.csv("~/BSDA/data-raw/GREENRIV.csv")
devtools::use_data(Greenriv, overwrite = TRUE)
Greenriv
str(Greenriv)
EDA(Greenriv$thick)
SIGN.test(Greenriv$thick,md=7.3,alternative="greater")

Grnriv2 <- read.csv("~/BSDA/data-raw/GRNRIV2.csv")
devtools::use_data(Grnriv2, overwrite = TRUE)
Grnriv2
str(Grnriv2)
EDA(Grnriv2$thick)
t.test(Grnriv2$thick,mu=8,alternative="less")
SIGN.test(Grnriv2$thick,md=8,alternative="less")

Groupabc <- read.csv("~/BSDA/data-raw/GROUPABC.csv")
devtools::use_data(Groupabc, overwrite = TRUE)
Groupabc
str(Groupabc)
STACKED <-stack(Groupabc)
STACKED[1:5,]
boxplot(values~ind,col=c("red","blue","green"),data=STACKED)
anova(lm(values~ind,data=STACKED)) #error in contrasts
remove(STACKED)

Groups <- read.csv("~/BSDA/data-raw/GROUPS.csv")
devtools::use_data(Groups, overwrite = TRUE)
Groups
str(Groups)
STACKED <-stack(Groups)
STACKED[1:5,]
boxplot(values~ind,col=c("red","blue","green"),data=STACKED)
anova(lm(values~ind,data=STACKED)) #error in contrasts
remove(STACKED)

Gym <- read.csv("~/BSDA/data-raw/GYM.csv")
devtools::use_data(Gym, overwrite = TRUE)
Gym
str(Gym)
plot(Gym$age,Gym$number)
model <- lm(Gym$number~Gym$age)
abline(model)
cor(Gym$age,Gym$number)

Habits <- read.csv("~/BSDA/data-raw/HABITS.csv")
devtools::use_data(Habits, overwrite = TRUE)
Habits #same wilcox problem
str(Habits)
qqnorm(Habits$differ)
qqline(Habits$differ)
shapiro.test(Habits$differ)
t.test(B,A,paired=TRUE,alternative="less") #objects B and A 
wilcox.test(B,A,paired=TRUE,alternative="less")

Haptoglo <- read.csv("~/BSDA/data-raw/HAPTOGLO.csv") 
devtools::use_data(Haptoglo, overwrite = TRUE)
Haptoglo
str(Haptoglo)
qqnorm(Haptoglo$concent,col="blue")
qqline(Haptoglo$concent,col="red")
shapiro.test(Haptoglo$concent)
t.test(Haptoglo$concent,mu=2,alternative="less")

Hardware <- read.csv("~/BSDA/data-raw/HARDWARE.csv")
devtools::use_data(Hardware, overwrite = TRUE)
Hardware
data(Hardware)

Hardwood <- read.csv("~/BSDA/data-raw/hardwood.csv")
devtools::use_data(Hardwood, overwrite = TRUE)
Hardwood
str(Hardwood)

Heat <- read.csv("~/BSDA/data-raw/HEAT.csv")
devtools::use_data(Heat, overwrite = TRUE)
Heat
str(Heat)
MAT <- cbind(Heat$Reserv, Heat$All.US, Heat$Not.Rese)
row.names(MAT) <- c("Utility Gas","LP bottled Gas","Electricity",
                    "Fuel Oil","Wood","Other Fuel")

Heat <- read.csv("~/BSDA/data-raw/HEAT.csv")
devtools::use_data(Heat, overwrite = TRUE)
MAT
barplot(t(MAT),beside=TRUE,legend=TRUE,main="Heating of American Indian Homes")
sum(Heat$Reserv)
sum(Heat$All.US)
sum(Heat$Not.Rese)

Heating <- read.csv("~/BSDA/data-raw/HEATING.csv")
devtools::use_data(Heating, overwrite = TRUE)
Heating
str(Heating)
boxplot(Heating$Rating~Heating$Type)
kruskal.test(Heating$Rating~as.factor(Heating$Type))

Hodgkin <- read.csv("~/BSDA/data-raw/HODGKIN.csv")
devtools::use_data(Hodgkin, overwrite = TRUE)
Hodgkin
str(Hodgkin)
HOD <- as.matrix(Hodgkin[,2:4])
rownames(HOD) <- Hodgkin$Histological
HOD
barplot(t(HOD),legend=TRUE,beside=TRUE)
remove(HOD)

Homes <- read.csv("~/BSDA/data-raw/HOMES.csv")
devtools::use_data(Homes, overwrite = TRUE)
Homes
str(Homes)
EDA(Homes$X2000)
boxplot(Homes$X1994,Homes$X2000,names=c("1994","2000"),col=c("red","blue"),ylab="Cost")
boxplot(Homes$X2000~Homes$Region)

Homework <- read.csv("~/BSDA/data-raw/HOMEWORK.csv")
devtools::use_data(Homework, overwrite = TRUE)
Homework
str(Homework)
boxplot(Homework$Private,Homework$Public)
t.test(Homework$Private,Homework$Public,conf.level=.98)

Honda <- read.csv("~/BSDA/data-raw/HONDA.csv")
devtools::use_data(Honda, overwrite = TRUE)
Honda
str(Honda)
t.test(Honda$mileage,mu=40,alternative="less")

Hostile <- read.csv("~/BSDA/data-raw/HOSTILE.csv")
devtools::use_data(Hostile, overwrite = TRUE)
Hostile
str(Hostile)
boxplot(Hostile$HLT~Hostile$Type)
kruskal.test(Hostile$HLT~as.factor(Hostile$Type))

Housing <- read.csv("~/BSDA/data-raw/HOUSING.csv")
devtools::use_data(Housing, overwrite = TRUE)
Housing
str(Housing)
stem(Housing$X1993)
stem(Housing$X1984)
par(mfrow=c(2,2))
stripchart(x=list(Housing$X1984,Housing$X1993),method="stack",pch=1,cex=1.2,
           col=c("orange","pink"),group.names=c("1984","1993"))
title(main="Problem 5.82 \n We have not talked about this kind of graph before...")
hist(Housing$X1993,breaks="Scott",col="pink")
hist(Housing$X1984,breaks="Scott",col="orange")
plot(density(Housing$X1993),col="red",xlab="",ylab="",main="",ylim=c(0,.00003))
lines(density(Housing$X1984),col="orange")
par(mfrow=c(1,1))
boxplot(Housing$X1993,Housing$X1984,col=c("pink","orange"),names=c("1993","1984"),main="Problem 5.82")
SIGN.test(Housing$X1984,conf.level=.98)
SIGN.test(Housing$X1993,conf.level=.98)
# 98% CI -> 63591.1 79622.56 and 85591.69 109915.4
# Placing on a common number line...
my.axis <- function(side, at, labels,...)
{for(i in seq(along=at)) axis(side=side, at=at[i], labels=labels[i],...) }

plot(1,type="n",xlim=c(63000,110000),ylim=c(0,1),
     xlab="Median House Price",ylab="",yaxt="n",main="")
title(main="98 Percent Confidence Intervals")
my.axis(2,at=c(.25,.75),labels=c("1984","1993"), cex.axis=1.2 ,las=2)
lines( c(63591.1, 79622.56),c(.25,.25),col="orange",lwd=24)
lines( c(85591.69, 109915.4),c(.75,.75),col="pink",lwd=24)

Hurrican <- read.csv("~/BSDA/data-raw/HURRICAN.csv")
devtools::use_data(Hurrican, overwrite = TRUE)
Hurrican
str(Hurrican)
barplot(table(Hurrican$hurrican),col="blue",main="Problem 1.38",
        xlab="Number of Hurricanes",ylab="Number of Seasons")
boxplot(Hurrican$storms~Hurrican$ElNino)
anova(lm(Hurrican$storms~Hurrican$ElNino))

Iceberg <- read.csv("~/BSDA/data-raw/ICEBERG.csv")
devtools::use_data(Iceberg, overwrite = TRUE)
Iceberg
str(Iceberg)
plot(Iceberg$GrandBk,Iceberg$Newfound)
abline(lm(Iceberg$Newfound~Iceberg$GrandBk))

Income <- read.csv("~/BSDA/data-raw/INCOME.csv")
devtools::use_data(Income, overwrite = TRUE)
Income
str(Income)
CATS <-factor(cut(Income$income,breaks=c(0.5,1.0,1.5,2,max(Income$income)) ))
table(CATS)
table(CATS)/length(Income$income)
barplot(table(CATS),col="lightblue",main="Problem 1.33")
remove(CATS)

Independent <- read.csv("~/BSDA/data-raw/INDEPENDENT.csv")
devtools::use_data(Independent, overwrite = TRUE)
Independent #cannot compute p-value with ties error
str(Independent)
boxplot(Independent$score~Independent$group)
wilcox.test(Independent$score~Independent$group)

Indian <- read.csv("~/BSDA/data-raw/INDIAN.csv")
devtools::use_data(Indian, overwrite = TRUE)
Indian
str(Indian)
par(mfrow=c(1,2))
plot(Indian$highsch,Indian$income,xlab="Percent High School Graduates", ylab="Per capita income")
plot(Indian$highsch,Indian$poverty,xlab="Percent High School Graduates", ylab="Poverty rate")
par(mfrow=c(1,1))
cor(cbind(Indian$highsch,Indian$income,Indian$poverty))

Indiapol <- read.csv("~/BSDA/data-raw/INDIAPOL.csv")
devtools::use_data(Indiapol, overwrite = TRUE)
Indiapol
str(Indiapol)
plot(Indiapol$year,Indiapol$speed,type="l")

Indy500 <- read.csv("~/simplemathr/data-raw/BSDAexcelData/INDY500.csv")
devtools::use_data(Indy500, overwrite = TRUE)
Indy500
str(Indy500)
stripchart(Indy500$qualif~Indy500$group, method="stack",pch=19,col=c("red","blue"))
boxplot(Indy500$qualif~Indy500$group)
t.test(Indy500$qualif~Indy500$group)

Inflatio <- read.csv("~/BSDA/data-raw/INFLATIO.csv")
devtools::use_data(Inflatio, overwrite = TRUE)
Inflatio
str(Inflatio)
plot(Inflatio$inflation,Inflatio$increase)
cor(Inflatio$inflation,Inflatio$increase,use="complete.obs")

Inletoil <- read.csv("~/BSDA/data-raw/INLETOIL.csv")
devtools::use_data(Inletoil, overwrite = TRUE)
Inletoil
str(Inletoil)
t.test(Inletoil$temp)$conf
t.test(Inletoil$temp,mu=98,alternative="less")

Inmate <- read.csv("~/BSDA/data-raw/INMATE.csv")
devtools::use_data(Inmate, overwrite = TRUE)
Inmate
str(Inmate)
Inmate
chisq.test(Inmate[,2:5])

Inspect <- read.csv("~/BSDA/data-raw/INSPECT.csv")
devtools::use_data(Inspect, overwrite = TRUE)
Inspect
str(Inspect)
Inspect
chisq.test(Inspect[,2:4])

Insulate <- read.csv("~/BSDA/data-raw/INSULATE.csv")
devtools::use_data(Insulate, overwrite = TRUE)
Insulate
str(Insulate)
summary(lm(Insulate$loss~Insulate$temp))

Iqgpa <- read.csv("~/BSDA/data-raw/IQGPA.csv")
devtools::use_data(Iqgpa, overwrite = TRUE)
Iqgpa 
str(Iqgpa)
plot(Iqgpa$IQ,Iqgpa$GPA)
model <- lm(Iqgpa$GPA~Iqgpa$IQ) #error
abline(model)
summary(model)
remove(model)

Irises <- read.csv("~/BSDA/data-raw/IRISES.csv")
devtools::use_data(Irises, overwrite = TRUE)
Irises
str(Irises)
EDA(Irises$sepalL1)
t.test(Irises$sepalL1,conf.level=.99)$conf

Jdpower <- read.csv("~/BSDA/data-raw/JDPOWER.csv")
devtools::use_data(Jdpower, overwrite = TRUE)
Jdpower
str(Jdpower)
plot(Jdpower$X1994,Jdpower$X1995)
model <- lm(Jdpower$X1995~Jdpower$X1994)
abline(model)
model
cor(Jdpower$X1995,Jdpower$X1994)

Jobsat <- read.csv("~/BSDA/data-raw/JOBSAT.csv")
devtools::use_data(Jobsat, overwrite = TRUE)
Jobsat
str(Jobsat)
plot(Jobsat$WSPT,Jobsat$satisfac)
model <- lm(Jobsat$satisfac~Jobsat$WSPT)
abline(model)
summary(model)
remove(model)

Kidsmoke <- read.csv("~/BSDA/data-raw/KIDSMOKE.csv")
devtools::use_data(Kidsmoke, overwrite = TRUE)
Kidsmoke
str(Kidsmoke)
table(Kidsmoke$gender,Kidsmoke$smoke)
addmargins(table(Kidsmoke$gender,Kidsmoke$smoke))
addmargins(table(Kidsmoke$gender,Kidsmoke$smoke)/1000)

Kilowatt <- read.csv("~/BSDA/data-raw/KILOWATT.csv")
devtools::use_data(Kilowatt, overwrite = TRUE)
Kilowatt
str(Kilowatt)
EDA(Kilowatt$rate)

Kinder <- read.csv("~/BSDA/data-raw/KINDER.csv")
devtools::use_data(Kinder, overwrite = TRUE)
Kinder
str(Kinder)
DIF <- Kinder$Kinder - Kinder$NoKinder
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
t.test(Kinder$Kinder, Kinder$NoKinder,paired=TRUE,alternative="greater")
remove(DIF)

Laminect <- read.csv("~/BSDA/data-raw/LAMINECT.csv")
devtools::use_data(Laminect, overwrite = TRUE)
Laminect
str(Laminect)
boxplot(Laminect$cost~Laminect$class)
anova(lm(Laminect$cost~as.factor(Laminect$class)))

Lead <- read.csv("~/BSDA/data-raw/LEAD.csv")
devtools::use_data(Lead, overwrite = TRUE)
Lead
str(Lead)
boxplot(Lead$exposed,Lead$control, names=c("Exposed","Control"),col=c("red","blue"))

Leader <- read.csv("~/BSDA/data-raw/LEADER.csv")
devtools::use_data(Leader, overwrite = TRUE)
Leader
str(Leader)
boxplot(Leader$under35,Leader$over35,names=c("Under 35","Over 35"),col=c("green","brown"))
t.test(Leader$under35,Leader$over35)

Lethal <- read.csv("~/BSDA/data-raw/LETHAL.csv")
devtools::use_data(Lethal, overwrite = TRUE)
Lethal
str(Lethal)
SIGN.test(Lethal$survival,md=45,alternative="less")

Life <- read.csv("~/BSDA/data-raw/LIFE.csv")
devtools::use_data(Life, overwrite = TRUE)
Life
str(Life)
plot(Life$year,Life$Men,type="l",ylim=c(min(Life$Men,Life$Women),max(Life$Men,Life$Women)),col="blue",
     main="Life Expectancy versus Year",ylab="Age",xlab="Year")
lines(Life$year,Life$Women,col="red")
text(1955,65,"Men",col="blue")
text(1955,70,"Women",col="red")

Lifespan <- read.csv("~/BSDA/data-raw/LIFESPAN.csv")
devtools::use_data(Lifespan, overwrite = TRUE)
Lifespan
str(Lifespan)
plot(Lifespan$heat,Lifespan$life)
model <- lm(Lifespan$life~Lifespan$heat)
model
resid(model)
sum((resid(model))^2)
anova(model)
# plot(model)  # Used for diagnostic purposes

Ligntmonth <- read.csv("~/BSDA/data-raw/LIGNTMONTH.csv")
devtools::use_data(Ligntmonth, overwrite = TRUE)
Ligntmonth
str(Ligntmonth)
plot(Ligntmonth$damage,Ligntmonth$deaths)

Lodge <- read.csv("~/BSDA/data-raw/LODGE.csv")
devtools::use_data(Lodge, overwrite = TRUE)
Lodge
str(Lodge)
boxplot(Lodge$Traffic~Lodge$Site)
anova(lm(Lodge$Traffic~as.factor(Lodge$Site)))

Longtail <- read.csv("~/BSDA/data-raw/LONGTAIL.csv")
devtools::use_data(Longtail, overwrite = TRUE)
Longtail
str(Longtail)
boxplot(Longtail$score~Longtail$Group) #error invalid type (null)
kruskal.test(Longtail$score~as.factor(Longtail$Group))
anova(lm(Longtail$score~as.factor(Longtail$Group)))

Lowabil <- read.csv("~/BSDA/data-raw/LOWABIL.csv")
devtools::use_data(Lowabil, overwrite = TRUE)
Lowabil 
str(Lowabil)
DIF <- Lowabil$Experimt - Lowabil$Control
qqnorm(DIF) #error has no y 
qqline(DIF)
shapiro.test(DIF)
t.test(Lowabil$Experimt,Lowabil$Control,paired=TRUE)

