setwd("F:/Cultivation/Records/2020/Bud Width")

library(dplyr)
library(tidyr)
library(readxl)
library(qtl)
library(ggplot2)
library(psych)

## pulling the data in, cleaning it up

budwidth <- as.data.frame(read_xlsx("BudWidth.xlsx", sheet = "Master"))
colnames(budwidth) <- c("room", "gen", "pheno", "row", "table",
                        "sq.across", "sq.width", "budwidth", "date")
budwidth <- filter(budwidth, !is.na(budwidth))
budwidth$treatment <- ifelse((budwidth$row == "KU" | budwidth$row == "LU"), "upper", 
                             ifelse((budwidth$row == "EL" | budwidth$row == "GL"), 
                             "hard prune", "standard"))
budwidth$treatment <- as.factor(budwidth$treatment)

##taking a peek at what numbers are there

summary(budwidth)
describe(budwidth)

## by phenotype/treatment

OROBbudwidth <- filter(budwidth, pheno == 'OROB054')
summary(OROBbudwidth)
describe(OROBbudwidth)
SOURbudwidth <- filter(budwidth, pheno == "SOUR011")
summary(SOURbudwidth)
describe(SOURbudwidth)

standardOROB <- filter(OROBbudwidth, treatment == 'standard')
standardSOUR <- filter(SOURbudwidth, treatment == 'standard')
prunedOROB <- filter(OROBbudwidth, treatment == 'hard prune')
prunedSOUR <- filter(SOURbudwidth, treatment == 'hard prune')
upperSOUR <- filter(SOURbudwidth, treatment == 'upper')

summary(standardOROB)
describe(standardOROB)
summary(prunedOROB)
describe(prunedOROB)
summary(standardSOUR)
describe(standardSOUR)
summary(prunedSOUR)
describe(prunedSOUR)

# for C34 analysis

c34budwidth <- filter(budwidth, gen == "C34")

## ADDING LABELS OF COUNT TO GGPLOT

# Create an aggregate of median & count
cts <- merge(aggregate(budwidth ~ treatment + pheno, budwidth, length), 
               aggregate(budwidth ~ treatment + pheno, budwidth, median), 
               by=c("treatment", "pheno"))
# Rename the col names to fit with the original dataset..
names(cts) <- c("treatment", "pheno", "count", "budwidth")
cts$budwidth <- cts$budwidth + 0.7
  
## C33 boxplot

ggplot(budwidth, aes(x=pheno, y=budwidth, fill=treatment)) + 
  geom_boxplot() + scale_fill_manual(values=c("#999999", "#E69F00", "blue")) + 
  labs(x = "Phenotype", y = "Bud Width (mm)", title = "Bud Width Measurements C33") +
  geom_jitter(color="blue", size=0.7, alpha = 0.5) + ### get rid of jitter to make the dots go away
  geom_text(data = cts, aes(label=paste("n = ", count)), position = position_dodge2(width = 0.8))


##make the boxplot C34

ggplot(c34budwidth, aes(x=pheno, y=budwidth, fill=pheno)) + 
  geom_boxplot() + scale_fill_manual(values=c("#999999", "#E69F00")) + 
  labs(x = "Phenotype", y = "Bud Width (mm)", title = "Bud Width Measurements C34")

SOURbudwidth <- filter(budwidth, pheno == "SOUR011")
anov.SOURbudwidth <- aov(budwidth ~ treatment, data = SOURbudwidth, )
anov.SOURbudwidth
TukeyHSD(anov.SOURbudwidth)

OROBbudwidth <- filter(budwidth, pheno == 'OROB054')
t.test(budwidth ~ treatment, data = OROBbudwidth)
