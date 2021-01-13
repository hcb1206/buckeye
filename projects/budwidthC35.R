setwd("F:/Cultivation/Records/2020/Bud Width")

library(dplyr)
library(tidyr)
library(readxl)
library(qtl)
library(ggplot2)
library(psych)
library(effsize)

## pulling the data in, cleaning it up

budwidth <- as.data.frame(read_xlsx("BudWidth.xlsx", sheet = "Master"))
colnames(budwidth) <- c("room", "gen", "pheno", "row", "table",
                        "sq.across", "sq.width", "budwidth", "date")

budwidth <- filter(budwidth, !is.na(budwidth)) ##filter out NA values

budwidth <- filter(budwidth, gen == "C35") ##filter for generation

soda <- filter(budwidth, pheno == "SODA011") ##separating by pheno for treatments
orob <- filter(budwidth, pheno =="OROB054")

soda$treatment <- ifelse((soda$row == "NU"), "upper", 
                             ifelse((soda$row == "YL"), "coco coir", "standard"))

orob$treatment <- ifelse((orob$table == "DL1" | orob$table == "DL2"), "hard prune", "standard")

budwidth <- rbind(soda, orob)
  
budwidth$treatment <- as.factor(budwidth$treatment)

##taking a peek at what numbers are there

summary(budwidth)
describe(budwidth)

## determine the sampling distribution
## along with looking at the variance between treatments for each phenotype,
## this will let us know if we can use parametric analyses on this ordinal data

sodadat <- soda$budwidth ## for all sodas

samplemeans <- rep(NA, 5000)
for(i in 1:5000){
  samp <- sample(sodadat, 25)
  sodadat[1] <- mean(samp)
}

hist(sodadat)

orobdat <- orob$budwidth ## for all orobs

samplemeans <- rep(NA, 5000)
for(i in 1:5000){
  samp <- sample(orobdat, 25)
  orobdat[1] <- mean(samp)
}

hist(orobdat)


# are these histograms normal? hint: they are.

shapiro.test(subset(budwidth, pheno == "SODA011")$budwidth)
shapiro.test(subset(budwidth, pheno == "OROB054")$budwidth)

# here's a look at the variances

orobhardprune <- filter(orob, treatment == "hard prune")
var(orobhardprune$budwidth)
orobstandard <- filter(orob, treatment == "standard")
var(orobstandard$budwidth)
sodaupper <- filter(soda, treatment == "upper")
var(sodaupper$budwidth)
sodastandard <- filter(soda, treatment == "standard")
var(sodastandard$budwidth)
sodacoco <- filter(soda, treatment == "coco coir")
var(sodacoco$budwidth)

## even though both OROB and SODA have normal distributions, the variances are not very 
## similar across the populations, so I'd say that parametric analyses are off the table.

# for plot labels, create an aggregate of median & count
cts <- merge(aggregate(budwidth ~ treatment + pheno, budwidth, length), 
             aggregate(budwidth ~ treatment + pheno, budwidth, mean), 
             by=c("treatment", "pheno"))
# Rename the col names to fit with the original dataset..
names(cts) <- c("treatment", "pheno", "count", "budwidth")
cts$budwidth <- cts$budwidth + 0.7

## C35 boxplot

ggplot(budwidth, aes(x=pheno, y=budwidth, fill=treatment)) + 
  geom_boxplot() + scale_fill_manual(values=c("#999999", "#E69F00", "blue", "red")) + 
  labs(x = "Phenotype", y = "Bud Width (mm)", title = "Bud Width Measurements C35") +
  geom_text(data = cts, aes(label=paste("n = ", count, ",", round(budwidth, 1), "mm")), 
                            position = position_dodge2(width = 0.8))
 


## here's what we'll use as non-parametric statistics: Mann-Whitney U Test
## also known as the Wilcoxon Signed Rank Test

select <- c("treatment", "budwidth")
wdata_orob <- orob[,select]
wdata_soda <- soda[,select]

orob1 <- as.vector(wdata_orob[wdata_orob$treatment == "standard", 2])
orob2 <- as.vector(wdata_orob[wdata_orob$treatment == "hard prune", 2])

orob1.1 <- as.vector(sample(orob1, 18))

res_orob <- wilcox.test(orob1, orob2, alternative = "two.sided")

soda1 <- as.vector(wdata_soda[wdata_soda$treatment == "standard", 2])
soda2 <- as.vector(wdata_soda[wdata_soda$treatment == "upper", 2])
soda3 <- as.vector(wdata_soda[wdata_soda$treatment == "coco coir", 2])

res_soda12 <- wilcox.test(soda1, soda2, alternative = "two.sided")
res_soda23 <- wilcox.test(soda2, soda3, alternative = "two.sided")
res_soda13 <- wilcox.test(soda1, soda3, alternative = "two.sided")

d.res_orob <- wilcox.test(orob1, orob2, alternative = "less")
d.res_soda12 <- wilcox.test(soda1, soda2, alternative = "less")
d.res_soda23 <- wilcox.test(soda2, soda3, alternative = "greater")
d.res_soda13 <- wilcox.test(soda1, soda3, alternative = "less")

## we see that the hard prune OROBs had a greater bud width than that of the standard treatment,
## that upper level SODAs had a greater budwidth than that of the coco coir and standard treatments,
## and that there was no significant difference in the bud width between SODA rockwool and 
## SODA coco coir.


# here are the effect sizes for the treatments that had one

eff_orob <- cliff.delta(orob2, orob1, return.dm = TRUE)
eff_soda12 <- cliff.delta(soda2, soda1, return.dm = TRUE)
eff_soda23 <- cliff.delta(soda2, soda3, return.dm = TRUE)
