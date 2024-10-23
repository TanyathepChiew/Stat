# load library
library(tidyverse)
library(lsr)
library(psych)
library(rstatix)
library(car)

# overall summary
summary(DataForStat$Sale) #DataForStat$Sale; Table = DataForStat , Column = Sale 
describe(DataForStat$Sale)

#Shapiro-Wilk test, reject H0 = not a normal distribution
DataForStat %>%
  group_by(Sex) %>%
  shapiro_test(Sale)

# Student’s independent sample t-test
# Assumptions: Normality, Indenpendence, no significant outlier, Homoscedasticity
independentSamplesTTest(
  formula = Sale ~ Sex,
  data = DataForStat,
  var.equal = TRUE
)

# Welch’s independent samples t-test
# Assumptions: Normality, Indenpendence, no significant outlier
independentSamplesTTest(
  formula = Sale ~ Sex,
  data = DataForStat,
  var.equal = FALSE
)
# t.test(formula = Sale ~ Sex, data = DataForStat)
# cohenD(formula = Sale ~ Sex, data = DataForStat, method = "unequal")

# One side test
independentSamplesTTest(
  formula = Sale ~ Sex,
  data = DataForStat,
  var.equal = FALSE,
  one.sided = "XX" # XX mean is higher than XY mean or not
)

# Mann Whitney U test
wilcox.test(formula = Sale ~ Sex, data = DataForStat)

# overall
xtabs(~Channel, DataForStat)
aggregate(Sale~Channel, DataForStat, mean)
aggregate(Sale~Channel, DataForStat, sd)

# one-way ANOVA
ANOVA <- aov(Sale~Channel, DataForStat)
summary(ANOVA)
pairwise.t.test(x = DataForStat$Sale,
                g = DataForStat$Channel,
                p.adjust.method = "none")
etaSquared(x = ANOVA)
posthocPairwiseT(ANOVA)
leveneTest(ANOVA)
ANOVA.residuals <- residuals(object = ANOVA)
shapiro.test(x = ANOVA.residuals)

# Welch one-way test
oneway.test(Sale~Channel, data = DataForStat, var.equal = FALSE)

# Kruskal-Wallis test
kruskal.test(Sale~Channel, data = DataForStat)

# two-way ANOVA without interaction term
model1 <- aov(Sale ~ Sex + Channel, DataForStat)
summary(model1)
etaSquared(model1)
leveneTest(model1)
resid1 <- residuals(model1)
shapiro.test(resid1)

# two-way ANOVA with interaction term
model2 <- aov(Sale ~ Sex*Channel, DataForStat)
summary(model2)
etaSquared(model1)
leveneTest(model2)
resid2 <- residuals(model2)
shapiro.test(resid2)