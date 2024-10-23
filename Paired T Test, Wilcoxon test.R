# t.test(), cohenD()

# load library
library(tidyverse)
library(lsr)
library(psych)
library(rstatix)

# overall summary
summary(PairedTest$Sale)
describe(PairedTest)

#Shapiro-Wilk test
PairedTest <- PairedTest %>% mutate(differences = Pre - Bt)
PairedTest %>% identify_outliers(differences)

# Paired Samples T Test
# Assumptions: Normality, Indenpendence, no significant outlier
pairedSamplesTTest(
  formula = ~ Pre + Bt, #wide form
  data = PairedTest
)

#t.test(x = PairedTest$Pre, y = PairedTest$Bt, paired = TRUE)
# cohenD(x = PairedTest$Pre, y = PairedTest$Bt, method = "paired")

# Paired Samples T Test
pairedSamplesTTest(
  formula = Sale ~ Time, #wide form
  data = PairedTest
)

# Paired Samples T Test
pairedSamplesTTest(
  formula = ~ Pre + Bt, #wide form
  data = PairedTest,
  one.sided = "Bt" # Bt mean is higher than Pre mean or not
)

#Wilcoxon test
wilcox.test(x = PairedTest$Pre, y = PairedTest$Bt, paired = TRUE)

