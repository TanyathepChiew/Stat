library(lsr)

# Chi-squre test of independence 
# Expected frequencies are sufficiently large, independent
associationTest(fomula = ~variable1+variable2, data = mydata)

# Fisher exact test
# independent
contingency_table <- table(mydata$variable1, mydata$variable2)
fisher.test(contingency_table)

# McNemar test
# dependent
mcnemar.test(table(data$before, data$after))
