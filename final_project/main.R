if (!requireNamespace("tidyverse", quietly = T)) {
    install.packages("tidyverse")
}

renv::restore(prompt = F)

library(tidyverse)
library(RKEEL)
library(arules)
library(caret)

df <- read.keel("data/housevotes.dat")
df <- df %>% mutate(
    across(
        Handicapped_infants:Export_south_africa,
        ~ factor(.x, levels = c("y", "n", "NA"), labels = c("Sí", "No", "Abstención"))
    )
) %>% rename(Party_affiliation=Class)

oneHotCodification <- dummyVars(~ Export_south_africa + Water_project_cost, data=df)
conversion <- data.frame(predict(oneHotCodification, newdata=df)) %>% mutate(
    across(
        1:6,
        ~ factor(.x, levels = c(0, 1), labels = c(FALSE, TRUE))
    )
)

dfWithNegativeItems <- df
dfWithNegativeItems$Export_south_africa <- NULL
dfWithNegativeItems$Water_project_cost <- NULL
dfWithNegativeItems <- cbind(dfWithNegativeItems, conversion)

dfTransactions <- as(dfWithNegativeItems, "transactions")
summary(dfTransactions)
image(dfTransactions)


itemFrequencyPlot(dfTransactions, support = 0.2, cex.names = 0.8)
rules <- apriori(dfTransactions, parameter = list(support = 0.2, confidence = 0.8, minlen=2, maxlen=4))
summary(rules)

redundant <- is.redundant(rules, measure = "confidence")
rules <- rules[!redundant]
summary(rules)

# Clean redundant rules regarding negative items

# No existen reglas redundantes
a <- subset(rules, lhs %in% "Export_south_africa.Abstención=TRUE" & lhs %in% "Export_south_africa.Sí=FALSE")
summary(a)
b <- subset(rules, lhs %in% "Export_south_africa.Abstención=TRUE" & lhs %in% "Export_south_africa.No=FALSE")
summary(b)
c <- subset(rules, lhs %in% "Export_south_africa.Sí=TRUE" & lhs %in% "Export_south_africa.Abstención=FALSE")
summary(c)
d <- subset(rules, lhs %in% "Export_south_africa.Sí=TRUE" & lhs %in% "Export_south_africa.No=FALSE")
summary(d)
e <- subset(rules, lhs %in% "Export_south_africa.No=TRUE" & lhs %in% "Export_south_africa.Sí=FALSE")
summary(e)
f <- subset(rules, lhs %in% "Export_south_africa.No=TRUE" & lhs %in% "Export_south_africa.Abstención=FALSE")
summary(f)
g <- subset(rules, lhs %in% "Water_project_cost.Abstención=TRUE" & lhs %in% "Water_project_cost.Sí=FALSE")
summary(g)
h <- subset(rules, lhs %in% "Water_project_cost.Abstención=TRUE" & lhs %in% "Water_project_cost.No=FALSE")
summary(h)
i <- subset(rules, lhs %in% "Water_project_cost.Sí=TRUE" & lhs %in% "Water_project_cost.Abstención=FALSE")
summary(i)
j <- subset(rules, lhs %in% "Water_project_cost.Sí=TRUE" & lhs %in% "Water_project_cost.No=FALSE")
summary(j)
k <- subset(rules, lhs %in% "Water_project_cost.No=TRUE" & lhs %in% "Water_project_cost.Sí=FALSE")
summary(k)
l <- subset(rules, lhs %in% "Water_project_cost.No=TRUE" & lhs %in% "Water_project_cost.Abstención=FALSE")
summary(l)


conviction <- interestMeasure(
    rules,
    measure=c("conviction"),
    transactions=dfTransactions
)

quality(rules) <- cbind(quality(rules), conviction)
summary(rules)

rulesgroup1 <- subset(rules, rhs %in% "Export_south_africa.Sí=TRUE" & size(lhs) == 1)
summary(rulesgroup1)
inspect(rulesgroup1)

rulesgroup2 <- subset(rules, rhs %in% "Export_south_africa.Sí=FALSE" & size(lhs) == 1)
summary(rulesgroup2)
inspect(rulesgroup2)

rulesgroup3 <- subset(rules, rhs %in% "Export_south_africa.No=TRUE" & size(lhs) == 1)
summary(rulesgroup3)
inspect(rulesgroup3)

rulesgroup4 <- subset(rules, rhs %in% "Export_south_africa.No=FALSE" & size(lhs) == 1)
summary(rulesgroup4)
inspect(rulesgroup4)

rulesgroup5 <- subset(rules, rhs %in% "Export_south_africa.Abstención=TRUE" & size(lhs) == 1)
summary(rulesgroup5)
inspect(rulesgroup5)

rulesgroup6 <- subset(rules, rhs %in% "Export_south_africa.Abstención=FALSE" & size(lhs) == 1)
summary(rulesgroup6)
inspect(rulesgroup6)


interestingRules <- subset(rules, conviction >= 1.01 & conviction <= 5)
summary(interestingRules)
inspect(head(interestingRules, n=50))

sortedRulesbyConviction <- sort(interestingRules, by="conviction")
inspect(head(sortedRulesbyConviction))

sortedRulesBySupport <- sort(interestingRules, by="lift")
inspect(head(sortedRulesBySupport, n=200))

inspect(subset(rules, rhs %in% "Aid_to_nicaraguan_contras=Sí" & size(lhs) == 1))

prueba <- subset(rules, rhs %in% "Party_affiliation=republican" & size(lhs) == 1)
summary(prueba)
inspect(prueba)

prueba1 <- subset(rules, rhs %in% "Party_affiliation=democrat" & size(lhs) == 2 & lhs %in% "Physician_fee_freeze=Sí")
summary(prueba1)
inspect(prueba1)

prueba2 <- subset(sortedRulesbyConviction, lhs %in% "Duty_free_exports=Sí")
summary(prueba2)

prueba3 <- subset(interestingRules, lhs %in% "Mx_missile=Sí" & rhs %in% "Anti_satellite_test_ban=Sí")
summary(prueba3)
inspect(prueba3)

prueba4 <- subset(interestingRules, lhs %in% "Anti_satellite_test_ban=No" & rhs %in% "Mx_missile=No")
summary(prueba4)
inspect(prueba4)

aa <- subset(rules, lhs %in% "Anti_satellite_test_ban=No" & lhs %in% "Mx_missile=No" & size(lhs) == 2)
inspect(aa)

ab <- subset(rules, lhs %in% "Anti_satellite_test_ban=Sí" & lhs %in% "Mx_missile=Sí" & size(lhs) == 2)
inspect(ab)

#rkeel MOPNAR

mopnar <- MOPNAR_A(df)
mopnar$run()
summary(mopnar$rules)

mopnar_rules <- mopnar$rules

mopnarRulesRedundant <- is.redundant(mopnar_rules, measure = "confidence")
mopnar_rules <- mopnar_rules[!mopnarRulesRedundant]
summary(mopnar_rules)

mopnar_rules <- subset(mopnar_rules, size(lhs) <= 3 & conviction >= 1.01 & conviction < 5)
summary(mopnar_rules)
