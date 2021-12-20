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
)

# handicapped infants -> https://www.congress.gov/bill/98th-congress/house-bill/808
# water project cost -> https://scorecard.lcv.org/roll-call-vote/1984-540-water-project-cost-sharing-maintenance-and-repairs
# adoption budget resolution -> https://www.congress.gov/bill/98th-congress/house-bill/5247
# physician fee freeze -> https://www.healthaffairs.org/doi/10.1377/hlthaff.8.1.21
# el salvador aid -> https://www.washingtonpost.com/archive/politics/1984/05/11/reagan-wins-narrowly-on-aid-to-el-salvador/6ed8ac74-99b1-4fcc-8d9d-c57ba1295891/
# religious groups -> https://www.nytimes.com/1984/05/16/us/bill-for-religion-in-public-schools-loses-house-vote.html
# anti-satellite test ban -> https://www.nytimes.com/1985/12/17/world/pentagon-assails-proposed-anti-satellite-test-ban.html
# aid to nicaraguan contras -> https://www.latimes.com/archives/la-xpm-1985-06-13-mn-10854-story.html
# mx missile -> https://www.nytimes.com/1984/05/17/us/compromise-on-mx-is-passed-by-house-missile-total-cut.html
# immigration -> https://www.nytimes.com/1984/06/21/us/house-by-216-211-approves-aliens-bill-after-retaining-amnesty-plan-in-final-test.html
# synfuel corporation cutback -> https://www.nytimes.com/1984/08/03/business/house-votes-to-tighten-synthetic-fuels-money.html
# education spending -> https://www.edweek.org/education/congress-set-to-pass-1984-e-d-budget/1983/10
# right to sue -> https://library.cqpress.com/cqalmanac/document.php?id=cqal84-1152958
# crime -> https://en.wikipedia.org/wiki/Comprehensive_Crime_Control_Act_of_1984
# duty free exports -> https://www.nytimes.com/1984/03/28/business/us-cuts-list-of-duty-free-imports.html
# export south africa -> https://www.congress.gov/bill/98th-congress/house-bill/4230?s=1&r=92

dfTransactions <- as(df, "transactions")

summary(dfTransactions)
image(dfTransactions)

itemFrequencyPlot(dfTransactions, support = 0.1, cex.names = 0.8)


aprioriItemsets <- apriori(dfTransactions, parameter = list(support = 0.15, target = "frequent"))
aprioriItemsets <- sort(aprioriItemsets, by="support")
inspect(head(aprioriItemsets, n=10))

rules <- apriori(dfTransactions, parameter = list(support = 0.2, confidence = 0.8, minlen=2, maxlen=4))
summary(rules)

# meow1 <- subset(sortedRules, rhs %in% "stabilityCoreTemperature=unstable")
# summary(meow1)
# inspect(head(meow1))

redundant <- is.redundant(rules, measure = "confidence")
rulesPruned <- rules[!redundant]
summary(rulesPruned)

conviction <- interestMeasure(
    rulesPruned,
    measure=c("conviction"),
    transactions=dfTransactions
)

quality(rulesPruned) <- cbind(quality(rulesPruned), conviction)

interestingRules <- subset(rulesPruned, conviction > 1 & conviction <= 5)
summary(interestingRules)
sortedRulesbyConviction <- sort(interestingRules, by="conviction")
inspect(head(sortedRulesbyConviction, n=20))


sortedRulesBySupport <- sort(rulesPruned, by="support")
inspect(head(sortedRulesBySupport, n=10))

meow1 <- subset(rulesPruned, rhs %in% "Class=republican")
summary(meow1)
inspect(head(sort(meow1, by="conviction"), n=10))

meow2 <- subset(rulesPruned, rhs %in% "Mx_missile=No" & lhs %in% "Anti_satellite_test_ban=Sí")
summary(meow2)    
inspect(head(meow2))




dfNegativeItems <- df
oneHotCodification <- dummyVars(~ Export_south_africa, data=dfNegativeItems)
conversion <- data.frame(predict(oneHotCodification, newdata=df)) %>% mutate(
    across(
        1:3,
        ~ factor(.x, levels = c(0, 1), labels = c(FALSE, TRUE))
    )
)

negativeItemsNames <- names(conversion)

dfNegativeItems$Export_south_africa <- NULL
dfNegativeItems <- cbind(dfNegativeItems, conversion)

dfTransactionsNegativeItems <- as(dfNegativeItems, "transactions")

itemFrequencyPlot(dfTransactionsNegativeItems, support = 0.2, cex.names = 0.8)
rules1 <- apriori(dfTransactionsNegativeItems, parameter = list(support = 0.2, confidence = 0.8, minlen=2))
summary(rules1)

redundant1 <- is.redundant(rules1, measure = "confidence")
rulesPruned1 <- rules1[!redundant1]
summary(rulesPruned1)

inspect(head(rulesPruned1))

# No existen reglas redundantes
a <- subset(rulesPruned1, lhs %in% "Export_south_africa.Abstención=TRUE" & lhs %in% "Export_south_africa.Sí=FALSE")
summary(a)
b <- subset(rulesPruned1, lhs %in% "Export_south_africa.Abstención=TRUE" & lhs %in% "Export_south_africa.No=FALSE")
summary(b)
c <- subset(rulesPruned1, lhs %in% "Export_south_africa.Sí=TRUE" & lhs %in% "Export_south_africa.Abstención=FALSE")
summary(c)
d <- subset(rulesPruned1, lhs %in% "Export_south_africa.Sí=TRUE" & lhs %in% "Export_south_africa.No=FALSE")
summary(d)
e <- subset(rulesPruned1, lhs %in% "Export_south_africa.No=TRUE" & lhs %in% "Export_south_africa.Sí=FALSE")
summary(e)
f <- subset(rulesPruned1, lhs %in% "Export_south_africa.No=TRUE" & lhs %in% "Export_south_africa.Abstención=FALSE")
summary(f)


#rkeel

mopnar <- MOPNAR_A(df)
mopnar$run()
summary(mopnar$rules)

mopnar_rules <- mopnar$rules

mopnarRulesRedundant <- is.redundant(mopnar_rules, measure = "confidence")
mopnar_rules <- mopnar_rules[!mopnarRulesRedundant]
summary(mopnar_rules)

sorted_mopnar_rules <- sort(mopnar_rules, by="support")
inspect(head(sorted_mopnar_rules))



