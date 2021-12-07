if (!requireNamespace("tidyverse", quietly = T)) {
    install.packages("tidyverse")
}

if (!requireNamespace("RKEEL", quietly = T)) {
    install.packages("RKEEL")
}

if (!requireNamespace("arules", quietly = T)) {
    install.packages("arules")
}

library(tidyverse)
library(RKEEL)
library(arules)

df <- read.keel("data/post-operative.dat")

df$COMFORT[47] <- "0"
df$COMFORT[49] <- "10"
df$COMFORT[71] <- "10"


df <- df %>% mutate(
    across(L.CORE:BP.STBL, ~ factor(.x))
) %>% rename(
    patientInternalTemperature=L.CORE,
    patientSurfaceTemperature=L.SURF,
    oxygenSaturation=L.O2,
    bloodPressure=L.BP,
    stabilitySurfaceTemperature=SURF.STBL,
    stabilityCoreTemperature=CORE.STBL,
    stabilityBloodPressure=BP.STBL,
) %>% mutate_if(is.character, as.numeric) %>% mutate(
    COMFORT=cut(COMFORT, c(0, 7, 14, 20), labels=c("Low", "Medium", "High"), include.lowest = T),
    Decision=factor(Decision, levels = c("I", "S", "A"), labels = c("ICU", "home", "general_hospital_floor"))
)


dfTransactions <- as(df, "transactions")

summary(dfTransactions)
image(dfTransactions)

itemFrequencyPlot(dfTransactions, support = 0.2, cex.names = 0.8)


aprioriItemsets <- apriori(dfTransactions, parameter = list(support = 0.1, target = "frequent"))
aprioriItemsets <- sort(aprioriItemsets, by="support")
inspect(head(aprioriItemsets, n=10))

rules <- apriori(dfTransactions, parameter = list(support = 0.1, confidence = 0.8, minlen=2))
summary(rules)

meow1 <- subset(sortedRules, rhs %in% "stabilityCoreTemperature=unstable")
summary(meow1)
inspect(head(meow1))

redundant <- is.redundant(rules, measure = "confidence")
rulesPruned <- rules[!redundant]
summary(rulesPruned)

interestingRules <- subset(rulesPruned, conviction > 1 & conviction <= 5)
summary(interestingRules)

sortedRules <- sort(interestingRules, by="conviction")
sortedRules1 <- subset(sortedRules, )
inspect(head(sortedRules))
    
    