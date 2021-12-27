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


# Obteniendo los items negativos de las variables Export_south_africa y Water_project_cost
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

# Obteninedo las transacciones del dataset con items negativos
dfTransactions <- as(dfWithNegativeItems, "transactions")
summary(dfTransactions)
image(dfTransactions)

# Gráfico con items con una frecuencia mayor o igual que el 0.2
itemFrequencyPlot(dfTransactions, support = 0.2, cex.names = 0.8)

# Generación de reglas con un soporte mínimo del 0.2, confianza mínima del 0.8, longitud mínima de 2 y máxima de 4
rules <- apriori(dfTransactions, parameter = list(support = 0.2, confidence = 0.8, minlen=2, maxlen=4))
summary(rules)

# Eliminación de las reglas redundantes
redundant <- is.redundant(rules, measure = "confidence")
rules <- rules[!redundant]
summary(rules)

# No existen reglas con items negativos redundantes, por lo que no hay que modificar ninguna
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

# Añadir la métrica conviction al conjunto de las reglas
conviction <- interestMeasure(
    rules,
    measure=c("conviction"),
    transactions=dfTransactions
)
quality(rules) <- cbind(quality(rules), conviction)
summary(rules)

# Búsqueda de reglas para hacer un análisis de reglas por grupos en la búsqueda de excepciones y/o anomalías
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

rulesgroup7 <- subset(rules, rhs %in% "Water_project_cost.Sí=TRUE" & size(lhs) == 1)
summary(rulesgroup7)
inspect(rulesgroup7)

rulesgroup8 <- subset(rules, rhs %in% "Water_project_cost.Sí=FALSE" & size(lhs) == 1)
summary(rulesgroup8)
inspect(rulesgroup8)

rulesgroup9 <- subset(rules, rhs %in% "Water_project_cost.No=TRUE" & size(lhs) == 1)
summary(rulesgroup9)
inspect(rulesgroup9)

rulesgroup10 <- subset(rules, rhs %in% "Water_project_cost.No=FALSE" & size(lhs) == 1)
summary(rulesgroup10)
inspect(rulesgroup10)

rulesgroup11 <- subset(rules, rhs %in% "Water_project_cost.Abstención=TRUE" & size(lhs) == 1)
summary(rulesgroup11)
inspect(rulesgroup11)

rulesgroup12 <- subset(rules, rhs %in% "Water_project_cost.Abstención=FALSE" & size(lhs) == 1)
summary(rulesgroup12)
inspect(rulesgroup12)


# Obtener aquellas reglas interesantes que tengan un valor de conviction entre 1.01 y 5
interestingRules <- subset(rules, conviction >= 1.01 & conviction <= 5)
summary(interestingRules)
inspect(head(interestingRules))

# ordenar reglas por conviction
sortedRulesbyConviction <- sort(interestingRules, by="conviction")
inspect(head(sortedRulesbyConviction))

# ordenar reglas por el soporte
sortedRulesBySupport <- sort(interestingRules, by="support")
inspect(head(sortedRulesBySupport))

# buscar aquellas reglas que tengan "Party_affiliation=republican" en el consecuente y un item en el antecedente
republicans <- subset(rules, rhs %in% "Party_affiliation=republican")
summary(republicans)
inspect(republicans)

democrats <- subset(rules, rhs %in% "Party_affiliation=democrat")
summary(democrats)
inspect(democrats)

reduce_weapons <- subset(interestingRules, lhs %in% "Mx_missile=Sí" & rhs %in% "Anti_satellite_test_ban=Sí")
summary(reduce_weapons)
inspect(reduce_weapons)

not_reduce_weapons <- subset(interestingRules, lhs %in% "Anti_satellite_test_ban=No" & rhs %in% "Mx_missile=No")
summary(not_reduce_weapons)
inspect(not_reduce_weapons)

not_ban_weapon_and_not_reduce_missile <- subset(
    rules,
    lhs %in% "Anti_satellite_test_ban=No" & lhs %in% "Mx_missile=No" & size(lhs) == 2
)
inspect(not_ban_weapon_and_not_reduce_missile)

ban_weapon_and_reduce_missile <- subset(
    rules, lhs %in% "Anti_satellite_test_ban=Sí" & lhs %in% "Mx_missile=Sí" & size(lhs) == 2
)
inspect(ban_weapon_and_reduce_missile)

#rkeel MOPNAR

# Construcción del objeto MOPNAR_A y ejecución del algoritmo
mopnar <- MOPNAR_A(df)
mopnar$run()

summary(mopnar$rules)
mopnar_rules <- mopnar$rules

# Eliminación de las reglas redundantes
mopnarRulesRedundant <- is.redundant(mopnar_rules, measure = "confidence")
mopnar_rules <- mopnar_rules[!mopnarRulesRedundant]
summary(mopnar_rules)

# Obtención de reglas interesantes
mopnar_rules <- subset(mopnar_rules, size(lhs) <= 3 & conviction >= 1.01 & conviction < 5)
summary(mopnar_rules)
