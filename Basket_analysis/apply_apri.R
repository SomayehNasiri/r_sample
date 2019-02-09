library(plotly)

### high support items
itemFrequencyPlot(basketDs,support=0.10)

##**Analyze Categorical items**
cat_rules <- apriori (catBasketDs, parameter = list(supp = 0.06, conf = 0.36,minlen=2,maxlen=4))
sorted_cat_rules <- sort (cat_rules, by="lift")
inspect(sorted_cat_rules)
ruleExplorer(cat_rules)

######## Make a table for generated sorted rules and write in file
cat_rules_tbl <- data.frame(inspect(cat_rules))
write.table(cat_rules_tbl, "~/mydata.csv", sep=",")
##**End Categorical Items


### **Analyse items in transacton
rules <- apriori(basketDs,
                 parameter = list(supp=0.009, conf=0.01,minlen=3))
#,
 #                appearance = list(lhs = c("iMac"), default="rhs"))

sorted_rules <- sort (rules, by="lift")[1:50]
rules_tbl <- data.frame(inspect((sorted_rules)))
toprules <- rules[1:10]
ruleExplorer(rules)

plot(toprules)
plot(toprules, method="grouped")
itemFrequencyPlot(basketDs,support=0.09)

## How to format numbers
corr <-round(cor(newAsData[,1:4]),2)
format(avgsw, digits=2, nsmall=1)