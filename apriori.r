# Reading the input dataset
thedataset = read.csv("C:\\Users\\Monica\\Documents\\data mining\\project1\\mba.csv")

# See first 10 observations
head(thedataset, n=10)

# Split data
datasplit <- split(thedataset$Products, thedataset$ID)

# Loading and installing arules package if not present
if(!require(arules)) install.packages("arules")

# Converting the given data to transactional level data
datasplit2 = as(datasplit,"transactions")
summary(datasplit2)
inspect(datasplit2)

# Most Frequent Items are found
itemFrequency(datasplit2, type = "relative")
itemFrequencyPlot(datasplit2,topN = 20)

# aggregated dataset
rules = apriori(datasplit2, parameter=list(support=0.4, confidence=0.6))
rules = apriori(datasplit2, parameter=list(support=0.4, confidence=0.6, minlen = 3))
rules = apriori(datasplit2, parameter=list(support=0.4, confidence=0.5, maxlen = 4))

#Convert rules into data frame
rules3 = as(rules, "data.frame")
write(rules, "C:\\Users\\Monica\\Documents\\data mining\\project1\\rules1.csv", sep=",")

# Show only particular product rules
inspect( subset( rules, subset = rhs %pin% "Product H" ))

# Show the top 10 rules
options(digits=2)
inspect(rules[1:10])

# Get Summary Information
summary(rules)

# Sort by Lift method
rules<-sort(rules, by="lift", decreasing=TRUE)

# Remove Unnecessary Rules using pruning
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules[!redundant]
rules<-rules.pruned

#Clean Rules
rules3$rules=gsub("\\{", "", rules3$rules)
rules3$rules=gsub("\\}", "", rules3$rules)
rules3$rules=gsub("\"", "", rules3$rules)

#Split the rules using the library
library(splitstackshape)
Rules4=cSplit(rules3, "rules","=>")
names(Rules4)[names(Rules4) == 'rules_1'] <- 'LHS'
Rules5=cSplit(Rules4, "LHS",",")
Rules6=subset(Rules5, select= -c(rules_2))
names(Rules6)[names(Rules6) == 'rules_3'] <- 'RHS'

# What are customers likely to buy before they purchase "Product A"
rules<-apriori(data=datasplit, parameter=list(supp=0.001,conf = 0.6), 
               appearance = list(default="lhs",rhs="Product A"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

# What are customers likely to buy if they purchased "Product A"
rules<-apriori(data=datasplit, parameter=list(supp=0.001,conf = 0.6), 
               appearance = list(default="rhs",lhs="Product A"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])
#visulizing the data
library(arulesViz)
plot(rules,method="graph",interactive=TRUE,shading="confidence")



