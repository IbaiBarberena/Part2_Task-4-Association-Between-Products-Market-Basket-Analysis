## Market Basket Analysis
## Ibai Barberena
## 19/06/2019


# Libraries for the Market Basket Analysis: 

pacman::p_load(arules, arulesViz, tidyverse, dplyr, magrittr, ggplot2, readr, RColorBrewer)
# install.packages("arulesViz")
# install.packages("caTools")
library(arulesViz)
library(caTools)

#################### 1st Delivery: data quality ############################

### Load data

orders <- read_csv2("C:/Users/Ibai/Desktop/Part_2/Task_4/Discover Associations Between Products/Data/orders.csv")

lineitems <- read_csv2("C:/Users/Ibai/Desktop/Part_2/Task_4/Discover Associations Between Products/Data/lineitems.csv")

# Filtering the list of total product orders for the orders that have more than a product.
# Which id orders are repeated?

lineitems_more <- lineitems %>%
  group_by(id_order) %>%
  filter(n()>1)

# How many id_orders do we have with more than a product?

length(unique(lineitems_more$id_order))

# Filtering completed orders

orders_completed <- orders %>% 
  filter(state == "Completed")

# From the list of total products ordered (lineitems_more), which id_orders have "Completed" status?

match <- which(lineitems_more$id_order  %in% orders_completed$id_order)

lineitems_trans <- lineitems_more[match,]


# How many unique orders do we have with Completed status and with more than a product?

length(unique(lineitems_trans$id_order))

# The amount of transactions (10,453) is the same as the amount of transactions that we have available in trans.csv
# Thus, we can trust the data that we have.


#  Let's check the quality of the data:

anyNA(lineitems_trans)

str(lineitems_trans)


# Let's sum all the prices of each id order. Later we will compare them with the total price of the orders.

lineitems_transf <- lineitems_trans %>%
  group_by(id_order) %>%
  mutate(paidperproduct=unit_price*product_quantity) %>% 
  summarize(price = sum(paidperproduct))



# Next step is to filter the "orders_completed" data frame by the same id_orders that we have in "lineitems_trans".
# The aim is to have the same id_orders so we can compare and combine them

match2 <- which(orders_completed$id_order %in% lineitems_transf$id_order)

orders_completed2 <- orders_completed[match2,]


# Are the prices of "orders" and "lineitems" datasets matching?

combinationDF <- cbind(lineitems_transf, orders_completed2[, c(2,4)])

names(combinationDF) <- c("id_order", "price_of_lineitems", "created_date", "price_of_orders")

combinationDF$difference <- combinationDF$price_of_orders - combinationDF$price_of_lineitems


dev.off()
ggplot(combinationDF, aes(x= difference)) +
  geom_histogram(color= "darkblue", fill= "lightblue", bins= 30)+
  xlim(-1,25) + ylim(0,5000) + 
  labs(title = "Differences of price in Orders & Lineitems dataset", y= "Frecuency of differences", x = "Difference")




#################### 2nd Delivery: Transaction data and modeling  ######################

trans <- read.transactions(file = "c:/Users/Ibai/Desktop/Part_2/Task_4/Discover Associations Between Products/Data/trans.csv", header= FALSE, format = 'basket', sep = ",")

summary(trans)

# inspect (trans) # You can view the transactions. Is there a way to see a certain # of transactions?
# length (trans) # Number of transactions.
# size (trans) # Number of items per transaction
# LIST(trans) # Lists the transactions by conversion (LIST must be capitalized)
# itemLabels(trans)# To see the item labels


# Visualizing the 20 most sold items frecuency:

dev.off()
itemFrequencyPlot(trans, names = TRUE, topN= 20, col = brewer.pal(8,'Pastel2'), type= 'absolute', main= "TOP 20 most sold items")

# Saving the purchase frecuency of each item:
Frecuency_items <- itemFrequency(trans)

Frecuency_items <- as.data.frame(Frecuency_items)

write.csv2(Frecuency_items, file="C:/Users/Ibai/Desktop/Part_2/Task_4/Discover Associations Between Products/Data/Frecuency_items.csv")

# Number of transactions of 2,000 random items

image(sample(trans, 2000))


## APPLYING APRIORI ALGORITHM TO THE ITEMSETS##

rules<- apriori(trans, parameter = list(supp = 0.0002, conf = 0.0002)) # minlen=3

summary(rules)

# View of the rules

inspect(sort(rules, decreasing = TRUE, na.last = NA, by = "lift")[1:50])

# How to see a specific item's rule? For example, "SNS0010", "NEA0015"

# ItemRules <- subset(rules, items %in% c("SNS0010", "NEA0015"))
# 
# inspect(ItemRules)


# Do the rules have redundancy?

reduncancy <- is.redundant(rules)

rules <- rules[!reduncancy]
inspect(rules)

## Visualizing Association Rules ##

plot(rules, method= "two-key plot")

# or interactive...

plotly_arules(rules)

inspectDT(rules)
ruleExplorer(rules)



#################### 3rd Delivery: Dimension reduction through categories  ######################


# Uploading the Products category txt to R studio:

categories <- read_table2("C:/Users/Ibai/Desktop/Part_2/Task_4/Discover Associations Between Products/Data/products_with_category.txt")



# How many different products are available in "trans" dataset?
# write.csv2(trans@itemInfo, file= "C:/Users/Ibai/Desktop/Part_2/Task_4/Discover Associations Between Products/Data/productsname.csv")


sku1 <- trans@itemInfo$labels

skudf <- as.data.frame(sku1)

names(skudf) <- "sku"


combination <- left_join(x= skudf, y= categories, by= "sku") #by "sku", as in both data frames, the column is called "sku"

#  We are having NA variables. Let's change the name of them to a new category called "Unknown"
combination <- combination %>% 
  mutate(newcategories = if_else(is.na(manual_categories), "unknown", manual_categories))


# Adding the categories into trans dataset:
trans@itemInfo$categories <- combination$newcategories

trans_cat <- aggregate(trans, itemInfo(trans)[["categories"]])

trans_cat@itemInfo$categories

# delete <- c("AP20089","AP20112","AP20166","AP20214","AP20229","AP20392","APP0601","APP0602","APP2441","APP2442","BLL0004","CAD0001","CAD0002","CAD0004","CAD0005","CAD0006","CRU0045-A","ENV0490","ENV0799","ENV1000","EVU0001","items","KAN0021-A","LIBRO","LIF0096","MYF0001","PAC0358","PAC1149","PAC1150","PAC1770","PAC1902","PAC2154","PAC2156","RAI0015","REP0082","REP0131","SAT0034","SYN0121","SYN0133-A","WAC0125","WDT0177-A")


### Modelling with categories ###
summary(trans_cat)

# inspect (trans_cat) # You can view the transactions. Is there a way to see a certain # of transactions?
# length (trans_cat) # Number of transactions.
# size (trans_cat) # Number of items per transaction
# LIST(trans_cat) # Lists the transactions by conversion (LIST must be capitalized)
# itemLabels(trans_cat)# To see the item labels

# Visualizing the 5 most sold items by category:

itemFrequencyPlot(trans_cat, names = TRUE, topN= 5, col = brewer.pal(8,'Pastel2'), type= 'absolute', main= "TOP 5 most sold items by category")

# Number of transactions of 2,000 random items
# image(sample(trans_cat, 2000))


## APPLYING APRIORI ALGORITHM TO THE CATEGORIES##

rules_cat<- apriori(trans_cat, parameter = list(supp = 0.0002, conf = 0.0002, minlen= 2))

summary(rules_cat)

# View of the rules

inspect(sort(rules_cat, decreasing = TRUE, na.last = NA, by = "lift")[1:50])

# Do the rules have redundancy?

reduncancy <- is.redundant(rules_cat)

rules_cat <- rules_cat[!reduncancy]
summary(rules_cat)

## Visualizing Association Rules ##

plot(rules_cat, method= "two-key plot")

# or interactive...

plotly_arules(rules_cat)

inspectDT(rules_cat)
ruleExplorer(rules_cat)


#################### 4th Delivery: Creating my own relevant factors  ######################

######### 4.1 Basket analysis based on the brand #########

brands <- read_csv2("C:/Users/Ibai/Desktop/Part_2/Task_4/Discover Associations Between Products/Data/Products_by_brand.csv")

trans@itemInfo$brands <- brands$Brand

trans_brands <- aggregate(trans, itemInfo(trans)[["brands"]])

trans_brands@itemInfo$brands

summary(trans_brands)

# Visualizing the 10 most sold brands:

itemFrequencyPlot(trans_brands, names = TRUE, topN= 10, col = brewer.pal(8,'Pastel2'), type= 'absolute', main= "TOP 10 most sold items by brand")

# Number of transactions of 200 random items
# image(sample(trans_brands, 200))


## APPLYING APRIORI ALGORITHM TO THE CATEGORIES##

rules_brands<- apriori(trans_brands, parameter = list(supp = 0.0002, conf = 0.0002, minlen= 2))

summary(rules_brands)

# View of the rules

inspect(sort(rules_brands, decreasing = TRUE, na.last = NA, by = "lift")[1:50])

# Do the rules have redundancy?

reduncancy <- is.redundant(rules_brands)

rules_brands <- rules_brands[!reduncancy]
summary(rules_brands)

## Visualizing Association Rules ##

plot(rules_brands, method= "two-key plot")

# or interactive...

plotly_arules(rules_brands)

inspectDT(rules_brands)
ruleExplorer(rules_brands)

######### 4.2 Basket analysis based on the brand + category #########

# Let's upload a document with all the categories arranged by trans, in order to combine with brand name.
# write.csv2(combination, file= "C:/Users/Ibai/Desktop/Part_2/Task_4/Discover Associations Between Products/Data/combination_product+category.csv")

# Dataframe with brand + category

brands_cat <- read_csv2("C:/Users/Ibai/Desktop/Part_2/Task_4/Discover Associations Between Products/Data/Products_by_brand+cat_def.csv")

trans@itemInfo$brands_cat <- brands_cat$Toguether

trans_brands_cat <- aggregate(trans, itemInfo(trans)[["brands_cat"]])

trans_brands_cat@itemInfo$brands_cat

summary(trans_brands_cat)

# Visualizing the 20 most sold brands and its category:

dev.off()
itemFrequencyPlot(trans_brands_cat, names = TRUE, topN= 30, col = brewer.pal(8,'Pastel2'), type= 'absolute', main= "TOP 30 most sold brands by categories")

# Number of transactions of 200 random items
# image(sample(trans_brands_cat, 200))


## APPLYING APRIORI ALGORITHM TO THE BRANDS AND CATEGORIES##

rules_brands_cat<- apriori(trans_brands_cat, parameter = list(supp = 0.0005, conf = 0.2, minlen= 2))

summary(rules_brands_cat)


# Finding rules related to given items:
rules_brand_cat_prod <- apriori(trans_brands_cat, parameter = list(supp = 0.000000005, conf = 0.05, minlen= 2), appearance = list(lhs="Apple_phone",default="rhs"))
summary(rules_brand_cat_prod)
inspect(rules_brand_cat_prod)

ruleExplorer(rules_brand_cat_prod)

# View of the rules

inspect(sort(rules_brands_cat, decreasing = TRUE, na.last = NA, by = "lift")[1:50])

# Do the rules have redundancy?

reduncancy <- is.redundant(rules_brands_cat)

rules_brands_cat <- rules_brands_cat[!reduncancy]
summary(rules_brands_cat)

## Visualizing Association Rules ##

plot(rules_brands_cat, method= "two-key plot")

# or interactive...

plotly_arules(rules_brands_cat)

inspectDT(rules_brands_cat)
ruleExplorer(rules_brands_cat)



######################### 5. OTHER GRAPHS FOR THE REPORT ####################

##### What is the total price of each product? ###

lineitems_product <- lineitems_trans %>%
  group_by(sku) %>%
  mutate(paidperproduct=unit_price*product_quantity) %>% 
  summarize(total_price = sum(paidperproduct))

write.csv2(lineitems_product, file="C:/Users/Ibai/Desktop/Part_2/Task_4/Discover Associations Between Products/Data/totalprice_per_product.csv")

# Plotting the distribution of the price

dev.off()
ggplot(lineitems_product, aes(x=total_price)) + 
  geom_density(fill="#FF6666") + 
  xlim(c(0, 5000))


