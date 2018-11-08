####TO DO####
#should i ignore missing values or fill
#are features correlated with target or other features
#may have to split categorical features with more than 2 categories
#check correlation and drop NA for low correlation values
#check for outliers and decide on mean or median

library(ggplot2)
library(rattle)
library(corrplot)

#loading data and combining targets
training <- read.table('C:\\Users\\Zach\\Desktop\\Classes\\Fall 2018\\DSA 6000\\Final Project\\orange_small_train\\orange_small_train.data', 
                header = TRUE,
                sep = '\t',
                na.strings = c('NA',''))
str(training)

churn <- read.table('C:\\Users\\Zach\\Desktop\\Classes\\Fall 2018\\DSA 6000\\Final Project\\orange_small_train\\orange_small_train_churn.labels.txt',
                    header = FALSE,
                    sep='\t')
training$churn <- churn$V1

appetency <- read.table('C:\\Users\\Zach\\Desktop\\Classes\\Fall 2018\\DSA 6000\\Final Project\\orange_small_train\\orange_small_train_appetency.labels.txt',
                        header = FALSE,
                        sep = '\t')
training$appetency <- appetency$V1

upselling <- read.table('C:\\Users\\Zach\\Desktop\\Classes\\Fall 2018\\DSA 6000\\Final Project\\orange_small_train\\orange_small_train_upselling.labels.txt',
                        header = FALSE,
                        sep='\t')
training$upselling <- upselling$V1
head(training)

#show data types for each column (convert any?)
table(sapply(training, class))

#shows all na values by variable
na_count <- sapply(training, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

na_count
#show all constant values (consider removing <=2 uniques)
apply(training, 2, function(x) length(unique(x)))


#fit = glm(training$churn ~., data = training, family = "binomial")

###################################DATA CLEANING###################################
##removing columns with 80% or more NA vals
x = (colSums(!is.na(training))/50000) > 0.2
y = (colSums(!is.na(training))/50000) < 0.2 #all columns with >80% NA
trainset <- training[, x, drop = FALSE]
yset <- training[, y, drop = FALSE]
#fit = glm(trainset$churn ~., data = training, family = "binomial")
dim(trainset)
dim(yset)
apply(trainset, 2, function(x) length(unique(x)))

rattle()

logit <- glm(training$churn ~ ., data = training, family = binomial)
apply(trainset, 2, function(x) length(unique(x)))


#training <- read.table(file = "C:\\Users\\Zach\\Desktop\\Classes\\Fall 2018\\DSA 6000\\Final Project\\orange_small_train\\orange_small_train.data", fileEncoding="UTF-8", sep = "\t" , header = TRUE, na.strings = "NA")
#head(training)
#dim(training)

as.data.frame(table(unlist(training$churn)))
as.data.frame(table(unlist(training$upselling)))
as.data.frame(table(unlist(training$appetency)))

