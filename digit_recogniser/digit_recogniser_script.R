# load the data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#remove lable of training data
train_label <- train$label
train$label <- NULL
# combine test and training data
combi <- rbind(train, test)

# to list all the column names of data
colnames(train)

# calculate principal component analysis
# no extra paramter fro scaling passed scale. = T
# prin_comp <- prcomp(combi, scale. = T)
prin_comp <- prcomp(combi)

# variables measures
names(prin_comp)

# plot the principal componenets 
biplot(prin_comp, scale = 0)

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

# print first variance of first 10 components
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

# plot proportion of variance for each principal component
plot(prop_varex, xlab = "principal component", ylab = "proportion of variance explained", type = "b")

# now choose first 50 principal components and APPLY KNN

#take whole new prin_comp dataset into x
x <- prin_comp$x

# we will be taking first 50 principal components only
train_new <- x[1:42000, 1:50]

test_new <- x[420001:70000, 1:50]

# KNN 
library(class)
#knn with train and test data with labels
# here we are taking k value as 3
prc_test_prd <- knn(train = train_new, test = test_new, cl = train_label, k = 3)
head(prc_test_prd)

#copy prediction into a new vector
p <- prc_test_prd
#generate a sequence for test imageids
imageids = seq(1, 28000, 1)

#combine both into a data frame
pred <- data.frame(imageids, p)

# add col names
colnames(pred) <- c("ImageId", "Label")

# write the submission csv
# row.names should be false as it would print row numbers
write.csv(pred, file = "submission_princomp_50_knn.csv", row.names = F)
