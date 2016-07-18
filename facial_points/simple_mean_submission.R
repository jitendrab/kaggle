# initial simple submission to check for submission file format
# we calculate mean of each facial key point in training data and 
# submit it corresponding to each image

# Libraries: using reshape2 library
library(doMC)
registerDoMC()
library(reshape2)

# specify training and test data directories
# parameters
data.dir <- '~/jitendra/kaggle/facial_points/'
train.file <- paste0(data.dir, 'training.csv')
test.file <- paste0(data.dir, 'test.csv')

# read data from csv files
message("reading data from csv files...")
d.train <- read.csv(train.file, stringsAsFactors = F)
# read the test csv 
d.test <- read.csv(test.file, stringsAsFactors = F)

# take image out of training data
im.train <- d.train$Image
d.train$Image <- NULL

# split the string, convert to verctor of strings and then convert to vector of integers
# and do this for each row
message("taking image string out of training data")
im.train <- foreach(im = im.train, .combine = rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

# repeat the same process for test images also
im.test <- foreach(im = d.test$Image, .combine = rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

# compute the mean for each column and build a matrix with number of rows is equal to 
# no of rows in test data and no of columns is equal to number of columns in training data
message("computing the mean for all the columns in training data set")
p <- matrix(data = colMeans(d.train, na.rm = T), nrow = nrow(d.test), ncol = ncol(d.train),
            byrow = T)

# column names from training data
colnames(p) <- names(d.train)

# create an data.frame containign ImageIds and 30 facial coordinates
predictions <- data.frame(ImageId = 1:nrow(d.test), p)

# according to expected submission we need one keyword in each row
submission <- melt(predictions, id.vars = "ImageId", variable.name = "FeatureName",
                   value.name = "Location")

# JOIN with the sample submission file to preserve the same order of entries
example.submission <- read.csv(paste0(data.dir, 'SampleSubmission.csv'))
# take names of sample submission file
sub.col.names <- names(example.submission)
# removing location column
example.submission$Location <- NULL
# merge imageids and submission file
submission <- merge(example.submission, submission, all.x = T, sort = F)

submission <- submission[ , sub.col.names]

write.csv(submission, file = "submission_means.csv", quote = F, row.names = F)

message("Done.")
