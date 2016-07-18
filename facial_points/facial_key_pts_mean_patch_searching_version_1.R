# Libraries: using reshape2 library
library(doMC)
registerDoMC()
library(reshape2)

# location of training and test data
# parameters
data.dir    <- '~/jitendra/kaggle/facial_points/'
patch_size  <- 10 # no of pixels we are going to extract in each direction
search_size <- 2


# specify training and test data directories
# parameters
data.dir <- '~/jitendra/kaggle/facial_points/'
train.file <- paste0(data.dir, 'training.csv')
test.file <- paste0(data.dir, 'test.csv')
data.file  <- paste0(data.dir, 'data.Rd')

# read data from csv files
message("reading data from csv files...")
d.train <- read.csv(train.file, stringsAsFactors = F)
# read the test csv 
d.test <- read.csv(test.file, stringsAsFactors = F)

# split the string, convert to verctor of strings and then convert to vector of integers
# and do this for each row
im.train   <- foreach(im = d.train$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

# do the same processing for test file also
im.test    <- foreach(im = d.test$Image, .combine=rbind) %dopar% {
	as.integer(unlist(strsplit(im, " ")))
}

# set them NULL as not required
d.train$Image <- NULL
d.test$Image  <- NULL


# list the coordinates we have to predict
# grep all the 15 x coordinate column names and replace _x from the name
coordinate.names <- gsub("_x", "", names(d.train)[grep("_x", names(d.train))])


# for each one, compute the average patch
mean.patches <- foreach(coord = coordinate.names) %dopar% {
	cat(sprintf("computing mean patch for %s\n", coord))
	# add x or y to the coordinate name to convert it to corresponding one
  coord_x <- paste(coord, "x", sep="_")
	coord_y <- paste(coord, "y", sep="_")

	# compute average patch
	patches <- foreach (i = 1:nrow(d.train), .combine=rbind) %do% {
		# get the i the image in a matrix form in im variable
	  im  <- matrix(data = im.train[i,], nrow=96, ncol=96)
		# x coordinate for current facial point and ith image
	  x   <- d.train[i, coord_x]
	  # y cooridanate for current facial point and ith image
		y   <- d.train[i, coord_y]
		x1  <- (x-patch_size)
		x2  <- (x+patch_size)
		y1  <- (y-patch_size)
		y2  <- (y+patch_size)
		# check if the coordinates are available and inside the image and not NA
		if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
		{
			as.vector(im[x1:x2, y1:y2])
		}
		else
		{
			NULL
		}
	}
	# calculate mean column wise for each coordinate using all image patches
	matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1)
}

# for each coordinate and for each test image, find the position that best correlates with the average patch
p <- foreach(coord_i = 1:length(coordinate.names), .combine=cbind) %dopar% {
	# the coordinates we want to predict
	coord   <- coordinate.names[coord_i]
	coord_x <- paste(coord, "x", sep="_")
	coord_y <- paste(coord, "y", sep="_")

	# the average of them in the training set (our starting point)
	mean_x  <- mean(d.train[, coord_x], na.rm=T)
	mean_y  <- mean(d.train[, coord_y], na.rm=T)

	# search space: 'search_size' pixels centered on the average coordinates 
	x1 <- as.integer(mean_x)-search_size
	x2 <- as.integer(mean_x)+search_size
	y1 <- as.integer(mean_y)-search_size
	y2 <- as.integer(mean_y)+search_size

	# ensure we only consider patches completely inside the image
	# images are 96 x 96
	x1 <- ifelse(x1-patch_size<1,  patch_size+1,  x1)
	y1 <- ifelse(y1-patch_size<1,  patch_size+1,  y1)
	x2 <- ifelse(x2+patch_size>96, 96-patch_size, x2)
	y2 <- ifelse(y2+patch_size>96, 96-patch_size, y2)

	# build a list of all positions to be tested
	params <- expand.grid(x = x1:x2, y = y1:y2)

	# for each image...
	r <- foreach(i = 1:nrow(d.test), .combine=rbind) %do% {
		if (( coord_i == 1) && (( i %% 100 ) == 0 )) { 
		  cat(sprintf("%d/%d\n", i, nrow(d.test))) 
		}
		
	  # get the test image
	  im <- matrix(data = im.test[i,], nrow=96, ncol=96)

		# ... compute a score for each position ...
		r  <- foreach(j = 1:nrow(params), .combine=rbind) %do% {
			x     <- params$x[j]
			y     <- params$y[j]
			
			# create patch vector around this x and y coordinate
			p     <- im[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)]
			
			# calculate the score between p and mean patch vector
			score <- cor(as.vector(p), as.vector(mean.patches[[coord_i]]))
			
			# if na then make score zero
			score <- ifelse(is.na(score), 0, score)
			
			data.frame(x, y, score)
		}

		# ... and return the best
		best <- r[which.max(r$score), c("x", "y")]
	}
	names(r) <- c(coord_x, coord_y)
	r
}

# prepare file for submission
predictions        <- data.frame(ImageId = 1:nrow(d.test), p)
submission         <- melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
example.submission <- read.csv(paste0(data.dir, 'submissionFileFormat.csv'))
sub.col.names      <- names(example.submission)
example.submission$Location <- NULL

submission <- merge(example.submission, submission, all.x=T, sort=F)
submission <- submission[, sub.col.names]

write.csv(submission, file="submission_mean_patch_search.csv", quote=F, row.names=F)

