#---------------- read data ---------------------

data.dir <- 'C:/data/'
train.file <- paste0(data.dir, 'training.csv')
test.file <- paste0(data.dir, 'test.csv')

d.train <- read.csv(train.file, stringsAsFactors=F)
# Compactly Display the Structure of an Arbitrary R Object
str(d.train)
# ?str
head(d.train)

#---------------- trim data ---------------------

# Unfortunately the rightmost column is quite long, 
# so the output is not very readable.
# Save the column as another variable, and remove it from d.train.
im.train <- d.train$Image

#  Assigning NULL to a column removes it from the dataframe.
d.train$Image <- NULL

head(d.train)

#----------------- trim data 2: Image -----------------

# For each image (i.e. in each row) it contains a long string of numbers,
# where each number represents the intensity of a pixel in the image.
im.train[1] # First value in the column

# To analyze these further, we convert these strings to integers 
# by splitting them and converting the result to integer.

# strsplit: splits the string, 
# unlist: simplifies its output to a vector of strings 
# as.integer: converts it to a vector of integers.

as.integer(unlist(strsplit(im.train[1], " ")))

# Iterate through each row in im.train 
# and apply the string to integers conversion above.
# We can therefore use a multi core approach using a new library.

library(foreach)
im.train <- foreach(im=im.train, .combine=rbind) %do% {
	as.integer(unlist(strsplit(im, " ")))
}
str(im.train)

# Repeat the process for test.csv.
d.test  <- read.csv(test.file, stringsAsFactors=F)
im.test <- foreach(im = d.test$Image, .combine=rbind) %do% {
    as.integer(unlist(strsplit(im, " ")))
}
d.test$Image <- NULL

#------------------- save data ---------------------

# Save the data as a R data file at this point, 
# so no need to repeat this process again

save(d.train, im.train, d.test, im.test, file='data.Rd')

# load data
# load('data.Rd')

#------------------- visualize --------------------

# To visualize each image, we thus need to first convert 
# these 9216 integers into a 96x96 matrix

im <- matrix(data=rev(im.train[1,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))

# check1
points(96-d.train$nose_tip_x[1],         96-d.train$nose_tip_y[1],         col="red")
points(96-d.train$left_eye_center_x[1],  96-d.train$left_eye_center_y[1],  col="blue")
points(96-d.train$right_eye_center_x[1], 96-d.train$right_eye_center_y[1], col="green")

# check2
for(i in 1:nrow(d.train)) {
    points(96-d.train$nose_tip_x[i], 96-d.train$nose_tip_y[i], col="red")
}

# check3
idx <- which.max(d.train$nose_tip_x) # Easter egg for min value.
im  <- matrix(data=rev(im.train[idx,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))
points(96-d.train$nose_tip_x[idx], 96-d.train$nose_tip_y[idx], col="red")

#--------------- simple benchmark ------------------



# TBD
