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






TBD
