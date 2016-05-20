# Author: tnybny

# clear workspace
rm(list = ls(all = T))

# load required libraries
library(rgdal)          # for reading and writing tifs
library(ggplot2)        # for cut_number
library(rpart)          # for decision tree

# read the tif file using GDAL library
grd <- readGDAL("./ilk-3b-1024.tif")
data <- data.frame(grd[])

# read the training and test sets
train <- read.table("./ilk-tr-xy.txt", header = FALSE, sep = ",")
test <- read.table("./ilk-te-xy.txt", header = FALSE, sep = ",")

# name the columns better
colnames(train) = c("Id", "X", "Y", "Class")
colnames(test) = c("Id", "X", "Y")

# increase size of training set by considering 3x3 window around point
for(i in 1:27)
{
    x <- train[i, 2]
    y <- train[i, 3]
    cl <- train[i, 4]
    for(j in (x - 1):(x + 1))
    {
        for(k in (y - 1):(y + 1))
        {
            if((j != x) || (k != y))
            {
                train[nrow(train) + 1, 2:4] <- c(j, k, cl)
            }
        }
    }
}
trset <- train

# retrieve RGB values for x,y coordinates of the training data
for(i in 1:nrow(train))
{
    r = (train[i, 2] + 1) + train[i, 3] * 1024
    trset[i, 1:3] <- data[r, 1:3]
}
colnames(trset) <- c("band1", "band2", "band3", "Class")

# retrieve RGB values for x,y coordinates of the test data
teset <- test
for(i in 1:nrow(test))
{
    r = (test[i, 2] + 1) + test[i, 3] * 1024
    teset[i, 1:3] <- data[r, 1:3]
}
colnames(teset) <- c("band1", "band2", "band3")

# factor the class variable
trset$Class <- factor(trset$Class)

# decision tree model
DTmodel <- rpart(Class ~ ., data = trset, method = "class")
DTpred <- predict(DTmodel, teset, type = "class")
DTresult <- test
DTresult$Class <- DTpred

# read the ground truth of the test data in order to facilitate accuracy 
# calculation
gtruth <- read.table("./ilk-te-xy-gt.txt", header = FALSE, sep = ",")
gtruth[, 4] <- as.factor(gtruth[, 4])

# color each class with descriptive unique color
col = matrix(0, nrow = 5, ncol = 3)
col[1, ] = c(255, 32, 32) # red buildings
col[2, ] = c(255, 255, 0) # yellow roads
col[3, ] = c(64, 0, 128) # purple grass
col[4, ] = c(0, 255, 0) # green trees
col[5, ] = c(0, 0, 255) # blue water

# function to color the first 3 columns of a data frame given the class in 4th
colorize <- function(d)
{
    for(i in 1:3)
    {
        d[, i] <- rep(col[d[1, 4], i], nrow(d))
    }
    d
}

# decision tree model
DT <- predict(DTmodel, data[, 1:3], type = "class")
temp <- cbind(data[, 1:3], DT)
temp$ID <- seq.int(nrow(temp))
t <- split(temp, DT, drop = T)
t <- lapply(t, FUN = colorize)
x <- do.call("rbind", t)
x <- x[order(x$ID), ]
xWithLabels <- x
x <- x[, -c(4, 5)]
#grd@data <- x
#writeGDAL(grd, fname = "./DTtiff.tif")

extnbrs <- function(i, data)
{
    nbrs <- vector()
    # extracts the neighbors of a given pixel and returns their indices
    k <- (data[i, 4] - 1):(data[i, 4] + 1)
    l <- (data[i, 5] - 1):(data[i, 5] + 1)
    k <- k[k >= 0 & k <= 1023]
    l <- l[l >= 0 & l <= 1023]
    possNbrs <- expand.grid(k, l)
    nbrs <- apply(possNbrs, 1, function(d) (d[1] + 1) + 1024 * (1023 - d[2]))
    nbrs <- nbrs[nbrs != i] # remove own index
}

# try probabilistic label relaxation
data[, 4] <- data[, 4] - 394530.5 # bring to 0-1023 range
data[, 5] <- data[, 5] - 4618982.5
nbrs <- lapply(seq.int(nrow(data)), extnbrs, data)

labels <- xWithLabels[, 4]

# label relaxation criteria
weights <- matrix(0, nrow = 5, ncol = 5)
weights[1, ] <- c(0, 0.8, 0.6, 0.7, 0.8) # buildings
weights[2, ] <- c(0.7, 0, 1, 1, 0.6) # roads
weights[3, ] <- c(1, 1, 0, 1, 0.7) # grass
weights[4, ] <- c(0.8, 0.8, 0.6, 0, 0.4) # trees
weights[5, ] <- c(0.7, 0.9, 1, 0.6, 0) # water
# row of weights represents thresholds for current class to change to others
# % of neighbors at least should have a common label to change
# more restrictive for water - 100%

repeat{
    oldlabels <- labels
    for(i in seq.int(length(nbrs)))
    {
        freqs <- as.data.frame(table(labels[nbrs[[i]]]))$Freq
        maxIdx <- which.max(freqs)
        if(labels[i] != maxIdx) # are the labels different?
        {
            if(freqs[maxIdx] >=
               length(nbrs[[i]]) * weights[labels[i], maxIdx]) # does the label need changing?
            {
                labels[i] <- maxIdx
            }
        }
    }
    print(s <- sum(oldlabels != labels))
    if(s < 5)
        break
}

xWithLabels_d <- xWithLabels
xWithLabels_d[, 4] <- labels
# recolor
t_d <- split(xWithLabels_d, labels, drop = T)
t_d <- lapply(t_d, FUN = colorize)
x_d <- do.call("rbind", t_d)
x_d <- x_d[order(x_d$ID), ]
x_d <- x_d[, -c(4, 5)]
grd@data <- x_d
writeGDAL(grd, fname = "./DTrelaxed.tif")
