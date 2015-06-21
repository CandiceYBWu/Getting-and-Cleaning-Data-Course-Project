# You should create one R script called run_analysis.R that does the following. 
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Read Data
train.set <- read.table("./UCI HAR Dataset/train/X_train.txt",head=F,stringsAsFactors=FALSE)
test.set <- read.table("./UCI HAR Dataset/test/X_test.txt",head=F,stringsAsFactors=FALSE)
dim(train.set);dim(test.set)

train.lab <- read.table("./UCI HAR Dataset/train/y_train.txt",head=F,stringsAsFactors=FALSE)
test.lab <- read.table("./UCI HAR Dataset/test/y_test.txt",head=F,stringsAsFactors=FALSE)
dim(train.lab);dim(test.lab)
names(train.lab) <- c("label_name")
names(test.lab) <- c("label_name")

act.lab <- read.table("./UCI HAR Dataset/activity_labels.txt",head=F,stringsAsFactors=FALSE)
feat <- read.table("./UCI HAR Dataset/features.txt",header=F,stringsAsFactors=FALSE)
dim(act.lab); dim(feat)
names(act.lab) <- c("label_name","activity_name")
names(feat) <- c("var_id","feature_name")


## Merge the traing and test sets and create one data set
## Uses descriptive activity names to name the activities in the data set
train.l <- cbind(train.set,train.lab)
test.l <- cbind(test.set,test.lab)
dat <- rbind(train.l,test.l)
names(dat) <- c(feat[,2],"label_name")

library(plyr)
dat.mg <- join(dat,act.lab,by="label_name",type="left",match="all")


## Extract the measurements on the mean and standard deviation
## Appropriately labels the data set with descriptive variable names
dat.mean <- dat.mg[,c(grep("mean()",names(dat.mg),value=FALSE),563)]
dat.mean <- dat.mean[,-c(grep("meanFreq()",names(dat.mean),value=FALSE))]
dat.sd <- dat.mg[,c(grep("std()",names(dat.mg),value=FALSE),563)]
dim(dat.mean);dim(dat.sd)
names(dat.mean);names(dat.sd)
str(dat.mean)
head(dat.mean)

###########################
########################### mean

dat.mean_t <- dat.mean[,which(substr(names(dat.mean),1,1)=="t")]
dat.mean_f <- dat.mean[,which(substr(names(dat.mean),1,1)=="f")]
dat.mean_a <- as.data.frame(dat.mean[,-which(substr(names(dat.mean),1,1)=="f" | substr(names(dat.mean),1,1)=="t")])
names(dat.mean_a) <- "activity_name" 
dim(dat.mean);dim(dat.mean_t);dim(dat.mean_f);dim(dat.mean_a)

###########################

head(dat.mean_t)
dat.mean_t.BodyAcc <- dat.mean_t[,c(1:3,16)]
dat.mean_t.GravityAcc <- dat.mean_t[,c(4:6,17)]
dat.mean_t.BodyAccJerk <- dat.mean_t[,c(7:9,18)]
dat.mean_t.BodyGyro <- dat.mean_t[,c(10:12,19)]
dat.mean_t.BodyGyroJerk <- dat.mean_t[,c(13:15,20)]

names(dat.mean_t.BodyAcc) <- c("Mean_of_X_axial","Mean_of_Y_axial","Mean_of_Z_axial","Mean_of_Magnitude_Euclidean_norm")
names(dat.mean_t.GravityAcc) <- c("Mean_of_X_axial","Mean_of_Y_axial","Mean_of_Z_axial","Mean_of_Magnitude_Euclidean_norm")
names(dat.mean_t.BodyAccJerk) <- c("Mean_of_X_axial","Mean_of_Y_axial","Mean_of_Z_axial","Mean_of_Magnitude_Euclidean_norm")
names(dat.mean_t.BodyGyro) <- c("Mean_of_X_axial","Mean_of_Y_axial","Mean_of_Z_axial","Mean_of_Magnitude_Euclidean_norm")
names(dat.mean_t.BodyGyroJerk) <- c("Mean_of_X_axial","Mean_of_Y_axial","Mean_of_Z_axial","Mean_of_Magnitude_Euclidean_norm")

fea_nam_t <- as.data.frame(rep(c("BodyAcc","GravityAcc","BodyAccJerk","BodyGyro","BodyGyroJerk"),
               times=1,each=nrow(dat.mean_t)))
names(fea_nam_t) <- "Measurement"

###########################

names(dat.mean_f)
dat.mean_f.BodyAcc <- dat.mean_f[,c(1:3,10)]
dat.mean_f.BodyAccJerk <- dat.mean_f[,c(4:6,11)]
dat.mean_f.BodyGyro <- dat.mean_f[,c(7:9,12)]
names(dat.mean_f.BodyAcc) <- c("Mean_of_X_axial","Mean_of_Y_axial","Mean_of_Z_axial","Mean_of_Magnitude_Euclidean_norm")
names(dat.mean_f.BodyAccJerk) <- c("Mean_of_X_axial","Mean_of_Y_axial","Mean_of_Z_axial","Mean_of_Magnitude_Euclidean_norm")
names(dat.mean_f.BodyGyro) <- c("Mean_of_X_axial","Mean_of_Y_axial","Mean_of_Z_axial","Mean_of_Magnitude_Euclidean_norm")

fea_nam_f <- as.data.frame(rep(c("BodyAcc","BodyAccJerk","BodyGyro"),times=1,each=nrow(dat.mean_t)))
names(fea_nam_f) <- "Measurement"

###########################
########################### std

names(dat.sd)
dat.sd_t <- dat.sd[,which(substr(names(dat.sd),1,1)=="t")]
dat.sd_f <- dat.sd[,which(substr(names(dat.sd),1,1)=="f")]
dat.sd_a <- as.data.frame(dat.sd[,-which(substr(names(dat.sd),1,1)=="f" | substr(names(dat.sd),1,1)=="t")])
names(dat.sd_a) <- "activity_name" 
dim(dat.sd);dim(dat.sd_t);dim(dat.sd_f);dim(dat.sd_a)

###########################

names(dat.sd_t)
dat.sd_t.BodyAcc <- dat.sd_t[,c(1:3,16)]
dat.sd_t.GravityAcc <- dat.sd_t[,c(4:6,17)]
dat.sd_t.BodyAccJerk <- dat.sd_t[,c(7:9,18)]
dat.sd_t.BodyGyro <- dat.sd_t[,c(10:12,19)]
dat.sd_t.BodyGyroJerk <- dat.sd_t[,c(13:15,20)]

names(dat.sd_t.BodyAcc) <- c("Sd_of_X_axial","Sd_of_Y_axial","Sd_of_Z_axial","Sd_of_Magnitude_Euclidean_norm")
names(dat.sd_t.GravityAcc) <- c("Sd_of_X_axial","Sd_of_Y_axial","Sd_of_Z_axial","Sd_of_Magnitude_Euclidean_norm")
names(dat.sd_t.BodyAccJerk) <- c("Sd_of_X_axial","Sd_of_Y_axial","Sd_of_Z_axial","Sd_of_Magnitude_Euclidean_norm")
names(dat.sd_t.BodyGyro) <- c("Sd_of_X_axial","Sd_of_Y_axial","Sd_of_Z_axial","Sd_of_Magnitude_Euclidean_norm")
names(dat.sd_t.BodyGyroJerk) <- c("Sd_of_X_axial","Sd_of_Y_axial","Sd_of_Z_axial","Sd_of_Magnitude_Euclidean_norm")

###########################

names(dat.sd_f)
dat.sd_f.BodyAcc <- dat.sd_f[,c(1:3,10)]
dat.sd_f.BodyAccJerk <- dat.sd_f[,c(4:6,11)]
dat.sd_f.BodyGyro <- dat.sd_f[,c(7:9,12)]
names(dat.sd_f.BodyAcc) <- c("Sd_of_X_axial","Sd_of_Y_axial","Sd_of_Z_axial","Sd_of_Magnitude_Euclidean_norm")
names(dat.sd_f.BodyAccJerk) <- c("Sd_of_X_axial","Sd_of_Y_axial","Sd_of_Z_axial","Sd_of_Magnitude_Euclidean_norm")
names(dat.sd_f.BodyGyro) <- c("Sd_of_X_axial","Sd_of_Y_axial","Sd_of_Z_axial","Sd_of_Magnitude_Euclidean_norm")



## From the data set in step 4, creates a second, independent tidy data set with the 
## average of each variable for each activity and each subject

newdat.t1 <- cbind(dat.mean_a,dat.mean_t.BodyAcc,dat.sd_t.BodyAcc) 
newdat.t2 <- cbind(dat.mean_a,dat.mean_t.GravityAcc,dat.sd_t.GravityAcc) 
newdat.t3 <- cbind(dat.mean_a,dat.mean_t.BodyAccJerk,dat.sd_t.BodyAccJerk) 
newdat.t4 <- cbind(dat.mean_a,dat.mean_t.BodyGyro,dat.sd_t.BodyGyro) 
newdat.t5 <- cbind(dat.mean_a,dat.mean_t.BodyGyroJerk,dat.sd_t.BodyGyroJerk) 
newdat.t1tot5 <- rbind(newdat.t1,newdat.t2,newdat.t3,newdat.t4,newdat.t5)
newdat.t6 <- cbind(fea_nam_t,newdat.t1tot5)

newdat.f1 <- cbind(dat.mean_a,dat.mean_f.BodyAcc,dat.sd_f.BodyAcc) 
newdat.f2 <- cbind(dat.mean_a,dat.mean_f.BodyAccJerk,dat.sd_f.BodyAccJerk) 
newdat.f3 <- cbind(dat.mean_a,dat.mean_f.BodyGyro,dat.sd_f.BodyGyro) 
newdat.f1tof3 <- rbind(newdat.f1,newdat.f2,newdat.f3)
newdat.f4 <- cbind(fea_nam_f,newdat.f1tof3)

dom_nam_f <- as.data.frame(rep(c("time","frequency"),times=c(nrow(newdat.t6),nrow(newdat.f4))))
names(dom_nam_f) <- "domain_signal"

newdat.tmp <- rbind(newdat.t6,newdat.f4)
newdat.tmp2 <- cbind(dom_nam_f,newdat.tmp)

newdat <- newdat.tmp2[,c(3,1:2,4:11)]



## Output the dataset

write.table(newdat,"./data/UCI_HAR_Dataset_New.txt",row.name=FALSE)























