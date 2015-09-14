# Source of data for this project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# This R script does the following:

# 1.Data set have been created by merging the training and the test data sets that are data set A, B and C.

sementara1 <- read.table("train/X_train.txt")
sementara2 <- read.table("test/X_test.txt")
A <- rbind(sementara1, sementara2)

sementara1 <- read.table("train/subject_train.txt")
sementara2 <- read.table("test/subject_test.txt")
B <- rbind(sementara1, sementara2)

sementara1 <- read.table("train/y_train.txt")
sementara2 <- read.table("test/y_test.txt")
C <- rbind(sementara1, sementara2)

# 2. Measurements that have value of mean and standard deviation have been extracted.

ciri <- read.table("features.txt")
ciri_baik <- grep("-mean\\(\\)|-std\\(\\)", ciri[, 2])
A <- A[, ciri_baik]
names(A) <- ciri[ciri_baik, 2]
names(A) <- gsub("\\(|\\)", "", names(A))
names(A) <- tolower(names(A))

# 3. Name the activities in the data set.

aktiviti <- read.table("activity_labels.txt")
aktiviti[, 2] = gsub("_", "", tolower(as.character(aktiviti[, 2])))
C[,1] = aktiviti[C[,1], 2]
names(C) <- "activity"

# 4. Labels the data set with activity names.

names(B) <- "subject"
bersih <- cbind(B, C, A)
write.table(bersih, "Data_bersih.txt")

# 5. An independent cleaned data set with the average of each variable for each activity and each subject have been created.

SubjekUnik = unique(B)[,1]
bilSubjek = length(unique(B)[,1])
bilAktiviti = length(aktiviti[,1])
bilLajur = dim(bersih)[2]
keputusan = bersih[1:(bilSubjek*bilAktiviti), ]

row = 1
for (s in 1:bilSubjek) {
    for (a in 1:bilAktiviti) {
        keputusan[row, 1] = SubjekUnik[s]
        keputusan[row, 2] = aktiviti[a, 2]
        sementara <- bersih[bersih$subject==s & bersih$activity==aktiviti[a, 2], ]
        keputusan[row, 3:bilLajur] <- colMeans(sementara[, 3:bilLajur])
        row = row+1
    }
}
write.table(keputusan, "set_data_dengan_purata.txt")
