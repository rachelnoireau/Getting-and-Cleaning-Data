
run_analysis <- function(){
  #get labels
  coltest <- read.table("C:\\Users\\rnoireau\\Downloads\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\Y_test.txt", h = FALSE)
  coltrain <- read.table("C:\\Users\\rnoireau\\Downloads\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\Y_train.txt", h = FALSE)
  col <- rbind(coltrain, coltest)
  
  #extract the data
  test <- read.table("C:\\Users\\rnoireau\\Downloads\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\X_test.txt", h = FALSE)
  train <- read.table("C:\\Users\\rnoireau\\Downloads\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\X_train.txt", h = FALSE)
  
  #merge the 2 data sets
  bigSet <- rbind(train, test)
  
  #add features labels
  features <- read.table("C:\\Users\\rnoireau\\Downloads\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\features.txt", h = FALSE)
  names(bigSet) <- features$V2
  
  #extract only means and standart derivation
  keep_info <- grep("*mean|std*",features$V2)
  new_data <- bigSet[, keep_info]
  new_data <- cbind(new_data, col)
  
  #get label of activities
  label <- read.table("C:\\Users\\rnoireau\\Downloads\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\activity_labels.txt", h = FALSE)
  
  #put good labels on data set
  new_data <- merge(new_data, label, by = "V1")

  #give name to variables
  colnames(new_data)[1] <- "id activity"
  colnames(new_data)[81] <- "activity"
  
  #read the subject
  subtest <- read.table("C:\\Users\\rnoireau\\Downloads\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\subject_test.txt", h = FALSE)
  subtrain <- read.table("C:\\Users\\rnoireau\\Downloads\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\subject_train.txt", h = FALSE)
  sub <- rbind(subtrain, subtest)
  
  #add subject
  datawithsub <- cbind(new_data, sub)
  datawithsub <- datawithsub[, !colnames(datawithsub)== "id activity"]
  
  #group by subject and activity
  library(dplyr)
  datawithsub <- datawithsub %>%
    group_by(V1, activity)  %>%
    summarise_all(mean)
  #apply(datawithsub[, 2:80],1, 
  
  #give names to variables
  colnames(datawithsub)[1] <- "subject"
  
  #export data sets
  write.table(new_data,"means_of_sport_session.txt", row.names = FALSE)
  write.table(datawithsub,"means_for_each_subject.txt", row.names = FALSE)
  
}
