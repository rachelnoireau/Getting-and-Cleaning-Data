
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
  
  #compute means and standart deviation
  Means <- data.frame(apply(bigSet, 1, mean))
  bigSet2 <- bigSet * bigSet
  Meanof2 <- data.frame(apply(bigSet2, 1, mean))
  Means2 <- Means*Means
  tmp <- data.frame(Meanof2 - Means2)
  stdev <- apply(tmp, 1, sqrt)
  
  #new data set
  new_data <- cbind(Means, stdev,col)
  
  #get label of activities
  label <- read.table("C:\\Users\\rnoireau\\Downloads\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\activity_labels.txt", h = FALSE)
  
  #put good labels on data set
  new_data <- merge(new_data, label, by = "V1")

  #give name to variables
  names(new_data) <- c("id activity", "means", "standard deviation",  "activity")
  
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
    summarise(mean(means),
              mean(`standard deviation`))
  
  #give names to variables
  names(datawithsub) <- c("subject", "activity", "means of means", "means of standard deviation")

  #export data sets
  write.table(new_data,"means_of_sport_session.txt", row.names = FALSE)
  write.table(datawithsub,"means_for_each subject.txt", row.names = FALSE)
  
}
