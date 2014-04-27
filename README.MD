The script create a tidy data of the train and test data for only features related to the mean or standard deviation. 
Additionally, a data set containing the average of each of these variables for each subject and activity.
Steps:
1. Read files X_test,X_train,Subject_test,Subject_train,features and activity_lables text files into dataframes.
2. Column bind Dataframes X_test,X_train and add futureframe columns' name -> tidy
3. Subset tidy rows related to features with mean and std
4. Read files y_test,y_train.
5. Column bind Dataframes y_test,y_train and activity_lables and change values -> y_tidy
6. Column bind y_tidy and tidy to obtain the data set
7. Appropriately labels the data set with descriptive activity names
8. use write.table to get a text file from the above tidy dataframe.
9. make an independent tidy data set with the average of each variable for each activity and each subject. -> tidy_final
10. use write.table to get a text file from the above tidy_final dataframe


library(data.table) mandatory for the script