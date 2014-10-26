# run_analysis function that inputs Samsung data uncompressed from 
#	https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
# located in root.directory (or current working directory if you don't pass the parameter
# Output is a file that is a mean of all mean & std data grouped by Activity & Subject

run_analysis <- function(root.directory = ".") {


	# 1. Merges the training and the test sets to create one data set.

	## Define the types of data we are merging together
	file.types <- c("test", "train")

	## Load the X data
	all.data <- do.call(rbind, 
				lapply(sprintf("%s/%s/X_%s.txt", root.directory, file.types, file.types),
					read.table,
					sep = "",
					header = FALSE,
					na.strings="",
					stringsAsFactors=FALSE
				)
			)

	# 4. Appropriately labels the data set with descriptive variable names. 
	## Note that I am doing this out of sequence so that I can more easily load the Y & Subject data, and also makes step #2 easier
	## Set the labels for the X columns			
	names(all.data) <- read.table(sprintf("%s/features.txt", root.directory),
					sep = "",
					header = FALSE,
					na.strings="",
					stringsAsFactors=FALSE,
					colClasses=c("character")
				      )$V2

	# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

	## Again this step is out of order as it's easier to do this data extaction at this time
	all.data <- all.data[,grep("mean[(]|std[(]",names(all.data))]


	# 1. (continued)

	## Load the subject data, placing the new column in the front of the data frame
	all.data <- cbind(Subject=do.call(rbind,
					lapply(sprintf("%s/%s/subject_%s.txt", root.directory, file.types, file.types),
						read.table,
						sep = "",
						header = FALSE,
						na.strings="",
						stringsAsFactors=FALSE,
						colClasses=c("numeric")
						)
					)$V1, all.data)

	## Load the Y data (aka Activity Code), placing the new column in the front of the data frame
	all.data <- cbind(Activity=do.call(rbind, 
					lapply(sprintf("%s/%s/Y_%s.txt", root.directory, file.types, file.types),
						read.table,
						sep = "",
						header = FALSE,
						na.strings="",
						stringsAsFactors=FALSE,
						colClasses=c("numeric")
						)
					)$V1, all.data)


	# 3. Uses descriptive activity names to name the activities in the data set
	## Load Activity Labels from description file
	activity.labels <- read.table(sprintf("%s/activity_labels.txt", root.directory),
					sep = "",
					header = FALSE,
					na.strings="",
					stringsAsFactors=FALSE,
					colClasses=c("numeric","character"),
					col.names=c("ActivityID","ActivityLabel")
				      )

	## Convert the Activity numbers 1:6 to their descriptions from the file loaded above
	all.data$Activity <- activity.labels$ActivityLabel[all.data$Activity]
					

	# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

	## Calculate average for each column
	write.table(aggregate(. ~ Activity+Subject, all.data, mean), sprintf("%s/AllDataMeanByActivityAndSubject.txt", root.directory), row.names=FALSE)
}
