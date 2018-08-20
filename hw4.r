setwd('D:/CS498/HW4 - KMEANS/')

raw_data<-read.csv('data.txt', header=FALSE)
raw_data_2 <- read.csv('data.txt', header=FALSE,sep='\t')
raw_data_2
as.character(raw_data[[1]][1])
strsplit(as.character(raw_data[[1]][1]),'\t')
header_labels <- strsplit(as.character(raw_data[[1]][1]),'\t')
header_labels <- as.factor(unlist(header_labels))
length(header_labels)
main_data <- array(NA, dim = c(26, 10))
for(x in 2:27)
{
  curr_data_point <- strsplit(as.character(raw_data[[1]][x]),'\t')
  unlist(curr_data_point)
  main_data[x-1,1:10] <- unlist(curr_data_point)
  
}
main_data <- as.data.frame(main_data)
#part 1
#Use an agglomerative clusterer to cluster this data. 
#Produce a dendrogram of this data for each of single link, 
#complete link, and group average clustering. 
#You should label the countries on the axis.
#use hclust to make phylogenetic tree
#

library(cluster)
dim(raw_data_2)
#change method from "single" to "complete" to "average" to get all 3 different dendograms
#single link dendogram
single_agn <- agnes(x=main_data[1:26,2:10], diss = FALSE,method = "single")
plot(single_agn,labels=raw_data_2[2:27,1],sub = paste("Agglomerative Coefficient = ",round(single_agn$ac, digits = 2)),main="single link dendrogram plot")
main_data[1:26,2:10]

#complete link dendogram
complete_agn <- agnes(x=main_data[1:26,2:10], diss = FALSE, method = "complete")
plot(complete_agn,labels=raw_data_2[2:27,1],sub = paste("Agglomerative Coefficient = ",round(complete_agn$ac, digits = 2)),main="complete link dendrogram plot")

#average link dendogram
average_agn <- agnes(x=main_data[1:26,2:10], diss = FALSE,method = "average")
plot(average_agn,labels=raw_data_2[2:27,1],sub= paste("Agglomerative Coefficient = ",round(average_agn$ac, digits = 2)),main="average link dendrogram plot")




#kmeans part
library(cluster)

#http://www.michaeljgrogan.com/kmeans-wss-clustering/
wss <- (nrow(main_data[1:26,2:10])-1)*sum(apply(main_data[1:26,2:10],2,var))
for (i in 2:25) wss[i] <- sum(kmeans(main_data[1:26,2:10],centers=i)$withinss)
plot(1:25, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
k_means_cluster <- kmeans(main_data[1:26,2:10],centers=8)
k_means_cluster$center

clusplot(main_data[1:26,2:10],k_means_cluster$cluster, color=TRUE,shade=TRUE,labels=0,lines=0)
with(raw_data_2[1:27,2:10], pairs(main_data[1:26,2:10], col=c(1:8)[k_means_cluster$cluster])) 



#part 2
#(a)
#Build a classifier that classifies sequences into one of the 14
#activities pro-vided. To make features, you should vector quantize, then use
#a histogram of cluster centers (as described in the subsection; this gives a pretty 
#ex-plicit set of steps to follow). You will find it helpful to use hierarchical
#k-means to vector quantize. You may use whatever multi-class classifier
#you wish, though I'd start with R's decision forest, because
#it's easy to use and effective. You should report 
#   (a) the total error rate and 
#   (b) the class confusion matrix of your classifier.


#(b)Now see if you can improve your classifier by 
#   (a) modifying the number of cluster centers in your hierarchical k-means and 
#   (b) modifying the size of the fixed length samples that you use


#data setup
setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Brush_teeth')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}
length(dataset_temp)
idx <- 1
new_idx <- 1
brush_teeth_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  brush_teeth_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}

setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Climb_stairs')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}

idx <- 1
new_idx <- 1
climb_stairs_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  climb_stairs_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}

setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Comb_hair')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}

idx <- 1
new_idx <- 1
comb_hair_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  comb_hair_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}



setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Descend_stairs')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}

idx <- 1
new_idx <- 1
descend_stairs_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  descend_stairs_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}

setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Drink_glass')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}

idx <- 1
new_idx <- 1
drink_glass_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  drink_glass_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}


setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Eat_meat')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}

idx <- 1
new_idx <- 1
eat_meat_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  eat_meat_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}

setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Eat_soup')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}

idx <- 1
new_idx <- 1
eat_soup_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  eat_soup_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}


setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Getup_bed')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}

idx <- 1
new_idx <- 1
getup_bed_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  getup_bed_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}

setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Liedown_bed')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}

idx <- 1
new_idx <- 1
liedown_bed_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  liedown_bed_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}

setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Pour_water')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}

idx <- 1
new_idx <- 1
pour_water_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  pour_water_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}

setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Sitdown_chair')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}

idx <- 1
new_idx <- 1
sitdown_chair_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  sitdown_chair_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}

setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Standup_chair')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}

idx <- 1
new_idx <- 1
standup_chair_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  standup_chair_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}

setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Walk')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}

idx <- 1
new_idx <- 1
walk_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  walk_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}

setwd('D:/CS498/HW4 - KMEANS/HMP_Dataset/Use_telephone')
file_list <- list.files()

dataset_temp <- c()
for (file in file_list)
{
  dataset_temp <- c(dataset_temp, read.table(file, header=TRUE, sep=" "))
}

idx <- 1
new_idx <- 1
use_telephone_files <- list()
while(idx < length(dataset_temp)+1)
{
  curr_file <- array(NA,dim=c(length(dataset_temp[[idx]]), 3))
  dim(curr_file)
  curr_file[,1] <- dataset_temp[[idx]]
  curr_file[,2] <- dataset_temp[[idx+1]]
  curr_file[,3] <- dataset_temp[[idx+2]]
  use_telephone_files[[new_idx]] <- as.data.frame(curr_file)
  idx <- idx+3
  new_idx <- new_idx+1
}

file_data <- list()
file_data[[1]] <- c("brush_teeth", brush_teeth_files)
file_data[[2]] <- c("climb_stairs", climb_stairs_files)
file_data[[3]] <- c("comb_hair", comb_hair_files)
file_data[[4]] <- c("descend_stairs", descend_stairs_files)
file_data[[5]] <- c("drink_glass", drink_glass_files)
file_data[[6]] <- c("eat_meat", eat_meat_files)
file_data[[7]] <- c("eat_soup", eat_soup_files)
file_data[[8]] <- c("getup_bed", getup_bed_files)
file_data[[9]] <- c("liedown_bed", liedown_bed_files)
file_data[[10]] <- c("pour_water", pour_water_files)
file_data[[11]] <- c("sitdown_chair", sitdown_chair_files)
file_data[[12]] <- c("standup_chair", standup_chair_files)
file_data[[13]] <- c("use_telephone", use_telephone_files)
file_data[[14]] <- c("walk", walk_files)

#divide up files in each activity to be training/testing data 80/20
training_data <- list()
training_data[[1]] <- file_data[[1]][1:10]
training_data[[2]] <- file_data[[2]][1:82]
training_data[[3]] <- file_data[[3]][1:25]
training_data[[4]] <- file_data[[4]][1:34]
training_data[[5]] <- file_data[[5]][1:81]
training_data[[6]] <- file_data[[6]][1:5]
training_data[[7]] <- file_data[[7]][1:3]
training_data[[8]] <- file_data[[8]][1:80]
training_data[[9]] <- file_data[[9]][1:23]
training_data[[10]] <- file_data[[10]][1:81]
training_data[[11]] <- file_data[[11]][1:81]
training_data[[12]] <- file_data[[12]][1:82]
training_data[[13]] <- file_data[[13]][1:11]
training_data[[14]] <- file_data[[14]][1:81]
test_data <- list()
test_data[[1]] <- file_data[[1]][c(1,11:13)]
test_data[[2]] <- file_data[[2]][c(1,83:103)]
test_data[[3]] <- file_data[[3]][c(1,26:32)]
test_data[[4]] <- file_data[[4]][c(1,35:43)]
test_data[[5]] <- file_data[[5]][c(1,82:101)]
test_data[[6]] <- file_data[[6]][c(1,6)]
test_data[[7]] <- file_data[[7]][c(1,4)]
test_data[[8]] <- file_data[[8]][c(1,81:102)]
test_data[[9]] <- file_data[[9]][c(1,24:29)]
test_data[[10]] <- file_data[[10]][c(1,82:101)]
test_data[[11]] <- file_data[[11]][c(1,82:101)]
test_data[[12]] <- file_data[[12]][c(1,83:103)]
test_data[[13]] <- file_data[[13]][c(1,12:14)]
test_data[[14]] <- file_data[[14]][c(1,82:101)]


#32 segmented data is 10432x97 for training data
#64 segmented data is 5045x193
ordered_segmented_data <- array(NA, dim=c(10432,97))
num_segments <- 0
for(i in 1:14)
{
  curr_file <- training_data[[i]]
  for(j in 2:length(training_data[[i]]))
  {
    segment_length <- 32 #32
    s_idx <- 1
    curr_activity <- training_data[[i]][j][[1]]
    temp_xyz <- array(NA,dim=c(1,97)) #97
    while(s_idx+segment_length <= nrow(curr_activity))
    {
      num_segments <- num_segments + 1
      temp_xyz[1] <- training_data[[i]][[1]]
      temp_xyz[2:33] <- as.numeric(curr_activity[s_idx:(s_idx+segment_length-1),1]) #2:33
      temp_xyz[34:65] <- as.numeric(curr_activity[s_idx:(s_idx+segment_length-1),2]) #34:65
      temp_xyz[66:97] <- as.numeric(curr_activity[s_idx:(s_idx+segment_length-1),3]) #66:97
      if(is.na(temp_xyz))
      {
        num_segments <- num_segments-1
        continue
      }
      ordered_segmented_data[num_segments,] <- temp_xyz
      s_idx <- s_idx + segment_length
      
    }
  }
}
num_segments
ordered_segmented_data

segmented_data <- ordered_segmented_data[sample(nrow(ordered_segmented_data)),]
segmented_data

#find a good k value for the vector quantized data
wss_accel <- (nrow(segmented_data[,-1])-1)*sum(apply(segmented_data[,-1],2,var))
for (i in 2:50) wss_accel[i] <- sum(kmeans(segmented_data[,-1],centers=i,iter.max=30)$withinss)
plot(1:50, wss_accel, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#k = 30
#create the histogram
final_kmeans <- kmeans(segmented_data[,-1],centers=30,iter.max=30)
centers <- final_kmeans$centers


#665 training activities, one histogram created for each
train_histograms <- array(NA, dim=c(665,30))
curr_hist_idx <- 1
train_labels <- array(NA,dim=c(665))
for(i in 1:14) #for each activity
{
  curr_file <- training_data[[i]]
  for(j in 2:length(training_data[[i]])) #for each file
  {
    segment_length <- 32 #32
    s_idx <- 1
    curr_activity <- training_data[[i]][j][[1]] #curr file
    segments_num <- 0
    if(((nrow(curr_activity)/32) %% 1) == 0)  #32
      segments_num <- (nrow(curr_activity)/32)-1  #32
    else
      segments_num <- floor(nrow(curr_activity)/32)  #32
    act_into_segments <- array(NA,dim=c(segments_num,97))  #97
    temp_xyz <- array(NA,dim=c(1,97))  #97
    num_segments <- 0
    while(s_idx+segment_length <= nrow(curr_activity))
    {
      num_segments <- num_segments + 1
      temp_xyz[1] <- training_data[[i]][[1]]
      temp_xyz[2:33] <- as.numeric(curr_activity[s_idx:(s_idx+segment_length-1),1])
      temp_xyz[34:65] <- as.numeric(curr_activity[s_idx:(s_idx+segment_length-1),2])
      temp_xyz[66:97] <- as.numeric(curr_activity[s_idx:(s_idx+segment_length-1),3])
      if(is.na(temp_xyz))
      {
        num_segments <- num_segments-1
        continue
      }
      temp_xyz
      act_into_segments[num_segments,] <- temp_xyz
      s_idx <- s_idx + segment_length
      
    }
    act_into_segments
    curr_train_label <- act_into_segments[1,1]
    curr_train_data <- act_into_segments[,-1]
    train_hist <- array(0,dim=c(30))
    train_labels[curr_hist_idx] <- curr_train_label
    for(x in 1:nrow(curr_train_data))
    {
      temp_act_data <- as.numeric(curr_train_data[x,])
      min_dist <- sqrt(sum((temp_act_data-centers[1,])^2))
      curr_c <- 1
      for(c in 1:30)
      {
        curr_dist <- sqrt(sum((temp_act_data-centers[c,])^2))
        if(curr_dist < min_dist)
        {
          min_dist <- curr_dist
          curr_c <- c
        }
      }
      train_hist[curr_c] <- train_hist[curr_c]+1
    }
    train_histograms[curr_hist_idx,] <-train_hist
    curr_hist_idx <- curr_hist_idx+1
    
  }
}


#plots of some example file histograms from training data
plot(train_histograms[1,],main=train_labels[1])
plot(train_histograms[2,],main=train_labels[2])
plot(train_histograms[3,],main=train_labels[3])
plot(train_histograms[10,],main=train_labels[10])
plot(train_histograms[100,],main=train_labels[100])

#174 test activities
test_histograms <- array(NA, dim=c(174,30))
curr_hist_idx <- 1
test_labels <- array(NA,dim=c(174))
for(i in 1:14) #for each activity
{
  curr_file <- test_data[[i]]
  for(j in 2:length(test_data[[i]])) #for each file
  {
    segment_length <- 32 #32
    s_idx <- 1
    curr_activity <- test_data[[i]][j][[1]] #curr file
    segments_num <- 0
    if(((nrow(curr_activity)/32) %% 1) == 0)
      segments_num <- (nrow(curr_activity)/32)-1
    else
      segments_num <- floor(nrow(curr_activity)/32)
    act_into_segments <- array(NA,dim=c(segments_num,97)) #97
    temp_xyz <- array(NA,dim=c(1,97))
    num_segments <- 0
    while(s_idx+segment_length <= nrow(curr_activity))
    {
      num_segments <- num_segments + 1
      temp_xyz[1] <- test_data[[i]][[1]]
      temp_xyz[2:33] <- as.numeric(curr_activity[s_idx:(s_idx+segment_length-1),1]) #2:33
      temp_xyz[34:65] <- as.numeric(curr_activity[s_idx:(s_idx+segment_length-1),2]) #34:65
      temp_xyz[66:97] <- as.numeric(curr_activity[s_idx:(s_idx+segment_length-1),3]) #66:97
      if(is.na(temp_xyz))
      {
        num_segments <- num_segments-1
        continue
      }
      temp_xyz
      act_into_segments[num_segments,] <- temp_xyz
      s_idx <- s_idx + segment_length
      
    }
    act_into_segments
    curr_test_label <- act_into_segments[1,1]
    curr_test_data <- act_into_segments[,-1]
    test_hist <- array(0,dim=c(30))
    test_labels[curr_hist_idx] <- curr_test_label
    for(x in 1:nrow(curr_test_data))
    {
      temp_act_data <- as.numeric(curr_test_data[x,])
      min_dist <- sqrt(sum((temp_act_data-centers[1,])^2))
      curr_c <- 1
      for(c in 1:30)
      {
        curr_dist <- sqrt(sum((temp_act_data-centers[c,])^2))
        if(curr_dist < min_dist)
        {
          min_dist <- curr_dist
          curr_c <- c
        }
      }
      test_hist[curr_c] <- test_hist[curr_c]+1
    }
    test_histograms[curr_hist_idx,] <-test_hist
    curr_hist_idx <- curr_hist_idx+1
  
  }
}


library(class)
#1-nearest neighbor
nearest_neighbor <- knn(train=train_histograms,test=test_histograms,cl=factor(train_labels), k = 1,use.all=TRUE)
nearest_neighbor

#random forest
rand_forest <- randomForest(x=train_histograms, y=factor(train_labels))
prediction <- predict(rand_forest,test_histograms)
prediction

#confusion matrix
cf <- confusionMatrix(data=prediction, test_labels)
cf
cf$table

#accuracy for random forest
num_correct <- 0
num_wrong <- 0
for(n in 1:174)
{
  if(as.character(prediction[n]) == test_labels[n])
    num_correct <- num_correct+1
  else
    num_wrong <- num_wrong+1
}
test_accuracy_rand_forest <- num_correct/(num_correct+num_wrong)
test_accuracy_rand_forest


#accuracy for nearest neighbor
num_correct <- 0
num_wrong <- 0
for(n in 1:174)
{
  if(as.character(nearest_neighbor[n]) == test_labels[n])
    num_correct <- num_correct+1
  else
    num_wrong <- num_wrong+1
}
test_accuracy_knn <- num_correct/(num_correct+num_wrong)
test_accuracy_knn







