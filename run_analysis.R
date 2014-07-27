x_test<-read.table("D:\\R\\UCI HAR Dataset\\test\\X_test.txt")
y_test<-read.table("D:\\R\\UCI HAR Dataset\\test\\y_test.txt")
subject_test<-read.table("D:\\R\\UCI HAR Dataset\\test\\subject_test.txt")
x_train<-read.table("D:\\R\\UCI HAR Dataset\\train\\x_train.txt")
y_train<-read.table("D:\\R\\UCI HAR Dataset\\train\\y_train.txt")
subject_train<-read.table("D:\\R\\UCI HAR Dataset\\train\\subject_train.txt")
features<-read.table("D:\\R\\UCI HAR Dataset\\features.txt")
labels<-read.table("D:\\R\\UCI HAR Dataset\\activity_labels.txt")

test<-cbind(y_test,subject_test,x_test)
train<-cbind(y_train,subject_train,x_train)

measure<-rbind(test,train)
colnames(measure)<-c("activity","subject",as.character(features$V2))

for(i in 1:nrow(measure))
{
  measure[i,"activity"]<-as.character(labels[labels$V1==measure[i,"activity"],2])
}

n1<-1:(6*ncol(measure)-1)
elm<-matrix(n1,6,ncol(measure)-1)
rs<-data.frame(elm)
colnames(rs)<-c("SubjectOrActivity",colnames(measure)[3:ncol(measure)])
acti<-levels(factor(measure$activity))

for(i in 1:6)
{
  rs[i,1]<-acti[i]
}
for(i in 1:6)
{
  for(j in 2:ncol(rs))
  {
    rs[i,j]<-mean(measure[measure$activity==acti[i],j+1])
  }
}

n2<-1:(30*ncol(measure)-1)
elm2<-matrix(n2,30,ncol(measure)-1)
rs2<-data.frame(elm2)
colnames(rs2)<-c("SubjectOrActivity",colnames(measure)[3:ncol(measure)])
for(i in 1:30)
{
  rs2[i,1]<-n2[i]
}
for(i in 1:30)
{
  for(j in 2:ncol(rs2))
  {
    rs2[i,j]<-mean(measure[measure$subject==i,j+1])
  }
}

result<-rbind(rs,rs2)

write.csv(result,"D:\\R\\UCI HAR Dataset\\Means.csv")