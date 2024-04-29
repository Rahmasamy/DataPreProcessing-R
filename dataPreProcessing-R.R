#train=read.csv("C:/Users/Owner/Downloads/graduation_project/train.csv")
#test=read.csv("C:/Users/Owner/Downloads/graduation_project/test.csv")
#sample_submission=read.csv("C:/Users/Owner/Downloads/graduation_project/sample_submission.csv")
#head(sample_submission)
library(ggplot2)
file=read.csv("C:/Users/Owner/Downloads/assigment1/data.csv", stringsAsFactors = FALSE)
#1. Show the first 10 rows and the last 10 rows.
# print first 10 rows 
file2<-file
print(file[1:10,])
# print last 10 rows 
d=dim(file)
# 2000 rows 12 features 
x<-d[1]-10
print(x)
y<-d[1]
print(y)
print(file[x:y,])
############################################################
#2.Using Date of Birth attribute, extracts the gender, average commuting time, and ancestry data for the oldest three. # convert file to data frame to make operations 
# convert date to vector  # Duplicate data
df2 <- file[order(file$dob),]
df3<-head(df2,3)
df3<-subset(df3, select = c("dob","gender", "ancestry","avg_commute"))
print(df3)
########################################################
#3.Identifies the gender, daily internet use, average commute time, ancestry,
#and diseases among those with more than two children.

file3<-file[,c("gender","daily_internet_use","avg_commute","ancestry","disease","children")]
subset(file3,children>2)
#######################################################
# 4.Using a table , indicate the number of rows that have any missing values
#and the number that do not.
# explore data 
print(sum(is.na(file))) # there is no null in all data wow o_o
rows<-rowSums(is.na(file))
rows2=rows[!rows==0]
rowsnot=rows[rows==0]
l1<-length(rows2)
l2<-length(rowsnot)
print(l1)
print(l2)
# another way 

table(rowSums(is.na(file)))

###########################################################
#5.Provide a summary of the data for each column, showing "Min, 1st Qu,
#Median Mean, 3rd Qu and Max" for each numerical column and the,
#Number of each Category for categorical data.

#id<-lapply(file,summary)
#print(id)
d<-file$gender
# table for each column to get count for each atrribute 
table(d)
gender<-unique(d)
print("the number for cateogrical data is ")
print(length(gender))

mat<-file$marital_status
print(file$marital_status)
table(file$marital_status)
matrialstat<-unique(mat)
print("the number of cateogrical for matrital status cateogry")
print(length(matrialstat))

emp<-file$employment_status
table(emp)
uniqueemp<-unique(emp)
print("the number of cateogrical for employement states cateogry")
print(length(uniqueemp))

ans<-file$ancestry
table(ans)
unians<-unique(ans)
print("the number of cateogrical for ancestry cateogry")
print(length(unians))

edu<-file$education
table(edu)
uniedu<-unique(edu)
print("the number of cateogrical for education cateogry")
print(length(uniedu))

dis<-file$disease
table(dis)
unidis<-unique(dis)
print("the number of cateogrical for diseases cateogry")
print(length(unidis))

idf<-file$id
table(idf)
uniid<-unique(idf)
print("the number of cateogrical for id cateogry")
print(length(uniid))

####
ch<-file$children
mea<-mean(ch)
mid<-median(ch)
res<-quantile(ch, probs = c(0.25,0.75))
minch<-min(ch)
print(res)
print(c(mea,mid,res,minch))

av<-file$avg_commute
mea_av<-mean(av)
mid_av<-median(av)
res_av<-quantile(av, probs = c(0.25,0.75))
minch_av<-min(av)

print(c(mea_av,mid_av,res_av,minch_av))

daily_int<-file$daily_internet_use
mea_int<-mean(daily_int)
mid_int<-median(daily_int)
res_int<-quantile(daily_int, probs = c(0.25,0.75),na.rm = TRUE)
minch_int<-min(daily_int)
print(c(mea_int,mid_int,res_int,minch_int))

zcode<-file$zipcode
mea_z<-mean(zcode)
mid_z<-median(zcode)
res_z<-quantile(zcode, probs = c(0.25,0.75))
minch_z<-min(zcode)
print(c(mea_z,mid_z,res_z,minch_z))

# another way to do it 
lapply(file,summary)

#######################################################
#6-Identify the columns that are having any missing values, and then remove
#any rows where all of the columns have missing values.
is.na(file)
colSums(is.na(file))
which(colSums(is.na(file))>0)
n<-names(which(colSums(is.na(file))>0))
print(n)
print(file[rowSums(is.na(file))<ncol(file),])


#####################################################
#7. Show the average daily usage of the internet for each level of education
# bachelors
buch<-subset(file,education=="bachelors")
print(buch)
print(mean(buch$daily_internet_use))
####
ph<-subset(file,education=="phd/md")
print(ph)
print(mean(ph$daily_internet_use))
####
mast<-subset(file,education=="masters" )
print(mast)
print(mean(mast$daily_internet_use))
####
highsc<-subset(file,education== "highschool" )
print(highsc)
print(mean(highsc$daily_internet_use))
#####
phM<-subset(file,education== "phD/MD" )
print(phM)
print(mean(phM$daily_internet_use))
#####
highscool<-subset(file,education==  "highscool"  )
print(highscool)
print(mean(highscool$daily_internet_use))
#another way to do it 
t<-tapply(file$daily_internet_use,file$education,mean)
print(t)
###################################################################
# 8.show children count using histogram 
hist(file$children,col=3)
###############################################
#9. Utilizing line graphs, compare how men and women's avg commute,
#distributions differ.
men<-subset(file,gender=="male")
women<-subset(file,gender=="female")
print(men)
plot(men$avg_commute,col="red",type = "l",bty= "n", lwd = "2")
par(new=T)
plot(women$avg_commute,col="blue",type = "l",bty= "n", lwd = "2")

####################################################################
#10. histogram for gender distrubtion 
# by barplot 
fg<-table(file$gender)
barplot(fg,col=2)
# another way to do it using histogram  
# i want to create another data frame to make gender female ==>2 male ===>1
tran<-factor(c(file$gender))
file$gender <- unclass(tran)
print(file)
xrange<-range(1,2)
hist(file$gender,xlim=xrange,col="brown1")
####################################################################
################################################################
# 11.Use a histogram to show gender distribution for each disease.
genderdis<-subset(file,gender==1)
print(genderdis)
#Make a chart to show the ancestry distribution
trandis<-factor(c(file$disease))
file$disease <- unclass(trandis)
trandis2<-factor(c(genderdis$disease))
genderdis$disease <- unclass(trandis2)
print(genderdis)
hist(genderdis$disease,col="red")
fg2<-table(genderdis$disease)
barplot(fg2,col=2)
#####
genderdisf<-subset(file,gender==2)
print(genderdisf)
trandisf<-factor(c(file$disease))
file$disease <- unclass(trandisf)
trandis2f<-factor(c(genderdisf$disease))
genderdisf$disease <- unclass(trandis2f)
print(genderdisf)
hist(genderdisf$disease,col="red")
fg3<-table(genderdisf$disease)
barplot(fg3,col="brown")
#################################################################
#12.Use a chart to demonstrate whether there is a relationship between age and the type of disease.
print(file2)
df2<-subset(file2,select = c("dob","disease"))
print(df2)
library(ggplot2)
fig<-ggplot(df2,aes(x=dob,y=disease,fill=disease))+
  geom_bar(stat="identity")
print(fig)
library("eeptools")
library ("lubridate")
library("dplyr")
Date_file<-as.Date(file2$dob)
print(Date_file)
Diff<-difftime("2022-12-5",Date_file,unit="weeks")/52
file2$Diff2<-floor(Diff)
library(ggplot2)
df2<-subset(file2,select = c("Diff2","disease"))
fig<-ggplot(df2,mapping=aes(x=Diff,y=disease,fill=disease))+
  geom_bar(stat="identity")
print(fig)
########################################################
#13.Make a chart to show the total number of children per disease.
print(max(file$disease))
disease1<-subset(file,disease==1)
sumch1<-sum(disease1$children)
print(sumch1)

disease2<-subset(file,disease==2)
sumch2<-sum(disease1$children)
print(sumch2)

disease3<-subset(file,disease==3)
sumch3<-sum(disease3$children)
print(sumch3)

disease4<-subset(file,disease==4)
sumch4<-sum(disease4$children)
print(sumch4)

disease5<-subset(file,disease==5)
sumch5<-sum(disease5$children)
print(sumch5)

disease6<-subset(file,disease==6)
sumch6<-sum(disease6$children)
print(sumch6)

disease7<-subset(file,disease==7)
sumch7<-sum(disease7$children)
print(sumch7)

disease8<-subset(file,disease==8)
sumch8<-sum(disease8$children)
print(sumch8)

disease9<-subset(file,disease==9)
sumch9<-sum(disease9$children)
print(sumch9)

disease10<-subset(file,disease==10)
sumch10<-sum(disease10$children)
print(sumch10)

disease11<-subset(file,disease==11)
sumch11<-sum(disease11$children)
print(sumch11)

disease12<-subset(file,disease==12)
sumch12<-sum(disease12$children)
print(sumch12)

disease13<-subset(file,disease==13)
sumch13<-sum(disease13$children)
print(sumch13)
vectotal<-c(sumch1,sumch2,sumch3,sumch4,sumch5,sumch6,sumch7,sumch8,sumch9,sumch10,sumch11,sumch12,sumch13)
print(vectotal)
diseases<-c(1,2,3,4,5,6,7,8,9,10,11,12,13)
plot(diseases,vectotal,type = "l",lwd = 2,xlab="diseases ",ylab="total of children",col="red")



###########################################################################
#14.Make a chart to show the ancestry distribution
print(file2$ancestry)
t<-table(file2$ancestry)
barplot(t,col="red")
tranans<-factor(c(file$ancestry))
file$ancestry <- unclass(tranans)
print(file)
plot(density(file$ancestry),col="blue")


#########################finally finished####################################


