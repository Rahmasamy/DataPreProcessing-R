
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install("antiProfilesData")
datasetName <- antiProfilesData::apColonData
##Then read pdata, edata, and fdata of the datasetName as you learned from the lab
bm=datasetName
#Show the type of the data and assign each of feature, phenotype and expression data to different variables
print(str(bm))
# or 
class(bm)
dim(bm)
summary(bm)
############################################################
pdata=pData(bm)#pheno type data (samples): sample X infornation
edata=exprs(bm)#expression data (genomics data): features(genes) X sample, [i,j] = count gene/sample
fdata = fData(bm)# features data: genomic features: features(genes) X features , feature1: gene name
n2<-as.data.frame(edata)
dim(edata)
dim(pdata)
dim(fdata)
#1.a Show the type of each column
# 1st way
print(str(pdata))
print(str(edata))
print(str(t(fdata)))
# or 
for (i in colnames(edata)){  
  print(class(edata[i]))
}
for (i in colnames(pdata)){  
  print(class(pdata[i]))
}
for (i in colnames(fdata)){  
  print(class(fdata[i]))
}
# or
# another way
print(typeof(pdata$filename))
print(typeof(pdata$DB_ID))
print(typeof(pdata$Tissue)) 
####################################################
#1.b Show column names and rows name
print(names(pdata))
print(names(fdata))
print(names(edata))
row.names(pdata)
row.names(edata)
row.names(fdata)
#1.c calculate summary fpr each column 
lapply(pdata,summary)
lapply(fdata, summary)
lapply(edata, summary)
#1.d Show frequency of categorical data, taking into the consideration, NA values frequency if any

table(pdata$Tissue,useNA="ifany")
table(pdata$SubType,useNA="ifany")
table(pdata$ClinicalGroup,useNA="ifany")
table(pdata$Status,useNA="ifany")
#1.e Calculate the correlation and covariance between the first 10 columns only of our data set and draw full correlation matrix.
dim(pdata)
h_edata=head(edata[,0:10])
cormatrix=cor(edata[,0:10],use = "everything",method = "pearson")
cormatrix
covmatrix=cov(edata[,0:10],use="everything",method = "pearson")
covmatrix
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = cormatrix, col = col, symm = TRUE)

# 1.f For both genes: GSM95478,GSM95473 show the plot with a line of their relation
n<-as.data.frame(edata)
plot(n$GSM95473,n$GSM95478,col="red")
fit1<-lm(n$GSM95473~n$GSM95478)
abline(fit1)

####################################################################
# question2 
PC1<-prcomp(n)
PC1
# cal colmeans 

expr_center<- n-colMeans(n) # normalization 
svd1<-svd(expr_center)
names(svd1)
# proving by plotting
plot(svd1$v[,1],PC1$rotation[,1],col="red")
# proving by values
print(svd1$v[,1])
print(PC1$rotation[,1])
######################################################################
#question 3

c<- as.factor(c(rep("Aries" ,29),rep("Aquarius",20),rep("Taurus",24) ,
rep("Gemini ",22), rep("Cancer ",19),rep("Leo",21),rep("Virgo",18),
rep("Libra",19),rep("Scorpio",20),rep("Sagittarius",23),rep("Capricorn",18),rep("Pisces",23)))
print(c)
t<-table(c)
chisq.test(t,p=rep(1/12,12))
#  p-value = 0.9265 so we accept the null hyposis 
#################################################################
#quest4
e<-edata[,1:10]
dim(e)
d<-dist(t(e))
print(d)
plot(hclust(d),hang=-1)

# kmeans 
k<-kmeans(edata,centers=3)
k$centers
table(k$cluster)
##########################################################################