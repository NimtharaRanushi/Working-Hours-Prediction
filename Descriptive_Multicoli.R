## Descriptive analysis for the Data set

library(ggplot2)
library(tidyverse)
install.packages("car")
library(car)
library(dplyr)
install.packages("ggcorrplot")
library(ggcorrplot)
library(GoodmanKruskal)
library(graphics)
library(readr)
install.packages("outliers")
library(outliers)
install.packages("EnvStats")
library(EnvStats)

getwd()
setwd("D:\\colombo uni\\3rd year 2nd sem\\ST 3082\\Group Project\\Project 1")

Data = read.table("train.csv",sep = ",",header = TRUE) # Importing the data set
anyNA(Data) 

df = as.data.frame(Data)
anyNA(df) #Check whether there is any missing values

dim(df) # Dimensions of the data set

str(df) #Structure of the data set
nrow(unique(df))==nrow(df)

N = nrow(df) #Number of observations
N


# Data cleaning part
# Omit the data with native.country is equal to the "South"
unique(df$native.country)
for(i in 1:N){ 
  if(df$native.country[i]==" South"){
    df$native.country[i] = NA 
    }else{ 
      next 
      } 
  } 

anyNA(df)
df1 = na.omit(df)
anyNA(df1)
N1 = nrow(df1)
N1
# Omit the data with work class is equal to the "Never-worked"
unique(df1$workclass)
for (j in 1:N1){
  if(df1$workclass[j]==' Never-worked'){
    df1$workclass[j]= NA
  }else{
    next
  }
}
anyNA(df1)
df2 = na.omit(df1)
anyNA(df2)
N2 = nrow(df2)
N2

# Split the data set into training and test sets
set.seed(1000)
dt=sort(sample(nrow(df2),nrow(df2)*.8))
train=df2[dt,] 
test=df2[-dt,]

anyNA(train)

n = nrow(train) # Number of observations in the training set
n


# Re-code the work-class variable
unique(train$workclass)
for (i in 1:n){
  if((train$workclass[i]==' Self-emp-not-inc')|(train$workclass[i]==' Self-emp-inc')){
    train$workclass[i]='Self-employed'
  }else if(train$workclass[i]==' State-gov'){
    train$workclass[i]='Government'
  }else if(train$workclass[i]==' Local-gov'){
    train$workclass[i]='Government'
  }else if(train$workclass[i]==' Federal-gov'){
    train$workclass[i]='Government'
  }
}

unique(train$workclass) # To confirm the re-coding process

# Re-code the marital-status variable
unique(train$marital.status)
for (i in 1:n){
  if(train$marital.status[i]==' Never-married'){
    train$marital.status[i]="Unmarried"
  }else if((train$marital.status[i]==' Married-civ-spouse')|(train$marital.status[i]==" Married-spouse-absent")|(train$marital.status[i]==" Married-AF-spouse")){
    train$marital.status[i]="Married"
  }else if((train$marital.status[i]==" Divorced")|(train$marital.status[i]==" Separated")|(train$marital.status[i]==" Widowed")){
    train$marital.status[i]="Seperated/Widowed"
  }
}
unique(train$marital.status) #To confirm the re-code

# Re-code the race variable
unique(train$race)
for (i in 1:n){
  if(train$race[i]==" White"){
    train$race[i]="White"
  }else{
    train$race[i]="Black/Other"
  }
}


unique(train$race) #To confirm the re-code

# Re-code the education variable
unique(train$education)
for (i in 1:n){
  if (train$education[i]==" HS-grad"){
    train$education[i]="HighSchool-Graduate"
  }else if((train$education[i]==" Some-college")|(train$education[i]==" 7th-8th")|(train$education[i]==" 9th")|(train$education[i]==" 11th")|(train$education[i]==" 12th")|(train$education[i]==" 5th-6th")|(train$education[i]==" 10th")|(train$education[i]==" 1st-4th")|(train$education[i]==" Preschool")){
    train$education[i]="School Level"
  }else if((train$education[i]==" Bachelors")|(train$education[i]==" Prof-school")|(train$education[i]==" Masters")|(train$education[i]==" Doctorate")){
    train$education[i]="Degree Holder"
  }else if((train$education[i]==" Assoc-voc")|(train$education[i]==" Assoc-acdm")){
    train$education[i]="Associatiate Degree Holder"
  }
}

unique(train$education)

# Re-code the native.country variable
unique(train$native.country)
for (i in 1:n){
 if(train$native.country[i]==" United-States" ){
   train$native.country[i]="USA"
 }else{
   train$native.country[i]="Other Country"
 }
}
unique(train$native.country)

rosnerTest(train$hours.per.week, k = 20, alpha = 0.05, warn = TRUE)

# # # Export the training set to a csv file
csv_file_path = "D:\\colombo uni\\3rd year 2nd sem\\ST 3082\\Group Project\\Project 1\\Images\\Book2.csv"
write.csv(train, file = csv_file_path, row.names = FALSE)
###### Plots & Other descriptive analysis #####

### Working hours per week
# Histogram for the working hours per week
p=hist(train$hours.per.week,col="blue",main="Distribution of the working hours per week",xlab="Working hours per week")

# Box plot for the working hours per week
boxplot(train$hours.per.week,col="purple",outcol= "black", cex=1,main="Boxplot for the working hours per week",ylab="The working hours per week")
df1 = train %>% select_if(is.numeric )
rosnerTest(train$hours.per.week, k = 50, alpha = 0.05, warn = TRUE)
### Age
#Histogram for the Age
p=hist(train$age,col="orange",main="Distribution of the Age",xlab="Age")
boxplot(train$age,col="light green",outcol= "black", cex=1,main="Boxplot for the age",ylab="The age")
df1 = train %>% select_if(is.numeric )

#Scatter plot for the Age vs working hours
ggplot(train, aes(x=age, y=hours.per.week))+geom_point(color="darkgreen")+
  ggtitle("Age vs Working hours per week")+
  geom_smooth(method=lm, color="darkred")



ggplot(train,aes(age,hours.per.week,color=income))+
  geom_point(alpha=0.5)+
  theme_bw()+
  facet_wrap(~income,scales='free')+
  labs(x="Age", y="Work hours per week")+
  geom_smooth(method=lm, color="darkred")







# Correlation coefficient of Age and Work hours
cor(train$age,train$hours.per.week)

### Marital status
#Box plot for the distribution of the working hours according to the Marital status
ggplot(data=train,mapping=aes( marital.status,hours.per.week))+
  geom_boxplot()+
  ggtitle("Distribution of working hours per week with Marital status")+
  geom_area(color="red")+
  xlab("Marital status")+
  ylab("Working hours per week")

# Bar chart for the marital status
names = unique(train$marital.status)
table(train$marital.status)
barplot(c(4559,7059,3213),names.arg=names,xlab="Marital status",ylab="Frequency",col="#879acb",
        main="Bar chart for marital status",border="black")

# pie chart for work classes
mycols = c("#42f55a", "#42f5e6", "#ecf542") 
ggplot(train, aes(x = "", y = marital.status, fill = marital.status)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0)+ 
  scale_fill_manual(values = mycols) + 
  theme_void()

## Work class
# box plot for the work hours and the work class
boxplot(train$hours.per.week~train$workclass,main="Boxplot for the working hours per week with Workclass",ylab="The working hours per week ",xlab="Work-class",col=c('red','blue',"yellow","green"))

##Education
# Bar chart for the education level
names = unique(train$education)
table(train$education)

barplot(c(4312,4012,5283,1224),names.arg=names,xlab="Educational level",ylab="Frequency",col="#98cde9",
        main="Bar chart for educational level",border="black")

#box plot for the work hours per week with education level
boxplot(train$hours.per.week~train$education,main="Boxplot for the working hours per week with Education Level",ylab="The working hours per week ",xlab="Education Level",col=c('blue',"#dfab29","purple","#fea78c"))

## Education Numbers
# scatter plot for the no of years vs work hours per week
ggplot(train, aes(x=education.num, y=hours.per.week))+geom_point(color="#1243ad")+
  ggtitle("Number of years of the education vs Working hours per week")+
  geom_smooth(method=lm, color="darkred")

# Correlation coefficient between education number and work hours per week
cor(train$education.num,train$hours.per.week)

##Occupation
#box plot for the work hours per week with marital status
boxplot(train$hours.per.week~train$occupation,main="Boxplot for the working hours per week with Occupation",ylab="The working hours per week ",xlab="Occupation",las=2,col=c('green','blue',"#dfab29","purple","#fea78c","red","#afc328","#43ab54","#12abc3","#54afeb","#bcfe78","#87faab","#fcab54","#ab4325"),boxwex = 0.525)

##Relationship

#box plot for the work hours per week with marital status
boxplot(train$hours.per.week~train$relationship,main="Boxplot for the working hours per week with Relationship",ylab="The working hours per week ",xlab="Relationship",col=c('blue',"purple","#bcfe78","red","#12abc3","#54afeb"),boxwex = 0.525)

## Native country
#box plot for the work hours per week with native country
boxplot(train$hours.per.week~train$native.country,main="Boxplot for the working hours per week with native country",ylab="The working hours per week ",xlab="Native Country",col=c("#bcfe78","#12abc3"),boxwex = 0.525)

## Income Category
# Pie chart for the distribution
mycols = c( "#da6123", "#ec62ba")
ggplot(train, aes(x = "", y = train$income, fill = train$income)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0)+ 
  scale_fill_manual(values = mycols) +
  theme_void()

#box plot for the work hours per week with native country
boxplot(train$hours.per.week~train$income,main="Boxplot for the working hours per week with income category",ylab="The working hours per week ",xlab="Income Level",col=c("#b13435","#198321",fill=train$workclass),boxwex = 0.525)

## Race
# Pie chart for the distribution
ggplot(train, aes(x = "", y = race, fill = race)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0)+ 
  theme_void()
#box plot for the work hours per week with native country
boxplot(train$hours.per.week~train$race,main="Boxplot for the working hours per week with the race",ylab="The working hours per week ",xlab="Race",col=c("#12abc3","#96feab"),boxwex = 0.525)

## Gender
# Pie chart for the distribution
ggplot(train, aes(x = "", y = sex, fill = sex)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0)+ 
  theme_void()

## Checking Multi-collinearity
# Correlation plot between response & the quantitative independent variables
train_num = train %>% select_if(is.numeric )
cor_mat = cor(train_num[-6])
ggheatmap <- ggcorrplot(cor_mat, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3)
print(ggheatmap)

#Goodman kruskal plot between the qualitative independent variables
GKp=data.frame(train$education,train$occupation,train$race,train$sex,train$workclass,train$marital.status,train$native.country,train$relationship) 
names(GKp)=c("Education","ocucupation","Race","Gender","Workclass","Marital status","Native country","Relationship") 
GKmatrix=GKtauDataframe(GKp) 
plot(GKmatrix)

install.packages("Factoshiny")
library(Factoshiny)
results = Factoshiny(train)
res.FAMD =FAMD(train,sup.var=c(1,2,3,5,6,11,12,13),graph=FALSE)
summary(res.FAMD)






library(stringr)
install.packages("clustertend")
library(clustertend)
install.packages("NbClust")
library(NbClust)
install.packages("factoextra")
library(factoextra)
install.packages("ClusterR")
library(ClusterR)
install.packages("fpc")
library(fpc)
install.packages("clusterSim")
library(clusterSim)
install.packages("psych")
library(psych)
install.packages("FactoMineR")
library(FactoMineR)
install.packages("clustMixType")
library(clustMixType)
load(dane2)
res.famd <- FAMD(train, ncp=50, graph=FALSE)
get_eigenvalue(famd)
fviz_eig(famd, ncp=20, addlabels=TRUE)
famd <- FAMD(train, ncp=14, graph=FALSE)
famdvar <- get_famd_var(famd)
fviz_famd_var(famd, col.var="cos2", alpha.var="contrib", gradient.cols = c("blue", "green", "red"), repel = TRUE)

fviz_contrib(famd, choice = "var", axes = 1, top = 20)
fviz_contrib(famd, choice = "var", axes = 2, top = 20)
install.packages("hopkins")
library(hopkins)
datafamd <- data.frame(famd$ind$coord)
hopkins(datafamd,100)
get_clust_tendency(datafamd, 2, graph=F, gradient=list(low="red", mid="white", high="blue"))$hopkins_stat
fviz_nbclust(datafamd, FUNcluster = kmeans, method = c("silhouette"), k.max = 20, nboot = 100,)
kmeanspca4 = eclust(datafamd, "kmeans", hc_metric="euclidean", k=9, graph = F)
fviz_cluster(kmeanspca4, ellipse.type = "convex", geom = "point", ggtheme=theme_classic())
fviz_silhouette(kmeanspca4)


help("hopkins")

x<-matrix(runif(200,1,100),50,4);
x
hopkins(x,n=10)

























gower_dist = daisy(train,
                    metric = "gower",
                    type = list(logratio = 3))

Esgower <- numeric(10)
for(i in 2:10){
  pames <- pam(gower_dist, diss = TRUE, k = i)
  Esgower[i] <- pames$silinfo$avg.width}
plot(1:10, Esgower, type = "b", ylab = "Silhouette", xlab = "Number of Clusters") 


res.famd$var$contrib

fviz_famd_var(res.famd, "quanti.var", repel=T, col.var="contrib", gradient.cols=rev(cols))

fviz_famd_var(res.famd, "quali.var", repel=T, col.var="contrib", gradient.cols=rev(cols))

fviz_famd_var(res.famd, "var", repel=T, col.var="coord", gradient.cols=rev(cols))

fviz_screeplot(res.famd)

as.data.frame(res.famd$eig)

res.famd$var$contrib[, 1:2]

res.km = kmeans(res.famd$ind$coord, centers=3, nstart=25, iter.max=50)
fviz_mfa_ind(res.famd, habillage=as.factor(res.km$cluster), palette=c("darkred", "indianred2", "salmon"), addEllipses=T, repel=T, geom="point")

dt["cluster"] = as.factor(res.km$cluster)
dq["cluster"] = as.factor(res.km$cluster)

fviz_nbclust(X.copy, kmeans, method = "silhouette")

ggplot(dt, aes(x=cluster, y=PremiumPrice, color=cluster)) +
  geom_jitter() +
  labs(title="Visualizing clusters against Premium price")

ggplot(dt, aes(x=cluster, y=Age, color=cluster)) +
  geom_jitter() +
  labs(title="Visualizing clusters against Premium price")

ggplot() +
  geom_bar(dq, mapping=aes(x=cluster, fill=Age_cat), position="fill")

ggplot() +
  geom_bar(dt, mapping=aes(x=cluster, fill=Diabetes), position="fill")

cat.cols = colnames(dt)[lapply(dt, class) == "factor"]
cat.cols = cat.cols[1:(length(cat.cols) - 1)]

for (i in cat.cols) {
  plt = ggplot(dt, aes(x=cluster, fill=!!sym(i))) +
    geom_bar(position="fill") +
    scale_fill_brewer(palette="Set2")
  print(plt)
}

temp = dt[, c(cat.cols, "cluster")]
summary(temp[temp$cluster == 1, ])
summary(temp[temp$cluster == 2, ])
summary(temp[temp$cluster == 3, ])



























