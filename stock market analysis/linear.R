library(ggplot2)

setwd("C:/Users/MAHE/Downloads")
mydata<-read.csv("kmeansstock.csv")

open<-mydata[2678:1166,2]
high<-mydata[2678:1166,3]
low<-mydata[2678:1166,4]
close<-mydata[2678:1166,5]

#close will have 2005-2010 given close values

model <- lm(close ~ (open+high+low),data = mydata)
model

coff1 <- coef(model)

coff1

open1<-mydata[1165:1,2]
high1<-mydata[1165:1,3]
low1<-mydata[1165:1,4]

close1=0;

i=1

for(i in 0:1165){
  close1[i] = open1[i]*coff1[2]+high1[i]*coff1[3]+low1[i]*coff1[4]
}
#displaying the final data
#close1 will have 2011-2015 close values predicted
close1

#10 subset calues from close1
cl <- close1[0:20]

#closeg will have given closed values for 2011-2015
closeg <- mydata[1165:1,5]

#10 subset from given closed values
clg <- closeg[0:20]

#date from 2011-2015
date <-mydata[1165:1,1]


d <- date[0:20]

#graph part is incomplete and we can find variance and sd btw given and predicted values corelation
#plot( d,cl, type="l", col="red" )
#par(new=TRUE)
#lines(d,clg,col="green")
#copy date closed price given closed price predicted to new csv file and plot graph in that file 


# library(xlsx)
my<-data.frame(d,clg,cl)
setwd("C:/Users/MAHE/Downloads")
write.csv(my,"output.csv")
mydata1<-read.csv("output.csv")
ggplot(mydata1,aes(x=d,y=clg))+geom_point(size=5,color="blue")+ggtitle("stockprice vs date")+ylab("stock price")+xlab("date")
ggplot(mydata1,aes(x=d,y=cl))+geom_point(size=5,color="red")+ggtitle("stockprice vs date")+ylab("stock price")+xlab("date")



