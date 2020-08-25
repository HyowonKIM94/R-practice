# R-practice 

chisq.test 교차분석 연습) / 가족 구성원 숫자가 차의 크기에 영향을 미치는가? 
#변수유형 : 가족의 수 (1:1~2명 , 2:3~4명 , 3:5~6명) 범주형, 차의 크기 (1: small , 2: medium, 3: large) 범주형 

setwd("C:/Rtest")
car<-read.csv("carS.csv",header=TRUE)

car$family2[car$family==1]<-"1~2"
car$family2[car$family==2]<-"3~4"
car$family2[car$family==3]<-"5~6"
car$carsize2[car$carsize==1]<-"small"
car$carsize2[car$carsize==2]<-"medium"
car$carsize2[car$carsize==3]<-"large"
car

table(car$family2,car$carsize2)

chisq.test(car$family2,car$carsize2)

install.packages("gmodels")
library(gmodels)

CrossTable(car$family2,car$carsize2,expected=TRUE,format="SPSS")


t.test
