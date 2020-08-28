setwd("C:/Rtest")
library(readxl)

movie<-read.csv("movieSF.csv",header=TRUE)

movie$mean<- (movie$관람환경만족1+movie$관람환경만족2+movie$관람환경만족3)/3
movie$mean
``
groupM<-subset(movie,movie$성별==1)
groupF<-subset(movie,movie$성별==2)

shapiro.test(movie$mean)
shapiro.test(groupM$mean)
shapiro.test(groupF$mean)

var.test(groupM$mean,groupF$mean)
t.test(groupM$mean,groupF$mean, alternative = "two.sided" , conf.level = 0.95)
t.test(groupM$mean,groupF$mean, alternative = "greater" , conf.level = 0.95)
t.test(groupM$mean,groupF$mean, alternative = "less" , conf.level = 0.95)


#커피 전문점의 재방문 의도에 환경 만족도와 커피 품질 만족도가 어떻게 영향을 미치는지 확인 
coffee<-read.csv("coffee.csv",header=TRUE)
str(coffee)
satis<- (coffee$재방문의도1+coffee$재방문의도2)/2
en<-(coffee$내부환경만족1+coffee$내부환경만족2+coffee$내부환경만족3)/3
qu<-(coffee$커피품질만족도1+coffee$커피품질만족도2+coffee$커피품질만족도3)/3
coffe_lm<-lm(satis~en+qu,data=coffee)
summary(coffe_lm)

#성별에 따라 학점에 차이가 있는지 검증
library(Hmisc)
library(prettyR)
library(psych)

libusiness<-read.csv("business.csv",header=TRUE)

groupM <- subset(business,business$성별==1)
groupF<-subset(business,business$성별==2)
score<-business$학점

var.test(groupM$학점,groupF$학점)
#p값이 0.05보다 크므로 두 집단이 서로 동질적 

t.test(groupM$학점,groupF$학점,alter="two.sided",conf.level=0.95)


#다이어트 약을 복용 전과 복용 후 몸무게의 차이가 있는지 검증하시오. (대응표본)

setwd("C:/Rtest")
diet<-read.csv("diet.csv",header=TRUE)
head(diet) # 5개 정도 데이터 보여주기 
names(diet) #데이터 변수명 보여주기 

dietbf<-diet$before
dietaf<-diet$after

var.test(dietbf,dietaf,paired=TRUE)
wilcox.test(dietbf,dietaf,paired=TRUE,conf.int = TRUE,conf.level = 0.95)





#1번.교차분석_적합도 검정 (요일별 a/s건수에 차이가 있는가?) by 하나의 범주형 데이터 빈도수 

setwd("C:/Rtest")
service2<-read.csv("service.csv",header=TRUE)
head(service2)
service2
table(service2) #빈도수 
chisq.test(table(service2))

# p값이 0.05보다 크므로 요일별로 a/s 처리 건수에는 차이가 없다. 

#2번. 교차분석_독립성 검정 (가족의 구성원 수에 따라 차의 크기에 차이가 있을까?) by 두 범주형 데이터의 빈도수 
carS2<-read.csv("carS.csv",header=TRUE)
head(carS2)

table(carS2$family,carS2$carsize) #carS2 데이터의 가족 데이터 빈도 , 차크기 데이터 빈도 
chisq.test(table(carS2$family,carS2$carsize))

#p값이 0.05보다 작으므로 가족의 구성원 수에 따라서 차의 크기에 차이가 있다 

#3번. t-test 독립표본 (성별에 따른 두 집단간에 영화관 환경 만족도 평균 차이가 있을까? ) by 범주형 -> 연속형

movie<-read.csv("moviesf.csv",header=TRUE) 
head(movie)

movie$mean<-(movie$관람환경만족1+movie$관람환경만족2+movie$관람환경만족3)/3 # movie 데이터에 $ 새로운 열 생성 
movie$mean

group_F<-subset(movie,movie$성별==2)
group_M<-subset(movie,movie$성별==1)

var.test(group_F$mean,group_M$mean)

#p값이 0.05보다 크므로 동질적임 따라서 t.test 양측검증을 돌린다 (양쪽간에 차이가 있는지 보기 위해서)

t.test(group_F$mean,group_M$mean,alternative = "two.sided",conf.level = 0.95)

#p값이 0.05보다 작으므로 남녀에 따른 관람환경만족 평균간에 차이가 있다. 관람환경 만족에 차이가 있다.

# 관람환경 데이터 평균 구하는 방법 1. 엑셀 파일에 새로운 열 추가 후 average함수를 이용하여 평균값 구하기 
#2. movie데이터에 $를 이용하여 새로운 열 생성 후 관람환경 만족도 3개를 합한 후 나누기 3해준 값 넣기 

#var.test 다음에 t.test 할때, 남녀 집단간의 관람환경만족도에 차이가 있는지 보고 싶은 것이므로 양측검정 

#4번. t=test 대응표본(영화 개봉 전 영화 평점과 영화 개봉 후 영화평점간에 차이가 있는가? ) by 연속형 -연속형 
rating<-read.csv("rating.csv",header = TRUE)
head(rating)

ratingbf<-rating$berating
ratingaf<-rating$afrating

var.test(ratingbf,ratingaf,paired=TRUE)
#P값이 0.05보다 작으므로 동질적이지 않아서 wilcox함수를 사용하여 양측검정을 해야함 

wilcox.test(ratingbf,ratingaf, paired=TRUE, alternative = "two.sided" , conf.level = 0.95)

# 틀린점 : wilcox 도 t.test 처럼 개봉 전 평점과 개봉 후 평점감의 양측검정을 통하여 차이가 있는지 확인 
# p값이 0.05보다 작으므로개봉 전 영화 평점과 개봉후 영화 평점간의 차이가 있다 

wilcox.test(ratingbf,ratingaf, paired=TRUE, alternative = "greater" , conf.level = 0.95)
#p값이 1이므로 (0.05보다 크므로) 개봉 전 영화 평점이 개봉 후 영화평점보다 크다고 할 수 없다 = 높다고 할 수 없다

wilcox.test(ratingbf,ratingaf, paired=TRUE, alternative = "less" , conf.level = 0.95)
#p값이0.05보다 작으므로 개봉 전 영화 평점이 개봉 후 영화평점보다 작다고 할 수 있다

#5번. 다중회귀분석 (포도의 크기와 숙성기간이 포도와인의 가격에 어떻게 영향을 미치는가?) by lm(종속~독립, data)
# 독립변수들기리 다중공선성 확인 필수 , 10이 넘으면 독립변수 제거 후 다시 회귀분석 돌리기 

grape<-read.csv("grape.csv",header=TRUE)
head(grape)

library(car)
grape.lm<-lm(price~size+period,data=grape)
vif(grape.lm) # 다중공선성이 10을 넘지 않으므로 제거할 필요 없음 
summary(grape.lm)
# p값이 0.05보다 작으므로 이 모델은 유의하고 포도의 크기와 숙성기간 모두 가격에 영향을 많이 미치는 것을 알 수 있다. 이 모델을 88%설명력을 지니고 있다. 


#6번. 아노바 (3가지 교육 방법별로 교육을 이수한 영업사원들의 영업실적을 조사 - 3개의 집단(범주형)이 영업실적(연속형)
# 3가지 교육 프로그램에 따른 영업실적= 판매실적에 차이가 날거다 (t.test의 독립표본과 다른점은 독립 변수의 집단이 3개 이상 )
# 1. 독립성 2. 정규성 3. 분산성 

jobedu<-read.csv("jobedu.csv",header=TRUE)
head(jobedu)

tapply(jobedu$performance,jobedu$method,shapiro.test) # method 방법별 실적이 정규성을 띄는가 확인 
bartlett.test(jobedu$performance,jobedu$method,data=jobedu) # 등분산성을 띔/ 동질적

library(dplyr)
jobedu.lm<-lm(jobedu$performance~jobedu$method,data=jobedu)
anova(jobedu.lm)
install.packages("agricolae")
library(agricolae)
comparison<-LSD.test(model,"method",p.adj="bonferroni",group=T)
