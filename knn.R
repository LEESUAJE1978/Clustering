iris<-read.csv("iris.csv")
head(iris)
str(iris)
dim(iris)
attach(iris)

#k-fold cross-validation(k=3, 5,10)
#training /test data : n=150

set.seed(9999) #난수 생성시 처음 시작값을 주어 동일한 훈련표본 사용
N=nrow(iris)
tr.idx=sample(1:N, size = N*2/3, replace = FALSE)
tr.idx

#attribute in training and test
iris.train<-iris[tr.idx,-5]
iris.test<-iris[-tr.idx,-5]

#target value in training and test
trainLabels<-iris[tr.idx,5]
testLabels<-iris[-tr.idx,5]

length(trainLabels)
table(testLabels)

#분류(classification)-지도학습, clustering 비지도학습

#k-nearest neighbor method
#최적 K는 데이터 구조를 파악하기 어렵고 너무 작으면 과적합(overfitting)위험이 있음
#교차검증(cross-validation)으로 정확도가 높은 k산정
#help(knn) 매뉴얼 탐색
library(class)#knn  수행 패키지
library(gmodels)#분류 분석 후 검증에 사용되는 cross table을 위한 패키
library(scales)#최적 k등 그래프를 위한 패키지

#knn함수 : knn(train = 학습데이터, test = 검증데이터, cl= 타겟변수, k= )
md1<-knn(train = iris.train, test = iris.test, cl =trainLabels,k=5)
md1

#accuracy of 5-nearest neighbor classification
CrossTable(x=testLabels, y=md1, prop.chisq = FALSE)

#k selection based in minimum misclassification
accuracy_k <-NULL

for(kk in c(1:nrow(iris.train)))
{
  set.seed(1234)
  knn_k<-knn(train = iris.train, test = iris.test, cl = trainLabels, k=kk)

  accuracy_k<-c(accuracy_k, sum(knn_k==testLabels)/length(testLabels))
}

accuracy_k
#test_k<-data.frame(k=c(1:nrow(iris.train)), accuracy=accuracy_k)
test_k<-data.frame(k=c(1:nrow(iris.train)/2), accuracy=accuracy_k)

plot(formula=accuracy~k, data=test_k, type="o", pch=20, col=3, main="validation-optimal k")
with(test_k,text(accuracy~k, labels = rownames(test_k), pos=1, cex=0.7))
min(test_k[test_k$accuracy %in% max(accuracy_k),"k"])

#k=7 knn
md1<-knn(train = iris.train, test = iris.test, cl= trainLabels, k=7)
CrossTable(x=testLabels, y=md1, prop.chisq = FALSE)

#with two variables, how classify?
#graphic display
plot(formula=Petal.Length~Petal.Width, 
     data=iris.train, col=alpha(c("purple","blue","green"),0.7)[trainLabels],
     main="knn(k=7)")
points(formula = Petal.Length~Petal.Width,
       data = iris.test,
       pch =17,
       cex =1.2,
       col=alpha(c("purple","blue","green"),0.7)[md2])
legend("bottomright",
       c(paste("train", levels(trainLabels)),paste("test",levels(testLabels))),
       pch = c(rep(1,3),rep(17,3)),
       col=c(rep(alpha(c("purple","blue","green"),0.7),2)),
       cex = 0.9)

##weighted KNN packages
install.packages("kknn")#weighted value knn
library(kknn)
help("kknn")

set.seed(999) #난수 생성시 처음 시작값을 주어 동일한 훈련표본 사용
N=nrow(iris)
tr.idx=sample(1:N, size = N*2/3, replace = FALSE)
tr.idx

#attribute in training and test
train<-iris[tr.idx,] #weighted KNN package 사용시에는 타겟 값 제거 안하
test<-iris[-tr.idx,]

#target value in training and test
trainLabels<-iris[tr.idx,5]
testLabels<-iris[-tr.idx,5]


md2<-kknn(Species~., train=train, test=test, k=5, distance = 1, kernel = "triangular")
md2_fit<-fitted(md2)
md2_fit

#accuracy of weighted knn
CrossTable(x=testLabels, y=md2_fit, prop.chisq = FALSE, prop.c = FALSE)

#weighted knn(k=7, diatance =2)
md3<-kknn(Species~., train = train, test = test, k=7, distance = 2, kernel = "triangular")
md3_fit<-fitted(md3)
md3_fit

#accuracy of weighted knn
CrossTable(x=testLabels, y=md3_fit, prop.chisq = FALSE, prop.c = FALSE)
#ref. POSTECH MOOC 이혜선교수 R