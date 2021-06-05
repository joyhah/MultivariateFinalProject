library(adabag)
library(randomForest)

## Reading Data
mydata <- read.csv("Tidydata.csv", h = T)
keep <- c("v23", "v8", "v9_3", "v37", "v1", "v31", "v32", "v33", "v36", "v24")
Int.addic <- mydata[, (names(mydata) %in% keep)]
# v23: 上網品質
# v8: 上網費用
# v9_3: 上網時間
# v37: 性別
# v1: 年齡
# v31: 教育程度
# v32: 身心障礙
# v33: 居住地區
# v36: 跟家人住
# v24: 網路成癮

#############
## Missing 
miss.pct <- apply(is.na(Int.addic), 2, sum)/1517
chisq.test(Int.addic$v23, Int.addic$v24)
Int.addic <- Int.addic[, !(names(Int.addic) == "v23")]

## Response table
table(Int.addic$v24)/length(Int.addic$v24)

## Dealing with site variable: v33
site <- table(Int.addic$v33, Int.addic$v24)/as.vector(table(Int.addic$v33))
site.diff <- site[, 2]
km <- kmeans(site.diff, 3, 20)
plot(site.diff, col = km$cluster)
points(km$centers, col = 1:3, pch = 8)
km$cluster[order(km$cluster)]
v33_1 <- Int.addic$v33
v33_1[(v33_1 %in% c(4, 11, 18))] <- 101
v33_1[(v33_1 %in% c(1, 2, 8, 9, 10, 14, 17, 19, 21))] <- 102
v33_1[(v33_1 %in% c(3, 5, 6, 7, 12, 13, 15, 16, 20))] <- 103
Int.addic$v33 <- v33_1 - 100
chisq.test(Int.addic$v33, Int.addic$v24)
table(Int.addic$v33)

# Define factor
Int.addic$v24 <- factor(Int.addic$v24)
#Int.addic$v23 <- factor(Int.addic$v23)
Int.addic$v37 <- factor(Int.addic$v37)
Int.addic$v1  <- factor(Int.addic$v1)
Int.addic$v31 <- factor(Int.addic$v31)
Int.addic$v32 <- factor(Int.addic$v32)
Int.addic$v33 <- factor(Int.addic$v33)
Int.addic$v36 <- factor(Int.addic$v36)

Int.addic <- na.omit(Int.addic) #刪掉遺失值

# Bagging ##############

#without c.v. run time 41.27287 sec
fit1 <- bagging(v24 ~ ., data = Int.addic,
                control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
fit1.pred <- predict.bagging(fit1, Int.addic)
fit1.pred$confusion
fit1.pred$error
fit1$importance
#bagging plot1
barplot(sort(fit1$importance/100, decreasing = T), ylab = "Importance Variable %", xlab = "Variable",
        main="Bagging",names.arg = c("費用","時間","年齡","教育","居住地",
                      "家人同居","性別","身障"))

#rmse <- NULL
#for(i in 1:10){
#  fit2 <- bagging.cv(v24 ~ ., 
#                     data = Int.addic,
#                    v = 10, mfinal = i*10,
#                     control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
#  rmse[i] <- fit2$error
#}
#rmse
#[1] 0.2886598 0.2982327 0.2916053 0.2960236
#[5] 0.2901325 0.2916053 0.2908689 0.2938144
#[9] 0.2930781 0.2938144

#with c.v. mfinal = 10 has smaller error
#run time 45.66219 sec
fit2 <- bagging.cv(v24 ~ ., data = Int.addic, v = 10, mfinal = 10, 
                   control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
fit2[-1]

# Random Forest #######
#run time 3.489261 secs
fit3 <- randomForest(v24 ~ ., data = Int.addic, importance = TRUE, 
                     mtry = 2,
                     proximity = TRUE)
print(fit3)

#1      2      3      4      5      6      7      8
#24.67% 25.26% 27.61% 28.42% 28.72% 29.82% 28.94% 29.38%

#random forest plot
barplot(sort(fit3$importance[,4],decreasing = T), ylab = "Mean Decrease Gini",
        main = "Random Forest", xlab = "Variable",
        names.arg = c("時間", "費用", "年齡", "教育", "居住地",
                      "性別", "家人同居", "身障"))

#run time 47.91497 secs
fit5 <- rfcv(Int.addic[, !(names(Int.addic) == "v24")], Int.addic$v24, 
             cv.fold = 10, step = 0.9)
fit5$error.cv

#Boosting ###########

#Boos = T run time 8.607431
fit62 <- boosting(v24 ~ ., data = Int.addic, 
                 boos = T, mfinal = 20,
                 control = rpart.control(minsplit = 1, 
                                         minbucket = 1, cp = 0))
fit62.pred <- predict.boosting(fit62, Int.addic)  
fit62.pred$error #0.01325479

#Boos = F time time faster 7.886561
fit6 <- boosting(v24 ~ ., data = Int.addic, 
                 boos = F, mfinal = 20,
                 control = rpart.control(minsplit = 1, 
                                         minbucket = 1, cp = 0))
fit6.pred <- predict.boosting(fit6, Int.addic)  
fit6.pred$error #0.01325479
fit6.pred$confusion
fit6$importance
#boosting plot
barplot(sort(fit6$importance/100, decreasing = T), ylab = "Importance Variable %", xlab = "Variable",
        main = "Boosting",names.arg = c("費用","時間","年齡", "教育","居住地",
                      "家人同居","性別","身障"))
#-------------
#Boostrap.cv Boos = F
#rmse2 <- NULL
#for(j in 1:10){
#  fit7 <- boosting.cv(v24 ~ ., data = Int.addic, v = 10, mfinal = j*10, boos = F,
#                      control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
#  rmse2[j] <- fit7$error
#}
#rmes2
# [1] 0.3254786 0.3173785 0.3203240 0.3159057 0.3166421
# [6] 0.3173785 0.3159057 0.3166421 0.3173785 0.3166421

#with c.v. mfinal = 40, 70 has smaller error
#run time 2.814244 mins
fit7 <- boosting.cv(v24 ~ ., data = Int.addic, v = 10, mfinal = 40, boos = F,
                    control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
fit7[-1] 

#Boostrap.cv Boos = T
#rmse3 <- NULL
#for(j in 1:10){
#  fit8 <- boosting.cv(v24 ~ ., data = Int.addic, v = 10, mfinal = i*10, boos = T,
#                      control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
#  rmse3[j] <- fit8$error
#}
#rmse3
# [1] 0.2938144 0.3033873 0.2974963 0.2997054 0.3033873 
# [6] 0.3026510 0.3092784 0.3019146 0.3041237 0.2945508

#with c.v. mfinal = 10 has smaller error
#run time 46.52852 sec
fit8 <- boosting.cv(v24 ~ ., data = Int.addic, v = 10, mfinal = 10, boos = T,
                    control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
fit8[-1] 