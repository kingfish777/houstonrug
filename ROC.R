
# borrowed with much flattery from http://myrcodes.blogspot.com/2013/12/area-under-curve-auc-proc-package.html
#
#

library(rpart)
library(pROC)

Dat <- kyphosis
str(Dat)
a <- rep(0, nrow(Dat))
a[which(Dat$Kyphosis == "present")] <- 1
Dat$Present <- a

a <- sample(c(1:nrow(Dat)), size = nrow(Dat) * 0.7, replace = FALSE)

Train <- Dat[a, ]  
Test <- Dat[-a, ]  

RP1 <- rpart(Present ~ Age + Number + Start, Train, control = rpart.control(minsplit = 15, cp = 1e-04))  

Pred1 <- predict(RP1, Test[, c("Age", "Number", "Start")])
Test$Prediction1 <- Pred1

ROC1 <- roc(Test$Present, Test$Prediction1)

plot(ROC1, col = "blue")

