# Reads train.csv and test.csv
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Converts NA's present in Age column to the mean Age
convertAgeNa <- function(x)
{
ifelse(is.na(x) , 30 , x)
}
train$Age <- sapply(train$Age , convertAgeNa)
test$Age <- sapply(test$Age , convertAgeNa)

# Converts NA's present in Fare column to the mean Fare
convertFareNa <- function(x)
{
ifelse(is.na(x) , 35 , x)
}
test$Fare <- sapply(test$Fare , convertFareNa)

# Cleans and creates a testClean.csv file which is most suitable to predict the data
summary(test)
testClean <- test[c(1,2,4,5,6,7,9,11)]
write.csv(testClean , "testClean.csv")
summary(testClean)

# Improving model performance by using linear Regression
train$Pclass2 <- train$Pclass^2
testClean$Pclass2 <- testClean$Pclass^2
model <- lm (Survived ~ Pclass2*Pclass2+Pclass*Sex*Sex*Sex*Age*SibSp*Parch , data = train_clean)
find <- predict(model ,testClean)
summary(model)

# Converts Age to Numerical values
ConvertSur <- function(x)
{
ifelse( x > 0.5 ,1, 0)
}
find <- sapply(find , ConvertSur)

# Creates a Result2.csv file for the prediction
write.csv(find , "Result2.csv")
Result2<- read.csv("Result2.csv")
colnames(Result2)[1] <- "PassengerId"
colnames(Result2)[2] <- "Survived"
PassengerId <- 892:1309
Result2$PassengerId <- PassengerId
write.csv(Result2 , "Result2.csv" ,row.names = FALSE)
#This produces 78% accuracy on test data set with the above model.