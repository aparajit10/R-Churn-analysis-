# column description 
# State - the state to which the subscriber belongs 
# Account length - no of weeks the account has been with the company 
# Area code - area code of the subscriber 
# international plan - weather the subscriber has international calling 
# voice mail plan - if the user is subscribed to voice mail service 
# Number_vmail messages - number of messages in user voicemail inbox 
# total_day_minutes - total minutes of callas made in the day per month
# total_eve_minutes - total minutes of callas made in the evening per month 
# total_night_minutes - total minutes of calls made in the  night per month
# total_day_calls - total number of calls made during the day per month 
# total_eve_calls - total number of calls made during the evening per month 
# total_night_calls - total number of calls made during the night per month 
# total_day_charge- total billed amount for day calls per month 
# total_eve_charge- total billed amount for evening calls per month 
# total_night_charge- total billed amount for night calls per month 
# total_intl_minutes - total minutes of calls made internationally per month
# total_intl_calls - total number of international calls made  per month 
# total_intl_charge- total billed amount for international calls per month 
# number_customer_service_calls - calls made to customer service per month 
# churn - binary variable describing if the customer has remained with the comppany or churned 
install.packages("InformationValue")
install.packages(sampleSelection)
install.packages("rpart")
install.packages("rpart.plot")
install.packages('party')
library(party)
library(C50)
library(ggplot2)
library(plyr)
library(reshape)
library(stats)
library(plotrix)
library(reshape2)
library(caret)
library(InformationValue)
library(sampleSelection)
library(pROC)
library(rpart)
library(rpart.plot)
# Data preperation
data(churn)
churnTest$churn_rate <- factor(ifelse(churnTest$churn == "yes", 1, 0))
churnTest$int_plan <- factor(ifelse(churnTest$international_plan == "yes", 1, 0))
churnTest$voice_plan <- factor(ifelse(churnTest$voice_mail_plan == "yes", 1, 0))
churnTest$Cde_510 <- factor(ifelse(churnTest$area_code == "area_code_510", 1, 0))
churnTest$Cde_408 <- factor(ifelse(churnTest$area_code == "area_code_408", 1, 0))
churnTrain$churn_rate <- factor(ifelse(churnTrain$churn == "yes", 1, 0))
churnTrain$int_plan <- factor(ifelse(churnTrain$international_plan == "yes", 1, 0))
churnTrain$voice_plan <- factor(ifelse(churnTrain$voice_mail_plan == "yes", 1, 0))
churnTrain$Cde_510 <- factor(ifelse(churnTrain$area_code == "area_code_510", 1, 0))
churnTrain$Cde_408 <- factor(ifelse(churnTrain$area_code == "area_code_408", 1, 0))
names(churnTest) == names(churnTrain)
master_data <- rbind.data.frame(churnTest,churnTrain)
Overall_churn <- sum(master_data$churn=='yes')/nrow(master_data)
paste("overall churn rate is",Overall_churn)
plot(master_data$churn,main="Churn Rate",
     xlab="Subscribers churn vs.No churn",col="grey")

# distribution by zip
area <- as.data.frame(master_data$area_code)
names(area)[names(area)=="master_data$area_code"] <- "area"
ggplot(area,
       aes(x = factor(""), fill = area) ) +
  geom_bar() +
  coord_polar(theta = "y") +
  scale_x_discrete("")
# subscriber distribution by state 
state <- as.data.frame(master_data$state)
names(state)[names(state)=="master_data$state"] <- "state"
ggplot(state,
       aes(x = factor(""), fill = state) ) +
  geom_bar() +
  coord_polar(theta = "y") +
  scale_x_discrete("")

# number of subscribers by state 
state1 <- master_data
state1$count <- (1)
dist_by_state <- aggregate(state1$count, by=list(state1$state),FUN=sum)
names(dist_by_state)[names(dist_by_state)=="Group.1"] <- "state"
names(dist_by_state)[names(dist_by_state)=="x"] <- "count"

ggplot(data.frame(state1$state),aes(x=state1$state)) + 
geom_bar() +
coord_flip()

# heat map for average usage by time 
usage <- aggregate(cbind(total_day_minutes, total_eve_minutes,total_night_minutes)~state, data=state1, sum, na.rm=TRUE)
usage1 <- melt(usage)
ggplot(usage1, aes(variable, state)) + geom_tile(aes(fill = value),colour = "white") +
scale_fill_gradient(low = "red",high = "green")

# plotting total revenue vs international revenue 
state1$total_rev <- rowSums(state1[,c("total_day_charge","total_eve_charge","total_night_charge")])
revenue <- aggregate(cbind(total_rev,total_intl_charge)~state, data=state1, sum, na.rm=TRUE)
p1 <- ggplot(revenue, aes(x = total_intl_charge, y = total_rev))
p1 + 
  geom_text(aes(label=state), size = 3)

# plotting customer centre calls vs churn 
cust_call_churn <- aggregate(cbind(churn_rate==1,number_customer_service_calls)~state, data=state1, sum, na.rm=TRUE)
names(cust_call_churn)[names(cust_call_churn)=="V1"] <- "Churn_rate"
cust_call_churn1 <- melt(cust_call_churn, id.var="state")
ggplot(cust_call_churn1, aes(x = state, y = value, fill = variable)) + 
  geom_bar(stat = "identity")

# running logit 

train <- subset(churnTrain, , -c(state,area_code, international_plan, voice_mail_plan,churn))
test <- subset(churnTest, , -c(state,area_code, international_plan, voice_mail_plan,churn))
summary(train)
# centreAndScale <- preProcess(train,method = c("BoxCox","center","scale"),thresh = 0.95)

probit <- glm(churn_rate ~., data=train, family=binomial(link="probit"))
predicted <- predict(probit, test, type="response")
summary(probit)
CutOff <- optimalCutoff(test$churn_rate, predicted)
plotROC(test$churn_rate, predicted)
Concordance(test$churn_rate, predicted)
misClassError(test$churn_rate, predicted, threshold = CutOff)
confusionMatrix(test$churn_rate, predicted, threshold = CutOff)
auc(roc(test$churn_rate, predicted))

#tree based models 

# Descision trees 
set.seed(198)
tree <- rpart(churn_rate ~ ., data = train, control = rpart.control(cp = 0.0001))
rsq.rpart(tree) 
printcp(tree)
plotcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
prune <- prune(tree, cp = bestcp)
tree_predict <- predict(prune,subset(test, , -c(churn_rate)),type = 'class')
tree_conf <- table(tree_predict, test$churn_rate)
plot(tree, uniform=TRUE, 
     main="Regression Tree")
text(tree, use.n=TRUE, all=TRUE, cex=.8)
plot(prune, uniform=TRUE, 
     main="Pruned Regression Tree")
text(prune, use.n=TRUE, all=TRUE, cex=.8)
prp(prune, faclen = 0, cex = 0.8, extra = 1)
X <- as.matrix(tree_conf)
Accuracy <- (X[1,1]+ X[2,2])/(X[1,1]+ X[2,2]+X[1,2]+X[2,1])
paste("Accuracy is",round(print(Accuracy),digits=2), sep=" ")

# Random Forest 

set.seed(487)
forest <- cforest(as.factor(churn_rate) ~.,
                  data = train, 
                  controls=cforest_unbiased(ntree=2000, mtry=3))
forest_predict <- predict(forest, test, OOB=TRUE, type = "response")
forest$confusion

