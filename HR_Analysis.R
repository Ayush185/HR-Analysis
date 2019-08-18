library(dplyr)
md = read.csv('hr_train.csv',stringsAsFactors = FALSE)
str(md)
table(md$left)

library(ROSE)

# balanced data set with over-sampling
over.md <- ovun.sample(left~., data=md,p=0.5, seed=1,method="over")$data

table(over.md$left)


#===================================Logistic Regression======================================
# Making Dummy Variables

str(over.md)
table(over.md$sales)

over.md=over.md%>%
  mutate(Support = as.numeric(sales == "support"),
        Technical =  as.numeric(sales == "technical"),
        Sales = as.numeric(sales == "sales"),
        IT = as.numeric(sales == "IT"),
        Mktg = as.numeric(sales == "marketing"),
        Prod_Mgt = as.numeric(sales == "product_mng"),
        Acct = as.numeric(sales == "accounting"),
        RnD = as.numeric(sales == "RandD"),
        HR = as.numeric(sales == "hr")) %>% 
  select(-sales)
 
str(over.md)

table(over.md$salary)

over.md <- over.md %>% 
  mutate(Low_Salary = as.numeric(salary == "low"),
         Med_Salary = as.numeric(salary == "medium")) %>% 
  select(-salary)

str(over.md)


library(car)

sort(vif(fit),decreasing = T)[1:3]



fit <- glm(left~., data = over.md, family = "binomial")
summary(fit)

fit <- glm(left~. -IT, data = over.md, family = "binomial")
summary(fit)

fit <- glm(left~. -IT -RnD, data = over.md, family = "binomial")
summary(fit)

fit <- glm(left~. -IT -RnD -Prod_Mgt, data = over.md, family = "binomial")
summary(fit)

formula(fit)

model <- lm(left ~ satisfaction_level + last_evaluation + number_project + 
                       average_montly_hours + time_spend_company + Work_accident + 
                       promotion_last_5years + Support + Technical + Sales + 
                       Mktg + Acct + HR + Low_Salary + Med_Salary, data = over.md, family = "binomial")


over.md$score=predict(model,newdata=over.md,type = "response")

head(over.md$left)
head(over.md$score)

library(ggplot2)
ggplot(over.md,aes(y=left,x=score,color=factor(left)))+
  geom_point()+geom_jitter()

library(ROCR)

ROCRPred <- prediction(over.md$score, over.md$left)
ROCRPerf <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(.1, by = 0.1))
abline(a=0, b=1)
# Area under the curve
auc <- performance(ROCRPred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc,4)
auc
legend(.5, .3, auc, title = "AUC", cex = .75)
res <- predict(model, over.md, type = "response")
# table(ActualValue=d_test$Type, PredictedValue=res>0.4)
PredictedValue <- res>.5
head(PredictedValue)
pv <- as.numeric(PredictedValue)
head(pv)
pv <- as.factor(pv)
over.md$left <- as.factor(over.md$left)

library(caret)
confusionMatrix(pv, over.md$left)

test.md <- read.csv(file.choose())
str(test.md)

test.data <- test.md %>% 
  mutate(Support = as.numeric(sales == "support"),
         Technical =  as.numeric(sales == "technical"),
         Sales = as.numeric(sales == "sales"),
         IT = as.numeric(sales == "IT"),
         Mktg = as.numeric(sales == "marketing"),
         Prod_Mgt = as.numeric(sales == "product_mng"),
         Acct = as.numeric(sales == "accounting"),
         RnD = as.numeric(sales == "RandD"),
         HR = as.numeric(sales == "hr")) %>% 
  select(-sales)

test.data<- test.md %>% 
  mutate(Low_Salary = as.numeric(salary == "low"),
         Med_Salary = as.numeric(salary == "medium")) %>% 
  select(-salary)

str(over.md)

res <- predict(model, test.data, type = "response")

PredictedValue <- res>.5
pv <- as.numeric(PredictedValue)
head(pv)
table(pv)



#==========================Decision Tree====================================================

library(tree)
str(md)
md$left <- as.factor(md$left)
str(md)
tree.hr=tree(left~.,data=md)
plot(tree.hr)
text(tree.hr,pretty=0)
tree.pred=predict(tree.hr,newdata=md,type="class")

summary(tree.hr)

table(tree.pred, md$left)

tree.pred=predict(tree.hr,test.md,type="class")
table(tree.pred)

#======================================Random Forest===========================================

library(randomForest)

class_hr=randomForest(.,data=over.md)
class_hr
plot(class_hr)
## ------------------------------------------------------------------------
forest.pred=predict(class_hr,newdata=test.md,type = 'class')
table(forest.pred)
