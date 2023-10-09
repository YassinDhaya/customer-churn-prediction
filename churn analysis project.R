# Step 1: Get data
df <- read.csv("C:/Users/yassi/OneDrive/Bureau/project1/churn analysis project/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Step 2: Explore Data
# Spot problems
str(df)
#Explore data interactively with Explore package
install.packages("explore")
library(explore)
explore(df)
#churn rate baseline
library(magrittr)
library(dplyr)
(churn.base <- df %>% 
    group_by(Churn) %>% 
    count(Churn) %>% 
    mutate(perc = n/nrow(df) * 100) %>% 
    rename(customers = n))
#ordered bar plots - need to arrange column in desc order b4 plotting
library(ggpubr)
ggbarplot(churn.base, x = "customers", y = "perc",               
          fill = "Churn", 
          color = "white",
          palette = "jco",
          sort.val = "desc",          # Sort the value in dscending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 0,          # Rotate vertically x axis texts
          legend = "right",
          xlab = " Churn",
          ylab = " Percentage",
          label = paste(round(churn.base$perc,0),"%", sep = ""),
          label.pos = "out",
          title = "Churn rate baseline",
          ggtheme = theme_minimal()
)
table(df$Churn)
prop.table(table(df$Churn))
#gender on x axis
box1 <- ggplot(data = df, aes(x = gender, y = MonthlyCharges, fill = gender))+geom_boxplot() ##+ stat_summary(fun=mean, geom="point", shape=20, size=8, color="red", fill="red")

box2 <- ggplot(data = df, aes(x = gender, y = TotalCharges, fill = gender))+geom_boxplot()

box3 <- ggplot(data = df, aes(x = gender, y = tenure, fill = gender))+geom_boxplot()


#phone service on x axis
box4 <- ggplot(data = df, aes(x = PhoneService, y = MonthlyCharges, fill = PhoneService))+geom_boxplot()

box5 <- ggplot(data = df, aes(x = PhoneService, y = TotalCharges, fill = PhoneService))+geom_boxplot()

box6 <- ggplot(data = df, aes(x = PhoneService, y = tenure, fill = PhoneService))+geom_boxplot()

#contract on x axis
box7 <- ggplot(data = df, aes(x = Contract, y = MonthlyCharges, fill = Contract))+geom_boxplot() + stat_summary(fun=mean, geom="point", shape=20, size=8, color="red", fill="red") + coord_flip()
#Similar avg monthly charges across all the different term plans,
#with the largest variability amongst 1-year contract holders

box8 <- ggplot(data = df, aes(x = Contract, y = TotalCharges, fill = Contract))+geom_boxplot() # some missing values
#higher than avg total charges for those on 2-year contracts

box9 <- ggplot(data = df, aes(x = Contract, y = tenure, fill = Contract))+geom_boxplot()
#The longer the contract the longer the average customer tenure with -year contract giving the highest avg tenure. Makes sense!
install.packages("sjPlot")
sjPlot::plot_grid(box1, box2, box3, box4, labels="AUTO")
bi1 <- ggplot(data = df, aes(x = factor(Churn), y = tenure, fill = Churn)) +geom_boxplot()
#High churn for those with lower tenures

bi2 <- ggplot(data = df, aes(x = factor(Churn), y = MonthlyCharges, fill = Churn))+geom_boxplot()
#Higher churn for those with higher than avg monthly charges

bi3 <- ggplot(data = df, aes(x = factor(Churn), y = TotalCharges, fill = Churn))+geom_boxplot()
#Lower churn for those with lower than avg total charges
library(cowplot)
plot_grid(bi1, bi2, bi3, labels = "AUTO")
bibar1 <- ggplot(data = df, aes(x=gender, fill = factor(Churn))) + geom_bar(position = "fill") + scale_fill_manual(values = c("#1b9e77", "#d95f02")) + theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  plot.title = element_text(hjust = 0.5),
  text=element_text(size=14,  family="Helvetica")) + labs(x = " ", title = "Churn by gender")
#no difference by gender


bibar2 <- ggplot(data = df, aes(x=Contract, fill = factor(Churn)))+ geom_bar(position = "fill") + scale_fill_manual(values = c("#1b9e77", "#d95f02")) + theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  plot.title = element_text(hjust = 0.5),
  text=element_text(size=14,  family="Helvetica")) + labs(x = " ", title = "Contract type") + coord_flip()
#We have significantly higher churn than for those on month-to-month contracts


bibar3 <- ggplot(data = df, aes(x=factor(SeniorCitizen), fill = factor(Churn)))+ geom_bar(position = "fill") + scale_fill_manual(values = c("#1b9e77", "#d95f02")) + theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  plot.title = element_text(hjust = 0.5),
  text=element_text(size=14,  family="Helvetica")) + labs(x = " ", title = "Is a senior citizen")
#Higher churn for Senior Citizens


bibar4  <- ggplot(data = df, aes(x=PaperlessBilling, fill = factor(Churn))) + geom_bar(position = "fill") + scale_fill_manual(values = c("#1b9e77", "#d95f02")) + theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  plot.title = element_text(hjust = 0.5),
  text=element_text(size=14,  family="Helvetica")) + labs(x = " ", title = "Has paperless billing")
#higher churn rate for those on paperless billing

bibar5 <- ggplot(data = df, aes(x=PaymentMethod, fill = factor(Churn))) + geom_bar(position = "fill") + scale_fill_manual(values = c("#1b9e77", "#d95f02")) + theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  plot.title = element_text(hjust = 0.5),
  text=element_text(size=12,  family="Helvetica")) + labs(x = " ", title = "Payment Method") + coord_flip() 
#higher churn rate for those who pay by electronic
#check - lowest churn rates among those with
#automatic billing.

bibar6 <- ggplot(data = df, aes(x=PhoneService, fill = factor(Churn))) + geom_bar(position = "fill") + labs(title = "By phone service")
#no difference by phone service


bibar7 <- ggplot(data = df, aes(x=Partner, fill = factor(Churn))) + geom_bar(position = "fill") + scale_fill_manual(values = c("#1b9e77", "#d95f02")) + theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  plot.title = element_text(hjust = 0.5),
  text=element_text(size=12,  family="Helvetica")) + labs(x = " ", title = "Has a partner") 
#slightly higher churn rate for singles


bibar8 <-ggplot(data = df, aes(x=Dependents, fill = factor(Churn))) + geom_bar(position = "fill") + scale_fill_manual(values = c("#1b9e77", "#d95f02")) + theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  plot.title = element_text(hjust = 0.5),
  text=element_text(size=12,  family="Helvetica")) + labs(x = " ", title = "Has dependents") + coord_flip() 
#higher churn rate for those with no dependents

bibar9 <- ggplot(data = df, aes(x=MultipleLines, fill = factor(Churn))) + geom_bar(position = "fill") + labs(title = "Has multiple phone lines")
##very little difference if have multiple lines


bibar10 <- ggplot(data = df, aes(x=InternetService, fill = factor(Churn))) + geom_bar(position = "fill") + scale_fill_manual(values = c("#1b9e77", "#d95f02")) + theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  plot.title = element_text(hjust = 0.5),
  text=element_text(size=12,  family="Helvetica")) + labs(x = " ", title = "Internet service") + coord_flip() 
#Much higher churn if have fiber optic internet service


bibar11 <- ggplot(data = df, aes(x=OnlineSecurity, fill = factor(Churn))) + geom_bar(position = "fill") + coord_flip() + labs(title = "Online security")
#higher churn if have no online security

bibar12 <- ggplot(data = df, aes(x=OnlineBackup, fill = factor(Churn))) + geom_bar(position = "fill") + coord_flip() + labs(title = "Online backup")
#higher churn for those who have no online backup

bibar13 <- ggplot(data = df, aes(x=DeviceProtection, fill = factor(Churn))) + geom_bar(position = "fill") + coord_flip() + labs(title = "Device protection")
#higher churn for those who have no device protection

bibar14 <- ggplot(data = df, aes(x=TechSupport, fill = factor(Churn))) + geom_bar(position = "fill") + coord_flip() + labs(title = "Tech support")
#higher churn for those with no tech support

bibar15 <- ggplot(data = df, aes(x=StreamingTV, fill = factor(Churn))) + geom_bar(position = "fill") + labs(title = "Streaming TV")
#no difference among TV streamers - but very low churners if you don't have internet service

bibar16 <- ggplot(data = df, aes(x=StreamingMovies, fill = factor(Churn))) + geom_bar(position = "fill") + labs(title = "Streaming movies")
#no difference among movie streamers - but very low churners if you don't have internet service


plot_grid(bibar1, bibar2, bibar3, bibar4,
          bibar5, bibar6,
          labels = "AUTO")

plot_grid(bibar7, bibar8,
          bibar9, bibar10,
          labels = "AUTO")

plot_grid(bibar11, bibar12, bibar13,
          labels = "AUTO")

plot_grid(bibar14,
          bibar15, bibar16,
          labels = "AUTO")
sp1 <- ggplot(data = df, aes(x=MonthlyCharges, y=tenure, color=factor(Churn))) + geom_point(alpha = 0.5) + geom_smooth(method=lm) + labs(title = " ")
#lots of vertical lines - no horizontal patterns
#suggests that monthly charges has some association with churn as the churned customers are heavily represented on the higher end of the monthly charges
#there is most likely a higher likelihood of churn once you reach a certain point on the monthly fee


#believe that total charges can also be a proxy for tenure-hence the triangle looking scatterplot
sp2 <- ggplot(data = df, aes(x=TotalCharges, y=tenure, color=factor(Churn))) + geom_point(alpha = 0.5) +geom_smooth(method=lm) + labs(title = " ")

#histogram of tenure
hist(df$tenure)

#log transformation
sp3 <- ggplot(data = df, aes(x=MonthlyCharges, y=tenure, color=factor(Churn))) + geom_point(alpha = 0.5) + geom_smooth(method=lm) + scale_x_log10() + scale_y_log10() + labs(title = "Log transformation")

sp4 <- ggplot(data = df, aes(x=TotalCharges, y=tenure, color=factor(Churn))) + geom_point(alpha = 0.5) +geom_smooth(method=lm) + scale_x_log10() + labs(title = "Log transformation")

#awesome looking charts
plot_grid(sp1, sp2, sp3, sp4, labels="AUTO")

#Tenure and total charges are possibly collinear or bordering on collinearity!

#Internet Service
library(gmodels)
CrossTable(df$InternetService, df$Churn, digits=2, prop.c = TRUE,
           prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)
#

table(df$InternetService, df$Churn)

round(addmargins(prop.table(table(df$InternetService, df$Churn))),3)


#Statistical test
chisq.test(df$Churn, df$InternetService)
#Contract
CrossTable(df$Contract, df$Churn, digits=2, prop.c = TRUE,
           prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)
#

table(df$Contract, df$Churn)

round(addmargins(prop.table(table(df$Contract, df$Churn))),3)

#statistical test
chisq.test(df$Churn, df$Contract)

#Let's take age and put it into buckets
#let's get min and max Monthly Charge 
min(df$MonthlyCharges)
max(df$MonthlyCharges)
mean(df$MonthlyCharges)
median(df$MonthlyCharges)
#quartiles
ggplot(df, aes(y=MonthlyCharges)) + geom_boxplot()
#quantiles - takes a vector of proportions
quantile(df$MonthlyCharges, probs = c(0, 0.2, 0.4, 0.6, 0.8,1))#create monthly fee bands
df$monthly_fee_bin <- cut(df$MonthlyCharges, 
                          breaks= 5, 
                          labels=c("bin1", "bin2", "bin3", "bin4", "bin5"), 
                          right=FALSE)

table(df$monthly_fee_bin) #check results
#check averages across the different fee bands
(fees <- df %>%  
    group_by(monthly_fee_bin) %>% 
    summarize(avg_monthly_fee = format(round(mean(MonthlyCharges),2)),
              median_monthly_fee = format(round(median(MonthlyCharges),2))))
#visualize fee bands
ggplot(data = df, aes(x=monthly_fee_bin, fill = factor(Churn))) + geom_bar(position = "fill")
##Step 3: Manage data

#any missing data?

#Using VIM package
library(VIM)
missing <- aggr(df, prop = TRUE, bars = TRUE) # - some missing values
summary(missing)
## Show cases with missing values
(missing.df <- df[!complete.cases(df),]) #11 rows
#Some total charges missing

#We can remove missing rows - removing them won't affect our results that much - this is the code to remove below
df <- df[complete.cases(df),] 


#check results
sum(is.na(df$TotalCharges)) #all gone
#visualize entire dataframe again
aggr(df, prop = TRUE, bars = TRUE)
#any duplicate rows -
anyDuplicated(df)
#any duplicate columns -
names(df)[duplicated(names(df))]
df[names(df)[!duplicated(names(df))]]
# Step 4: Statistical analysis & understanding relationships
# Now we want to dig deeper We have some ideas of what to look for thanks to our EDA

#pairs.panels
library(psych)
df %>% 
  select_if(is.numeric) %>% 
  scale() %>% 
  pairs.panels()
# i take these results with a grain of salt.
df %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot(type = "upper", insig = "blank", addCoef.col = "black", diag=FALSE)
str(df$Churn)
(churnRate <- round(prop.table(table(df$Churn)),5))
(churn_rate_overall <- churnRate[[2]])
# recode churn variable into a numeric variable
df <- df %>% 
  mutate(Churn = ifelse(Churn == "Yes", 1, 0)) 

#check results
head(df$Churn)
#data frame grouped by fee
monthly.fee.df <- df %>% 
  group_by(monthly_fee_bin) %>% 
  summarize(total_count = n(),
            total_churns = sum(Churn)) %>% 
  mutate(churn_rate = total_churns/total_count)
#data frame grouped by fee
internet.df <- df %>% 
  group_by(InternetService) %>% 
  summarize(total_count = n(),
            total_churns = sum(Churn)) %>% 
  mutate(churn_rate = total_churns/total_count)

head(internet.df) #check results

#check churn columns
sum(internet.df$total_churns)
sum(internet.df$total_count)

#add highlight flag column
internet.df <- internet.df %>% 
  mutate(highlight_flag =
           ifelse(churn_rate > churn_rate_overall, 1, 0))

#check results
head(internet.df$highlight_flag)

#plot response rate by fee difference
library(stringr)
ggplot(data=internet.df, aes(x=reorder(InternetService, churn_rate), y=churn_rate,
                             fill = factor(highlight_flag))) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=churn_rate_overall, linetype="dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
  scale_fill_manual(values = c('#595959', '#e41a1c')) +
  labs(x = ' ', y = 'Churn Rate', title = str_c("Churn rate by internet service")) +
  theme(legend.position = "none")
#data frame grouped by payment method
pymt.method.df <- df %>% 
  group_by(PaymentMethod) %>% 
  summarize(total_count = n(),
            total_churns = sum(Churn)) %>% 
  mutate(churn_rate = total_churns/total_count)

head(pymt.method.df) #check results

#check churn columns
sum(pymt.method.df$total_churns)
sum(pymt.method.df$total_count)

#add highlight flag column
pymt.method.df <- pymt.method.df %>% 
  mutate(highlight_flag =
           ifelse(churn_rate > churn_rate_overall, 1, 0))

#check results
head(pymt.method.df$highlight_flag)

#plot response rate by fee difference
ggplot(data=pymt.method.df, aes(x=reorder(PaymentMethod, churn_rate), y=churn_rate,
                                fill = factor(highlight_flag))) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=churn_rate_overall, linetype="dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
  scale_fill_manual(values = c('#595959', '#e41a1c')) +
  labs(x = ' ', y = 'Churn Rate', title = str_c("Churn rate by payment method")) +
  theme(legend.position = "none")
# Step 5A: Data pre-proessing
#let's use a fresh dataset
df_dummy <- read.csv("C:/Users/yassi/OneDrive/Bureau/project1/churn analysis project/WA_Fn-UseC_-Telco-Customer-Churn.csv")
str(df_dummy$Churn) #check churn variable
table(df_dummy$Churn)
# Part 5B: Modeling - Logistic Regression

# Check total number of rows in our dataset before splitting
nrow(df_dummy)
#Set seed for reproducibility
set.seed(123) 

#Split the dataset into training and test sets at 70:30 ratio using caTools package
library(caTools)
split <- sample.split(df_dummy$Churn, SplitRatio = 0.7)
train_data <- subset(df_dummy, split == TRUE)
test_data <- subset(df_dummy, split == FALSE)

#Check if distribution of partition data is correct for train and test set
prop.table(table(train_data$Churn))
prop.table(table(test_data$Churn)) 
set.seed(1)  # set seed for reproducibility

#Use glm function from glmnet package to create model
train_data$Churn <- ifelse(train_data$Churn == "yes", 1, 0)    
model_01_glm <- glm(formula = Churn ~ ., 
                    data = train_data, family = "binomial")

#use to debug if glm function not working   
train_data %>%
   select_if(is.factor) %>%
   lapply(table)
unique(train_data$Churn)

#Print the model output
summary(model_01_glm)
##Individual coefficients significance and interpretation

#library(coef.lmList)
summary(model_01_glm)
#prints confidence intervals
exp(confint(model_01_glm))
#plot coefficients on odds ratio using sjPlot package
plot_model(model_01_glm, vline.color = "red",
           sort.est = TRUE, show.values = TRUE)
#Calculate Variance Inflation factor using car package
vif(model_01_glm)

pred <- predict(model_01_glm, test_data, type = "response") #predict using test data

#check results
head(pred)
predicted <- round(pred) #>0.5 will convert to 1

#contingency table
contingency_tab <- table(test_data$Churn, predicted)
contingency_tabpredicted <- round(pred) #>0.5 will convert to 1

#contingency table
contingency_tab <- table(test_data$Churn, predicted)
contingency_tab
# Confusion Matrix using the caret package
caret::confusionMatrix(contingency_tab)
#Plot ROC Curve & Calculate AUC area
library(ROCR)

#ROC Curves are useful for comparing classifiers

#Check data structures first or else the ROC curve won't plot
typeof(predicted)
typeof(test_data$Churn)
pr <- prediction(pred, test_data$Churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(prf)

#The ideal ROC curve hugs the top left corner, indicating a high
#true positive rate and a low false positive rate.
#True positive rate on y-axis
#False positive rate on the x-axis

#The larger the AUC, the better the classifier
#The AUC line is insufficient to identify a best model
#It's used in combination with qualitative examination
#of the ROC curve
auc <- performance(pr, measure = "auc")
auc
as.numeric(performance(pr, measure = "auc")@y.values)
#Double density plot for explaining and picking thresholds of predicted churn probabilities. Perhaps better alternative than AUC or ROC curve
ggplot(data = test_data) + geom_density(aes(x=pred, color = Churn,linetype = Churn))
set.seed(1)  # for reproducibility
model_02_glm <- glm(formula = Churn ~ . -MonthlyCharges,
                    data = train_data, family = "binomial")

summary(model_02_glm)
vif(model_02_glm)
#plot coefficients on odds ratio
plot_model(model_02_glm, vline.color = "red",
           sort.est = TRUE, show.values = TRUE)
pred <- predict(model_02_glm, test_data, type = "response") #predict using test data

#check results
head(pred)
predicted <- round(pred) #>0.5 will convert predicted values to 1

#contingency table - manually done with table function
contingency_tab <- table(test_data$Churn, predicted)
contingency_tab
# Confusion Matrix using the caret package
caret::confusionMatrix(contingency_tab)
#Plot ROC Curve & Calculate AUC area
library(ROCR)


#check data structures first
typeof(predicted)
typeof(test_data$Churn)
pr <- prediction(pred, test_data$Churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(prf)
#The ideal ROC curve hugs the top left corner, indicating a high
#true positive rate and a low false positive rate.
#True positive rate on y-axis
#False positive rate on the x-axis

#The larger the AUC, the better the classifier
#The AUC line is insufficient to identify a best model
#It's used in combination with qualitative examination
#of the ROC curve
auc <- performance(pr, measure = "auc")
auc
as.numeric(performance(pr, measure = "auc")@y.values)
# Part 5B: Modeling - Random Forest
# Train a Random Forest using randomForest package
set.seed(1)  # for reproducibility
model_01_rf <- randomForest(formula = Churn ~ ., data = train_data, importance = TRUE, na.action=na.exclude)

# Print the model output                
print(model_01_rf)
# prints variable importance
summary(model_01_rf)


varImpPlot(model_01_rf, main="Variable Importance")
# Train a Random Forest
set.seed(1)  # for reproducibility
model_02_rf <- randomForest(formula = Churn ~ . -tenure,
                            data = train_data, importance = TRUE)

# Print the model output
print(model_02_rf)

varImpPlot(model_02_rf, main="Variable Importance without tenure")
# Grab OOB error matrix & take a look
err <- model_01_rf$err.rate
head(err)

# Look at final OOB error rate (last row in err matrix)
oob_err <- err[nrow(err), "OOB"]
print(oob_err)

# Plot the model trained
plot(model_01_rf)

# Add a legend since it doesn't have one by default
legend(x = "right", 
       legend = colnames(err),
       fill = 1:ncol(err))
# Generate predicted classes using the model object
class_prediction <- predict(object = model_01_rf,   # model object 
                            newdata = test_data,  # test dataset
                            type = "class") # return classification labels

# Calculate the confusion matrix for the test set
cm <- caret::confusionMatrix(data = class_prediction,#predicted classes
                             reference = test_data$Churn)  # actual classes
print(cm)

# Compare test set accuracy to OOB accuracy
paste0("Test Accuracy: ", cm$overall[1])
paste0("OOB Accuracy: ", 1 - oob_err)
# Generate predictions on the test set
pred <-predict(object = model_01_rf,
               newdata = test_data,
               type = "prob") 

# Uncomment to take a look at the pred format - `pred` object is a matrix
#head(pred)

# Compute the AUC (`actual` must be a binary 1/0 numeric vector)
round(auc(actual = ifelse(test_data$Churn == "1", 1, 0),
          predicted = pred[,"1"]) ,2)# Train a Random Forest
set.seed(1)  # for reproducibility
model_03_rf_final <- randomForest(formula = Churn ~ ., 
                                  mtry = 4,
                                  nodesize = 7,
                                  sampsize = 3445.4,
                                  data = train_data, importance = TRUE, keep.forest=TRUE)

# Print the model output
print(model_03_rf_final)

#variable importance
importance(model_03_rf_final)

#variable importance plot
varImpPlot(model_03_rf_final, main="Variable importance on the tuned model")
# Generate predictions on the test set
pred <-predict(object = model_03_rf_final,
               newdata = test_data,
               type = "prob") 

round(auc(actual = ifelse(test_data$Churn == "1", 1, 0),
          predicted = pred[,"1"]) ,2)
require(pROC)
rf.roc<-roc(train_data$Churn,model_03_rf_final$votes[,2])
plot(rf.roc)
auc(rf.roc)
# Train the model (to predict 'Churn') using rpart package
tree_mod_01 <- rpart(formula = Churn ~., 
                     data = train_data,
                     method = "class")

# Look at the model output                  
print(tree_mod_01)


# Display the results using rpart.plot package
rpart.plot(x = tree_mod_01, yesno = 2, type = 0, extra = 0)

rpart.plot(tree_mod_01,
           yesno = 2,
           extra = 104, # show fitted class, probs, percentages
           box.palette = "GnBu", # color scheme
           branch.lty = 3, # 1= solid, 3 = dotted branch lines
           shadow.col = "gray", # shadows under the node boxes
           nn = TRUE, 
           main = "Classification tree on our churn data")
# Train a gini-based model
tree_mod_02<- rpart(formula = Churn ~., 
                    data = train_data_tree,
                    method = "class",
                    parms = list(split = "gini"))

# Display the results
rpart.plot(x = tree_mod_02, yesno = 2, type = 0, extra = 0)

# Train an information-based model
tree_mod_03<- rpart(formula = Churn ~., 
                    data = train_data_tree, 
                    method = "class",
                    parms = list(split = "information"))

# Display the results
rpart.plot(x = tree_mod_03, yesno = 2, type = 5, extra = 6)

# Generate predictions on the test set using the gini model
pred1 <- predict(object = tree_mod_02, 
                 newdata = test_data_tree,
                 type = "class")    

# Generate predictions on the test set using the information model
pred2 <- predict(object = tree_mod_03, 
                 newdata = test_data_tree,
                 type = "class")

# Compare classification error - using ModelMetrics library
ce(actual = test_data$Churn, 
   predicted = pred1)
ce(actual = test_data$Churn, 
   predicted = pred2)  
# Part 6: Summary of Insights and hypotheses generated and testing them
# What insights have we discovered and can confidently report to our stakeholders?
  
  
  df %>% 
  filter(Churn == "Yes" & InternetService == "Fiber optic") %>% 
  summarize(lost_revenue = sum(TotalCharges))