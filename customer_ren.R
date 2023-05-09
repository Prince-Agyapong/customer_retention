# Import libraries
library(earth)
library(faraway)
library(ROCR)
library(tidyverse) 


# Load data
data <- read.csv("/Users/prince/Downloads/customerretentionMARS.csv")
#view(data)

# Check for duplicated rows based on ID column
duplicated_rows <- data[duplicated(data$ID), ]
names(data)
# View duplicated rows
duplicated_rows

# remove duplicated rows based on ID column
data <- distinct(data, ID, .keep_all = TRUE)

# Remove commas
data$Credit_Limit <- gsub(",", "", data$Credit_Limit)
data$Ending_Balance <- gsub(",", "", data$Ending_Balance)
data$Net_Payments_During_Cycle <- gsub(",", "", data$Net_Payments_During_Cycle )
data$Opening_Balance <- gsub(",", "", data$Opening_Balance)


# Check for missing values
any(is.na(data))

# columns with missing values
cols_missing <- colSums(is.na(data))

# column names with missing values
colnames(data)[which(cols_missing > 0)]

# Convert to correct data types
data$Months_On_Book <- as.integer(data$Months_On_Book)
data$Credit_Limit <- as.numeric(data$Credit_Limit)
data$Opening_Balance <- as.numeric(data$Opening_Balance)
data$Ending_Balance <- as.numeric(data$Ending_Balance)
data$Net_Payments_During_Cycle <- as.numeric(data$Net_Payments_During_Cycle)

bad_rate <- sum(data$Bad)/length(data$Bad)
#view(data)

# Remove Score2 since it has NAs - MARS model
data_mars = data %>%
  select(-Score2)

# check for missing values
any(is.na(data_mars))

# columns with missing values
cols_missing <- colSums(is.na(data_mars))

# column names with missing values
colnames(data_mars)[which(cols_missing > 0)]


# Check structure of data
str(data_mars)

# view data after wrangling
#view(data_mars)
#as_tibble(data_mars)

# summary statistics
summary(data)

# Exploratory Plots
# Plot 1: Bar plot of Outcome

ggplot(data , aes(x = factor(Bad), fill = factor(Bad))) +
  geom_bar(position = "dodge", color = "black") +
  ggtitle("Bar Plot of Bad") +
  xlab("bad") +
  ylab("Number of Customers") +
  scale_fill_manual(values = c("green", "red"), 
                    labels = c("NO", "YES")) +
  labs(fill = "Bad") +
  theme_light() 



# Splitting the dataset into the Training set and Test set (60% / 40%)
set.seed(123)
n = dim(data_mars)[1]
split = sample(1:n,size = n*0.6, replace = F)

# Train set
train_set = data_mars[split,] 
train_set_subset = subset(train_set, select = -c(ID))


#view(train_set_subset)

# Test set
test_set = data_mars[-split,] 
test_set_subset <- subset(test_set, select = -c(ID))
#view(test_set_subset)

# MARS model
mars_model <- earth(Bad ~ ., data = train_set_subset, 
                    glm = list(family = binomial))
# Summarize model

summary(mars_model)$coefficients

# MARS Model Prediction
# set scipen parameter to a higher value
options(scipen = 10)
mars_pred <- round(predict(mars_model, type ='response', newdata = test_set_subset), 3)
mars_pred


# Gains for MARS
mars_gains = gains(test_set_subset$Bad, mars_pred)
mars_gains 

# Extract columns from gains object into data frame
marsgains_df <- data.frame(depth = mars_gains$depth,
                           mean.prediction = mars_gains$mean.prediction,
                           mean.response = mars_gains$mean.resp)

# Plot of Depth against mean responses
ggplot(marsgains_df, aes(x = depth)) +
  geom_line(aes(y = mean.prediction, color = "Predicted"), linetype = "dashed") +
  geom_line(aes(y = mean.response, color = "Actual")) +
  labs(x = "Depth", y = "Mean Response",
       title = "Mean Response by Sampled Population for MARS Model") +
  scale_color_manual(name = "Response", values = c("Predicted" = "blue", 
                                                   "Actual" = "red")) +
  theme_minimal()

# Data frame with actual and predicted values
mars_results = as.data.frame(cbind(test_set_subset$Bad, mars_pred))
mars_results 

# Prediction
mars_roc.pred = prediction(mars_pred, test_set_subset$Bad,
                           label.ordering = NULL)

# Performance
mars.perf = performance(mars_roc.pred, measure = "tpr", x.measure = "fpr")
plot(mars.perf)

# Create a data frame with the ROC curve data
roc_mars_df <- data.frame(fpr = mars.perf@x.values[[1]], 
                          tpr = mars.perf@y.values[[1]])

# Calculate AUC 
auc_mars <- performance(mars_roc.pred, measure = "auc")@y.values[[1]]

# Plot ROC curve with reference line
ggplot(data = roc_mars_df, aes(x = fpr, y = tpr)) +
  geom_line(colour = "black", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  ggtitle(paste0("ROC Curve For MARS Model (AUC = ", round(auc_tree, 2), ")")) +
  theme_minimal()

# KS for mars
test2 = data.frame(mars.perf@x.values,mars.perf@y.values)
names(test2) = c('FPR','TPR')
head(test2)

#  plot percentile on the x axis and TPR and FPR as lines
cutcount = unlist(mars_roc.pred@tp) + unlist(mars_roc.pred@fp)
cutcount

# Percentile
percentile = cutcount/2448
percentile

# KS value
ksdif = test2$FPR - test2$TPR
ksdif
ks = max(abs(ksdif))

plot(percentile,test2$FPR,type='l',xlab ='Depth of File',
     ylab ='Cumulative Proportion',col='red',
     main = 'Kolmogorov-Smirnov Plot for MARS Model',)
points(percentile,test2$TPR,type = 'l',pch=1,col='forestgreen', lwd=1.5)
abline(0,1)
text(.2,.9,paste('Kolmogorov-Smirnov statistic =',round(ks,3)),cex=0.8)
which(abs(ksdif)==ks)

# Add a legend to the plot
legend("bottomright", 
       legend = c("False Positive Rate", "True Positive Rate"),
       col = c("red", "forestgreen"),
       lty = 1,
       lwd = 2,
       bty = "n")

fprks=test2$FPR[which(abs(ksdif)==ks)]
tprks=test2$TPR[which(abs(ksdif)==ks)]
x=percentile[which(abs(ksdif)==ks)]
segments(x0=x,y0=fprks,x1=x,y1=tprks,col='blue',lwd=2,lty=2)


# Splitting the dataset into the Training set and Test set (60% / 40%)
set.seed(123)
n1 = dim(data)[1]
split = sample(1:n,size = n1*0.6, replace = F)



# Splitting the dataset into the Training set and Test set (60% / 40%)
set.seed(123)
n = dim(data_ann)[1]
split = sample(1:n,size = n*0.6, replace = F)

# Train set
train_set = data_ann[split,] 
names(train_set)


# Test set
test_set = data_ann[-split,] 

# Feature scaling
train_set[-13] = scale(train_set[-13])
names(train_set)
test_set[-13] = scale(test_set[-13])
#view(test_set)

# ANN to the training set
library(h2o)
h2o.init(nthreads = -1) # Connect R to h2o cluster
#?h2o.deeplearning
ann_model = h2o.deeplearning(y = 'Bad',
                             training_frame = as.h2o(train_set),
                             activation = 'Rectifier',
                             hidden = c(6,6),
                             epochs = 100,
                             train_samples_per_iteration = -2)
summary(ann_model)

# Generate predictions using the trained model and the test set
ann_pred <- h2o.predict(ann_model, newdata = as.h2o(test_set[-13]))

# View the predicted values
ann_pred


# Extract the predicted probabilities from the data frame and convert to a vector
ann_pred = as.vector(ann_pred$predict)

# apply logistic function to convert to probabilities
ann_prob = 1 / (1 + exp(-ann_pred))

# Check the length of the predicted vector
length(ann_pred)


y_pred = if_else(ann_pred > 0.5, 1, 0)
y_pred = as.vector(y_pred)

cm = table(test_set[, 13], y_pred)
cm



library(ROCR)
library(gains)
# Gains for ANN
ann_gains = gains(test_set$Bad, ann_pred)
ann_gains

# Extract columns from gains object into data frame
anngains_df <- data.frame(depth = ann_gains$depth,
                          mean.prediction = ann_gains$mean.prediction,
                          mean.response = ann_gains$mean.resp)

# Plot of Depth against mean responses
ggplot(anngains_df, aes(x = depth)) +
  geom_line(aes(y = mean.prediction, color = "Predicted"), linetype = "dashed") +
  geom_line(aes(y = mean.response, color = "Actual")) +
  labs(x = "Depth", y = "Mean Response",
       title = "Mean Response by Sampled Population for ANN Model") +
  scale_color_manual(name = "Response", values = c("Predicted" = "blue", 
                                                   "Actual" = "black")) +
  theme_minimal() 


# Data frame with actual and predicted values
ann_results = as.data.frame(cbind(test_set$Bad, ann_pred))
ann_results 

# Prediction
ann_roc.pred = prediction(ann_pred, test_set$Bad)

# Performance
ann.perf = performance(ann_roc.pred, measure = "tpr", x.measure = "fpr")
plot(ann.perf)

# Create a data frame with the ROC curve data
roc_ann_df <- data.frame(fpr = ann.perf@x.values[[1]], 
                         tpr = ann.perf@y.values[[1]])

# Calculate AUC 
auc_ann <- performance(ann_roc.pred, measure = "auc")@y.values[[1]]

# Plot ROC curve with reference line
ggplot(data = roc_ann_df, aes(x = fpr, y = tpr)) +
  geom_line(colour = "purple", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  ggtitle(paste0("ROC Curve For ANN Model (AUC = ", round(auc_ann, 2), ")")) +
  theme_minimal()



# KS for ann
test3 = data.frame(ann.perf@x.values,ann.perf@y.values)
names(test3) = c('FPR','TPR')
head(test3)

#  plot percentile on the x axis and TPR and FPR as lines
cutcount = unlist(ann_roc.pred@tp) + unlist(ann_roc.pred@fp)
cutcount

# Percentile
percentile = cutcount/2448
percentile

# KS value
ksdif = test3$FPR - test3$TPR
ksdif
ks = max(abs(ksdif))

plot(percentile,test3$FPR,type='l',xlab ='Depth of File',
     ylab ='Cumulative Proportion',col='orange',
     main = 'Kolmogorov-Smirnov Plot for ANN Model',)
points(percentile,test3$TPR,type = 'l',pch=1,col='chocolate', lwd=1.5)
abline(0,1)
text(.2,.9,paste('Kolmogorov-Smirnov statistic =',round(ks,3)),cex=0.8)
which(abs(ksdif)==ks)

# Add a legend to the plot
legend("bottomright", 
       legend = c("False Positive Rate", "True Positive Rate"),
       col = c("orange", "chocolate"),
       lty = 1,
       lwd = 2,
       bty = "n")

fprks=test3$FPR[which(abs(ksdif)==ks)]
tprks=test3$TPR[which(abs(ksdif)==ks)]
x=percentile[which(abs(ksdif)==ks)]
segments(x0=x,y0=fprks,x1=x,y1=tprks,col='red',lwd=2,lty=2)

# Shut down h2o
#h2o.shutdown()











