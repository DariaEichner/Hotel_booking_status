if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org")
#if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
#if(!require(CatEncoders)) install.packages("CatEncoders", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")


# Please put the data file hotel_bookings.csv into the same path as this R-script. 
# Please be aware it takes up to 1h to run the whole model. 

# Hotel booking demand datasets:
# https://www.sciencedirect.com/science/article/pii/S2352340918315191
# https://www.kaggle.com/jessemostipak/hotel-booking-demand?select=hotel_bookings.csv

#Load CSV file 
file_name <- "hotel_bookings.csv" 
csv_file <-  read.csv(file_name, na.strings=c("NA","<NA>",""," ",".","?","NULL"))

# What is the size of the dataset
csv_file%>% dim()

#what is the data type of each column  and the frist 3 values in the data frame
col_type <-sapply(csv_file, class)
knitr::kable(cbind(col_type,csv_file %>% head(3) %>% t()))


# show  summary statistics on the columns of the data frame
csv_file %>%summary() 

#Exploratory data analysis
# setting order of arrival_date_month -> Jan,Feb...Dec
csv_file$arrival_date_month<-factor(csv_file$arrival_date_month,levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))

# how high is the cancellation rate
csv_file %>% 
  select(is_canceled) %>%  
  group_by(is_canceled) %>%
  summarise(count_n=n()) %>%
  mutate(percent=count_n/sum(count_n))%>%
  ggplot(aes(is_canceled,percent,fill =factor(is_canceled))) +
  geom_bar(stat="identity",width = 0.6)+
  geom_text(aes(label=round(percent,digits = 2)), vjust=1.6, color="black", size=3.5)+
  scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    label = c("Not Canceled", "Canceled")
  )


# how high is the cancellation rate by hotel
csv_file %>% 
  select( hotel,is_canceled) %>%  
  group_by(hotel,is_canceled) %>%
  summarise(count_n=n()) %>%
  mutate(percent=count_n/sum(count_n))%>%
  ggplot(aes(is_canceled,percent,fill =factor(is_canceled))) +
  geom_bar(stat="identity",width = 0.6)+
  geom_text(aes(label=round(percent,digits = 2)), vjust=1.6, color="black", size=3.5)+
  scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    label = c("Not Canceled", "Canceled")
  )+
  facet_grid(.~hotel)+
  labs(title = "Cancellation Rate by Hotel",x = "Hotel")



# how does the Average Daily Rate change across Hotels
csv_file %>% 
  select( hotel,adr, is_canceled,arrival_date_month) %>% 
  filter(is_canceled==0) %>% 
  ggplot(aes(adr,fill=hotel)) +
  geom_density(alpha = 0.5)+
  scale_fill_manual(values=c("#999999", "#56B4E9"))+
  labs(title = "Distribution of Average Daily Rate by Hotel",x = "ADR")



# how does the the Average Daily Rate change across different months
csv_file %>% filter(adr<5000)%>%
  ggplot(aes(arrival_date_month,adr, fill = hotel)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#999999", "#56B4E9"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Change of Average Daily Rate by Month and Hotel",x = "")



# what is the most used customer type  by hotel
table(csv_file$customer_type,csv_file$hotel)

# what is the prefered  customer type  by hotel
csv_file%>%ggplot(aes(customer_type, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Hotel Preference by Customer Type",
       x = "Customer Type",
       y = "Count") +
  scale_fill_manual(values=c("#999999", "#56B4E9"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank())

# how long do people stay at the hotel by month and hotel
csv_file %>%
  group_by(arrival_date_month,hotel) %>%
  mutate(nights = mean(stays_in_weekend_nights + stays_in_week_nights)) %>%
  arrange(arrival_date_month,nights)%>%
  ggplot(aes(arrival_date_month,nights,fill=hotel))+
  geom_bar(stat = "identity",position = 'dodge')+
  scale_fill_manual(values=c("#999999", "#56B4E9"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank())+
  labs(title = " Avg # of stays by Month and Hotel",x = "")


# how does the reservation_status change by cancellation status
table(ifelse(csv_file$is_canceled==1,"Canceled ","Not Canceled"),csv_file$reservation_status)


##Preprocessing and Transformation

# check for missing values
apply(is.na(csv_file), 2, sum) %>%  knitr::kable()


# replacing missing values
hotel_booking <-csv_file %>% 
  mutate(children = ifelse(is.na(children), 0,children),
         country= factor(ifelse(is.na(country), 'Unknown',levels(country))),
         agent= ifelse(is.na(agent), 0,agent),
         company= ifelse(is.na(company), 0,company))


# Transforming and adding new features 
hotel_booking <-hotel_booking%>%
  mutate(
    arrival_month =as.numeric(arrival_date_month), 
    total_nights=stays_in_weekend_nights + stays_in_week_nights,
    guests=as.integer(adults + children+ babies),
    is_family= ifelse(children+ babies>0,1,0)
  )


#Drop reservation_status from data set as well as features that have been transformed
hotel_booking <-select(hotel_booking, -c(reservation_status,
                                         stays_in_weekend_nights,
                                         stays_in_week_nights,
                                         children, 
                                         babies,
                                         adults,
                                         reservation_status_date,
                                         arrival_date_month
)) 

#Feature selection for categorical and numerical 


# Saving names of categorical variables
categorical_name <- names(which(sapply(hotel_booking, is.factor)))
# Feature importance of categorical variables with rpart model
set.seed(1, sample.kind="Rounding")
fitControl <- trainControl(## 3-fold CV
  method = "cv",
  number = 3
)


train_cat_var <- train(factor(is_canceled)~.,
                       hotel_booking[,c(categorical_name,"is_canceled")],
                       method = "rpart",
                       metric = "Accuracy",
                       trControl = fitControl)



#Variable Importance of catecorical columns
imp_cat <-varImp(train_cat_var)
plot(imp_cat, top = 10)
imp_cat

#only the 6 variabels with a ranking >0 will remain
remain_cat_name <- c("deposit_type","distribution_channel","market_segment","assigned_room_type",
                     "customer_type","hotel")

# Saving names of numerical variables
numeric_name <- names(which(sapply(hotel_booking, is.numeric)))
#use PCA to reduce the number of features in the model
set.seed(1, sample.kind="Rounding")
pca_numerical<-hotel_booking[,c(numeric_name)] %>% select(-is_canceled) %>%prcomp(scale=TRUE)

#Find the the number of Components covering 95% of variance 
summary(pca_numerical)

#create dataset
dataset <- as.data.frame(cbind(pca_numerical$x[,1:16],hotel_booking[,c(remain_cat_name,"is_canceled")]))
# Train/Test set will be 20%
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = dataset$is_canceled, times = 1, p = 0.2, list = FALSE)
train_data <- dataset[-test_index,]
test_data <- dataset[test_index,]

#Setting parameters for Cross Validation
fitControl <- trainControl(method = "cv", 
                           number = 10, 
                           p = .9,
                           savePredictions = 'final', # To save out of fold predictions for best parameter combinantions
                           classProbs = T # To save the class probabilities of the out of fold predictions
)
#Training the general logistic regression  model

train_glm <- train(is_canceled~.,
                   train_data,
                   method = "glm",
                   family = "binomial",
                   trControl = fitControl)

#predict y_hat_glm for test_data
p_hat_glm <- predict(train_glm, newdata = test_data)
y_hat_glm <- ifelse(p_hat_glm >= 0.5, 1, 0) %>% factor()
# saving F1 Score for Logistic Regression Model
result_1 <- F_meas(data = y_hat_glm, reference = factor(test_data$is_canceled))
f1_results <- data_frame(method = "glm", f1_score =result_1,Recall=sensitivity(data = y_hat_glm, reference = factor(test_data$is_canceled)), Precision=specificity(data = y_hat_glm, reference = factor(test_data$is_canceled)))

# 2 Model RPart
set.seed(1, sample.kind="Rounding")
train_rp <- train(is_canceled~.,
                  train_data,
                  method = "rpart",
                  tuneGrid = data.frame(cp = 0),
                  trControl = fitControl)

# Predict y_hat_rp
p_hat_rp <- predict(train_rp, newdata = test_data)
y_hat_rp <- ifelse(p_hat_rp >= 0.5, 1, 0) %>% factor()
# saving F1 Score for Rpart Model
result_2 <-F_meas(data = y_hat_rp, reference = factor(test_data$is_canceled))
f1_results <- bind_rows(f1_results,data_frame(method = "rpart", f1_score =result_2,Recall=sensitivity(data = y_hat_rp, reference = factor(test_data$is_canceled)), Precision=specificity(data = y_hat_rp, reference = factor(test_data$is_canceled))))


# 3 Model KNN
train_knn <- train(is_canceled~.,
                   data=train_data,
                   method = "knn",
                   tuneGrid = data.frame(k = 7),
                   trControl = fitControl)

#predict  y_hat_knn
p_hat_knn <- predict(train_knn, newdata = test_data)
y_hat_knn <- ifelse(p_hat_knn >= 0.5, 1, 0) %>% factor()
#The F1-score can be adapted to weigh specificity and sensitivity differently.
result_3 <-F_meas(data = y_hat_knn, reference = factor(test_data$is_canceled))
f1_results <- bind_rows(f1_results,
                        data_frame(method = "knn", 
                                   f1_score =result_3,
                                   Recall=sensitivity(data = y_hat_knn, reference = factor(test_data$is_canceled)), 
                                   Precision=specificity(data = y_hat_knn, reference = factor(test_data$is_canceled))))


#Out of fold predictions for train Data
#Predicting the out of fold prediction probabilities for training data

train_pred_oof <- data.frame(glm  = train_glm$pred$pred[order(train_glm$pred$rowIndex)],
                             rpart= train_rp$pred$pred[order(train_rp$pred$rowIndex)],
                             knn  = train_knn$pred$pred[order(train_knn$pred$rowIndex)]
)

#Predicting probabilities for the test data
test_pred <- data.frame(glm  = predict(train_glm,newdata = test_data),
                        rpart= predict(train_rp,newdata = test_data),
                        knn  = predict(train_knn,newdata = test_data)
                        )
#Using GBM as top layer model and OOF as features
top_layer_gbm<- train(x=train_pred_oof,y=train_data$is_canceled,
                      method='gbm',
                      trControl = fitControl,
                      verbose = FALSE)

#predict test data using GBM as final layer model
p_hat_stacked <- predict(top_layer_gbm,newdata=test_pred)
y_hat_stacked <- ifelse(p_hat_stacked >= 0.5, 1, 0) %>% factor()
#The F1-score can be adapted to weigh specificity and sensitivity differently.
result_4 <-F_meas(data = y_hat_stacked, reference = factor(test_data$is_canceled))
#Saving results to Acc_results
f1_results <- bind_rows(f1_results,
                        data_frame(method = "GBM", 
                                   f1_score =result_4,
                                   Recall=sensitivity(data = y_hat_stacked, reference = factor(test_data$is_canceled)), 
                                   Precision=specificity(data = y_hat_stacked, reference = factor(test_data$is_canceled))))

#Final results of all models
f1_results %>%knitr::kable()





