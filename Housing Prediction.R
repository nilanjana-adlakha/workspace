#GET train, test and macros data
data_macro <- read.csv("/Users/nilanjana/Documents/Springboard/Kaggle Competition/macro.csv", header = TRUE, stringsAsFactors = FALSE)
data_train <- read.csv("/Users/nilanjana/Documents/Springboard/Kaggle Competition/train.csv", header = TRUE, stringsAsFactors = FALSE)
data_test <- read.csv("/Users/nilanjana/Documents/Springboard/Kaggle Competition/test.csv", header = TRUE, stringsAsFactors = FALSE)

#convert the timestamp to date objects so that a join can be created between the housing and macros data
data_train$timestamp <- as.Date(as.character(data_train$timestamp), format = "%m/%d/%y")
data_macro$timestamp <- as.Date(data_macro$timestamp, format = "%Y-%m-%d")
data_test$timestamp <- as.Date(as.character(data_test$timestamp), format = "%Y-%m-%d")

summary(data_train$price_doc)

library(ggplot2)
library(dplyr)
library(lubridate)
library(caret)
# show the distribution of price_doc in the dataset
ggplot(aes(x=price_doc), data=data_train) + 
  geom_density(fill='red', color='red') + 
 # facet_grid(~product_type) + 
  scale_x_continuous(trans='log')
#transform price_doc - create log transformation of price_doc
data_train$price_doc <- log1p(data_train$price_doc)
#replace NA's with -1
data_train[is.na(data_train)] = -1
data_test[is.na(data_test)] = -1
# merge both the train and test dataset to apply data wrangling on train and test dataset
data_test$price_doc <- NA
data <- rbind(data_train,data_test)

#clean full_sq and life_sq. sometime full_sq is smaller than life_sq
data <- data%>% dplyr::mutate(life_sq = ifelse(is.na(life_sq),full_sq,life_sq))%>%
                dplyr::mutate(full_sq = ifelse(life_sq>full_sq,life_sq,full_sq))
                

#build_year - the dataset has some annomalies - ceratin values look like data entry error and thus all the values before 1690 and 2020 are replaced by NA's
data <- data %>% dplyr::mutate(build_year = ifelse((build_year >1690 & build_year<2020),build_year,NA))
data$build_year <- as.integer(data$build_year)

#num_rooms- 0 is replaced with NA, as 0 can be misleading
data <- data %>% dplyr::mutate(num_room = ifelse(num_room==0,'NA',num_room))

data$num_room <- as.integer(data$num_room)

#state - there is one value of 33 in the entire dataset, it looks like a data entry error and is replaced with 3.
data <- data %>% dplyr::mutate(state = ifelse(state==33,3,state))
#=====================================================================================================================================================
#Exploratory Data Analysis
#-----------------------------------------------------------------------------------------------------------------------------------------------------

#Sale Type - distribution of price_doc for each product type - Investment & Owner Occupied
ggplot(aes(x=price_doc), data=data[which(!is.na(data$price_doc)),]) + 
  geom_density(fill='red', color='red') + 
  facet_grid(~product_type) + 
  scale_x_continuous(trans='log')

data %>% group_by(product_type) %>% summarize(median(price_doc, na.rm = TRUE))

#Build Year - distribution of price_doc for each build year

data %>% 
  filter(build_year > 1690 & build_year < 2020) %>%
  group_by(build_year) %>% 
  summarize(mean_build_price=mean(price_doc, na.rm = TRUE)) %>%
  ggplot(aes(x=build_year, y=mean_build_price)) +
  geom_line(stat='identity', color='red') + 
  geom_smooth(color='darkgrey') +
  ggtitle('Mean price by year of build')

#Timestamp - Time series plot for each day over the time period
data %>%
  group_by(timestamp) %>%
  summarize(med_price = median(price_doc)) %>%
  ggplot(aes(x = timestamp, y = med_price)) +
  geom_line(color = 'red') +
  geom_smooth(method = 'lm', color = 'grey', alpha = 0.7) + 
  ggtitle('Daily median price over time')
#plot to identify seasonality over the time period
data %>% 
  mutate(month=month(timestamp)) %>%
  group_by(month) %>% 
  summarize(med_price=median(price_doc, na.rm= TRUE)) %>%
  ggplot(aes(x=as.integer(month), y=med_price)) +
  geom_line(color='red', stat='identity') + 
  geom_point(color='red', size=2) + 
  scale_x_continuous(breaks=seq(1,12,1)) + 
  labs(x='Month', title='Price by month of year')
#State of the house - distribution of the price_doc with respect to state of the house
data %>% 
  filter(!is.na(state)) %>% 
  ggplot(aes(x=as.factor(state), y=log10(price_doc))) + 
  geom_jitter(color='grey', alpha=0.2) + 
  geom_violin(fill='red', alpha=0.7) +
  ggtitle('Log10 of median price by state of home')
#material used to build the house - distribution of price_doc with respect to material of the house
data %>%
  filter(!is.na(material)) %>% 
  ggplot(aes(x=as.factor(material), y=log(price_doc))) + 
  geom_jitter(alpha=0.4, color='grey') + 
  geom_violin(fill='red', color='red',  alpha=0.6) + 
  ggtitle('Distribution of price by build material')
#floor - distribution of price_doc with respect to floor of the house
ggplot(aes(x=floor, y=log(price_doc)), data=data) + 
  geom_point(color='red', alpha=0.4) + 
  geom_smooth(method='lm', color='darkgrey') + 
  ggtitle('Price by floor of home')
#demographic variables
demo_vars <- c('area_m', 'raion_popul', 'full_all', 'male_f', 'female_f', 'young_all', 
               'young_female', 'work_all', 'work_male', 'work_female', 'price_doc')
require(corrplot)
corrplot(cor(data[, demo_vars], use='complete.obs'))
# how price_doc varies with population density 
data %>% 
  mutate(area_km=area_m/1000000, density=raion_popul/area_km) %>%
  select(sub_area, density, price_doc) %>%
  group_by(sub_area) %>%
  summarize(density=median(density, na.rm = TRUE), med_price=median(price_doc, na.rm = TRUE)) %>% 
  ggplot(aes(x=density, y=med_price)) +
  geom_point(color='grey') + 
  geom_smooth(method='lm', color='red') + 
  ggtitle('Median home price by raion population density (people per sq. km)')
#how price_doc varies with working age population
data %>% 
  mutate(work_share=work_all/raion_popul) %>%
  group_by(sub_area) %>%
  summarize(mean_price=mean(price_doc, na.rm  = TRUE), work_share=mean(work_share, na.rm = TRUE)) %>% 
  ggplot(aes(x=work_share, y=mean_price)) + 
  geom_point(color='red') + 
  geom_smooth(color='gray') + 
  ggtitle('District mean home price by share of working age population')

#how price_doc varies with distance from nearest park
ggplot(aes(x=park_km, y=price_doc), data=data) + 
  geom_point(color='red', alpha=0.4) + 
  geom_smooth(method='lm', color='grey') +
  ggtitle('Home price by distance to nearest park')

# Distance from Kremlin
ggplot(aes(x=kremlin_km, y=price_doc), data=data) +
  geom_point(color='grey') + 
  geom_smooth(method='lm', color='red') +
  ggtitle('Home price by distance to Kremlin')
#=====================================================================================================================================================

#=====================================================================================================================================================
# function that maps a categoric value to its corresponding numeric value and returns that column to the data frame
map.fcn <- function(cols, map.list, df){
  for (col in cols){
    df[col] <- as.numeric(map.list[data[,col]])
  }
  return(df)
}

line.list <- c("no" = 0,"yes"=1)
# convert categorical data to numeric data
char.cols <- c("water_1line","big_road1_1line","railroad_1line","culture_objects_top_25","thermal_power_plant_raion","incineration_raion",
               "oil_chemistry_raion","radiation_raion","railroad_terminal_raion","big_market_raion","nuclear_reactor_raion","detention_facility_raion")
data <- map.fcn(char.cols,line.list,data)
#extract year of day the house was valued at price_doc amount
data <- data %>% dplyr::mutate(year = year(data$timestamp))

#data <- data %>% dplyr::mutate(naCount = rowSums(is.na(data)))
#Create new variables from the existing house features so as to derive a better relation between the price_doc and floor,max floor,kitch_sq
#etc. as the area/floors in the building will vary for different regions based on it's geographical location. A metropolitan city might have
#more number of floors compared to a smaller city. 
data <- data %>% dplyr::mutate(rel_floor = floor/max_floor)%>%
                 dplyr::mutate(diff_floor = max_floor-floor)%>%
                 dplyr::mutate(rel_kitchen_sq = kitch_sq/full_sq)%>%
                 dplyr::mutate(rel_life_sq = life_sq/full_sq)%>%
                 dplyr::mutate(rel_kitchen_life = kitch_sq/life_sq)%>%
                 dplyr::mutate(rel_sq_per_floor = full_sq/floor)%>%
                 dplyr::mutate(diff_life_sq = full_sq-life_sq)%>%
                 dplyr::mutate(building_age = year - build_year)
# only certain imp varibales are picked from macros dataset based on different papers that suggest important macros economic variables used to predict prices of real estate 
data_macro <- subset(data_macro,select = c(timestamp,
                                            balance_trade,
                                            balance_trade_growth,
                                            eurrub,
                                            average_provision_of_build_contract,
                                            micex_rgbi_tr,
                                            micex_cbi_tr, 
                                            deposits_rate, 
                                            mortgage_value, mortgage_rate,
                                            income_per_cap, 
                                            museum_visitis_per_100_cap,
                                            cpi,
                                            apartment_build))
#NA's are imputed with -1
data_macro[is.na(data_macro)] = -1
features = colnames(data)
#function to convert characters and factors into numbers
for (f in features){
  if( (class(data[[f]]) == "character") || (class(data[[f]]) == "factor"))
  {
    levels = unique(data[[f]])
    data[[f]] = as.numeric(factor(data[[f]], level = levels)) 
  }
}

#material,build_year,state,product_type,sub area,ecology should be treated as numeric facgtors by the model
data$material <- as.factor(data$material)
data$build_year <- as.factor(data$build_year)
data$state <- as.factor(data$state)
data$product_type <- as.factor(data$product_type)
data$sub_area <- as.factor(data$sub_area)
data$ecology <- as.factor(data$ecology)

# one-hot-encoding features - create a dummy variable of the all the factors in the dataset
data = as.data.frame(data)
ohe_feats = c('material', 'build_year', 'state', 'product_type', 'sub_area', 'ecology')
dummies = dummyVars(~ material + build_year + state + product_type + sub_area + ecology , data = data)
df_all_ohe <- as.data.frame(predict(dummies, newdata = data))
df_all_combined <- cbind(data[,-c(which(colnames(data) %in% ohe_feats))],df_all_ohe)
#data = as.data.table(df_all_combined)
#join housing data to macros economic data
data <- left_join(data,data_macro,by = c('timestamp'))
data_final <- data
#=====================================================================================================================================================
#based on correlation matrix these features have very week correlation to price_doc, hence removing them from the data used to train the model
data[,c("date_year", "timestamp", "young_male", "school_education_centers_top_20_raion", "0_17_female", "railroad_1line", "7_14_female", "0_17_all", "children_school",
            "16_29_male", "mosque_count_3000", "female_f", "church_count_1000", "railroad_terminal_raion",
            "mosque_count_5000", "big_road1_1line", "mosque_count_1000", "7_14_male", "0_6_female", "oil_chemistry_raion",
            "young_all", "0_17_male", "ID_bus_terminal", "university_top_20_raion", "mosque_count_500","ID_big_road1",
            "ID_railroad_terminal", "ID_railroad_station_walk", "ID_big_road2", "ID_metro", "ID_railroad_station_avto",
            "0_13_all", "mosque_count_2000", "work_male", "16_29_all", "young_female", "work_female", "0_13_female",
            "ekder_female", "7_14_all", "big_church_count_500",
            "leisure_count_500", "cafe_sum_1500_max_price_avg", "leisure_count_2000",
            "office_count_500", "male_f", "nuclear_reactor_raion", "0_6_male", "church_count_500", "build_count_before_1920",
            "thermal_power_plant_raion", "cafe_count_2000_na_price", "cafe_count_500_price_high",
            "market_count_2000", "museum_visitis_per_100_cap", "trc_count_500", "market_count_1000", "work_all", 
            "build_count_slag", "leisure_count_1000", "0_13_male", "office_raion",
            "raion_build_count_with_builddate_info", "market_count_3000", "ekder_all", "trc_count_1000", "build_count_1946-1970",
            "office_count_1500", "cafe_count_1500_na_price", "big_church_count_5000", "big_church_count_1000", "build_count_foam",
            "church_count_1500", "church_count_3000", "leisure_count_1500",
            "16_29_female", "build_count_after_1995", "cafe_avg_price_1500", "office_sqm_1000", "cafe_avg_price_5000", "cafe_avg_price_2000",
            "big_church_count_1500", "full_all", "cafe_sum_5000_min_price_avg",
            "office_sqm_2000", "church_count_5000","0_6_all", "detention_facility_raion", "cafe_avg_price_3000",
            'children_preschool', 'preschool_quota', 'preschool_education_centers_raion',
             'school_quota', 'school_education_centers_raion', 
             'additional_education_raion',  'university_km')] <- NULL

#=======================================================================================================================================
#Ensemble Modeling
#---------------------------------------------------------------------------------------------------------------------------------------
set.seed(200)
#Divide the data into train and test data
train.data <- data[which(!is.na(data.new$price_doc)),]
train.data <- subset(train.data, select = -c(id))
test.data <- data[which(is.na(data.new$price_doc)),]
test_ids <- test.data$id
test.data <- subset(test.data, select = -c(price_doc,id))
#check if there are any NA's in the data
colnames(train.data)[colSums(is.na(train.data)) >0]
#Replace Nan with NA's
train.data <- data.frame(lapply(train.data, function(x) replace(x, is.infinite(x),NA)))
test.data <- data.frame(lapply(test.data, function(x) replace(x, is.infinite(x),NA)))
#impute NA's with -1
train.data[is.na(train.data)] = -1
test.data[is.na(test.data)] = -1
# function to normalize the data
normalize <- function(df){
  for(i in 1: ncol(df)){
    colName <- paste0(colnames(df[i]))
    min.val <- min(df[i], na.rm = TRUE)
    max.val <- max(df[i], na.rm = TRUE)
    normalized.val <- lazyeval::interp(~((colmn - min)/(max-min)),colmn = as.name(colName),min=min.val,max= max.val)
    df <- df %>% mutate_(.dots = setNames(list(normalized.val), colName))
  }
  return(df)
}

#train.data <- train.data[which(train.data$price_doc < oulier),]
train.data <- cbind(train.data, price_doc = train_price)
n <- nrow(train.data)
#Split Train data into train and validation dataset in 70:30 ratio
train.index <- sample(n,round(0.7*n) , replace = FALSE)
tr <- train.data[train.index, ]
val <- train.data[-train.index, ]

x.tr <- model.matrix(price_doc~.,data = tr)[,-1]
y.tr <- tr$price_doc

x.val <- model.matrix(price_doc~.,data = val)[,-1]
y.val <- val$price_doc
#calculate RMSE for the train data
sqrt(mean((mean(tr$price_doc) - val$price_doc)^2))
#linear model to predict price_doc
lmfit<- lm(price_doc~. , data = tr)
summary(lmfit)

#Lasso/Ridge/Elastic Net models to get min lambda values
glm.cv.ridge <- cv.glmnet(x.tr, y.tr, alpha = 0)
glm.cv.lasso <- cv.glmnet(x.tr, y.tr, alpha = 1)
glm.cv.net <- cv.glmnet((x.tr), y.tr, alpha = 0.005)

#train model on training data at minimum lambda for each regularized regression model
glm.ridge <- glmnet(x = x.tr, y = y.tr, alpha = 0, lambda = glm.cv.ridge$lambda.min )
glm.lasso <- glmnet(x = x.tr, y = y.tr, alpha = 1, lambda = glm.cv.lasso$lambda.min)
glm.net <- glmnet(x = x.tr, y = y.tr, alpha = 0.005, lambda = glm.cv.net$lambda.min)

coef(glm.net)
#predict price_doc for validation data
y_pred.ridge <- as.numeric(predict(glm.ridge, x.val))
y_pred.lasso <- as.numeric(predict(glm.lasso, x.val))
y_pred.net <- as.numeric(predict(glm.net, x.val))

#calculate RMSE for validation data
sqrt(mean((y_pred.ridge - y.val)^2))
sqrt(mean((y_pred.lasso - y.val)^2))
sqrt(mean((y_pred.net - y.val)^2))

#XGBoost Algorithm
#-----------------------------------------------------------------------------------------------------------------------------------------
library(Matrix)
library(xgboost)
data.new <- data
varnames <- setdiff(colnames(data.new), c("id","price_doc"))

cat("Create sparse matrix")
# To sparse matrix
train_sparse <- Matrix(as.matrix(sapply(data.new[which(!is.na(data.new$price_doc)),varnames],as.numeric)), sparse=TRUE)
test_sparse <- Matrix(as.matrix(sapply(data.new[which(is.na(data.new$price_doc)),varnames],as.numeric)), sparse=TRUE)
test_sparse <- Matrix(as.matrix(sapply(data.frame(x.val[,varnames]),as.numeric)), sparse=TRUE)
y_train <- data.new[which(!is.na(data.new$price_doc)),"price_doc"]
test_ids <- data.new[which(is.na(data.new$price_doc)),"id"]

dtrain <- xgb.DMatrix(data=train_sparse, label=y_train)
dtest <- xgb.DMatrix(data=test_sparse)
dtest <- xgb.DMatrix(data = x.val)
gc()

# Params for xgboost
param <- list(objective="reg:linear",
              eval_metric = "rmse",
              booster = "gbtree",
              eta = .05,
              gamma = 1,
              max_depth = 4,
              min_child_weight = 1,
              subsample = .7,
              colsample_bytree = .7
)

rounds = 1000
mpreds = data.table(id=test_ids)

for(random.seed.num in 1:10) {
  print(paste("[", random.seed.num , "] training xgboost begin ",sep=""," : ",Sys.time())) 
  set.seed(random.seed.num)
  xgb_model <- xgb.train(data = dtrain,
                         params = param,
                         watchlist = list(train = dtrain),
                         nrounds = rounds,
                         verbose = 1,
                         print.every.n = 5
                         # missing='NAN'
  )
  
  vpreds = predict(xgb_model,dtest) 
  mpreds = cbind(mpreds, vpreds)    
  colnames(mpreds)[random.seed.num+1] = paste("pred_seed_", random.seed.num, sep="")
}
#predict values for the validation dataset
mpreds_2 = mpreds[, id:= NULL]
colnames(mpreds_test)[2:10]<- c("out2","out3","out4","out5","out6","out7","out8","out9","out10")
mpreds_test <- mpreds[,11:20 ]
mpreds_test <- mpreds_test %>% group_by (out2,out3,out4,out5,out6,out7,out8,out9,out10,pred_seed_10)%>%
  dplyr::mutate(price_doc = max(out2,out3,out4,out5,out6,out7,out8,out9,out10,pred_seed_10))

#predict values for the test dataset - take max of all the 10 values as the price_doc output
mpreds_final <- mpreds %>% dplyr::group_by(pred_seed_1,pred_seed_2,pred_seed_3,pred_seed_4,pred_seed_5,pred_seed_6,pred_seed_7,
                                           pred_seed_8,pred_seed_9,pred_seed_10)%>%
  dplyr::mutate(price = max(pred_seed_1,pred_seed_2,pred_seed_3,pred_seed_4,pred_seed_5,pred_seed_6,pred_seed_7,
                            pred_seed_8,pred_seed_9,pred_seed_10))

#Get mean of all the models to get the final price_doc value on the validation data
y_pred <- (y_pred.ridge + y_pred.lasso + y_pred.net 
           + mpreds_test$price_doc)/4.0
head(y_pred)
#calculate accuracy of the model 
output <- data.frame(y_pred.ridge,y_pred.lasso,y_pred.net,y.val, y_pred)
accuracy <- ifelse(round(y_pred) == round(y.val),1,0)
output <- cbind(output,accuracy)
acc <- sum(accuracy)/nrow(output)

# Predict price_doc for the test data
y_pred.ridge_test <- as.numeric(predict(glm.ridge, as.matrix(test.data)))
y_pred.lasso_test <- as.numeric(predict(glm.lasso, as.matrix(test.data)))
y_pred.net_test <- as.numeric(predict(glm.net, as.matrix(test.data)))

#Get inverse log of price_doc for the final output for test data
#y_pred.ridge_test <- as.double(expm1(y_pred.ridge_test))
#y_pred.lasso_test <- as.double(expm1(y_pred.lasso_test))
#y_pred.net_test <- as.double(expm1(y_pred.net_test) )

#Get mean for all four models to get the final value for the train dataset
y_pred_test <- (y_pred.lasso_test+y_pred.net_test+y_pred.ridge_test+mpreds_final$price)/4.0
head(y_pred_test)
#FINAL DATA SET WITH ID AND PRICE_DOC (PREDICTED)
submission_data <- data.frame(id = test_ids, price_doc = expm1(y_pred_test))
write.csv(submission_2,"/Users/nilanjana/Documents/Springboard/Kaggle Competition/submission_11.csv")
#======================================================================================================================================
#=======================================================================================================================================
                      
