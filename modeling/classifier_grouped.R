library(data.table)
library(dplyr)
library(caret)
library(ggplot2)


################################################################################################################################################
################################################################################################################################################
### DATA PREPARATION
################################################################################################################################################
################################################################################################################################################

AVG_YRLY_RISE = 23.5/51

df = fread('../data/processed/training_data.csv')
df = df[!is.na(df$mean_sc),]
df = df[!is.na(df$reedy_sl),]
df$date = as.Date(df$date)


df <- df %>% filter(location != 'reedy') %>%
            mutate(inflow = inflow/0.0283168,
                      above_1000 = ifelse(mean_sc > 1120, 1, 0),
                    year = year(date),
                    month = month(date),
                    half = ifelse(month <= 6, 1,2),
                   adjusted_sl := reedy_sl + 23.5,
                   years_to_2060 = 2015 - year,
                   ratio = reedy_sl / inflow,
                   rise_ratio = adjusted_sl / inflow)
                  

df = as.data.table(df)



#che_bfb[, adjusted_sl := (reedy_sl- 49.229 + 2.79 + 23.5)]
#che_bfb[, reedy_sl := (reedy_sl- 49.229)]

#che_bfb[, assumed_sl_rise := (51-years_to_2060)*AVG_YRLY_RISE]
#che_bfb[, bad_sc := ifelse(above_1000 == 1, 0, -1000)]
#che_bfb[, ratio := 10000*reedy_sl / inflow]
#che_bfb[, ratio2 := 10000*adjusted_sl / inflow]

# thresholds obtained via summary stats
df[, inflow_type := ifelse(inflow < 3810, 'low_flow', 
                                ifelse( (inflow < 8305 & inflow >= 3810), 'med_flow', 'high_flow'))]
df[, very_low_flow := ifelse(inflow_type == 'low_flow', 1, 0)]
df[, low_flow := ifelse(inflow_type == 'med_flow', 1, 0)]
df[, high_flow := ifelse(inflow_type == 'high_flow', 1, 0)]
#che_bfb = che_bfb[inflow > 1 & ratio > -500]

# visualize adjusted sea levels
ggplot(df, aes(x = date, y = reedy_sl))+
  geom_point(size = .1)+
  geom_point(aes(y = adjusted_sl, color = 'adjusted_sl'), size = .1)

agged_df <- df[, c('inflow', 'mean_sc', 'reedy_sl', 'above_1000', 'location',
                          'year', 'half', 'ratio', 'very_low_flow', 'low_flow', 'high_flow')] %>%
  group_by(year, half, location) %>%
  summarise(bad_sc_days = sum(above_1000),
            high_flow = sum(high_flow),
            low_flow = sum(low_flow),
            very_low_flow = sum(very_low_flow),
            avg_ratio = mean(ratio),
            max_ratio = max(ratio),
            min_ratio = min(ratio),
            problem_year_10 = ifelse(bad_sc_days >= 10, 'salty', 'fresh'),
            problem_year_20 = ifelse(bad_sc_days >= 20, 'salty', 'fresh'))


scale_df$year = agged_df$year
scale_df$half = agged_df$half
scale_df$location = agged_df$location
scale_df$problem_year_10 = agged_df$problem_year_10  
scale_df$problem_year_20 = agged_df$problem_year_20  

scale_df$problem_year_10 = as.factor(scale_df$problem_year_10)
scale_df$problem_year_20 = as.factor(scale_df$problem_year_20)


# my tuneGrid object:
tgrid <- expand.grid(
  mtry = c(1,2,3,4,5,6),
  splitrule = c("gini"),
  min.node.size = c(1, 2,3,4)
)



indices = sort(sample(nrow(scale_df), nrow(scale_df)*.75))
train<-scale_df[indices,]
test<-scale_df[-indices,]


model_caret_10 <- train(problem_year_10 ~ high_flow + low_flow  + very_low_flow+ avg_ratio+
                       max_ratio + min_ratio,
                     data = train,
                     method = "ranger",
                     trControl = trainControl(method="cv", number = 5, verboseIter = T, classProbs = T),
                     tuneGrid = tgrid,
                     importance = 'impurity' #permutation
)


model_caret_20 <- train(problem_year_20 ~ high_flow + low_flow  + very_low_flow+ avg_ratio+
                          max_ratio + min_ratio,
                        data = train,
                        method = "ranger",
                        trControl = trainControl(method="cv", number = 5, verboseIter = T, classProbs = T),
                        tuneGrid = tgrid,
                        importance = 'impurity' #permutation
)

plot(varImp(model_caret_10))
plot(varImp(model_caret_20))

# TEST MODELS ON HOLDOUT DATA

test_second_half = as.data.table(test %>% filter(half == 2))

#BFB CTRL 10: 11 fresh predictions, 100% accuracy
probsTest <- predict(model_caret_10, test_second_half %>% filter(location == 'del_bfb'), type = "prob")
threshold <- 0.4
bfb_ctrl_10  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(bfb_ctrl_10)
confusionMatrix(bfb_ctrl_10, test_second_half[location == 'del_bfb']$problem_year_10)

#CHESTER CTRL 10: 7 fresh, 4 salty, 100% accuracy
probsTest <- predict(model_caret_10, test_second_half %>% filter(location == 'schu_chester'), type = "prob")
threshold <- 0.4
chester_ctrl_10  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(chester_ctrl_10)
confusionMatrix(chester_ctrl_10, test_second_half[location == 'schu_chester']$problem_year_10)


#PHASE 3: BFB CTRL 20: 11 fresh predictions, 100% accuracy
probsTest <- predict(model_caret_20, test_second_half %>% filter(location == 'del_bfb'), type = "prob")
threshold <- 0.4
bfb_ctrl_20  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(bfb_ctrl_20)
confusionMatrix(bfb_ctrl_20, test_second_half[location == 'del_bfb']$problem_year_20)



#PHASE 4: CHESTER CTR 20: 9 fresh, 2 salty,; 90% accuracy
probsTest <- predict(model_caret_20, test_second_half %>% filter(location == 'schu_chester'), type = "prob")
threshold <- 0.4
chester_ctrl_20  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(chester_ctrl_20)
confusionMatrix(chester_ctrl_20, test_second_half[location == 'schu_chester']$problem_year_20)



# ADJUST DATA TO INCLUDE SL RISE



agged_df2 <- as.data.table(df[, c('inflow', 'mean_sc', 'adjusted_sl', 'above_1000', 'location',
                           'year', 'half', 'rise_ratio', 'very_low_flow', 'low_flow', 'high_flow')] %>%
  group_by(year, half, location) %>%
  summarise(bad_sc_days = sum(above_1000),
            high_flow = sum(high_flow),
            low_flow = sum(low_flow),
            very_low_flow = sum(very_low_flow),
            avg_ratio = mean(rise_ratio),
            max_ratio = max(rise_ratio),
            min_ratio = min(rise_ratio)))

agged_df2[, problem_year_10 := ifelse(bad_sc_days >= 10, 'salty', 'fresh')]
agged_df2[, problem_year_20 := ifelse(bad_sc_days >= 20, 'salty', 'fresh')]

# normalize data

normalized_avg_ratio <- approx(agged_df$avg_ratio, scale_df$avg_ratio, agged_df2$avg_ratio)$y
normalized_min_ratio <- approx(agged_df$min_ratio, scale_df$min_ratio, agged_df2$min_ratio)$y
normalized_max_ratio <- approx(agged_df$max_ratio, scale_df$max_ratio, agged_df2$max_ratio)$y


scale_df2 = scale_df
scale_df2$avg_ratio = normalized_avg_ratio
scale_df2$max_ratio = normalized_max_ratio
scale_df2$min_ratio = normalized_min_ratio





# use the same rows of data for re-testing the models predictions using the revised data
test2 <- agged_df2[-indices,]

test_second_half2 = as.data.table(test2 %>% filter(half == 2))

# COMPARE MODEL OUTPUT USING UNADJUSTED DATA (WHICH HAD HIGH ACCURACY)
# TO MODEL OUTPUT USING ADJUSTED DATA (WHICH HAS TO REAL VALIDATION TO USE)

#BFB VAR 10: 11 fresh predictions BECOMES 11 fresh predictions -> no change 
probsTest <- predict(model_caret_10, test_second_half2 %>% filter(location == 'del_bfb'), type = "prob")
threshold <- 0.4
bfb_var_10  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(bfb_var_10)


#CHESTER VAR 10: 7 fresh, 4 salty BECOMES  3 fresh, 13 salty -> 44% increase in number of years with 10+ salty days
probsTest <- predict(model_caret_10, test_second_half2 %>% filter(location == 'schu_chester'), type = "prob")
threshold <- 0.4
chester_var_10  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(chester_var_10)


#PHASE 3: BFB CTRL 20: 10 fresh predictions BECOMES 10 fresh predictions -> no change 
probsTest <- predict(model_caret_20, test_second_half2 %>% filter(location == 'del_bfb'), type = "prob")
threshold <- 0.4
bfb_var_20  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(bfb_var_20)


#PHASE 4: CHESTER CTR 20: 5 fresh, 11 salty; 69% accuracy BECOMES 4 fresh years 12 salty years -> 10% increase in number of years with 20 or more salty days
probsTest <- predict(model_caret_20, test_second_half2 %>% filter(location == 'schu_chester'), type = "prob")
threshold <- 0.4
chester_var_20  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(chester_var_20)









