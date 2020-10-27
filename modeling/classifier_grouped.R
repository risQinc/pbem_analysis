library(data.table)
library(dplyr)
library(caret)
library(ggplot2)


################################################################################################################################################
################################################################################################################################################
### DATA PREPARATION
################################################################################################################################################
################################################################################################################################################

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
df[, inflow_type := ifelse(inflow < 3800, 'low_flow', 
                                ifelse( (inflow >= 3800 & inflow < 8300), 'med_flow', 'high_flow'))]
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
            problem_year_10 = ifelse(bad_sc_days >= 10, 'salty', 'fresh'))

####
library(corrplot)
corr_mat = cor(agged_df[, c('bad_sc_days', 'high_flow', 'low_flow', 'very_low_flow', 'avg_ratio', 'min_ratio', 'max_ratio')])
corrplot(corr_mat)
# as high flow goes up, bad sc_days go down (makes sense)
# as low flow goes up, bad sc_days go down (does not make sense)
# as very low flow goes up, bad sc_days go up (makes sense)
# as very avg_ratio goes up, bad sc_days go up (makes sense)
# as very min_ratio goes up, bad sc_days go up (makes sense)
# as very max_ratio goes up, bad sc_days go up (makes sense)


# NORMALIZE THE RAW DATA # normalize data (TODO FIXME, leaving data unscaled for now)
library(scales)
selected_vars = agged_df[, c('problem_year_10', 'problem_year_20', 'high_flow', 'very_low_flow', 'avg_ratio', 'max_ratio', 'min_ratio', 'half', 'year', 'location')]


#selected_vars$high_flow = as.numeric(scale(selected_vars$high_flow))
##selected_vars$very_low_flow = as.numeric(scale(selected_vars$very_low_flow))
#selected_vars$avg_ratio = as.numeric(scale(selected_vars$avg_ratio))
#selected_vars$max_ratio = as.numeric(scale(selected_vars$max_ratio))
#selected_vars$min_ratio = as.numeric(scale(selected_vars$min_ratio))

selected_vars$problem_year_10 = as.factor(selected_vars$problem_year_10)


# my tuneGrid object:
tgrid <- expand.grid(
  mtry = c(1,2,3,4,5),
  splitrule = c("gini"),
  min.node.size = c(1, 2,3,4)
)


years = unique(selected_vars$year)
train_years = sample(years, size = .75*length(years))
test_years = setdiff(years, train_years)


train <- selected_vars[selected_vars$year %in% train_years,]
test  <- selected_vars[selected_vars$year %in% test_years,]



model_caret_10 <- train(problem_year_10 ~ high_flow  + very_low_flow + avg_ratio+ max_ratio + min_ratio,
                     data = train,
                     method = "ranger",
                     trControl = trainControl(method="cv", number = 5, verboseIter = T, classProbs = T),
                     tuneGrid = tgrid,
                     importance = 'impurity' #permutation
)




plot(varImp(model_caret_10))


# TEST MODELS ON HOLDOUT DATA -- Good results indicate the model can be "trusted" when predicting on new data



#BFB CTRL 10: 23 fresh predictions, 100% accuracy
probsTest <- predict(model_caret_10, test %>% filter(location == 'del_bfb'), type = "prob")
threshold <- 0.4
bfb_ctrl_10  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(bfb_ctrl_10)
bfb = test %>% filter(location == 'del_bfb')
confusionMatrix(bfb_ctrl_10, bfb$problem_year_10)

#CHESTER CTRL 10: 24 fresh, 7 salty, 94% overall accuracy, 86% positive accuracy (weak trust factor)
probsTest <- predict(model_caret_10, test %>% filter(location == 'schu_chester'), type = "prob")
threshold <- 0.4
chester_ctrl_10  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(chester_ctrl_10)
che = test %>% filter(location == 'schu_chester')

table(che$problem_year_10)
confusionMatrix(chester_ctrl_10, che$problem_year_10)



## RETRAIN ON FULL HISTORICAL DATA 

tgrid <- expand.grid(
  mtry = 3,
  splitrule = c("gini"),
  min.node.size = c(2)
)

final_mod <- train(problem_year_10 ~ high_flow  + very_low_flow+ avg_ratio+ max_ratio + min_ratio,
                        data = selected_vars,
                        method = "ranger",
                        trControl = trainControl(method="cv", number = 5, verboseIter = T, classProbs = T),
                        tuneGrid = tgrid,
                        importance = 'impurity' #permutation
)


plot(varImp(final_mod))


# ADJUST DATA TO INCLUDE SL RISE (MAKING SYNTHETIC "FUTURE" DATA)

# when sea levels rise the ratio of sl to inflow will INCREASE
# average_ratio will increase, max ratio will increase and min ratio will increase


agged_df2 <- as.data.table(df[, c('inflow', 'mean_sc', 'adjusted_sl', 'above_1000', 'location',
                           'year', 'half', 'rise_ratio', 'very_low_flow', 'low_flow', 'high_flow')] %>%
                             group_by(year, half, location) %>%
                             summarise(bad_sc_days = sum(above_1000),
                                       high_flow = sum(high_flow),
                                       low_flow = sum(low_flow),
                                       very_low_flow = sum(very_low_flow),
                                       avg_ratio = mean(rise_ratio),
                                       max_ratio = max(rise_ratio),
                                       min_ratio = min(rise_ratio),
                                       problem_year_10 = ifelse(bad_sc_days >= 10, 'salty', 'fresh'))
  )
agged_df2$problem_year_10 = as.factor(agged_df2$problem_year_10)


# normalize data (TODO FIXME, leaving data unscaled for now)

selected_vars2 = agged_df2[, c('problem_year_10', 'high_flow', 'very_low_flow', 'avg_ratio', 'max_ratio', 'min_ratio', 'half', 'year', 'location')]



# historically, chester had 21 years with predicted 10 or more days above threshold
# historically, bfb had 0 years with predicted 10 or more days above threshold
agged_df %>% group_by(location) %>% summarise(salty_days = sum(ifelse(problem_year_10 == 'salty', 1, 0)))

# use the trained model to predict on all of the synthetic future data


#BFB VAR 10: 98 fresh predictions -> no change due to SL rise
probsTest <- predict(final_mod, selected_vars2 %>% filter(location == 'del_bfb'), type = "prob")
threshold <- 0.4
bfb_var_10  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(bfb_var_10)


#CHESTER VAR 10: 77 fresh, 21 salty BECOMES  53 fresh, 42 salty -> 100% increase in number of years with 10+ salty days
probsTest <- predict(final_mod, selected_vars2 %>% filter(location == 'schu_chester'), type = "prob")
threshold <- 0.3
chester_var_10  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(chester_var_10)

chester_df = as.data.table(selected_vars2 %>% filter(location == 'schu_chester'))
chester_df$predicted_salty = probsTest$salty
chester_df[, prediction := ifelse(predicted_salty > .3, 1, 0)]

over = chester_df[prediction == 1]

length(unique(over$year))












