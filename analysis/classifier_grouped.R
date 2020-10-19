library(data.table)
library(dplyr)
library(caret)
library(ggplot2)

df = fread('../data/processed/training_data.csv')
df[, inflow := inflow/0.0283168]
df = df[!is.na(df$mean_sc),]

chester = df[location != 'reedy']
chester$date = as.Date(chester$date)


AVG_YRLY_RISE = 23.5/51

chester[, above_1000 := ifelse(mean_sc > 1120, 1, 0)]
chester[, year := year(date)]
chester[, month := month(date)]
chester[, half := ifelse(month <= 6, 1, 2)]
chester = chester[!is.na(chester$reedy_sl),]
chester[, adjusted_sl := (reedy_sl- 49.229 + 2.79 + 23.5)]
chester[, reedy_sl := (reedy_sl- 49.229)]
chester[, years_to_2060 := (2015-year)]
chester[, assumed_sl_rise := (51-years_to_2060)*AVG_YRLY_RISE]
chester[, bad_sc := ifelse(above_1000 == 1, 0, -1000)]
chester[, ratio := 10000*reedy_sl / inflow]
chester[, ratio2 := 10000*adjusted_sl / inflow]
#chester[, inflow_type := ifelse(inflow < 760, 'very_low_flow', 
#                               ifelse( (inflow < 3100 & inflow >= 760), 'low_flow', 'high_flow'))]
chester[, inflow_type := ifelse(inflow < 3810, 'low_flow', 
                                ifelse( (inflow < 8305 & inflow >= 3810), 'med_flow', 'high_flow'))]
chester[, very_low_flow := ifelse(inflow_type == 'low_flow', 1, 0)]
chester[, low_flow := ifelse(inflow_type == 'med_flow', 1, 0)]
chester[, high_flow := ifelse(inflow_type == 'high_flow', 1, 0)]
chester = chester[inflow > 1 & ratio > -500]

ggplot(chester[inflow < 4000], aes(x = date, y = ratio))+
  geom_point(size = .1)+
  geom_smooth(method = 'loess')+
  geom_point(aes(y = bad_sc, color = 'high sc day'), size = .2)

ggplot(chester, aes(x = date, y = ratio))+
  geom_point(size = .1)+
  geom_smooth(method = 'loess', colour = 'black')+
  geom_point(aes(y = ratio2, color = 'ratios adjusted for sl rise'), size = .2)


drill_down <- chester[, c('inflow', 'mean_sc', 'reedy_sl', 'above_1000', 'location',
                          'year', 'half', 'ratio', 'very_low_flow', 'low_flow', 'high_flow')] %>%
  group_by(year, half, location) %>%
  summarise(bad_sc_days = sum(above_1000),
            high_flow = sum(high_flow),
            low_flow = sum(low_flow),
            very_low_flow = sum(very_low_flow),
            avg_ratio = mean(ratio),
            max_ratio = max(ratio),
            min_ratio = min(ratio))
drill_down = as.data.table(drill_down)
drill_down[, problem_year := ifelse(bad_sc_days > 10, 1, 0)]

ggplot(chester, aes(x = date, y = reedy_sl))+
  geom_point(size = .1)+
  geom_point(aes(y = adjusted_sl, color = 'adjusted_sl'), size = .1)


# my tuneGrid object:
tgrid <- expand.grid(
  mtry = 2,
  splitrule = c("gini"),
  min.node.size = c(1, 3)
)

drill_down[, problem_year := ifelse(problem_year == 0, 'fresh', 'salty')]
drill_down$problem_year = as.factor(drill_down$problem_year)

indices = sort(sample(nrow(drill_down), nrow(drill_down)*.75))
train<-drill_down[indices,]
test<-drill_down[-indices,]


model_caret <- train(problem_year ~ high_flow + low_flow  + very_low_flow+ avg_ratio+
                       max_ratio + min_ratio,
                     data = train,
                     method = "ranger",
                     trControl = trainControl(method="cv", number = 5, verboseIter = T, classProbs = T),
                     tuneGrid = tgrid,
                     importance = 'impurity' #permutation
)

plot(varImp(model_caret))

probsTest <- predict(model_caret, test, type = "prob")
threshold <- 0.5
pred_baseline      <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
confusionMatrix(pred_baseline, test$problem_year)

table(pred_baseline)





# ADJUST DATA TO INCLUDE SL RISE


drill_down2 <- chester[, c('inflow', 'mean_sc', 'adjusted_sl', 'above_1000', 'location',
                           'year', 'half', 'ratio2', 'very_low_flow', 'low_flow', 'high_flow')] %>%
  group_by(year, half, location) %>%
  summarise(bad_sc_days = sum(above_1000),
            high_flow = sum(high_flow),
            low_flow = sum(low_flow),
            very_low_flow = sum(very_low_flow),
            avg_ratio = mean(ratio2),
            max_ratio = max(ratio2),
            min_ratio = min(ratio2))
drill_down2 = as.data.table(drill_down2)
drill_down2[, problem_half := ifelse(bad_sc_days > 10, 1, 0)]
drill_down2[, problem_year := ifelse(problem_year == 0, 'fresh', 'salty')]
drill_down2$problem_year = as.factor(drill_down2$problem_year)

test2 <- drill_down2[-indices,]


#bfb preds
probsTest <- predict(model_caret, test2[location == 'del_bfb'], type = "prob")
threshold <- 0.4
pred_sl_adjusted  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(pred_sl_adjusted)

#chester preds
probsTest <- predict(model_caret, drill_down2[location == 'schu_chester'], type = "prob")
threshold <- 0.4
pred_sl_adjusted  <- factor( ifelse(probsTest[, "salty"] > threshold, "salty", "fresh") )
table(pred_sl_adjusted)

ggplot(chester[location == 'del_bfb'], aes(x = date, y = reedy_sl))+
  geom_point(size = .1)+
  geom_point(aes(y = adjusted_sl, color = 'adjusted_sl'), size = .1)


