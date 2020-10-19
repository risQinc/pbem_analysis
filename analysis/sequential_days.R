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
train.dat <- chester %>% mutate(above_1120 = ifelse(mean_sc > 1120, 1, 0))
train.dat <- as.data.table(train.dat %>% arrange(location,date))

# cumsum with reset
cumsum_with_reset <- function(above_1120) {
  sequence <- c(1)
  run = 1
  result <- numeric()
  
  for (i in 2:length(above_1120)) {
    
    if (above_1120[i] == 1) {
      run = run + 1
      sequence[i] <- run 
    }
    else {
      run = 0
      sequence[i] <- run
      
    }
  }
  
  return (sequence)
}

sequence = cumsum_with_reset(train.dat$above_1120)
train.dat$sequential_days = sequence
run = train.dat[sequential_days != 0]

ggplot(run, aes(date, sequential_days))+
  geom_point(size = .1)+
  ggtitle('Sequential Days Above Threshold, Chester')


train.dat[, streak_7 := ifelse(sequential_days == 3, 1 , 0)]
train.dat[, year := year(date)]
train.dat[, month := month(date)]
train.dat[, half := ifelse(month <= 6, 1, 2)]
train.dat = train.dat[!is.na(train.dat$reedy_sl),]
train.dat[, adjusted_sl := (reedy_sl- 49.229 + 2.79 + 23.5)]
train.dat[, reedy_sl := (reedy_sl- 49.229)]
train.dat[, years_to_2060 := (2015-year)]
train.dat[, assumed_sl_rise := (51-years_to_2060)*AVG_YRLY_RISE]
train.dat[, ratio := 10000*reedy_sl / inflow]
train.dat[, ratio2 := 10000*adjusted_sl / inflow]
#chester[, inflow_type := ifelse(inflow < 760, 'very_low_flow', 
#                               ifelse( (inflow < 3100 & inflow >= 760), 'low_flow', 'high_flow'))]
train.dat[, inflow_type := ifelse(inflow < 3810, 'low_flow', 
                                ifelse( (inflow < 8305 & inflow >= 3810), 'med_flow', 'high_flow'))]
train.dat[, very_low_flow := ifelse(inflow_type == 'low_flow', 1, 0)]
train.dat[, low_flow := ifelse(inflow_type == 'med_flow', 1, 0)]
train.dat[, high_flow := ifelse(inflow_type == 'high_flow', 1, 0)]




drill_down <- train.dat[, c('inflow', 'streak_7', 'reedy_sl', 'location',
                          'year', 'half', 'ratio', 'very_low_flow', 'low_flow', 'high_flow')] %>%
  group_by(year, half, location) %>%
  summarise(streak_7s = sum(streak_7),
            high_flow = sum(high_flow),
            low_flow = sum(low_flow),
            very_low_flow = sum(very_low_flow),
            avg_ratio = mean(ratio),
            max_ratio = max(ratio),
            min_ratio = min(ratio))
drill_down = as.data.table(drill_down)
drill_down[, problem_year := ifelse(streak_7s >= 1, 1, 0)]

ggplot(chester, aes(x = date, y = reedy_sl))+
  geom_point(size = .1)+
  geom_point(aes(y = adjusted_sl, color = 'adjusted_sl'), size = .1)


# my tuneGrid object:
tgrid <- expand.grid(
  mtry = 2,
  splitrule = c("gini"),
  min.node.size = c(1, 3)
)

drill_down[, problem_year := ifelse(problem_year == 0, 'fresh', 'streaks')]
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

probsTest <- predict(model_caret, test[location == 'schu_chester'], type = "prob")
threshold <- 0.5
pred_baseline      <- factor( ifelse(probsTest[, "streaks"] > threshold, "streaks", "fresh") )
confusionMatrix(pred_baseline, test[location == 'schu_chester']$problem_year)

table(pred_baseline)



# ADJUST DATA TO INCLUDE SL RISE


drill_down2 <- train.dat[, c('inflow', 'streak_7', 'reedy_sl', 'location',
                             'year', 'half', 'ratio2', 'very_low_flow', 'low_flow', 'high_flow')] %>%
  group_by(year, half, location) %>%
  summarise(streak_7s = sum(streak_7),
            high_flow = sum(high_flow),
            low_flow = sum(low_flow),
            very_low_flow = sum(very_low_flow),
            avg_ratio = mean(ratio2),
            max_ratio = max(ratio2),
            min_ratio = min(ratio2))
drill_down2 = as.data.table(drill_down2)
drill_down2[, problem_year := ifelse(streak_7s >= 1, 1, 0)]
drill_down2[, problem_year := ifelse(problem_year == 0, 'fresh', 'streaks')]
drill_down2$problem_year = as.factor(drill_down2$problem_year)

test2 <- drill_down2[-indices,]


#chester preds
probsTest2 <- predict(model_caret, test2[location == 'schu_chester'], type = "prob")
probsTest2 <- predict(model_caret, test2[location == 'del_bfb'], type = "prob")
threshold <- 0.38
pred_sl_adjusted  <- factor( ifelse(probsTest2[, "streaks"] > threshold, "streaks", "fresh") )

table(pred_sl_adjusted)


