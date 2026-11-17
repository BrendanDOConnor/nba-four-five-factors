library(dplyr)
NBA_Regular_Season <- NBA_Regular_Season %>%
  group_by(Season) %>%
  mutate(
    z_TS
  )) %>%
  ungroup()
colnames(NBA_Regular_Season)
Season24_25 <- subset(NBA_Regular_Season, NBA_Regular_Season$Season == "2024-25")
Season23_24 <- subset(NBA_Regular_Season, NBA_Regular_Season$Season == "2023-24")
Season22_23 <- subset(NBA_Regular_Season, NBA_Regular_Season$Season == "2022-23")
Season21_22 <- subset(NBA_Regular_Season, NBA_Regular_Season$Season == "2021-22")
TrueShooting <- aggregate(`TS%` ~ Season, data = NBA_Regular_Season, FUN = mean)
TurnoverRate <- aggregate(`TOV%` ~ Season, data = NBA_Regular_Season, FUN = mean)
print(TrueShooting)
print(TurnoverRate)
EffectiveFG <- aggregate(`EFG%` ~ Season, data = NBA_Regular_Season, FUN = mean)
FreeThrowRate <- aggregate(`FTR` ~ Season, data = NBA_Regular_Season, FUN = mean)
print(EffectiveFG)
print(FreeThrowRate)
TSbyTeam <- aggregate(`z_TS%` ~ Team, data = NBA_Regular_Season, FUN = mean)
print(TSbyTeam)
Rebound <- aggregate(`REB%` ~ Season, data = NBA_Regular_Season, FUN = mean)
print(Rebound)
O_Rebound <- aggregate(`OREB%` ~ Season, data = NBA_Regular_Season, FUN = mean)
D_Rebound <- aggregate(`DREB%` ~ Season, data = NBA_Regular_Season, FUN = mean)
print(O_Rebound)
print(D_Rebound)
TSbyTeam <- aggregate(`z_TS%` ~ Team, data = NBA_Regular_Season, FUN = mean)
REBbyTeam <- aggregate(`z_REB%` ~ Team, data = NBA_Regular_Season, FUN = mean)
TOVbyTeam <- aggregate(`z_TOV%` ~ Team, data = NBA_Regular_Season, FUN = mean)
print(REBbyTeam)
print(TOVbyTeam)
print(TSbyTeam)
library(DescTools)
Desc(NBA_Regular_Season$W)
Desc(Season24_25$W)
Desc(Season23_24$W)
Desc(Season21_22$W)
Desc(Season22_23$W)
NBA_Regular_Season$W[91:120]
New22Wins <- c(43, 51, 44, 43, 46, 44, 52, 48, 23, 53, 20, 25, 42, 33, 56, 53, 51,
               46, 36, 37, 24, 22, 51, 64, 27, 30, 34, 48, 49, 35)
length(New22Wins)
NBA_Regular_Season$W[91:120] <- New22Wins
Desc(NBA_Regular_Season$`z_TS%`)
Desc(NBA_Regular_Season$`z_EFG%`)
Desc(NBA_Regular_Season$`z_FTR`)
Desc(NBA_Regular_Season$`z_TOV%`)
Desc(NBA_Regular_Season$`z_REB%`)
Desc(NBA_Regular_Season$`z_OREB%`)
Desc(NBA_Regular_Season$`z_DREB%`)
plot(NBA_Regular_Season$`z_TS%`, NBA_Regular_Season$`z_Opp TS%`)
plot(NBA_Regular_Season$`z_TS%`, NBA_Regular_Season$`z_TOV%`)
plot(NBA_Regular_Season$`z_TS%`, NBA_Regular_Season$`z_REB%`)
plot(NBA_Regular_Season$`z_TS%`, NBA_Regular_Season$`z_Opp TOV%`)
plot(NBA_Regular_Season$`z_TOV%`, NBA_Regular_Season$`z_Opp TS%`)
plot(NBA_Regular_Season$`z_Opp TOV%`, NBA_Regular_Season$`z_Opp TS%`)
plot(NBA_Regular_Season$`z_REB%`, NBA_Regular_Season$`z_Opp TS%`)
plot(NBA_Regular_Season$`z_REB%`, NBA_Regular_Season$`z_Opp TOV%`)
plot(NBA_Regular_Season$`z_REB%`, NBA_Regular_Season$`z_TOV%`)
plot(NBA_Regular_Season$`z_Opp TOV%`, NBA_Regular_Season$`z_TOV%`)
plot(NBA_Regular_Season$`z_EFG%`, NBA_Regular_Season$`z_OPP EFG%`)
plot(NBA_Regular_Season$`z_EFG%`, NBA_Regular_Season$`z_OPP FTR`)
plot(NBA_Regular_Season$`z_EFG%`, NBA_Regular_Season$`z_FTR`)
plot(NBA_Regular_Season$`z_EFG%`, NBA_Regular_Season$`z_TOV%`)
plot(NBA_Regular_Season$`z_EFG%`, NBA_Regular_Season$`z_Opp TOV%`)
plot(NBA_Regular_Season$`z_EFG%`, NBA_Regular_Season$`z_OREB%`)
plot(NBA_Regular_Season$`z_EFG%`, NBA_Regular_Season$`z_DREB%`)
plot(NBA_Regular_Season$`z_FTR`, NBA_Regular_Season$`z_OPP EFG%`)
plot(NBA_Regular_Season$`z_FTR`, NBA_Regular_Season$`z_OPP FTR`)
plot(NBA_Regular_Season$`z_FTR`, NBA_Regular_Season$`z_TOV%`)
plot(NBA_Regular_Season$`z_FTR`, NBA_Regular_Season$`z_Opp TOV%`)
plot(NBA_Regular_Season$`z_FTR`, NBA_Regular_Season$`z_OREB%`)
plot(NBA_Regular_Season$`z_FTR`, NBA_Regular_Season$`z_DREB%`)
plot(NBA_Regular_Season$`z_OPP FTR`, NBA_Regular_Season$`z_DREB%`)
plot(NBA_Regular_Season$`z_OPP FTR`, NBA_Regular_Season$`z_OREB%`)
plot(NBA_Regular_Season$`z_OPP FTR`, NBA_Regular_Season$`z_TOV%`)
plot(NBA_Regular_Season$`z_OPP FTR`, NBA_Regular_Season$`z_Opp TOV%`)
plot(NBA_Regular_Season$`z_Opp TOV%`, NBA_Regular_Season$`z_TOV%`)
plot(NBA_Regular_Season$`z_OREB%`, NBA_Regular_Season$`z_TOV%`)
plot(NBA_Regular_Season$`z_DREB%`, NBA_Regular_Season$`z_TOV%`)
plot(NBA_Regular_Season$`z_DREB%`, NBA_Regular_Season$`z_Opp TOV%`)
plot(NBA_Regular_Season$`z_OREB%`, NBA_Regular_Season$`z_Opp TOV%`)
plot(NBA_Regular_Season$`z_OREB%`, NBA_Regular_Season$`z_DREB%`)
cor_matrix_five_factors <- cor(NBA_Regular_Season[, c('z_TS%', 'z_Opp TS%', 'z_TOV%',
                                                      'z_REB%', 'z_Opp TOV%')])
print(cor_matrix_five_factors)
cor_matrix_four_factors <- cor(NBA_Regular_Season[, c('TOV%', 'z_Opp TOV%', 'z_EFG%',
                              'z_OPP EFG%', 'z_FTR', 'z_OPP FTR', 'z_OREB%', 'z_DREB%')])
print(cor_matrix_four_factors)
TS_EFG <- cor(NBA_Regular_Season$`z_EFG%`, NBA_Regular_Season$`z_TS%`)
print(TS_EFG)
#K-Fold Regression begins
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
tov_only_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  tov_only_fit <- lm(W ~ `z_TOV%` + `z_Opp TOV%`, data = train_data)
  tov_only_preds <- predict(tov_only_fit, newdata = test_data)
  tov_only_mse[i] <- mean((test_data$W - tov_only_preds)^2)
}
mean(tov_only_mse)
#TOV_REB
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
tov_reb_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  tov_reb_fit <- lm(W ~ `z_TOV%` + `z_Opp TOV%` + `z_OREB%` + `z_DREB%`, data = train_data)
  tov_reb_preds <- predict(tov_reb_fit, newdata = test_data)
  tov_reb_mse[i] <- mean((test_data$W - tov_reb_preds)^2)
}
mean(tov_reb_mse)
#TOV_REB_FTR
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
tov_reb_ftr_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  tov_reb_ftr_fit <- lm(W ~ `z_TOV%` + `z_Opp TOV%` + `z_OREB%` + `z_DREB%` + `z_FTR` + `z_OPP FTR`, data = train_data)
  tov_reb_ftr_preds <- predict(tov_reb_ftr_fit, newdata = test_data)
  tov_reb_ftr_mse[i] <- mean((test_data$W - tov_reb_ftr_preds)^2)
}
mean(tov_reb_ftr_mse)
#TOV_FTR
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
tov_ftr_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  tov_ftr_fit <- lm(W ~ `z_TOV%` + `z_Opp TOV%` + `z_FTR` + `z_OPP FTR`, data = train_data)
  tov_ftr_preds <- predict(tov_ftr_fit, newdata = test_data)
  tov_ftr_mse[i] <- mean((test_data$W - tov_ftr_preds)^2)
}
mean(tov_ftr_mse)
#TOV_EFG
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
tov_efg_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  tov_efg_fit <- lm(W ~ `z_TOV%` + `z_Opp TOV%` + `z_EFG%` + `z_OPP EFG%`, data = train_data)
  tov_efg_preds <- predict(tov_efg_fit, newdata = test_data)
  tov_efg_mse[i] <- mean((test_data$W - tov_efg_preds)^2)
}
mean(tov_efg_mse)
#TOV_EFG_FTR
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
tov_efg_ftr_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  tov_efg_ftr_fit <- lm(W ~ `z_TOV%` + `z_Opp TOV%` + `z_EFG%` + `z_OPP EFG%` + `z_FTR` + `z_OPP FTR`, data = train_data)
  tov_efg_ftr_preds <- predict(tov_efg_ftr_fit, newdata = test_data)
  tov_efg_ftr_mse[i] <- mean((test_data$W - tov_efg_ftr_preds)^2)
}
mean(tov_efg_ftr_mse)
#TOV_EFG_REB
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
tov_efg_reb_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  tov_efg_reb_fit <- lm(W ~ `z_TOV%` + `z_Opp TOV%` + `z_EFG%` + `z_OPP EFG%` + `z_OREB%` + `z_DREB%`, data = train_data)
  tov_efg_reb_preds <- predict(tov_efg_reb_fit, newdata = test_data)
  tov_efg_reb_mse[i] <- mean((test_data$W - tov_efg_reb_preds)^2)
}
mean(tov_efg_reb_mse)
#TOV_EFG_REB_FTR
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
tov_efg_reb_ftr_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  tov_efg_reb_ftr_fit <- lm(W ~ `z_TOV%` + `z_Opp TOV%` + `z_EFG%` + `z_OPP EFG%` + `z_OREB%` + `z_DREB%` + `z_FTR` + `z_OPP FTR`, data = train_data)
  tov_efg_reb_ftr_preds <- predict(tov_efg_reb_ftr_fit, newdata = test_data)
  tov_efg_reb_ftr_mse[i] <- mean((test_data$W - tov_efg_reb_ftr_preds)^2)
}
mean(tov_efg_reb_ftr_mse)
#TOV_ONE_REB
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
tov_efg_reb_ftr_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  tov_efg_reb_ftr_fit <- lm(W ~ `z_TOV%` + `z_Opp TOV%` + `z_EFG%` + `z_OPP EFG%` + `z_OREB%` + `z_DREB%` + `z_FTR` + `z_OPP FTR`, data = train_data)
  tov_efg_reb_ftr_preds <- predict(tov_efg_reb_ftr_fit, newdata = test_data)
  tov_efg_reb_ftr_mse[i] <- mean((test_data$W - tov_efg_reb_ftr_preds)^2)
}
mean(tov_efg_reb_ftr_mse)
#TOV_TS
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
tov_ts_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  tov_ts_fit <- lm(W ~ `z_TOV%` + `z_Opp TOV%` + `z_TS%` + `z_Opp TS%`, data = train_data)
  tov_ts_preds <- predict(tov_ts_fit, newdata = test_data)
  tov_ts_mse[i] <- mean((test_data$W - tov_ts_preds)^2)
}
mean(tov_ts_mse)
#TOV_ONE_REB
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
tov_reb1_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  tov_reb1_fit <- lm(W ~ `z_TOV%` + `z_Opp TOV%` + `z_REB%`, data = train_data)
  tov_reb1_preds <- predict(tov_reb1_fit, newdata = test_data)
  tov_reb1_mse[i] <- mean((test_data$W - tov_reb1_preds)^2)
}
mean(tov_reb1_mse)
#TOV_ONE_REB_TS
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
tov_reb1_ts_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  tov_reb1_ts_fit <- lm(W ~ `z_TOV%` + `z_Opp TOV%` + `z_REB%` + `z_TS%` + `z_Opp TS%`, data = train_data)
  tov_reb1_ts_preds <- predict(tov_reb1_ts_fit, newdata = test_data)
  tov_reb1_ts_mse[i] <- mean((test_data$W - tov_reb1_ts_preds)^2)
}
mean(tov_reb1_ts_mse)
#TS
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
ts_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  ts_fit <- lm(W ~ `z_TS%` + `z_Opp TS%`, data = train_data)
  ts_preds <- predict(ts_fit, newdata = test_data)
  ts_mse[i] <- mean((test_data$W - ts_preds)^2)
}
mean(ts_mse)
#EFG_FTR
set.seed(123)
k <- 8
n <- nrow(NBA_Regular_Season)
folds <- sample(rep(1:k, length.out = n))
efg_ftr_mse <- numeric(k)
for (i in 1:k) {
  train_data <- NBA_Regular_Season[folds != i, ]
  test_data <- NBA_Regular_Season[folds == i, ]
  efg_ftr_fit <- lm(W ~ `z_EFG%` + `z_OPP EFG%` + `z_FTR` + `z_OPP FTR`, data = train_data)
  efg_ftr_preds <- predict(efg_ftr_fit, newdata = test_data)
  efg_ftr_mse[i] <- mean((test_data$W - efg_ftr_preds)^2)
}
mean(efg_ftr_mse)
FiveStat <- lm(NBA_Regular_Season$W ~ NBA_Regular_Season$`z_TS%` + NBA_Regular_Season$`z_Opp TS%`
               + NBA_Regular_Season$`z_Opp TOV%` + NBA_Regular_Season$`z_TOV%` + NBA_Regular_Season$`z_REB%`)
summary(FiveStat)
FourFactors <- lm(NBA_Regular_Season$W ~ NBA_Regular_Season$`z_EFG%` + NBA_Regular_Season$`z_OPP EFG%`
               + NBA_Regular_Season$`z_Opp TOV%` + NBA_Regular_Season$`z_TOV%` + NBA_Regular_Season$`z_OREB%`
               + NBA_Regular_Season$`z_DREB%` + NBA_Regular_Season$z_FTR + NBA_Regular_Season$`z_OPP FTR`)
summary(FourFactors)
Desc(NBA_Regular_Season$`z_Opp TS%`)
Desc(NBA_Regular_Season$`z_TS%`)
Desc(NBA_Regular_Season$`z_OPP EFG%`)
