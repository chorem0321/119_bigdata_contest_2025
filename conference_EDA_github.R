install.packages("tidyr")
install.packages("dplyr")
install.packages("FactoMineR")
library(ggplot2)
library(tidyr)
library(dplyr)
library(FactoMineR)
options(max.print = 10000)

##########
#변수셀
season2020 <- c(20200204,20200505,20200807,20201107)
season2021 <- c(20210203,20210505,20210807,20211107)
season2022 <- c(20220204,20220505,20220807,20221107)
season2023 <- c(20230204,20230506,20230808,20231108)
season2024 <- c(20240204,20240505,20240807,20241107)
##########

call_data <- read.csv("UR_FILE_HERE")
cat_data <- read.csv("UR_FILE_HERE")

#call_data 먼저 EDA 하겠습니다
colnames(call_data) <- c("x","tm","시","구","상세","stn","ta_max","ta_min","ta_max_min","hm_min","hm_max","ws_max","ws_ins_max","rn_day","call_count") #열 이름들 너무 길어서 바꿔줄게요
str(call_data) #데이터 구조 확인
summary(call_data) #각 변수 기본 통계 요약 (평균, 중앙값, 최솟값, 최댓값 등)
#boxplot으로 outlier 탐지 할게요
call_long <- call_data %>%
  gather(key = "variable", value = "value", ta_max, ta_min, ta_max_min, hm_min, hm_max, ws_max, rn_day, call_count)

ggplot(call_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  theme_minimal()

#각 열마다 outlier 몇개 있는지 확인할게요
sapply(call_data[, c("ta_max", "ta_min", "ta_max_min", "hm_min", "hm_max", "ws_max", "rn_day", "call_count")], function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  sum(x < (Q1 - 1.5 * IQR_value) | x > (Q3 + 1.5 * IQR_value), na.rm = TRUE)
})

#call_data의 총 행 갯수
nrow(call_data)
#call_data의 각 열마다 몇개의 결측치가 있는지 확인
colSums(call_data == -99.0)
#이건 혹시나 싶어서 넣어봤는데 하루에 call_count가 0인 데이터가 단 하나도 없음..! 신기하네..
sum(call_data$call_count == 0)


#cat_data 구조 살펴보기
#일단 이것도 보기 쉽게 열 이름부터 바꿀게요
colnames(cat_data) <- c("x","tm","시","구","상세","cat","sub_cat","stn","call_count")
summary(cat_data)
str(cat_data)
table(cat_data$cat)
table(cat_data$sub_cat)



######################
#자 여기부터 데이터 전처리 시작합니다
######################

#call_data 결측치 날립니다
call_clean <- call_data[!apply(call_data == -99.0, 1, any), ]
call_clean

#call_data tm 기준으로 그룹화합니다
call_processed <- call_clean %>%
  select(-x, -시, -구, -상세,-stn) %>%  #필요 없는 열들 날립니다
  group_by(tm) %>%  #기준 열(tm) 기준으로 그룹화
  summarise(across(everything(), list(mean = mean))) #나머지 열들 mean시켜버립니다
#열이름 재설정
colnames(call_processed) <- c("tm","ta_max","ta_min","ta_max_min","hm_min","hm_max","ws_max","ws_ins_max","rn_day","call_count") #열 이름들 너무 길어서 바꿔줄게요

#비가 많이 왔는지 아닌지 나누기 (1,0)
call_processed <- call_processed %>%
  mutate(rain = ifelse(rn_day >= 15.0, 1, 0))

####################################################

#rn_day와 call_count의 상관관계 분석

#그룹화된 데이터(call_processed)에서의 상관관계
cat("=== 그룹화된 데이터 상관관계 분석 ===\n")

#상관계수 및 p-value
call_corr2 <- cor(call_processed$rn_day, call_processed$call_count)
call_test2 <- cor.test(call_processed$rn_day, call_processed$call_count)

cat("상관계수:", round(call_corr2, 4), "\n")
cat("p-value:", call_test2$p.value, "\n")

#결과 요약
cat("=== 결과 요약 ===\n")
cat("그룹화 데이터 - correlation:", round(call_corr2, 4), "\n")
cat("그룹화 데이터 - p-value:", call_test2$p.value, "\n")

#########################################
#최적의 임계값을 찾는 함수 (rn_day를 임계값으로 변환)
find_optimal_threshold_simple <- function(data) {
  #임계값 범위 설정 (0부터 100까지 0.1씩)
  thresholds <- seq(0, 100, by = 0.1)
  
  #결과를 저장할 벡터
  correlations <- numeric(length(thresholds))
  p_values <- numeric(length(thresholds))
  
  cat("임계값 최적화 진행 중...\n")
  
  #각 임계값에 대해 상관계수 계산
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    
    #rn_day를 임계값으로 변환 (임계값 이상이면 그 값, 미만이면 0)
    rn_day_threshold <- ifelse(data$rn_day >= threshold, data$rn_day, 0)
    
    #상관계수 계산
    corr_result <- cor.test(rn_day_threshold, data$call_count)
    correlations[i] <- abs(corr_result$estimate)  #절댓값으로 저장
    p_values[i] <- corr_result$p.value
    
    #진행상황 출력 (100개마다)
    if (i %% 100 == 0) {
      cat("진행률:", round(i/length(thresholds)*100, 1), "%\n")
    }
  }
  
  #결과 데이터프레임 생성
  results <- data.frame(
    threshold = thresholds,
    correlation = correlations,
    p_value = p_values
  )
  
  #최고 상관계수와 해당 임계값 찾기
  max_idx <- which.max(correlations)
  optimal_threshold <- thresholds[max_idx]
  max_correlation <- correlations[max_idx]
  optimal_p_value <- p_values[max_idx]
  
  cat("\n=== 최적화 결과 ===\n")
  cat("최적 임계값:", optimal_threshold, "\n")
  cat("최고 상관계수:", round(max_correlation, 6), "\n")
  cat("p-value:", optimal_p_value, "\n")
  
  #상위 10개 결과 출력
  cat("\n=== 상위 10개 결과 ===\n")
  top_results <- results[order(results$correlation, decreasing = TRUE)[1:10], ]
  print(top_results)
  
  return(list(
    optimal_threshold = optimal_threshold,
    max_correlation = max_correlation,
    optimal_p_value = optimal_p_value,
    all_results = results
  ))
}

#최적화 실행
optimization_result <- find_optimal_threshold_simple(call_processed)

#최적 임계값으로 상관관계 분석
optimal_threshold <- optimization_result$optimal_threshold
rn_day_optimal <- ifelse(call_processed$rn_day >= optimal_threshold, call_processed$rn_day, 0)

cat("\n=== 최적 임계값으로 변환된 데이터 상관관계 분석 ===\n")

#상관계수 및 p-value
optimal_corr <- cor(rn_day_optimal, call_processed$call_count)
optimal_test <- cor.test(rn_day_optimal, call_processed$call_count)

cat("최적 임계값:", optimal_threshold, "\n")
cat("상관계수:", round(optimal_corr, 4), "\n")
cat("p-value:", optimal_test$p.value, "\n")

#기존 방법과 비교
cat("\n=== 기존 방법 vs 최적화 방법 비교 ===\n")
original_corr <- cor(call_processed$rn_day, call_processed$call_count)
original_test <- cor.test(call_processed$rn_day, call_processed$call_count)

cat("기존 방법 - 상관계수:", round(original_corr, 4), ", p-value:", original_test$p.value, "\n")
cat("최적화 방법 - 상관계수:", round(optimal_corr, 4), ", p-value:", optimal_test$p.value, "\n")
cat("상관계수 개선:", round(abs(optimal_corr) - abs(original_corr), 4), "\n")

#임계값별 상관계수 계산 및 그래프
thresholds <- seq(0, 100, by = 0.1)
cor_vals <- numeric(length(thresholds))

for(i in seq_along(thresholds)) {
  th <- thresholds[i]
  rn_day_th <- ifelse(call_processed$rn_day >= th, call_processed$rn_day, 0)
  cor_vals[i] <- cor(rn_day_th, call_processed$call_count)
}

#그래프 그리기
plot(thresholds, cor_vals, type = "l",
     xlab = "임계값", ylab = "상관계수",
     main = "임계값에 따른 rn_day 변환값과 call_count의 상관계수 변화",
     col = "blue", lwd = 2)
grid()

#최고점 표시
max_idx <- which.max(abs(cor_vals))
points(thresholds[max_idx], cor_vals[max_idx], col = "red", pch = 19, cex = 1.2)
text(thresholds[max_idx], cor_vals[max_idx],
     labels = paste0("최고점: ", round(thresholds[max_idx],2), "\n상관계수: ", round(cor_vals[max_idx],4)),
     pos = 4, col = "red")

#비가 많이 왔는지 아닌지 다시 나누기 (1,0) -> rn_day랑 call_count의 상관관계가 가장 높아지는 rn_day 값 (가설: 비가오면, 사고가 늘어난다)
call_processed <- call_processed %>%
  mutate(rain = ifelse(rn_day >= 35.2, 1, 0))
#####################################
#boxplot으로 outlier 탐지 할게요
call_processed_long <- call_processed %>%
  gather(key = "variable", value = "value", ta_max, ta_min, ta_max_min, hm_min, hm_max, ws_max, ws_ins_max, rn_day, call_count)

boxplot(call_processed$rn_day ~ call_processed$rain,
        main = "Rain 그룹별 rn_day 분포",
        xlab = "Rain (0: 적은 강수, 1: 많은 강수)",
        ylab = "rn_day (강수량)",
        col = c("lightblue", "lightcoral"),
        names = c("Rain = 0", "Rain = 1"))

#각 열마다 outlier 몇개 있는지 확인할게요
sapply(call_processed[, c("ta_max", "ta_min", "ta_max_min", "hm_min", "hm_max", "ws_max", "ws_ins_max", "rn_day", "call_count")], function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  sum(x < (Q1 - 1.5 * IQR_value) | x > (Q3 + 1.5 * IQR_value), na.rm = TRUE)
})


####################################
# 자 저희가 원하는 것은 모든 변수들의 correlation입니다, 따라서 모든 변수들과 call_count와의 correlation 분석 해줄게요
# rn_day는 이미 했기 때문에 다른 것들을 해줍시다, 그리고 correlation에 따라서 ai learning 시킬 때 weight 나눠서 넣을게요
ta_max_cor <- cor(call_processed$ta_max, call_processed$call_count)
ta_min_cor <- cor(call_processed$ta_min, call_processed$call_count)
ta_max_min_cor <- cor(call_processed$ta_max_min, call_processed$call_count)
hm_min_cor <- cor(call_processed$hm_min, call_processed$call_count)
hm_max_cor <- cor(call_processed$hm_max, call_processed$call_count)
ws_max_cor <- cor(call_processed$ws_max, call_processed$call_count)
ws_ins_max_cor <- cor(call_processed$ws_ins_max, call_processed$call_count)

ta_max_cor
ta_min_cor
ta_max_min_cor
hm_min_cor
hm_max_cor
ws_max_cor
ws_ins_max_cor

# ta_max와 ta_min처럼 correlation이 분명히 있는 변수들은 학습시킬 때 ta라는 (기온) 공통적인 부분에 과적합할 수 있어서 이것을 처리하기위해 각 변수들의 correlation 계산해볼게요
print(cor(call_processed$ta_max, call_processed$ta_min)) # 0.83
print(cor(call_processed$ta_min, call_processed$ta_max_min)) # -0.55
print(cor(call_processed$ta_max, call_processed$ta_max_min)) # 0.01
print(cor(call_processed$hm_min, call_processed$hm_max)) # 0.70
print(cor(call_processed$ws_ins_max, call_processed$ws_max)) # 0.96

# correlation이 개판인걸 볼 수 있죠? 0.96이 뭐에요... 이걸 어떻게 해결할까 하다가 PCA를 하는 방법, or 변수 선택을 하는 방법이 있다는 것을 떠올렸습니다..
# 아니면 일단 XGBoost 돌려볼게요 귀찮네
######################################################
install.packages("xgboost")
install.packages("caret")
library(xgboost)
library(caret)

# 데이터 준비
set.seed(123)
k <- 5  # K-fold 수 설정

# K-fold 인덱스 생성
call_processed$fold <- createFolds(call_processed$call_count, k = k, list = FALSE)

# 결과 저장을 위한 리스트 초기화
results <- list()

# K-fold 교차 검증
for (i in 1:k) {
  # 훈련 데이터와 검증 데이터 분리
  train_data <- call_processed[call_processed$fold != i, ]
  test_data <- call_processed[call_processed$fold == i, ]
  
  # XGBoost 모델 훈련
  dtrain <- xgb.DMatrix(data = as.matrix(train_data[, -c(1, 10, 12)]), label = train_data$call_count)
  dtest <- xgb.DMatrix(data = as.matrix(test_data[, -c(1, 10, 12)]), label = test_data$call_count)
  
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.11,
    max_depth = 3,
    subsample = 1
  )
  
  # 모델 학습
  model <- xgb.train(params = params, data = dtrain, nrounds = 333) #일단 nrounds 밑에서 333이 최적이라고 나왔습니다.
  
  # 예측
  predictions <- predict(model, dtest)
  
  # RMSE 계산
  rmse <- sqrt(mean((predictions - test_data$call_count)^2))
  results <- rbind(results, data.frame(Fold = i, RMSE = rmse))
  
  # 결과 출력
  cat("Fold:", i, "RMSE:", round(rmse, 4), "\n")
}

# K-fold 평균 RMSE
avg_rmse <- mean(results$RMSE)
cat("평균 RMSE:", round(avg_rmse, 4), "\n")

#######################하이퍼파라미터튜닝########################

# K-fold 설정
set.seed(123)
k <- 5
call_processed$fold <- createFolds(call_processed$call_count, k = k, list = FALSE)

# 하이퍼파라미터 조정 범위 설정
eta_values <- seq(0.01, 0.3, by = 0.01)
max_depth_values <- 3:10
subsample_values <- seq(0.5, 1, by = 0.1)

# 결과 저장을 위한 데이터 프레임
results <- data.frame(eta = numeric(), max_depth = integer(), subsample = numeric(), avg_rmse = numeric())

# 하이퍼파라미터 조정
total_iterations <- length(eta_values) * length(max_depth_values) * length(subsample_values)
current_iteration <- 0  # 진행 상황 카운트

for (eta in eta_values) {
  for (max_depth in max_depth_values) {
    for (subsample in subsample_values) {
      rmse_values <- numeric()
      
      for (i in 1:k) {
        # 훈련 데이터와 검증 데이터 분리
        train_data <- call_processed[call_processed$fold != i, ]
        test_data <- call_processed[call_processed$fold == i, ]
        
        # XGBoost 모델 훈련
        dtrain <- xgb.DMatrix(data = as.matrix(train_data[, -c(1, 10, 12)]), label = train_data$call_count)
        dtest <- xgb.DMatrix(data = as.matrix(test_data[, -c(1, 10, 12)]), label = test_data$call_count)
        
        params <- list(
          objective = "reg:squarederror",
          eval_metric = "rmse",
          eta = eta,
          max_depth = max_depth,
          subsample = subsample
        )
        
        # 모델 학습
        model <- xgb.train(params = params, data = dtrain, nrounds = 100)
        
        # 예측
        predictions <- predict(model, dtest)
        
        # RMSE 계산
        rmse <- sqrt(mean((predictions - test_data$call_count)^2))
        rmse_values <- c(rmse_values, rmse)
      }
      
      # 평균 RMSE 계산
      avg_rmse <- mean(rmse_values)
      
      # 결과 저장
      results <- rbind(results, data.frame(eta = eta, max_depth = max_depth, subsample = subsample, avg_rmse = avg_rmse))
      
      # 진행률 계산 및 출력
      current_iteration <- current_iteration + 1
      progress <- (current_iteration / total_iterations) * 100
      cat("진행률:", round(progress, 1), "% - 현재 설정: eta =", eta, ", max_depth =", max_depth, ", subsample =", subsample, "\n")
    }
  }
}

# 최적의 하이퍼파라미터 조합 찾기
best_params <- results[which.min(results$avg_rmse), ]
cat("\n최적의 파라미터 조합:\n")
cat("eta:", best_params$eta, "\n")
cat("max_depth:", best_params$max_depth, "\n")
cat("subsample:", best_params$subsample, "\n")
cat("평균 RMSE:", best_params$avg_rmse, "\n")

# 최적의 파라미터 조합:
# > cat("eta:", best_params$eta, "\n")
# eta: 0.11 
# > cat("max_depth:", best_params$max_depth, "\n")
# max_depth: 3 
# > cat("subsample:", best_params$subsample, "\n")
# subsample: 1 
# > cat("평균 RMSE:", best_params$avg_rmse, "\n")
# 평균 RMSE: 0.3969853 

# subsample: 1인 것을 확인하고 아, 이거 과적합 상태인가? 의문점 듦, 나중에 확인해보겠음.

################rounds비교#################

# Early Stopping 시각화 함수
plot_early_stopping <- function(data, max_rounds = 1000, early_stopping_rounds = 50) {
  library(xgboost)
  library(ggplot2)
  
  # 데이터 준비
  feature_cols <- c("ta_max", "ta_min", "ta_max_min", "hm_min", "hm_max", 
                    "ws_max", "ws_ins_max", "rn_day")
  
  set.seed(123)
  train_idx <- sample(nrow(data), 0.8 * nrow(data))
  
  dtrain <- xgb.DMatrix(
    data = as.matrix(data[train_idx, feature_cols]),
    label = data$call_count[train_idx]
  )
  
  dval <- xgb.DMatrix(
    data = as.matrix(data[-train_idx, feature_cols]),
    label = data$call_count[-train_idx]
  )
  
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.11,
    max_depth = 3,
    subsample = 1
  )
  
  # Early stopping으로 모델 훈련
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = max_rounds,
    watchlist = list(train = dtrain, val = dval),
    early_stopping_rounds = early_stopping_rounds,
    verbose = FALSE
  )
  
  # 평가 로그 추출
  eval_log <- model$evaluation_log
  best_iteration <- model$best_iteration
  
  # 기본 플롯
  par(mfrow = c(2, 2))
  
  # 1. 전체 학습 곡선
  plot(eval_log$iter, eval_log$train_rmse, 
       type = "l", col = "blue", lwd = 2,
       xlab = "Iterations", ylab = "RMSE",
       main = "Training Curves with Early Stopping")
  lines(eval_log$iter, eval_log$val_rmse, col = "red", lwd = 2)
  
  # Early stopping 지점 표시
  abline(v = best_iteration, col = "green", lwd = 2, lty = 2)
  abline(v = nrow(eval_log), col = "orange", lwd = 1, lty = 3)
  
  # 최적점 표시
  points(best_iteration, eval_log$val_rmse[best_iteration], 
         col = "green", pch = 19, cex = 1.5)
  
  legend("topright", 
         legend = c("Train RMSE", "Validation RMSE", "Best Iteration", "Actual Stop"),
         col = c("blue", "red", "green", "orange"),
         lty = c(1, 1, 2, 3), lwd = c(2, 2, 2, 1))
  
  # 2. 최적점 주변 확대
  window_start <- max(1, best_iteration - 100)
  window_end <- min(nrow(eval_log), best_iteration + 100)
  
  plot(eval_log$iter[window_start:window_end], 
       eval_log$train_rmse[window_start:window_end], 
       type = "l", col = "blue", lwd = 2,
       xlab = "Iterations", ylab = "RMSE",
       main = paste("Zoom: Best Iteration =", best_iteration))
  lines(eval_log$iter[window_start:window_end], 
        eval_log$val_rmse[window_start:window_end], col = "red", lwd = 2)
  
  abline(v = best_iteration, col = "green", lwd = 2, lty = 2)
  points(best_iteration, eval_log$val_rmse[best_iteration], 
         col = "green", pch = 19, cex = 1.5)
  
  # 3. 성능 격차 변화
  gap <- eval_log$val_rmse - eval_log$train_rmse
  plot(eval_log$iter, gap, 
       type = "l", col = "purple", lwd = 2,
       xlab = "Iterations", ylab = "RMSE Gap (Val - Train)",
       main = "Overfitting Gap Over Time")
  abline(v = best_iteration, col = "green", lwd = 2, lty = 2)
  abline(h = 0, col = "gray", lty = 3)
  
  # 4. 개선 추이 (validation RMSE의 변화율)
  val_improvement <- c(0, diff(eval_log$val_rmse))
  plot(eval_log$iter, val_improvement, 
       type = "l", col = "red", lwd = 2,
       xlab = "Iterations", ylab = "Validation RMSE Change",
       main = "Validation Improvement Rate")
  abline(v = best_iteration, col = "green", lwd = 2, lty = 2)
  abline(h = 0, col = "gray", lty = 3)
  
  # 최근 early_stopping_rounds 구간 표시
  recent_start <- max(1, best_iteration - early_stopping_rounds)
  rect(recent_start, min(val_improvement), best_iteration, max(val_improvement), 
       col = rgb(1, 0, 0, 0.1), border = NA)
  
  par(mfrow = c(1, 1))
  
  # 결과 요약
  cat("=== Early Stopping 결과 ===\n")
  cat("최대 반복 횟수:", max_rounds, "\n")
  cat("Early stopping 기준:", early_stopping_rounds, "회\n")
  cat("실제 최적 반복:", best_iteration, "\n")
  cat("실제 종료 반복:", nrow(eval_log), "\n")
  cat("절약된 반복:", max_rounds - nrow(eval_log), "\n")
  cat("최적 Validation RMSE:", round(eval_log$val_rmse[best_iteration], 4), "\n")
  cat("최종 성능 격차:", round(gap[best_iteration], 4), "\n")
  
  return(list(
    model = model,
    eval_log = eval_log,
    best_iteration = best_iteration
  ))
}

early_result <- plot_early_stopping(call_processed, max_rounds = 500, early_stopping_rounds = 30)

# === Early Stopping 결과 ===
# 최대 반복 횟수: 500 
# Early stopping 기준: 30 회
# 실제 최적 반복: 333 
# 실제 종료 반복: 363 
# 절약된 반복: 137 
# 최적 Validation RMSE: 0.8826 
# 최종 성능 격차: 0.8046 

################여러모델비교###############
install.packages("randomForest")
install.packages("e1071")
library(caret)
library(randomForest)
library(e1071)

# 데이터 준비
set.seed(123)
trainIndex <- createDataPartition(call_processed$call_count, p = 0.8, list = FALSE)
train_data <- call_processed[trainIndex, ]
test_data <- call_processed[-trainIndex, ]

# 교차 검증 설정
control <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final"
)

# 여러 모델 정의
models <- list(
  "Linear Regression" = train(call_count ~ ta_max + ta_min + ws_max + rn_day + ta_max_min + hm_min + hm_max + ws_ins_max, 
                              data = train_data, 
                              method = "lm",
                              trControl = control),
  
  "Random Forest" = train(call_count ~ ta_max + ta_min + ws_max + rn_day + ta_max_min + hm_min + hm_max + ws_ins_max, 
                          data = train_data, 
                          method = "rf",
                          trControl = control),
  
  "SVM" = train(call_count ~ ta_max + ta_min + ws_max + rn_day + ta_max_min + hm_min + hm_max + ws_ins_max, 
                data = train_data, 
                method = "svmRadial", 
                trControl = control)
  
)

# 모델 성능 비교
model_results <- resamples(models)
summary(model_results)

# 시각화
bwplot(model_results)
dotplot(model_results)

# confidence level 0.95로 RMSE 평균:
# Linear Regression: 0.54
# Random Forest: 0.44
# SVM: 0.53

# 우리가 위해서 구했던 xgboost는 0.40, 가장 좋은 결과를 보여줌


######문제가 있음######
# 시계열 데이터라는 것을 망각한채 진행해서 그런지 fold에 따라서 RMSE가 지 마음대로 나옴.. 이거 해결해야됨..
# 일단 위에서 나온 k-fold들 싹다 망각하세요, 이게 맞다 ㅇㅇ..
# 5월부터 10월까지의 데이터니까 일단 계절 나눠서 계절대로 모델을 두 개 만들고나서 그 두 개를 앙상블 하겠습니다.
library(xgboost)
library(caret)

# 계절 정의
season2020 <- c(20200204, 20200505, 20200807, 20201107)
season2021 <- c(20210203, 20210505, 20210807, 20211107)
season2022 <- c(20220204, 20220505, 20220807, 20221107)
season2023 <- c(20230204, 20230506, 20230808, 20231108)
season2024 <- c(20240204, 20240505, 20240807, 20241107)

# 모든 계절 기준점을 하나의 데이터프레임으로 만들기
all_seasons <- data.frame(
  year = rep(2020:2024, each = 4),
  season = rep(c("spring", "summer", "fall", "winter"), 5), 
  date = c(season2020, season2021, season2022, season2023, season2024)
)

# 계절 분류 함수
classify_season <- function(tm_date) {
  year <- as.numeric(substr(tm_date, 1, 4))
  month_day <- as.numeric(substr(tm_date, 5, 8))  # 0501, 0807 형태
  
  # 해당 연도의 계절 기준점들 가져오기
  year_seasons <- all_seasons[all_seasons$year == year, ]
  
  if(nrow(year_seasons) == 0) {
    return("unknown")
  }
  
  # 계절 기준점들 (MMDD 형태)
  winter_start <- as.numeric(substr(year_seasons$date[year_seasons$season == "winter"], 5, 8))  # 예: 1107
  spring_start <- as.numeric(substr(year_seasons$date[year_seasons$season == "spring"], 5, 8))  # 예: 0204  
  summer_start <- as.numeric(substr(year_seasons$date[year_seasons$season == "summer"], 5, 8))  # 예: 0505
  fall_start <- as.numeric(substr(year_seasons$date[year_seasons$season == "fall"], 5, 8))      # 예: 0807
  
  # 계절 분류 (겨울이 연도를 넘나드므로 특별 처리)
  if(month_day >= spring_start & month_day < summer_start) {
    return("spring")   # 0204 <= month_day < 0505
  } else if(month_day >= summer_start & month_day < fall_start) {
    return("summer")   # 0505 <= month_day < 0807  
  } else if(month_day >= fall_start & month_day < winter_start) {
    return("fall")     # 0807 <= month_day < 1107
  } else {
    return("winter")   # 1107 <= month_day 또는 month_day < 0204
  }
}

# 데이터에 계절 정보 추가
call_processed$season <- sapply(call_processed$tm, classify_season)

# 계절별 데이터 확인
cat("=== 계절별 데이터 분포 ===\n")
table(call_processed$season) # spring이 너무 적으므로 summer에 병합시키겠음, 

# spring -> summer 병합
# 기존 spring을 모두 summer로 변경
call_processed$season[call_processed$season == "spring"] <- "summer"

# 피처 컬럼 정의
feature_cols <- c("ta_max", "ta_min", "ta_max_min", "hm_min", "hm_max", 
                  "ws_max", "ws_ins_max", "rn_day")

# 계절별 모델 훈련 함수
train_seasonal_model <- function(season_name, data) {
  cat("Training", season_name, "model...\n")
  
  season_data <- data[data$season == season_name, ]
  
  if(nrow(season_data) < 10) {
    cat("Warning: Not enough data for", season_name, "season\n")
    return(NULL)
  }
  
  # XGBoost 데이터 준비
  X <- as.matrix(season_data[, feature_cols])
  y <- season_data$call_count
  
  dtrain <- xgb.DMatrix(data = X, label = y)
  
  # 파라미터 설정
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.1,
    max_depth = 3,
    subsample = 0.8
  )
  
  # 모델 훈련
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = FALSE
  )
  
  cat(season_name, "model trained with", nrow(season_data), "samples\n")
  return(model)
}

# 각 계절별 모델 훈련
seasonal_models <- list()
seasons <- c("spring", "summer", "fall", "winter")

for(season in seasons) {
  seasonal_models[[season]] <- train_seasonal_model(season, call_processed)
}

# 계절별 가중치 함수 (날짜 기반)
get_seasonal_weights <- function(tm_date) {
  season <- classify_season(tm_date)
  
  # 기본 가중치 (주 계절에 높은 가중치) # 가중치도 추후 코드로 제일 좋은 값을 찾아보겠음
  weights <- list(spring = 0, summer = 0, fall = 0, winter = 0)
  
  if(season == "spring") {
    weights <- list(spring = 0.7, summer = 0.2, fall = 0.05, winter = 0.05)
  } else if(season == "summer") {
    weights <- list(spring = 0.2, summer = 0.7, fall = 0.1, winter = 0)
  } else if(season == "fall") {
    weights <- list(spring = 0.05, summer = 0.2, fall = 0.7, winter = 0.05)
  } else if(season == "winter") {
    weights <- list(spring = 0.05, summer = 0, fall = 0.05, winter = 0.9)
  }
  
  return(weights)
}

# 앙상블 예측 함수
ensemble_predict <- function(new_data, models) {
  results <- numeric(nrow(new_data))
  
  for(i in 1:nrow(new_data)) {
    tm_date <- new_data$tm[i]
    weights <- get_seasonal_weights(tm_date)
    
    # 각 모델로 예측
    X_new <- as.matrix(new_data[i, feature_cols])
    dnew <- xgb.DMatrix(data = X_new)
    
    predictions <- list()
    for(season in seasons) {
      if(!is.null(models[[season]])) {
        predictions[[season]] <- predict(models[[season]], dnew)
      } else {
        predictions[[season]] <- mean(call_processed$call_count)  # 대체값
      }
    }
    
    # 가중 평균 계산
    ensemble_pred <- weights$spring * predictions$spring +
      weights$summer * predictions$summer +
      weights$fall * predictions$fall +
      weights$winter * predictions$winter
    
    results[i] <- ensemble_pred
  }
  
  return(results)
}

# 모델 성능 평가 함수
evaluate_ensemble <- function(data, models) {
  # 시간 순서 기반 분할 (2020-2022 훈련, 2023 검증) # 이건 나중에 2024 검증 데이터가 나오면 그걸로 바꾸겠음
  train_data <- data[data$tm < 20230101, ]
  test_data <- data[data$tm >= 20230101, ]
  
  # 계절별 모델 재훈련 (훈련 데이터만 사용)
  train_models <- list()
  for(season in seasons) {
    train_models[[season]] <- train_seasonal_model(season, train_data)
  }
  
  # 앙상블 예측
  ensemble_pred <- ensemble_predict(test_data, train_models)
  
  # RMSE 계산
  rmse <- sqrt(mean((ensemble_pred - test_data$call_count)^2))
  
  # 계절별 성능도 확인
  cat("\n=== 앙상블 모델 성능 평가 ===\n")
  cat("전체 RMSE:", round(rmse, 4), "\n\n")
  
  cat("계절별 성능:\n")
  for(season in seasons) {
    season_indices <- test_data$season == season
    if(sum(season_indices) > 0) {
      season_rmse <- sqrt(mean((ensemble_pred[season_indices] - test_data$call_count[season_indices])^2))
      cat(season, "RMSE:", round(season_rmse, 4), 
          "(", sum(season_indices), "samples )\n")
    }
  }
  
  return(list(
    overall_rmse = rmse,
    predictions = ensemble_pred,
    models = train_models
  ))
}

# 모델 평가 실행
evaluation_result <- evaluate_ensemble(call_processed, seasonal_models)

# # 새로운 데이터 예측 예시
# predict_new_data <- function(new_data, models) {
#   cat("\n=== 새로운 데이터 예측 ===\n")
#   
#   predictions <- ensemble_predict(new_data, models)
#   
#   result <- data.frame(
#     tm = new_data$tm,
#     season = sapply(new_data$tm, classify_season),
#     predicted_call_count = round(predictions, 2)
#   )
#   
#   return(result)
# }
# 
# # 사용 예시: 2024년 데이터 예측
# if(any(call_processed$tm >= 20240101)) {
#   future_data <- call_processed[call_processed$tm >= 20240101, ]
#   future_predictions <- predict_new_data(future_data, evaluation_result$models)
#   
#   cat("\n2024년 예측 결과 (처음 5개):\n")
#   print(head(future_predictions))
# }
# 
# # 모델 저장 함수
# save_ensemble_model <- function(models, filename) {
#   saveRDS(models, file = filename)
#   cat("Ensemble model saved to", filename, "\n")
# }
# 
# # 모델 로드 함수
# load_ensemble_model <- function(filename) {
#   models <- readRDS(filename)
#   cat("Ensemble model loaded from", filename, "\n")
#   return(models)
# }
# 
# # 최종 모델 저장
# # save_ensemble_model(evaluation_result$models, "seasonal_ensemble_model.rds")

#############하이퍼 파라미터 튜닝###############
# 계절 가중치를 자동으로 최적화
# 파라미터 eta 자동으로 최적화
# 파라미터 max_depth 자동으로 최적화
# 파라미터 subsample 자동으로 최적화
# 모델훈련 nrounds 자동으로 최적화
# 하이퍼파라미터 최적화 함수
# 가중치 자동 최적화 포함 그리드 서치
optimize_weights_and_params <- function(data, verbose = TRUE) {
  
  cat("=== 파라미터 + 가중치 동시 최적화 ===\n")
  
  # 1. XGBoost 파라미터 그리드
  param_grid <- expand.grid(
    eta = c(0.15, 0.2, 0.25),
    max_depth = c(4, 5, 6, 7),
    subsample = c(0.6, 0.7, 0.8),
    nrounds = c(100, 150, 200)
  )
  
  # 2. 가중치 그리드 (주요 계절의 가중치만)
  weight_grid <- expand.grid(
    summer_self = seq(0.5, 0.9, by = 0.1),    # summer 계절에서 summer 모델 가중치
    fall_self = seq(0.5, 0.9, by = 0.1),      # fall 계절에서 fall 모델 가중치
    winter_self = seq(0.5, 0.9, by = 0.1)     # winter 계절에서 winter 모델 가중치
  )
  
  cat("XGBoost 파라미터 조합:", nrow(param_grid), "개\n")
  cat("가중치 조합:", nrow(weight_grid), "개\n")
  cat("총 조합:", nrow(param_grid) * nrow(weight_grid), "개\n\n")
  
  # 데이터 준비
  feature_cols <- c("ta_max", "ta_min", "ta_max_min", "hm_min", "hm_max", 
                    "ws_max", "ws_ins_max", "rn_day")
  
  train_data <- data[data$tm < 20230101, ]
  val_data <- data[data$tm >= 20230101, ]
  
  # 결측치 제거
  train_data <- train_data[complete.cases(train_data[, c(feature_cols, "call_count")]), ]
  val_data <- val_data[complete.cases(val_data[, c(feature_cols, "call_count")]), ]
  
  cat("훈련 데이터:", nrow(train_data), "개\n")
  cat("검증 데이터:", nrow(val_data), "개\n\n")
  
  # 최적화 변수
  best_rmse <- Inf
  best_config <- NULL
  all_results <- data.frame()
  
  total_combinations <- nrow(param_grid) * nrow(weight_grid)
  current_iteration <- 0
  
  # 이중 루프: 파라미터 × 가중치
  for(i in 1:nrow(param_grid)) {
    current_params <- as.list(param_grid[i, ])
    
    for(j in 1:nrow(weight_grid)) {
      current_iteration <- current_iteration + 1
      current_weights_raw <- as.list(weight_grid[j, ])
      
      # 가중치 정규화 (합이 1이 되도록)
      current_weights <- normalize_weights(current_weights_raw)
      
      tryCatch({
        # 현재 조합 평가
        rmse <- evaluate_full_combination(train_data, val_data, current_params, current_weights, feature_cols)
        
        # 결과 저장
        result_row <- data.frame(
          iteration = current_iteration,
          eta = current_params$eta,
          max_depth = current_params$max_depth,
          subsample = current_params$subsample,
          nrounds = current_params$nrounds,
          summer_self = current_weights_raw$summer_self,
          fall_self = current_weights_raw$fall_self,
          winter_self = current_weights_raw$winter_self,
          rmse = rmse
        )
        
        all_results <- rbind(all_results, result_row)
        
        # 최고 성능 업데이트
        if(rmse < best_rmse) {
          best_rmse <- rmse
          best_config <- list(
            params = current_params,
            weights = current_weights,
            raw_weights = current_weights_raw,
            rmse = rmse
          )
          
          if(verbose) {
            cat("new best Iteration", current_iteration, "/", total_combinations, 
                "RMSE:", round(rmse, 4), "\n")
            cat("XGB: eta=", current_params$eta, " depth=", current_params$max_depth, 
                " sub=", current_params$subsample, " rounds=", current_params$nrounds, "\n")
            cat("Weights: summer=", round(current_weights_raw$summer_self, 2), 
                " fall=", round(current_weights_raw$fall_self, 2), 
                " winter=", round(current_weights_raw$winter_self, 2), "\n\n")
          }
        }
        
        # 진행 상황 출력
        if(verbose && current_iteration %% 50 == 0) {
          progress <- round(current_iteration / total_combinations * 100, 1)
          cat("Progress:", progress, "% - Current RMSE:", round(rmse, 4), 
              "Best RMSE:", round(best_rmse, 4), "\n")
        }
        
      }, error = function(e) {
        if(verbose) {
          cat("Error in iteration", current_iteration, ":", e$message, "\n")
        }
      })
    }
  }
  
  cat("\n=== 최적화 완료 ===\n")
  cat("최고 RMSE:", round(best_rmse, 4), "\n")
  cat("최적 XGBoost 파라미터:\n")
  print(best_config$params)
  cat("\n최적 가중치:\n")
  print(best_config$weights)
  
  return(list(
    best_rmse = best_rmse,
    best_config = best_config,
    all_results = all_results
  ))
}

# 가중치 정규화 함수
normalize_weights <- function(raw_weights) {
  
  # 각 계절별 가중치 생성
  weights <- list()
  
  # Summer 계절에서의 가중치
  summer_remaining <- (1 - raw_weights$summer_self) / 2
  weights$summer <- list(
    summer = raw_weights$summer_self,
    fall = summer_remaining,
    winter = summer_remaining
  )
  
  # Fall 계절에서의 가중치
  fall_remaining <- (1 - raw_weights$fall_self) / 2
  weights$fall <- list(
    summer = fall_remaining,
    fall = raw_weights$fall_self,
    winter = fall_remaining
  )
  
  # Winter 계절에서의 가중치
  winter_remaining <- (1 - raw_weights$winter_self) / 2
  weights$winter <- list(
    summer = winter_remaining,
    fall = winter_remaining,
    winter = raw_weights$winter_self
  )
  
  return(weights)
}

# 전체 조합 평가 함수
evaluate_full_combination <- function(train_data, val_data, params, weights, feature_cols) {
  
  seasons <- c("summer", "fall", "winter")
  models <- list()
  
  # 각 계절별 모델 훈련
  for(season in seasons) {
    season_data <- train_data[train_data$season == season, ]
    
    if(nrow(season_data) < 5) {
      models[[season]] <- NULL
      next
    }
    
    X <- as.matrix(season_data[, feature_cols])
    y <- season_data$call_count
    
    dtrain <- xgb.DMatrix(data = X, label = y)
    
    xgb_params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = params$eta,
      max_depth = params$max_depth,
      subsample = params$subsample
    )
    
    models[[season]] <- xgb.train(
      params = xgb_params,
      data = dtrain,
      nrounds = params$nrounds,
      verbose = FALSE
    )
  }
  
  # 앙상블 예측
  predictions <- ensemble_predict_with_weights(val_data, models, weights, feature_cols)
  
  # RMSE 계산
  rmse <- sqrt(mean((predictions - val_data$call_count)^2))
  
  return(rmse)
}

# 가중치 포함 앙상블 예측 함수
ensemble_predict_with_weights <- function(new_data, models, weights, feature_cols) {
  results <- numeric(nrow(new_data))
  seasons <- c("summer", "fall", "winter")
  
  for(i in 1:nrow(new_data)) {
    current_season <- new_data$season[i]
    season_weights <- weights[[current_season]]
    
    X_new <- as.matrix(new_data[i, feature_cols])
    dnew <- xgb.DMatrix(data = X_new)
    
    predictions <- list()
    for(season in seasons) {
      if(!is.null(models[[season]])) {
        predictions[[season]] <- predict(models[[season]], dnew)
      } else {
        predictions[[season]] <- mean(new_data$call_count, na.rm = TRUE)
      }
    }
    
    # 가중 평균 계산
    ensemble_pred <- season_weights$summer * predictions$summer +
      season_weights$fall * predictions$fall +
      season_weights$winter * predictions$winter
    
    results[i] <- ensemble_pred
  }
  
  return(results)
}

# 최종 모델 훈련 (최적화된 가중치 포함)
train_final_model_with_optimized_weights <- function(data, best_config) {
  
  cat("=== 최적화된 파라미터+가중치로 최종 모델 훈련 ===\n")
  
  # 2024년 제외하고 모든 데이터로 훈련
  train_data <- data[data$tm < 20240101, ]
  
  feature_cols <- c("ta_max", "ta_min", "ta_max_min", "hm_min", "hm_max", 
                    "ws_max", "ws_ins_max", "rn_day")
  
  seasons <- c("summer", "fall", "winter")
  final_models <- list()
  
  best_params <- best_config$params
  
  for(season in seasons) {
    season_data <- train_data[train_data$season == season, ]
    
    if(nrow(season_data) < 5) {
      final_models[[season]] <- NULL
      next
    }
    
    X <- as.matrix(season_data[, feature_cols])
    y <- season_data$call_count
    
    dtrain <- xgb.DMatrix(data = X, label = y)
    
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = best_params$eta,
      max_depth = best_params$max_depth,
      subsample = best_params$subsample
    )
    
    final_models[[season]] <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = best_params$nrounds,
      verbose = FALSE
    )
    
    cat("Final", season, "model trained with", nrow(season_data), "samples\n")
  }
  
  return(list(
    models = final_models,
    params = best_params,
    weights = best_config$weights
  ))
}

# 실행 예시
cat("=== 파라미터 + 가중치 동시 최적화 실행 ===\n")

full_optimization_result <- optimize_weights_and_params(call_processed)
final_optimized_model <- train_final_model_with_optimized_weights(call_processed, full_optimization_result$best_config)