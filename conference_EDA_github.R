install.packages("tidyr")
install.packages("dplyr")
library(ggplot2)
library(tidyr)
library(dplyr)
options(max.print = 10000)

##########
#변수셀
season2020 <- c(20200204,20200505,20200807,20201107)
season2021 <- c(20210203,20210505,20210807,20211107)
season2022 <- c(20220204,20220505,20220807,20221107)
season2023 <- c(20230204,20230506,20230808,20231108)
season2024 <- c(20240204,20240505,20240807,20241107)
##########

call_data <- read.csv(YOUR_FILE_HERE)
cat_data <- read.csv(YOUR_FILE_HERE)

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
  gather(key = "variable", value = "value", ta_max, ta_min, ta_max_min, hm_min, hm_max, ws_max, rn_day, call_count)

boxplot(call_processed$rn_day ~ call_processed$rain,
        main = "Rain 그룹별 rn_day 분포",
        xlab = "Rain (0: 적은 강수, 1: 많은 강수)",
        ylab = "rn_day (강수량)",
        col = c("lightblue", "lightcoral"),
        names = c("Rain = 0", "Rain = 1"))

#각 열마다 outlier 몇개 있는지 확인할게요
sapply(call_processed[, c("ta_max", "ta_min", "ta_max_min", "hm_min", "hm_max", "ws_max", "rn_day", "call_count")], function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  sum(x < (Q1 - 1.5 * IQR_value) | x > (Q3 + 1.5 * IQR_value), na.rm = TRUE)
})