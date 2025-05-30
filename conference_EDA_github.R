library(ggplot2)
install.packages("tidyr")
library(tidyr)

call_data <- read.csv(YOUR_CALL119_FILE_HERE)
cat_data <- read.csv(YOUR_CAT119_FILE_HERE)

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
