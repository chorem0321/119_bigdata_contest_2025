import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from collections import Counter
from scipy import stats
from scipy.stats import pearsonr
import warnings
warnings.filterwarnings('ignore')

# 출력 옵션 설정
pd.set_option('display.max_rows', 10000)
pd.set_option('display.max_columns', None)

##########
# 변수설정
season2020 = [20200204, 20200505, 20200807, 20201107]
season2021 = [20210203, 20210505, 20210807, 20211107]
season2022 = [20220204, 20220505, 20220807, 20221107]
season2023 = [20230204, 20230506, 20230808, 20231108]
season2024 = [20240204, 20240505, 20240807, 20241107]
##########

# 데이터 읽기
call_data = pd.read_csv("C:/Users/Junsu Choi/Desktop/jikbak/대학/Matrix/2025-1/conference/call119_train_utf-8.csv")
cat_data = pd.read_csv("C:/Users/Junsu Choi/Desktop/jikbak/대학/Matrix/2025-1/conference/cat119_train_utf-8.csv")

# call_data 먼저 EDA
# 열 이름들 너무 길어서 바꿔줄게요
call_data.columns = ["x","tm","시","구","상세","stn","ta_max","ta_min","ta_max_min","hm_min","hm_max","ws_max","ws_ins_max","rn_day","call_count"]

# 데이터 구조 확인
print("데이터 구조:")
print(call_data.info())
print("\n기본 통계 요약:")
print(call_data.describe())

# boxplot으로 outlier 탐지
numeric_cols = ["ta_max", "ta_min", "ta_max_min", "hm_min", "hm_max", "ws_max", "rn_day", "call_count"]
call_long = pd.melt(call_data[numeric_cols], var_name='variable', value_name='value')

plt.figure(figsize=(12, 8))
sns.boxplot(data=call_long, x='variable', y='value', color='lightblue')
plt.xticks(rotation=45)
plt.title('변수별 Boxplot')
plt.tight_layout()
plt.show()

# 각 열마다 outlier 몇개 있는지 확인
def count_outliers(series):
    Q1 = series.quantile(0.25)
    Q3 = series.quantile(0.75)
    IQR = Q3 - Q1
    lower_bound = Q1 - 1.5 * IQR
    upper_bound = Q3 + 1.5 * IQR
    outliers = (series < lower_bound) | (series > upper_bound)
    return outliers.sum()

outlier_counts = {}
for col in numeric_cols:
    outlier_counts[col] = count_outliers(call_data[col])

print("\n각 열의 outlier 개수:")
for col, count in outlier_counts.items():
    print(f"{col}: {count}")

# call_data의 총 행 갯수
print(f"\ncall_data 총 행 개수: {len(call_data)}")

# call_data의 각 열마다 몇개의 결측치(-99.0)가 있는지 확인
missing_counts = (call_data == -99.0).sum()
print("\n각 열의 결측치(-99.0) 개수:")
print(missing_counts)

# 하루에 call_count가 0인 데이터 확인
zero_calls = (call_data['call_count'] == 0).sum()
print(f"\ncall_count가 0인 데이터 개수: {zero_calls}")

# cat_data 구조 살펴보기
# 보기 쉽게 열 이름부터 바꿀게요
cat_data.columns = ["x","tm","시","구","상세","cat","sub_cat","stn","call_count"]

print("\ncat_data 기본 통계:")
print(cat_data.describe())
print("\ncat_data 구조:")
print(cat_data.info())

print("\ncat 열의 빈도표:")
print(cat_data['cat'].value_counts())
print("\nsub_cat 열의 빈도표:")
print(cat_data['sub_cat'].value_counts())

######################
# 자 여기부터 데이터 전처리 시작합니다
######################

# call_data 결측치 제거
call_clean = call_data[~(call_data == -99.0).any(axis=1)].copy()
print(f"\n결측치 제거 후 데이터 개수: {len(call_clean)}")

# call_data tm 기준으로 그룹화
# 필요 없는 열들 제거
columns_to_drop = ['x', '시', '구', '상세', 'stn']
call_for_grouping = call_clean.drop(columns=columns_to_drop)

# tm 기준으로 그룹화하고 나머지 열들 평균 계산
call_processed = call_for_grouping.groupby('tm').mean().reset_index()

# 열이름 재설정
call_processed.columns = ["tm","ta_max","ta_min","ta_max_min","hm_min","hm_max","ws_max","ws_ins_max","rn_day","call_count"]

# 비가 많이 왔는지 아닌지 나누기 (1,0)
call_processed['rain'] = (call_processed['rn_day'] >= 15.0).astype(int)

####################################################

# rn_day와 call_count의 상관관계 분석

# 그룹화된 데이터(call_processed)에서의 상관관계
print("=== 그룹화된 데이터 상관관계 분석 ===")

# 상관계수 및 p-value
call_corr2, call_p2 = pearsonr(call_processed['rn_day'], call_processed['call_count'])

print(f"상관계수: {round(call_corr2, 4)}")
print(f"p-value: {call_p2}")

# 결과 요약
print("=== 결과 요약 ===")
print(f"그룹화 데이터 - correlation: {round(call_corr2, 4)}")
print(f"그룹화 데이터 - p-value: {call_p2}")

#########################################
# 최적의 임계값을 찾는 함수 (rn_day를 임계값으로 변환)
def find_optimal_threshold_simple(data):
    # 임계값 범위 설정 (0부터 100까지 0.1씩)
    thresholds = np.arange(0, 100.1, 0.1)
    
    # 결과를 저장할 리스트
    correlations = []
    p_values = []
    
    print("임계값 최적화 진행 중...")
    
    # 각 임계값에 대해 상관계수 계산
    for i, threshold in enumerate(thresholds):
        # rn_day를 임계값으로 변환 (임계값 이상이면 그 값, 미만이면 0)
        rn_day_threshold = np.where(data['rn_day'] >= threshold, data['rn_day'], 0)
        
        # 상관계수 계산
        corr, p_val = pearsonr(rn_day_threshold, data['call_count'])
        correlations.append(abs(corr))  # 절댓값으로 저장
        p_values.append(p_val)
        
        # 진행상황 출력 (100개마다)
        if (i + 1) % 100 == 0:
            print(f"진행률: {round((i+1)/len(thresholds)*100, 1)}%")
    
    # 결과 데이터프레임 생성
    results = pd.DataFrame({
        'threshold': thresholds,
        'correlation': correlations,
        'p_value': p_values
    })
    
    # 최고 상관계수와 해당 임계값 찾기
    max_idx = np.argmax(correlations)
    optimal_threshold = thresholds[max_idx]
    max_correlation = correlations[max_idx]
    optimal_p_value = p_values[max_idx]
    
    print("\n=== 최적화 결과 ===")
    print(f"최적 임계값: {optimal_threshold}")
    print(f"최고 상관계수: {round(max_correlation, 6)}")
    print(f"p-value: {optimal_p_value}")
    
    # 상위 10개 결과 출력
    print("\n=== 상위 10개 결과 ===")
    top_results = results.nlargest(10, 'correlation')
    print(top_results)
    
    return {
        'optimal_threshold': optimal_threshold,
        'max_correlation': max_correlation,
        'optimal_p_value': optimal_p_value,
        'all_results': results
    }

# 최적화 실행
optimization_result = find_optimal_threshold_simple(call_processed)

# 최적 임계값으로 상관관계 분석
optimal_threshold = optimization_result['optimal_threshold']
rn_day_optimal = np.where(call_processed['rn_day'] >= optimal_threshold, call_processed['rn_day'], 0)

print("\n=== 최적 임계값으로 변환된 데이터 상관관계 분석 ===")

# 상관계수 및 p-value
optimal_corr, optimal_p = pearsonr(rn_day_optimal, call_processed['call_count'])

print(f"최적 임계값: {optimal_threshold}")
print(f"상관계수: {round(optimal_corr, 4)}")
print(f"p-value: {optimal_p}")

# 기존 방법과 비교
print("\n=== 기존 방법 vs 최적화 방법 비교 ===")
original_corr, original_p = pearsonr(call_processed['rn_day'], call_processed['call_count'])

print(f"기존 방법 - 상관계수: {round(original_corr, 4)}, p-value: {original_p}")
print(f"최적화 방법 - 상관계수: {round(optimal_corr, 4)}, p-value: {optimal_p}")
print(f"상관계수 개선: {round(abs(optimal_corr) - abs(original_corr), 4)}")

# 임계값별 상관계수 계산 및 그래프
thresholds = np.arange(0, 100.1, 0.1)
cor_vals = []

for th in thresholds:
    rn_day_th = np.where(call_processed['rn_day'] >= th, call_processed['rn_day'], 0)
    corr, _ = pearsonr(rn_day_th, call_processed['call_count'])
    cor_vals.append(corr)

# 그래프 그리기
plt.figure(figsize=(12, 8))
plt.plot(thresholds, cor_vals, color='blue', linewidth=2)
plt.xlabel('임계값')
plt.ylabel('상관계수')
plt.title('임계값에 따른 rn_day 변환값과 call_count의 상관계수 변화')
plt.grid(True)

# 최고점 표시
max_idx = np.argmax(np.abs(cor_vals))
plt.scatter(thresholds[max_idx], cor_vals[max_idx], color='red', s=100, zorder=5)
plt.annotate(f'최고점: {round(thresholds[max_idx], 2)}\n상관계수: {round(cor_vals[max_idx], 4)}',
             xy=(thresholds[max_idx], cor_vals[max_idx]),
             xytext=(thresholds[max_idx] + 10, cor_vals[max_idx]),
             color='red',
             fontsize=10)
plt.tight_layout()
plt.show()

# 비가 많이 왔는지 아닌지 다시 나누기 (1,0) -> rn_day랑 call_count의 상관관계가 가장 높아지는 rn_day 값
call_processed['rain'] = (call_processed['rn_day'] >= 35.2).astype(int)

#####################################
# boxplot으로 outlier 탐지
numeric_cols_processed = ["ta_max", "ta_min", "ta_max_min", "hm_min", "hm_max", "ws_max", "rn_day", "call_count"]
call_processed_long = pd.melt(call_processed[numeric_cols_processed], var_name='variable', value_name='value')

# Rain 그룹별 rn_day 분포 boxplot
plt.figure(figsize=(10, 6))
rain_groups = ['Rain = 0', 'Rain = 1']
data_to_plot = [call_processed[call_processed['rain'] == 0]['rn_day'],
                call_processed[call_processed['rain'] == 1]['rn_day']]

box_plot = plt.boxplot(data_to_plot, labels=rain_groups, patch_artist=True)
box_plot['boxes'][0].set_facecolor('lightblue')
box_plot['boxes'][1].set_facecolor('lightcoral')

plt.title('Rain 그룹별 rn_day 분포')
plt.xlabel('Rain (0: 적은 강수, 1: 많은 강수)')
plt.ylabel('rn_day (강수량)')
plt.grid(True, alpha=0.3)
plt.show()

# 각 열마다 outlier 몇개 있는지 확인
print("\n각 열의 outlier 개수 (call_processed):")
outlier_counts_processed = {}
for col in numeric_cols_processed:
    outlier_counts_processed[col] = count_outliers(call_processed[col])
    print(f"{col}: {outlier_counts_processed[col]}")