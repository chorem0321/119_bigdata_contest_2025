import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from collections import Counter

# 데이터 읽기
call_data = pd.read_csv("C:/Users/Junsu Choi/Desktop/jikbak/대학/Matrix/2025-1/conference/call119_train_utf-8.csv")
cat_data = pd.read_csv("C:/Users/Junsu Choi/Desktop/jikbak/대학/Matrix/2025-1/conference/cat119_train_utf-8.csv")

# call_data 먼저 EDA 하겠습니다
# 열 이름들 너무 길어서 바꿔줄게요
call_data.columns = ["x","tm","시","구","상세","stn","ta_max","ta_min","ta_max_min","hm_min","hm_max","ws_max","ws_ins_max","rn_day","call_count"]

# 데이터 구조 확인
print("call_data 구조:")
print(call_data.dtypes)
print("\ncall_data 기본 통계:")
print(call_data.describe())

# boxplot으로 outlier 탐지
# R의 gather와 같은 역할을 하는 melt 사용
numeric_cols = ["ta_max", "ta_min", "ta_max_min", "hm_min", "hm_max", "ws_max", "rn_day", "call_count"]
call_long = pd.melt(call_data[numeric_cols], var_name='variable', value_name='value')

plt.figure(figsize=(12, 8))
sns.boxplot(data=call_long, x='variable', y='value')
plt.xticks(rotation=45)
plt.title('Boxplot for Outlier Detection')
plt.tight_layout()
plt.show()

# 각 열마다 outlier 몇개 있는지 확인
def count_outliers(series):
    Q1 = series.quantile(0.25)
    Q3 = series.quantile(0.75)
    IQR = Q3 - Q1
    lower_bound = Q1 - 1.5 * IQR
    upper_bound = Q3 + 1.5 * IQR
    return ((series < lower_bound) | (series > upper_bound)).sum()

outlier_counts = {}
for col in numeric_cols:
    outlier_counts[col] = count_outliers(call_data[col])

print("\n각 변수별 이상치 개수:")
for col, count in outlier_counts.items():
    print(f"{col}: {count}")

# call_data의 총 행 갯수
print(f"\ncall_data 총 행 개수: {len(call_data)}")

# call_data의 각 열마다 몇개의 결측치가 있는지 확인 (-99.0을 결측치로 간주)
missing_counts = (call_data == -99.0).sum()
print("\n각 열별 결측치(-99.0) 개수:")
print(missing_counts)

# 하루에 call_count가 0인 데이터 확인
zero_calls = (call_data['call_count'] == 0).sum()
print(f"\ncall_count가 0인 데이터 개수: {zero_calls}")

# cat_data 구조 살펴보기
# 일단 이것도 보기 쉽게 열 이름부터 바꿀게요
cat_data.columns = ["x","tm","시","구","상세","cat","sub_cat","stn","call_count"]

print("\n" + "="*50)
print("cat_data 분석")
print("="*50)

print("\ncat_data 기본 통계:")
print(cat_data.describe())

print("\ncat_data 구조:")
print(cat_data.dtypes)

print("\ncat 변수의 빈도:")
cat_counts = cat_data['cat'].value_counts()
print(cat_counts)

print("\nsub_cat 변수의 빈도:")
sub_cat_counts = cat_data['sub_cat'].value_counts()
print(sub_cat_counts)