setwd('C:/Users/wkdgu/Desktop/논문')
getwd()
library(ecos); library(seasonal); library(mFilter); library(tempdisagg); library(tidyverse)
library(tsbox); library(xts); library(patchwork); library(ggfortify); library(timeSeries)
library(readxl);  library(vars);  library(stargazer)

my_key <- c('D9X3413P7XEYGVU65ZHL')

# ------------------------------------------------------------------------------------------------
# 기준금리 2002년 1분기 ~ 2022 2분기
# 통계표[코드][주기] : 한국은행 기준금리 및 여수신금리 [722Y001][A,D,M,Q]
# 통계항목[코드][단위] : 한국은행 기준금리 [0101000][연%]
kr <- statSearch(api_key = my_key, stat_code = '722Y001', item_code1='0101000', cycle='Q', start_time='2002Q1', end_time='2022Q2')
kr <- ts(kr$data_value, frequency = 4, start=c(2002,1), end=c(2022,2))


# --------------------------------------------------------------------------------
# 경제성장률 2002 1분기 ~ 2022 2분기
# 통계표[코드][주기] : 국제 주요국 경제성장률 [902Y015][Q]
# 통계항목[코드][단위] : 한국 [KOR][%]
gr <- statSearch(api_key = my_key, stat_code = '902Y015', item_code1='KOR', cycle='Q', start_time='2002Q1', end_time='2022Q2')
gr <- ts(gr$data_value, frequency = 4, start=c(2002,1), end=c(2022,2))


# --------------------------------------------------------------------------------
# 실업률 2002 1분기 ~ 2022 2분기
# 통계표[코드][주기] : 국제 주요국 실업률(계절변동조정) [902Y021][A,M,Q]
# 통계항목[코드][단위] : 한국 [KOR][%]
ur <- statSearch(api_key = my_key, stat_code = '902Y021', item_code1='KOR', cycle='Q', start_time='2002Q1', end_time='2022Q2')
ur <- ts(ur$data_value, frequency = 4, start=c(2002,1), end=c(2022,2))


# --------------------------------------------------------------------------------
# 가계부채 2002 1분기 ~ 2022 2분기
# 통계표[코드][주기] : 대출행태서베이(신용위험) [514Y002][Q]
# 계정항목-코드1 : 국내은행-가계 [BB03]
debt <- statSearch(api_key = my_key, stat_code = '514Y002', item_code1='BB03', cycle='Q', start_time='2002Q1', end_time='2022Q2')
debt <- ts(debt$data_value, frequency = 4, start=c(2002,1), end=c(2022,2))


# --------------------------------------------------------------
# 데이터 통합
# 실업률, 성장률, 가계부채, 금리
data <- cbind(ur, gr, debt, kr)
colnames(data) <- c('ur', 'gr', 'debt', 'kr')
data[,'debt'] <- ifelse (data[,'debt'] == 0 , 3, data[,'debt']) # debt칼럼의 15행 값인 0을 전기 값인 3으로 바꿔줌 

# -----------------------------------------------------------------------------------------
# 데이터 시각화

# data2는 data를 차분한 변수
data2 <- diff(data)
colnames(data2) <- c('ur', 'gr', 'debt', 'kr')

# 수준변수 시각화
aa1 <- autoplot(data[,'kr'], ylab='kr')
aa2 <- autoplot(data[,'gr'], ylab='gr')
aa3 <- autoplot(data[,'ur'], ylab='ur')
aa4 <- autoplot(data[,'debt'], ylab='debt')

(aa1 / aa2) | (aa3 / aa4)

# 차분변수 시각화
bb1 <- autoplot(data2[,'kr'], ylab='kr')
bb2 <- autoplot(data2[,'gr'], ylab='gr')
bb3 <- autoplot(data2[,'ur'], ylab='ur')
bb4 <- autoplot(data2[,'debt'], ylab='debt')

(bb1 / bb2) | (bb3 / bb4)



#-----------------------------------------------------------------------------------------------
# 단위근 검정 

# 첫번째 : 결과 저장용 matrix 만들기
# 먼저 단위근 검정 결과 저장용 matrix 만들기 : 8개의 변수를 단위근 검정
# 결과물은 변수, deterministic terms, 시차, 검정통계량, 1%, 5%, 10% 유의수준을 저장
urresults <- matrix(nrow=8, ncol=7)
urresults[,1] <- c('ur', 'd_ur', 'gr', 'd_gr', 'debt', 'd_debt', 'kr', 'd_kr')
urresults[,2] <- c('const', 'none', 'const', 'none', 'const', 'none', 'const', 'none')
colnames(urresults) <- c('Variable', 'Det. terms', 'Lags', 'Test value', '1%', '5%', '10%') 


# 두번째 : 최적시차 설정
# 최적시차는 vars패캐지의 VARselect()함수를 이용하면 됨
# 분기자료는 최대시차를 4로 설정
# type은 c('constant', 'trend', 'both', 'none')중에서 택일

# 단위근 검정시 type 설정법
# 추세가 있어보이면 trend, 
# 추세는 없지만 평균이 0이 아닌 것 같으면 const
# 추세도 없고 평균도 0인 것 같으면 none
ur1lagselect <- VARselect(data[, 'ur'], lag.max=4, type='const')
ur2lagselect <- VARselect(data2[, 'ur'], lag.max=4, type='none')
ur3lagselect <- VARselect(data[, 'gr'], lag.max=4, type='const')
ur4lagselect <- VARselect(data2[, 'gr'], lag.max=4, type='none')
ur5lagselect <- VARselect(data[, 'debt'], lag.max=4, type='const')
ur6lagselect <- VARselect(data2[, 'debt'], lag.max=4, type='none')
ur7lagselect <- VARselect(data[, 'kr'], lag.max=4, type='const')
ur8lagselect <- VARselect(data2[, 'kr'], lag.max=4, type='none')


# 세번째 : 최적시차 뽑아내기
# 시차는 AIC로 결정
# AIC결과는 아래에 저장되어 있음
ur1lag <- ur1lagselect[['selection']][['AIC(n)']]
ur2lag <- ur2lagselect[['selection']][['AIC(n)']]
ur3lag <- ur3lagselect[['selection']][['AIC(n)']]
ur4lag <- ur4lagselect[['selection']][['AIC(n)']]
ur5lag <- ur5lagselect[['selection']][['AIC(n)']]
ur6lag <- ur6lagselect[['selection']][['AIC(n)']]
ur7lag <- ur7lagselect[['selection']][['AIC(n)']]
ur8lag <- ur8lagselect[['selection']][['AIC(n)']]



# 네번째 : 단위근 검정
# 위에서 단위근 검정에 필요한 시차를 정한 것
# 단위근 검정결과 테이블 첫번째 열은 변수명, 두번째는 추정시 포함되는 사항들(예 : 상수항, 추세)
# 3번째는 최적시차, 4번째는 단위근 통계량, 5,6,7은 각각 1%, 5%, 10% 유의수준이 들어감

# 1.단위근 검정결과 
ur1 <- summary(ur.df(data[, 'ur'], type = 'drift', lags=ur1lag))
urresults[1, 3:7] <- c(ur1lag, round(ur1@teststat[1],2), ur1@cval[1,])

# 2. 단위근 검정결과
ur2 <- summary(ur.df(data2[, 'ur'], type = 'none', lags=ur2lag))
urresults[2, 3:7] <- c(ur2lag, round(ur2@teststat[1],2), ur2@cval[1,])

# 3. 단위근 검정결과
ur3 <- summary(ur.df(data[, 'gr'], type = 'drift', lags=ur3lag))
urresults[3, 3:7] <- c(ur3lag, round(ur3@teststat[1],2), ur3@cval[1,])

# 4. 단위근 검정결과
ur4 <- summary(ur.df(data2[, 'gr'], type = 'none', lags=ur4lag))
urresults[4, 3:7] <- c(ur4lag, round(ur4@teststat[1],2), ur4@cval[1,])

# 5. 단위근 검정결과
ur5 <- summary(ur.df(data[, 'debt'], type = 'drift', lags=ur5lag))
urresults[5, 3:7] <- c(ur5lag, round(ur5@teststat[1],2), ur5@cval[1,])

# 6. 단위근 검정결과
ur6 <- summary(ur.df(data2[, 'debt'], type = 'none', lags=ur6lag))
urresults[6, 3:7] <- c(ur6lag, round(ur6@teststat[1],2), ur6@cval[1,])

# 7. 단위근 검정결과
ur7 <- summary(ur.df(data[, 'kr'], type = 'drift', lags=ur7lag))
urresults[7, 3:7] <- c(ur7lag, round(ur7@teststat[1],2), ur7@cval[1,])

# 8. 단위근 검정결과
ur8 <- summary(ur.df(data2[, 'kr'], type = 'none', lags=ur8lag))
urresults[8, 3:7] <- c(ur8lag, round(ur8@teststat[1],2), ur8@cval[1,])


# 워드파일로 단위근 검정결과를 변환
stargazer(urresults, type='html', out='ur_results.doc')



# --------------------------------------------------------------------------------
# 공적분 검정

# 단위근 검정은 단일변수에 대해서 검정을 수행
# 공적분 검정은 단위근을 갖는 여러 변수들간의 안정적인 관계여부를 테스트하는 것

# 공적분 검정을 위해서는 먼저 수준변수를 이용해서 VAR모형을 추정하고 최적시차 선택
# 수준변수에서 단위근이 있을 시에 공적분 검정 시행후 공적분이 있으면 var모형을 못 쓸수도 있음
# type은 c('const', 'trend', 'both', 'none') 중에서 택일
VARselect(data, lag.max=8, type = 'both')

# $selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 1       1      1      1 
# AIC 기준으로 시차는 1

# 공적분 검정 수행(시차는 2)
ect <- summary(ca.jo(data, type='trace', ecdet='trend', K=2, spec='transitory'))


# 공적분결과 테이블로 만들기
# 1열은 공적분의 갯수, 2열은 test statistics, 3-5 열은 90%, 95%, 99% 신뢰구간
ectresults = matrix(nrow=4, ncol=5)

ectresults[1:4,1] <- c(0,1,2,3)
ectresults[1:4,2] <- round(c(ect@teststat[4],ect@teststat[3],ect@teststat[2],ect@teststat[1]), 2)
ectresults[1:4,3] <- c(ect@cval[4,1], ect@cval[3,1], ect@cval[2,1], ect@cval[1,1])
ectresults[1:4,4] <- c(ect@cval[4,2], ect@cval[3,2], ect@cval[2,2], ect@cval[1,2])
ectresults[1:4,5] <- c(ect@cval[4,3], ect@cval[3,3], ect@cval[2,3], ect@cval[1,3])

colnames(ectresults) <- c('r', 'Test statistics', '90%', '95%', '99%')
ectresults

stargazer(ectresults, type='html', out='coint_results.doc')



# -------------------------------------------------------------------------------------------------
# 그랜져 인과관계
# 그랜져 인과관계를 사용하기 위해서는 정상성이 전제가 되어야 하는데 그래서 모두 차분변수를 이용한다. 
# 실업률 - 성장률 - 가계부채 - 금리

# 금리 ~ 성장률
grangertest(data2[,'kr'] ~ data2[,'gr'], order=4) # p-value = 0.7495

# 금리 ~ 실업률
grangertest(data2[,'kr'] ~ data2[,'ur'], order=4) # p-value = 0.6797

# 금리 ~ 가계부채
grangertest(data2[,'kr'] ~ data2[,'debt'], order=4) # p-value = 0.6586


# 성장률 ~ 금리
grangertest(data2[,'gr'] ~ data2[,'kr'], order=4) # p-value =  0.02164 *

# 성장률 ~ 실업률
grangertest(data2[,'gr'] ~ data2[,'ur'], order=4) # p-value = 0.05577 .

# 성장률 ~ 가계부채
grangertest(data2[,'gr'] ~ data2[,'debt'], order=4)  # p-value = 0.3539


# 실업률 ~ 금리
grangertest(data2[,'ur'] ~ data2[,'kr'], order=4)  # p-value = 0.08541 .

# 실업률 ~ 성장률
grangertest(data2[,'ur'] ~ data2[,'gr'], order=4) # p-value = 0.006846 **

# 실업률 ~ 가계부채
grangertest(data2[,'ur'] ~ data2[,'debt'], order=4) # p-value = 0.03606 *


# 가계부채 ~ 금리
grangertest(data2[,'debt'] ~ data2[,'kr'], order=4)  # p-value = 0.4858

# 가계부채 ~ 성장률
grangertest(data2[,'debt'] ~ data2[,'gr'], order=4) # p-value = 0.1612

# 가계부채 ~ 실업률
grangertest(data2[,'debt'] ~ data2[,'ur'], order=4) # p-value = 0.00714 **



# ------------------------------------------------------------------
# VAR 추정 : 차분된 변수들을 이용
# 변수 순서 : 금리, 가계부채, 실업률, 성장률


# 최적시차 결정
VARselect(data2, lag.max = 1, type = 'const') # AIC기준 최적시차는 1

# VAR 모형추정
varresult <- VAR(data2, p=8, type='const') # 차분변수는 추세가 없어서 var모형을 돌릴 때 const만 넣음
summary(varresult)




# SVAR 모형추정예제 : A 모형(단기제약조건 부과)
# 변수의 순서가 중요한데, 실업율 -> 성장률 -> 산업생산지수 -> 기준금리 
# 단기제약조건을 통해 n(n-1)/2 = 4*3/2 = 6개의 0의 단기제약조건 부과

# amat
#      [,1]  [,2]  [,3]  [,4]
#[1,]   na    0     0     0
#[2,]   na    na    0     0
#[3,]   na    na    na    0
#[4,]   na    na    na    na

amat <- matrix(NA, 4, 4)
amat[upper.tri(amat)] <- 0 
amat

svarresult <- SVAR(varresult, estmethod='scoring', Amat = amat)
summary(svarresult)


# 기준 금리 충격이 왔을 때의 변수들의 반응을 살펴본다.
# n.head=13 : 13기까지 살펴본다. 분기자료여서 3년치 자료를 확인한다.
# 대부분 충격에 대한 변수들의 반응은 차분변수보다는 수준변수를 이용한다.
# "cumulative=TRUE"로 충격반응함수를 누적시켜서 차분변수를 이용해서 수준변수의 반응을 구할 수 있다.
# ci=0.68 : 신뢰구간은 1시그마이다. 

# 금리충격
irfresult_kr <- irf(svarresult, impulse=c('kr'), response=c('gr', 'ur', 'debt'), n.ahead=16, cumulative=TRUE,
                    boot=TRUE, runs=100, ci=0.68)

plot(irfresult_kr)


# 금리충격 - 성장률 반응
irfresult_kr_gr <- irf(svarresult, impulse=c('kr'), response=c('gr'), n.ahead=16, cumulative=TRUE,
                    boot=TRUE, runs=100, ci=0.68)

plot(irfresult_kr_gr)


# 금리충격 - 실업률 반응
irfresult_kr_ur <- irf(svarresult, impulse=c('kr'), response=c('ur'), n.ahead=16, cumulative=TRUE,
                       boot=TRUE, runs=100, ci=0.68)

plot(irfresult_kr_ur)


# 금리충격 - 가계부채 반응
irfresult_kr_debt <- irf(svarresult, impulse=c('kr'), response=c('debt'), n.ahead=16, cumulative=TRUE,
                       boot=TRUE, runs=100, ci=0.68)

plot(irfresult_kr_debt)




