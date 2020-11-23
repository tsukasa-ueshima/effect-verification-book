# (1) havenパッケージのインストール（初回のみ）
#install.packages("haven")

# (2) ライブラリの読み込み
library("tidyverse")
library("haven")
library("broom")
library("MatchIt")
library("WeightIt")
library("cobalt")

# (3)NBER archiveからデータを読み込む
cps1_data <- read_dta("https://users.nber.org/~rdehejia/data/cps_controls.dta")
cps3_data <- read_dta("https://users.nber.org/~rdehejia/data/cps_controls3.dta")
nswdw_data <- read_dta("https://users.nber.org/~rdehejia/data/nsw_dw.dta")

# (4)データセットの準備
## NSWデータから介入グループだけ取り出してCPS1と付ける
cps1_nsw_data <- nswdw_data %>%
  filter(treat == 1) %>%
  rbind(cps1_data)

## NSWデータから介入グループだけ取り出してCPS3と付ける
cps3_nsw_data <- nswdw_data %>%
  filter(treat == 1) %>%
  rbind(cps3_data)

# (5) RCT データでの分析
## 共変量なしの回帰分析
nsw_nocov <- lm(data = nswdw_data,
                formula = re78 ~ treat) %>%
  tidy()

## 共変量付きの回帰分析
nsw_cov <- lm(data = nswdw_data,
              formula = re78 ~ treat + re74 + re75 + age + education + black +
                hispanic + nodegree + married
) %>%
  tidy() %>%
  filter(term == "treat")

# (6) バイアスのあるデータでの回帰分析
## CPS1の分析結果
cps1_reg <- lm(data = cps1_nsw_data,
               formula = re78 ~ treat + re74 + re75 + age + education + black +
                 hispanic + nodegree + married
) %>%
  tidy() %>%
  filter(term == "treat")

## CPS3の分析結果
cps3_reg <- lm(data = cps3_nsw_data,
               formula = re78 ~ treat + re74 + re75 + age + education + black +
                 hispanic + nodegree + married
) %>%
  tidy() %>%
  filter(term == "treat")

# (7) 傾向スコアマッチングによる効果推定
## 傾向スコアを用いたマッチング
m_near <- matchit(formula = treat ~ age + education + black + hispanic +
                    nodegree + married + re74 + re75 + I(re74^2) + I(re75^2),
                  data = cps1_nsw_data,
                  method = "nearest")

## 共変量のバランスを確認
love.plot(m_near,
          threshold = .1)

## マッチング後のデータを作成
matched_data <- match.data(m_near)

## マッチング後のデータで効果の推定
PSM_result_cps1 <- lm(data = matched_data,
                      formula = re78 ~ treat) %>%
  tidy()

# (8) IPWによる効果推定
## 重みの推定
weighting <- weightit(formula = treat ~ age + education + black + hispanic +
                        nodegree + married + re74 + re75 +
                        I(re74^2) + I(re75^2),
                      data = cps1_nsw_data,
                      method = "ps",
                      estimand = "ATE")

## 共変量のバランスを確認
love.plot(weighting,
          threshold = .1)

## 重み付きデータでの効果の推定
IPW_result <- lm(data = cps1_nsw_data,
                 formula = re78 ~ treat,
                 weights = weighting$weights) %>%
  tidy()

# (9) IPWによる効果推定(ATT)
## 重みの推定
weighting <- weightit(formula = treat ~ age + education + black + hispanic + nodegree + married + re74 + re75 + I(re74^2) + I(re75^2),
                      data = cps1_nsw_data,
                      method = "ps",
                      estimand = "ATT")

## 共変量のバランスを確認
love.plot(weighting,
          threshold = .1)

## 重み付きデータでの効果の推定
IPW_result <- lm(data = cps1_nsw_data,
                 formula = re78 ~ treat,
                 weights = weighting$weights) %>%
  tidy()

# (1) ライブラリのインストール（初回のみ）
install.packages("cobalt")
install.packages("WeightIt")
install.packages("MatchIt")
install.packages("Matching")

# (2) tidyverseの読み出し
library("tidyverse")
library("broom")

# (3) データの読み込み
email_data <- read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

# (4) 女性向けメールが配信されたデータを削除したデータを作成
male_df <- email_data %>%
  filter(segment != "Womens E-Mail") %>% # 女性向けメールが配信されたデータを削除
  mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0)) # 介入を表すtreatment変数を追加

# (5) セレクションバイアスのあるデータを作成
## seedを固定する
set.seed(1)

## 条件に反応するサンプルの量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5

## バイアスのあるデータを作成
biased_data <- male_df %>%
  mutate(obs_rate_c = ifelse( (history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1),
         obs_rate_t = ifelse( (history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t),
         random_number = runif(n = NROW(male_df))) %>%
  filter( (treatment == 0 & random_number < obs_rate_c ) |
            (treatment == 1 & random_number < obs_rate_t) )

# (6) 傾向スコアの推定
ps_model <- glm(data = biased_data,
                formula = treatment ~ recency + history + channel,
                family = binomial)


# (7) 傾向スコアマッチング
## ライブラリの読み込み
library("MatchIt")

## 傾向スコアを利用したマッチング
m_near <- matchit(formula = treatment ~ recency + history + channel,
                  data = biased_data,
                  method = "nearest",
                  replace = TRUE)


## マッチング後のデータを作成
matched_data <- match.data(m_near)

## マッチング後のデータで効果の推定
PSM_result <- lm(data = matched_data,
                 formula = spend ~ treatment) %>%
  tidy()

# (8) 逆確率重み付き推定（IPW）
## ライブラリの読み込み
library("WeightIt")

## 重みの推定
weighting <- weightit(treatment ~ recency + history + channel,
                      data = biased_data,
                      method = "ps",
                      estimand = "ATE")

## 重み付きデータでの効果の推定
IPW_result <- lm(data = biased_data,
                 formula = spend ~ treatment,
                 weights = weighting$weights) %>%
  tidy()

# (9) 共変量のバランスを確認
##ライブラリの読み込み
library("cobalt")

## マッチングしたデータでの共変量のバランス
love.plot(m_near,
          threshold = .1)

## 重み付きデータでの共変量のバランス
love.plot(weighting,
          threshold = .1)

# (10) 統計モデルを用いたメールの配信のログを分析
## 学習データと配信ログを作るデータに分割
set.seed(1)

train_flag <- sample(NROW(male_df), NROW(male_df)/2, replace = FALSE)

male_df_train <- male_df[train_flag,] %>%
  filter(treatment == 0)

male_df_test <- male_df[-train_flag,]

## 売上が発生する確率を予測するモデルを作成
predict_model <- glm(data = male_df_train,
                     formula = conversion ~ recency + history_segment +
                       channel + zip_code,
                     family = binomial)

## 売上の発生確率からメールの配信確率を決める
pred_cv <- predict(predict_model,
                   newdata = male_df_test,
                   type = "response")
pred_cv_rank <- percent_rank(pred_cv)

## 配信確率を元にメールの配信を決める
mail_assign <- sapply(pred_cv_rank, rbinom, n = 1, size = 1)

## 配信ログを作成
ml_male_df <- male_df_test %>%
  mutate(mail_assign = mail_assign,
         ps = pred_cv_rank) %>%
  filter( (treatment == 1 & mail_assign == 1) |
            (treatment == 0 & mail_assign == 0) )

## 実験をしていた場合の平均の差を確認
rct_male_lm <- lm(data = male_df_test, formula = spend ~ treatment) %>%
  tidy()

## 平均の比較
ml_male_lm <- lm(data = ml_male_df, formula = spend ~ treatment) %>%
  tidy()

## 傾向スコアマッチングの推定(TPS)
library(Matching)
PSM_result <- Match(Y = ml_male_df$spend,
                    Tr = ml_male_df$treatment,
                    X = ml_male_df$ps,
                    estimand = "ATT")

## 推定結果の表示
summary(PSM_result)

## IPWの推定
W.out <- weightit(treatment ~ recency + history_segment +
                    channel + zip_code,
                  data = ml_male_df,
                  ps = ml_male_df$ps,
                  method = "ps",
                  estimand = "ATE")

## 重み付けしたデータでの共変量のバランスを確認
love.plot(W.out,
          threshold = .1)

## 重みづけしたデータでの効果の分析
IPW_result <- ml_male_df %>%
  lm(data = .,
     spend ~ treatment,
     weights = W.out$weights) %>%
  tidy()