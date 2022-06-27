if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, xts, lubridate, PerformanceAnalytics, parallel, furrr, readxl, writexl, xlsx)#*xlsx

# Import functions needed for Hirarquical Risk Parity
source("99_functions.R")

##################################################################
##                      Balance Sheet Data                      ##
##################################################################

## ... and current market captalization

bal_sheet <- read_excel('StatsOrd.xlsx')

for (i in seq_along(bal_sheet)) {
  if(!is.na(bal_sheet[3, i])){
    bal_sheet[2, i] <- bal_sheet[3, i]
  }
}

bal_sheet <- as.data.frame(t(bal_sheet[-3, -1]))

bal_sheet <- bal_sheet %>%
  mutate(Assets = rownames(bal_sheet), .before = 1) %>%
  set_names('assets', 'indic', 'value') %>%
  mutate(assets = word(assets, 1)) %>%
  pivot_wider(names_from = indic, values_from = value) %>%
  set_names('assets', 'size', 'value', 'profit')


##################################################################
##                          Price Data                          ##
##################################################################

price <- read_excel('StatsOrd.xlsx', sheet = 2) 

price <- price %>%
  slice(-1) %>%
  set_names(c('Date', word(colnames(price)[-1], 1))) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  arrange(Date)

# Import the IBX Index returns
price_ind <- read_excel('StatsOrd.xlsx', sheet = 3)

price_ind <- price_ind %>%
  slice(-1) %>%
  set_names('Date', 'IBX') %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  arrange(Date) %>%
  dplyr::filter(Date >= '2021-05-27') 

price <- merge(price, price_ind[, 1, drop = FALSE], by = 'Date', all.y = TRUE)

price[, -1] <- apply(price[, -1], 2, as.numeric)

price <- xts(price[,-1], price$Date)

price <- na.locf(price)

price <- data.frame(Date = as.Date(index(price)), price) %>%
  select_if(~ !any(is.na(.)))

return <- apply(price[,-1], 2, function(x) diff(x)/x[-length(x)])
price_stat <- apply(return, 2, function(x) c(prod(1 + x) - 1, sd(x)))

return <- data.frame(Date = price$Date[-1], return)

price_stat <- as.data.frame(t(price_stat)) %>%
  set_names('mom', 'vol') %>%
  mutate(assets = colnames(price_stat), .before = 1)

##################################################################
##                     Merge the dataframes                     ##
##################################################################

df_indic <- merge(price_stat, bal_sheet, by = 'assets', all.x = TRUE)

df_indic[, -1] <- apply(df_indic[, -1], 2, as.numeric)

df_indic[, -1] <- apply(df_indic[, -1], 2, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))

df_indic[, c(3, 4, 5)] <- df_indic[, c(3, 4, 5)] * (-1)

df_indic <- as.data.frame(apply(df_indic[, -1], 1, function(x) mean(x, na.rm  = TRUE)), row.names = df_indic$assets) %>%
  set_names('statistic') %>%
  arrange(desc(statistic)) %>%
  drop_na()

port_assets <- rownames(df_indic[1:15, , drop = FALSE])

#################################################################
##                             ESG                             ##
#################################################################

#### If you want to also add ESG, uncomment ####

#df_indic <- df_indic %>%
  mutate(assets = rownames(df_indic), .before = 1)

#esg <- read_excel('ESG_data.xlsx', sheet = 2, na = "#N/A") %>%
  set_names('assets', 'esg') %>%
  mutate(assets = word(assets, 1))

#df_indic <- merge(df_indic, esg, by = 'assets', all.x = TRUE) %>%
  arrange(desc(statistic)) %>%
  head(30) %>%
  arrange(esg) %>%
  head(15)

#port_assets <- df_indic$assets

##################################################################
##               Hierarquical Risk Parity Weights               ##
##################################################################

port_assets_ret <- return %>%
  dplyr::filter(Date >= '2021-05-27') %>%
  dplyr::select(all_of(port_assets))

weights <- hierarc_risk_opt(port_assets_ret)

final_df <- data.frame(Assets = colnames(port_assets_ret), Weigths = round(weights*100, 2))

verify_price <- price %>%
  dplyr::filter(Date == '2022-05-20') %>%
  dplyr::select(!Date)

verify_price <- as.data.frame(t(verify_price)) 

verify_price <- verify_price%>%
  mutate(assets = rownames(verify_price), .before = 1) %>%
  set_names('Assets', 'Price')

final_df <- merge(final_df, verify_price, by = 'Assets', all.x = TRUE)
