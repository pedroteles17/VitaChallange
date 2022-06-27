############################################################################
############################################################################
###                                                                      ###
###                              SECTION 0:                              ###
###                             INTRODUCTION                             ###
###                                                                      ###
############################################################################
############################################################################

# code author: Pedro Teles (pteles@avantgardeam.com.br)

"%ni%" <- Negate("%in%")

plan(multisession, workers = parallel::detectCores())

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 1:                             ###
###                             IMPORT DATA                             ###
###                                                                     ###
###########################################################################
###########################################################################

# Function to import the table with the financial indicators
base_indic <- function(path2file){
  indic_import <- read_csv(path2file)
  # Transform date to date and the indicators to numeric
  indic <- data.frame(as.Date(indic_import$Date), apply(indic_import[,-1], 2, as.numeric))
  colnames(indic) <- colnames(indic_import)
  
  return(indic)
}

# To export our xts data we need to transform it to a data frame object
xts2df <- function(xts_object){
  df <- data.frame(Date = index(xts_object), xts_object)
}

# Import functions needed for running the backtests
source("99_functions.R")

# Where are the cleaned data stored?
path2file <- paste(getwd(), "brazil", sep = "/")

# Import index composition
## We need to eliminate "RIPI4 BS Equity" because of a data quality issue:
## On the day 2004-06-03 the price jumps from R$0.0078 to R$7.78
## We don't have price data for "PRGA4 BS Equity"
comp <- read_csv(paste(path2file, "comp.csv", sep = "\\"), col_types = paste0("c", paste(rep("n", 241), collapse = ""), collapse = ""))  %>% 
  dplyr::filter(Assets %ni% c("RIPI4 BS Equity", "PRGA4 BS Equity", "AELP3 BV Equity"))

# Import the IBX Index returns
ret_ind <- read_csv(paste(path2file, "index_returns.csv", sep = "\\"), col_types = "Dn")

# Import the risk free return (CDI)
ret_risk_free <- read_csv(paste(path2file, "risk_free_returns.csv", sep = "\\"), col_types = "Dn")

# Import the assets returns
ret_assets <- read_csv(paste(path2file, "asset_returns.csv", sep = "\\"), col_types = paste0("D", paste(rep("n", 304), collapse = ""), collapse = ""))

# Get the backtests dates. This will be useful for seeing how many times 
## we need to rebalance the portfolios 
backtest_dates <- get_backtest_dates(20021231, 20211231, c(1), c(12))

############################################################################
############################################################################
###                                                                      ###
###                              SECTION 2:                              ###
###                 GET THE ORDERED FINANCIAL INDICATORS                 ###
###                                                                      ###
############################################################################
############################################################################

# Low Volatility
ord_indic_vol <- future_map(1:nrow(backtest_dates), get_ord_indic,
                     start_date = 20021231, end_date = 20211231,
                     hold_period = 1, estim_period = c(12),
                     fin_indic = ret_assets, comp_matrix = comp,
                     factor_name = 'Low Volatility', order = 'ascending', 
                     exist_restr = 12, assets_port_returns = ret_assets)

# Momentum
ord_indic_mom <- future_map(1:nrow(backtest_dates), get_ord_indic,
                     start_date = 20021231, end_date = 20211231,
                     hold_period = 1, estim_period = c(6),
                     fin_indic = ret_assets, comp_matrix = comp,
                     factor_name = 'Momentum', order = 'descending', 
                     exist_restr = 12, assets_port_returns = ret_assets)

# Value
value <- base_indic(paste0(path2file, '\\value.csv'))

ord_indic_value <- future_map(1:nrow(backtest_dates), get_ord_indic,
                       start_date = 20021231, end_date = 20211231,
                       hold_period = 1, estim_period = c(0), # estim_period won't be used
                       fin_indic = value, comp_matrix = comp,
                       factor_name = 'Value', order = 'ascending', 
                       exist_restr = 12, assets_port_returns = ret_assets)

# Size
size <- base_indic(paste0(path2file, '\\size.csv'))
## We make sure that the dates correspond to the last day of a month
size$Date <- as.Date(as.Date(size$Date[1]) %m+% months(0:(nrow(size) - 1)))

## We don't have market cap information for two assets.
### We drop them for the size factor.
diff_size_comp <- setdiff(comp$Assets, colnames(size))
comp_size <- comp %>% dplyr::filter(Assets %ni% diff_size_comp)

ord_indic_size <- future_map(1:nrow(backtest_dates), get_ord_indic,
                      start_date = 20021231, end_date = 20211231,
                      hold_period = 1, estim_period = c(0),
                      fin_indic = size, comp_matrix = comp_size,
                      factor_name = 'Size', order = 'ascending', 
                      exist_restr = 12, assets_port_returns = ret_assets)

# profitability
profit <- base_indic(paste0(path2file, '\\profitability.csv'))

ord_indic_profit <- future_map(1:nrow(backtest_dates), get_ord_indic,
                        start_date = 20021231, end_date = 20211231,
                        hold_period = 1, estim_period = c(0),
                        fin_indic = profit, comp_matrix = comp,
                        factor_name = 'Profitability', order = 'descending', 
                        exist_restr = 12, assets_port_returns = ret_assets)

rm(value, size, diff_size_comp, comp_size, profit)

merge.all <- function(x, ...) {
  L <- list(...)
  for (i in seq_along(L)) {
    x <- merge(x, L[[i]], by = "row.names", all = TRUE)
    rownames(x) <- x$Row.names
    x$Row.names <- NULL
  }
  return(x)
}

# Agraggate ordered financial indicators
final_ord <- vector('list', length = length(ord_indic_vol))
for (i in seq_along(ord_indic_vol)) {
  indic_df <- merge.all(ord_indic_vol[[i]], ord_indic_mom[[i]], ord_indic_value[[i]], ord_indic_size[[i]], ord_indic_profit[[i]])
  colnames(indic_df) <- c('vol', 'mom', 'value', 'size', 'profit')
  nomes_ativos <- rownames(indic_df)
  
  indic_df <- as.data.frame(lapply(indic_df, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)), row.names = nomes_ativos)
  
  indic_df[, c(1, 3, 4)] <- indic_df[, c(1, 3, 4)] * (-1)
  
  final_ord[[i]] <- data.frame(indic_df, apply(indic_df, 1, function(x) mean(x, na.rm  = TRUE)), row.names = nomes_ativos) %>%
    set_names(c(colnames(indic_df), 'multiple'))
}

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 2:                             ###
###                          RUN THE BACKTESTS                          ###
###                                                                     ###
###########################################################################
###########################################################################

ret_rf <- ret_risk_free %>%
  dplyr::filter(Date > '2002-12-31' & Date <= '2021-12-31') %>%
  set_names('Date', 'rf')
ret_rf <- xts(ret_rf[,-1], ret_rf$Date)

ret_ind <- ret_ind %>%
  dplyr::filter(Date > '2002-12-31' & Date <= '2021-12-31') %>%
  set_names('Date', 'index')
ret_ind <- xts(ret_ind[,-1], ret_ind$Date)

run_backtest_multiple <- function(i){
  
  fator <- hyperparameters[i, 1]
  hold_period <- hyperparameters[i, 2]
  percent <- hyperparameters[i, 3]
  weight_type <- hyperparameters[i, 4]
  
  final_ord_hp <- final_ord[seq(1, length(final_ord), by = hold_period)]
  
  final_ord_hp <- lapply(final_ord_hp, function(x) x %>%
                           dplyr::select(all_of(fator)) %>%
                           set_names('statistic') %>%
                           arrange(desc(statistic)) %>%
                           drop_na())
  
  backtest <- run_backtest(ret_assets, final_ord_hp, 20021231, 20211231, percent, weight_type)
  
  backtest[, 2] <- backtest[, 2] + ret_rf
  
  base_name <- paste(fator, hold_period, percent, weight_type, sep = '_')
  colnames(backtest) <- paste(base_name, c('lo', 'ls'), sep =  '_')
  
  return(backtest)
} 

opt_hold_period <- c(1, 3, 6, 12)
opt_fator <- c('vol', 'mom', 'value', 'size', 'profit', 'multiple')
opt_percent <- c(0.3)
opt_weight_type <- c('ew', 'hrp')

hyperparameters <- expand.grid(opt_fator, opt_hold_period,
                               opt_percent, opt_weight_type)

backtest_hyper <- future_map(1:nrow(hyperparameters), run_backtest_multiple)

backtest_hyper <- do.call('cbind.xts', backtest_hyper)

index_rf <- cbind.xts(ret_ind, ret_rf)

###########################################################################
###########################################################################
###                                                                     ###
###                             EXPORT DATA                             ###
###                                                                     ###
###########################################################################
###########################################################################

saveRDS(xts2df(backtest_hyper), file = 'all_hyper_ports.rds')

saveRDS(xts2df(index_rf), file = 'index_rf.rds')

##################################################################
##                  Tranform Funds Data to RDS                  ##
##################################################################

funds <- read_excel('fundos_vita_raw.xlsx') %>%
  dplyr::select(!c("Codigo", "Variacao", "Cotistas")) %>%
  mutate(capt_liq = Captacao - Resgate) %>%
  dplyr::filter(Data >= '2003-01-02')

for (i in 1:nrow(funds)) {
  if(funds[i, , drop = FALSE]$Data > '2021-12-31'){
    funds[i, , drop = FALSE]$Cota <- NA
  }
}

saveRDS(funds, file = 'fundos.rds')

