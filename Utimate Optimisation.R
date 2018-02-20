library(bigrquery)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(openxlsx)

# Get the asset price and calculate the return
set_service_token("igenie-project-key.json")
project <- "igenie-project"
sql <- 'SELECT * FROM pecten_dataset.historical_closing_prices WHERE date BETWEEN "2018-02-01 00:00:00.000 UTC" AND "2018-02-06 00:00:00.000 UTC"'
price_data <- query_exec(project=project, sql, billing = project, use_legacy_sql = FALSE)
price_data <- price_data[order(price_data$date),]

  # initiating re-index
  price_data$date = as.Date(price_data$date)
  rownames(price_data) <- price_data$date
  price_data$date <- NULL

port_returns <- ROC(price_data, type="discrete")[-1,,drop = FALSE]
VaR.Mod <- VaR(port_returns, p=0.99, weights = NULL, portfolio_method = "single", method = "modified")

#1. Get the price data
DAX_return <- port_returns[,10]
constituent_returns <- port_returns[,-10]
constituent_names <- colnames(constituent_returns)

R <- constituent_returns

#2. Setup portfolio
portfolio_weight <- c(0.0315	,
                      0.0837	,
                      0.0815	,
                      0.0827	,
                      0.0088	,
                      0.0280	,
                      0.0126	,
                      0.0247	,
                      0.0694	,
                      0.0271	,
                      0.0181	,
                      0.0336	,
                      0.0438	,
                      0.0174	,
                      0.0182	,
                      0.0247	,
                      0.0123	,
                      0.0181	,
                      0.0252	,
                      0.0127	,
                      0.0317	,
                      0.0108	,
                      0.0268	,
                      0.0063	,
                      0.0080	,
                      0.0896	,
                      0.0921	,
                      0.0121	,
                      0.0167	,
                      0.0310	)

p <- portfolio.spec(assets = constituent_names,
                    weight_seq = portfolio_weight)

p <- add.constraint(portfolio = p, type = 'weight_sum',
                    min_sum = 0.99, max_sum = 1.01)

p <- add.objective(portfolio = p, type = 'return', name = 'mean',
                   multiplier = 0)
p <- add.objective(portfolio = p, type = 'risk', name = 'StdDev')

p <- add.constraint(portfolio = p, type = 'box',
                    min = 0.005, max = 0.09, indexnum = 2)

betas <- c(1.296311686	,
           0.827509888	,
           1.09702561	,
           1.112174945	,
           0.642727769	,
           0.941983278	,
           1.046960948	,
           0.934110656	,
           0.799040318	,
           0.576321942	,
           1.455087329	,
           0.973931843	,
           0.595884787	,
           0.825040726	,
           0.950891031	,
           0.836985241	,
           1.044805767	,
           0.79149074	,
           1.268980152	,
           0.6060441	,
           0.937533	,
           0.728974625	,
           0.774190332	,
           0.914578163	,
           0.981151838	,
           0.913554522	,
           1.228524177	,
           0.999240955	,
           0.420722678	,
           0.880181339	)

p <- add.constraint(portfolio = p, type="factor_exposure", B=betas,
                           lower=0, upper=0.5)



# Get an optimized portfolio, directional
opt <- optimize.portfolio(R, portfolio=p, optimize_method='random',
                          search_size=10000)

# Randomly generated 50,000 portfolio
rp <- random_portfolios(portfolio = p, permutations = 1000,
                        method = 'sample')
rp <- normalize.weights(rp)

opt.box <- optimize.portfolio(R = R, portfolio = p,
                              rp = rp, optimize_method = 'random',
                              trace = TRUE)

omitR <- na.omit(R)
opt.box.rebal <- optimize.portfolio.rebalancing(R = R, portfolio = p,
                                                optimize_method = 'random',
                                                rp = rp, trace = TRUE,
                                                rebalance_on = 'days')
                                                #training_period = 36,rolling_window = 36)

opt.box.rebal.r <- Return.portfolio(R, weights = extractWeights(opt.box.rebal))
colnames(opt.box.rebal.r) <- 'box'
