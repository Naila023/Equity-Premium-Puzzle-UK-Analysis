# Load necessary packages
library(readxl)
library(tidyverse)
library(broom)
library(lmtest)
library(sandwich)

# Read the dataset
df <- read_excel("C:/Users/murad/Downloads/USstocks.xlsx", sheet = "in")

# Ensure necessary columns are numeric
df <- df %>% 
  mutate(across(c(ri, rm, rf), as.numeric))

# Compute excess returns
df <- df %>%
  mutate(ri_excess = ri - rf,
         rm_excess = rm - rf)

## -----------------------------------------------------------------------------------------
# Estimate rolling beta (5-year window)
permno_pfid <- df %>% 
  filter(between(year, 1980, 1984)) %>% 
  group_by(permno) %>% 
  filter(n() >= 24) %>%  # Ensure at least 24 months of data
  nest() %>% 
  mutate(regression_estimates = map(data, ~ lm(ri_excess ~ rm_excess, data = .x)),
         tidyestimates = map(regression_estimates, tidy)) %>% 
  unnest(tidyestimates) %>% 
  filter(term == "rm_excess") %>% 
  ungroup() %>% 
  select(permno, beta = estimate) %>%   
  mutate(pfid = ntile(beta, 10))  # Create 10 decile portfolios based on beta ranking

# Compute portfolio returns for the year 1985
pfreturns_1985 <- df %>% 
  filter(year == 1985) %>% 
  left_join(permno_pfid, by = "permno") %>% 
  group_by(pfid, month) %>% 
  summarise(rp = mean(ri_excess, na.rm = TRUE), .groups = "drop")  # Use summarise instead of mutate

# Print the first few rows of results
print(head(pfreturns_1985))

## -----------------------------------------------------------------------------------------
# Function to estimate rolling beta (5-year window)
get_portfoliogroups <- function(mydf, yearstart, yearend) {
  
  result <- mydf %>%
    filter(between(year, yearstart, yearend)) %>%
    group_by(permno) %>%
    filter(n() >= 24) %>%  # Ensure at least 24 months of data
    nest() %>%
    mutate(estim = map(data, ~ lm(ri_excess ~ rm_excess, data = .x)),
           tidyestim = map(estim, tidy)) %>%
    unnest(tidyestim) %>%
    filter(term == "rm_excess") %>%
    ungroup() %>%
    select(permno, beta = estimate) %>%
    mutate(pfid = ntile(beta, 10))  # Create deciles based on beta
  return(result)  
}

## -----------------------------------------------------------------------------------------
# Function to assign firms to portfolios for the target year
construct_portfolio <- function(mydf, dfwithpfgroup, pfyear) {
  
  result <- mydf %>%
    filter(year == pfyear) %>%
    left_join(dfwithpfgroup, by = "permno")
  return(result)
}

## -----------------------------------------------------------------------------------------
# Initialize an empty dataframe to store results
df_temp <- data.frame()
for (i in 0:30) {
  # Compute rolling betas
  x <- get_portfoliogroups(df, 1980 + i, 1984 + i)
  
  # Assign firms to portfolios in year (1985 + i)
  y <- construct_portfolio(df, x, 1985 + i)
  
  # Gather results
  df_temp <- bind_rows(df_temp, y)
}

## -----------------------------------------------------------------------------------------
# Compute portfolio returns (arithmetic mean per month)
df_final <- df_temp %>%
  group_by(pfid, year, month) %>%
  summarise(rp = mean(ri_excess, na.rm = TRUE), .groups = "drop")

# Ensure market return is included in the dataset
df_final <- df_final %>%
  left_join(df %>% select(year, month, rm_excess) %>% distinct(), by = c("year", "month"))

## -----------------------------------------------------------------------------------------
# Time series regression for each portfolio
ts_test <- df_final %>%
  group_by(pfid) %>%
  nest() %>%
  mutate(estimates = map(data, ~ lm(rp ~ rm_excess, data = .x)),
         tidyestimates = map(estimates, tidy)) %>%
  unnest(tidyestimates) %>%
  select(-data, -estimates) %>%
  arrange(pfid) %>%
  ungroup()

print(ts_test, n=20)

## -----------------------------------------------------------------------------------------
# Compute average portfolio returns
rpmean <- df_final %>%
  group_by(pfid) %>%
  summarise(rpmean = mean(rp, na.rm = TRUE))

# Get beta estimates from time series regression
betap <- ts_test %>%
  select(pfid, term, estimate) %>%
  filter(term == "rm_excess") %>%
  select(pfid, betap = estimate)

# Merge portfolio returns with betas
csdf <- left_join(rpmean, betap, by = "pfid")

# Cross-sectional regression: average return ~ beta
cs_reg <- lm(rpmean ~ betap, data = csdf)
summary(cs_reg)

# Compute mean market excess return
mean_rm_excess <- mean(df$rm_excess, na.rm = TRUE)

# Standard error of mean market excess return
se_rm_excess <- sd(df$rm_excess, na.rm = TRUE) / sqrt(nrow(df))

# Extract lambda_1 estimate and standard error from cross-sectional regression
lambda1_hat <- coef(cs_reg)["betap"]
lambda1_se <- summary(cs_reg)$coefficients["betap", "Std. Error"]

# Compute t-statistic
lambda1_t_stat <- (lambda1_hat - mean_rm_excess) / sqrt(lambda1_se^2 + se_rm_excess^2)

# Compute p-value
lambda1_p_value <- 2 * (1 - pt(abs(lambda1_t_stat), df = nrow(csdf) - 1))

# Print results
cat("Mean Market Excess Return:", mean_rm_excess, "\n")
cat("Lambda_1 Estimate:", lambda1_hat, "\n")
cat("T-Statistic:", lambda1_t_stat, "\n")
cat("P-Value:", lambda1_p_value, "\n")

## -----------------------------------------------------------------------------------------
# Heteroskedasticity-robust t-test
coeftest(cs_reg, vcov = vcovHC(cs_reg, type = "HC0"))

## -----------------------------------------------------------------------------------------
# Plot estimated beta vs annualized portfolio return
csdf %>%
  ggplot(aes(betap, rpmean * 1200)) +  # Convert to annualized return
  geom_point(shape = 24, fill = "black", size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Average Annualized Monthly Return vs. CAPM Beta",
       x = "Systematic Risk",
       y = "Average Annualized Monthly Excess Return (%)")

## -----------------------------------------------------------------------------------------
# Check for potential outliers
ggplot(csdf, aes(x = rpmean)) + geom_histogram(binwidth = 0.01, fill = "blue", color = "black") + labs(title = "Return Distribution")

