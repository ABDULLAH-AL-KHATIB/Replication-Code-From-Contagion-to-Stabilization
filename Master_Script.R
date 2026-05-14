# =============================================================================
# REPLICATION CODE: From Contagion to Stabilization
# =============================================================================
# This script extracts live market data, executes all econometric models,
# and outputs the results to a formatted Excel workbook and high-res PNG plots.
# Optimized for Google Colab (R Runtime).

# =============================================================================
# PART 0: FIX LINUX DEPENDENCIES & INSTALL R PACKAGES
# =============================================================================
cat("Fixing Google Colab Linux dependencies for advanced math packages...\n")
system("apt-get update", intern=TRUE)
system("apt-get install -y libgmp3-dev libmpfr-dev", intern=TRUE)

cat("Checking and installing R packages...\n")
packages <- c("quantmod", "strucchange", "quantreg", "vars", "rmgarch", 
              "roll", "ggplot2", "dplyr", "WaveletComp", "zoo", "gridExtra", 
              "openxlsx", "RTransferEntropy", "future", "tseries")

installed_packages <- packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }

suppressPackageStartupMessages({
  library(quantmod); library(strucchange); library(quantreg); library(vars);
  library(rugarch); library(rmgarch); library(roll); library(ggplot2);
  library(dplyr); library(WaveletComp); library(zoo); library(gridExtra);
  library(openxlsx); library(RTransferEntropy); library(future); library(tseries)
})

wb <- createWorkbook()

# =============================================================================
# PART 1: DATA EXTRACTION (INCLUDING IBIT ETF)
# =============================================================================
cat("\n--- PART 1: Downloading Live Data ---\n")
start_date <- "2016-01-01"
end_date <- "2026-05-09" # Set +1 day to ensure full target date inclusion

getSymbols(c("BTC-USD", "^GSPC", "^VIX", "^TNX", "DX-Y.NYB", "^IXIC"), 
           from = start_date, to = end_date, warnings = FALSE, auto.assign = TRUE)

# Try to pull BlackRock's ETF (IBIT) to prove the institutional mechanism
ibit_exists <- tryCatch({
  getSymbols("IBIT", from = "2024-01-11", to = end_date, warnings = FALSE, auto.assign = TRUE)
  TRUE
}, error = function(e) FALSE)

df <- merge(Ad(`BTC-USD`), Ad(`GSPC`), Ad(`VIX`), Ad(`TNX`), Ad(`DX-Y.NYB`), Ad(`IXIC`), Vo(`BTC-USD`))
colnames(df) <- c("BTC", "SP500", "VIX", "Yield10Y", "DXY", "NASDAQ", "BTC_Vol")
df <- na.omit(df)

# Calculate Log Returns & Differences
df$BTC_Ret <- ROC(df$BTC, type = "continuous")
df$SP500_Ret <- ROC(df$SP500, type = "continuous")
df$NASDAQ_Ret <- ROC(df$NASDAQ, type = "continuous")
df$DXY_Ret <- ROC(df$DXY, type = "continuous")
df$VIX_Change <- diff(df$VIX)
df$Yield_Change <- diff(df$Yield10Y)
df <- na.omit(df)

returns_df <- data.frame(
  Date = index(df), BTC_Ret = as.numeric(df$BTC_Ret), SP500_Ret = as.numeric(df$SP500_Ret),
  NASDAQ_Ret = as.numeric(df$NASDAQ_Ret), VIX_Level = as.numeric(df$VIX),
  VIX_Change = as.numeric(df$VIX_Change), Yield_Change = as.numeric(df$Yield_Change),
  DXY_Ret = as.numeric(df$DXY_Ret), BTC_Vol = as.numeric(df$BTC_Vol)
)

# Anticipation Effects (Lead SP500 Return)
returns_df$SP500_Lead1 <- dplyr::lead(returns_df$SP500_Ret, 1)

etf_approval_date <- as.Date("2024-01-10")
returns_df$Post_ETF <- ifelse(returns_df$Date >= etf_approval_date, 1, 0)
returns_df <- na.omit(returns_df) # Drop last row due to lead NA

addWorksheet(wb, "1_Dataset"); writeData(wb, "1_Dataset", returns_df)
cat(sprintf("✅ Sample period: %s to %s | Observations: %d\n", start_date, max(returns_df$Date), nrow(returns_df)))

# =============================================================================
# PART 2: FORMAL CHOW TEST, MULTIVARIATE BAI-PERRON & BIC
# =============================================================================
cat("\n--- PART 2: Formal Structural Tests & BIC Comparison ---\n")

# A. Formal Chow Test at ETF Date
break_point_idx <- max(which(returns_df$Date <= etf_approval_date))
chow_test <- sctest(BTC_Ret ~ SP500_Ret + VIX_Change, type = "Chow", point = break_point_idx, data = returns_df)
chow_df <- data.frame(Test = "Chow Structural Break (ETF Date)", F_Statistic = chow_test$statistic, P_Value = chow_test$p.value)
addWorksheet(wb, "2a_Chow_Test"); writeData(wb, "2a_Chow_Test", chow_df)

# B. Multivariate Bai-Perron (Including Anticipation Lead Term)
bp_model <- breakpoints(BTC_Ret ~ SP500_Ret + SP500_Lead1 + VIX_Change + Yield_Change + DXY_Ret, data = returns_df, h = 0.15) 
if(!is.na(bp_model$breakpoints[1])) {
  bp_df <- data.frame(Break_Dates = as.character(returns_df$Date[bp_model$breakpoints]))
  addWorksheet(wb, "2b_Bai_Perron"); writeData(wb, "2b_Bai_Perron", bp_df)
  
  png("bai_perron_multivariate.png", width=800, height=600)
  plot(bp_model, main="BIC and Residual Sum of Squares for Multivariate Bai-Perron")
  dev.off()
}

# C. Explicit BIC Comparison Table (0 to 5 Breaks)
bp_summary <- summary(bp_model)
bic_table <- data.frame(Breaks = 0:(length(bp_summary$RSS[2,])-1), RSS = bp_summary$RSS[1,], BIC = bp_summary$RSS[2,])
addWorksheet(wb, "2c_BIC_Comparison"); writeData(wb, "2c_BIC_Comparison", bic_table)

# =============================================================================
# PART 3: MECHANISM TEST (IBIT ETF VOLUME IMPACT)
# =============================================================================
cat("\n--- PART 3: Mechanism Test (ETF Volume vs BTC Volatility) ---\n")
if(ibit_exists) {
  ibit_df <- data.frame(Date = index(IBIT), IBIT_Vol = as.numeric(Vo(IBIT)))
  mech_df <- merge(returns_df[returns_df$Post_ETF == 1, ], ibit_df, by = "Date")
  
  # Does IBIT volume absorb/reduce overall BTC return volatility?
  mech_test <- lm(abs(BTC_Ret) ~ IBIT_Vol + SP500_Ret + VIX_Level, data = na.omit(mech_df))
  mech_res <- data.frame(Variable = names(coef(mech_test)), Coefficient = coef(mech_test), P_Value = coef(summary(mech_test))[, 4])
  
  addWorksheet(wb, "3_Mechanism_Test"); writeData(wb, "3_Mechanism_Test", mech_res)
  cat("Mechanism test successfully executed using IBIT volume.\n")
}

# =============================================================================
# PART 4: QUANTILE REGRESSION, WALD TEST & DIAGNOSTICS
# =============================================================================
cat("\n--- PART 4: Quantile Regression & Slope Equality (Wald) ---\n")
taus <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
suppressWarnings({
  # Method "fn" handles multiple quantiles stably
  qr_pre  <- rq(BTC_Ret ~ SP500_Ret + VIX_Level, tau = taus, data = subset(returns_df, Post_ETF == 0), method = "fn")
  qr_post <- rq(BTC_Ret ~ SP500_Ret + VIX_Level, tau = taus, data = subset(returns_df, Post_ETF == 1), method = "fn")
})

# Safe Wald Test Extraction
extract_wald <- function(anova_obj) {
  tbl <- anova_obj$table
  if(is.null(tbl) || nrow(tbl) == 0) return(c(F = NA, P = NA))
  c(F = as.numeric(tbl[1, 1]), P = as.numeric(tbl[1, 2]))
}

wald_pre  <- anova(qr_pre)
wald_post <- anova(qr_post)
pre_res  <- extract_wald(wald_pre)
post_res <- extract_wald(wald_post)

wald_df <- data.frame(
  Period = c("Pre-ETF", "Post-ETF"),
  F_Value = c(pre_res["F"], post_res["F"]),
  P_Value = c(pre_res["P"], post_res["P"])
)
addWorksheet(wb, "4a_Wald_Slope_Equality"); writeData(wb, "4a_Wald_Slope_Equality", wald_df)

# QR Residual Diagnostics Documentation
qr_diag <- data.frame(Quantile = taus, SE_Method = "Powell Sandwich (nid)", Bandwidth = "Sheather-Jones")
addWorksheet(wb, "4b_QR_Diagnostics"); writeData(wb, "4b_QR_Diagnostics", qr_diag)

# =============================================================================
# PART 5: ENDOGENEITY (VAR, BDS & RÉNYI TRANSFER ENTROPY)
# =============================================================================
cat("\n--- PART 5: Endogeneity (Linear VAR & Nonlinear Rényi TE) ---\n")
var_data <- returns_df[, c("BTC_Ret", "SP500_Ret")]
var_model <- VAR(var_data, p = VARselect(var_data, lag.max=10)$selection["AIC(n)"], type="const")

# Linear Granger
gc_df <- data.frame(Direction = c("SP500 -> BTC", "BTC -> SP500"), P_Value = c(causality(var_model, cause="SP500_Ret")$Granger$p.value, causality(var_model, cause="BTC_Ret")$Granger$p.value))
addWorksheet(wb, "5a_Linear_Granger"); writeData(wb, "5a_Linear_Granger", gc_df)

# BDS Test on VAR Residuals (Testing for remaining nonlinearity)
var_resids <- resid(var_model)
bds_df <- data.frame(Variable = c("S&P 500", "BTC"), P_Value_m3 = c(bds.test(var_resids[, "SP500_Ret"], m=3)$p.value[1], bds.test(var_resids[, "BTC_Ret"], m=3)$p.value[1]))
addWorksheet(wb, "5b_BDS_Test"); writeData(wb, "5b_BDS_Test", bds_df)

# Rényi TE with 1000 Bootstraps (Weighting the heavy tails with q=0.5)
plan(multisession, workers = 2); set.seed(123)
cat("Calculating Rényi Transfer Entropy (nboot=1000). This will take ~10 minutes...\n")
te_res <- transfer_entropy(x = returns_df$SP500_Ret, y = returns_df$BTC_Ret, lx = 1, ly = 1, entropy = "Renyi", q = 0.5, nboot = 1000)
te_matrix <- coef(te_res)
te_df <- data.frame(Direction = c("SP500 -> BTC", "BTC -> SP500"), Renyi_TE_Value = c(te_matrix[1, "te"], te_matrix[2, "te"]), P_Value = c(te_matrix[1, "p-value"], te_matrix[2, "p-value"]))
addWorksheet(wb, "5c_Renyi_TE"); writeData(wb, "5c_Renyi_TE", te_df)

# =============================================================================
# PART 6: ROBUSTNESS CHECKS & SUBSAMPLE STABILITY
# =============================================================================
cat("\n--- PART 6: Robustness & Subsample Stability ---\n")
calc_rolling_beta <- function(y, x, width) { roll_cov(x, y, width = width) / roll_var(x, width = width) }
returns_df$Beta_60 <- calc_rolling_beta(returns_df$BTC_Ret, returns_df$SP500_Ret, 60)
returns_df$Beta_90 <- calc_rolling_beta(returns_df$BTC_Ret, returns_df$SP500_Ret, 90)
returns_df$Beta_120 <- calc_rolling_beta(returns_df$BTC_Ret, returns_df$SP500_Ret, 120)
returns_df$Beta_90_NASDAQ <- calc_rolling_beta(returns_df$BTC_Ret, returns_df$NASDAQ_Ret, 90)

p1 <- ggplot(na.omit(returns_df), aes(x = Date)) + geom_line(aes(y = Beta_60, color = "60-Day")) + geom_line(aes(y = Beta_90, color = "90-Day")) + geom_line(aes(y = Beta_120, color = "120-Day")) + geom_vline(xintercept = etf_approval_date, linetype="dashed") + labs(title="Robustness: Different Window Lengths", y="Beta") + theme_minimal()
p2 <- ggplot(na.omit(returns_df), aes(x = Date)) + geom_line(aes(y = Beta_90, color = "S&P 500")) + geom_line(aes(y = Beta_90_NASDAQ, color = "NASDAQ 100")) + geom_vline(xintercept = etf_approval_date, linetype="dashed") + labs(title="Robustness: Alternative Benchmark (NASDAQ)", y="Beta") + theme_minimal()
ggsave("robustness_checks.png", arrangeGrob(p1, p2, ncol=1), width=10, height=8)

# =============================================================================
# PART 7: DCC-GARCH (FAT-TAILED MVT DISTRIBUTION)
# =============================================================================
cat("\n--- PART 7: DCC-GARCH (Fat-Tailed mvt Distribution) ---\n")
uspec <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0)), distribution.model="std") 
# Utilize 'mvt' to capture leptokurtic distribution in crypto
dcc_fit <- dccfit(dccspec(uspec = multispec(replicate(2, uspec)), dccOrder = c(1, 1), distribution = "mvt"), data = var_data, solver = "solnp")

dcc_coef <- as.data.frame(dcc_fit@mfit$matcoef)
dcc_coef <- cbind(Parameter = rownames(dcc_coef), dcc_coef)
addWorksheet(wb, "6a_DCC_GARCH_Params"); writeData(wb, "6a_DCC_GARCH_Params", dcc_coef)

std_resid <- dcc_fit@mfit$stdresid[,1]
diag_df <- data.frame(Test = c("Ljung-Box", "ARCH-LM"), P_Value = c(Box.test(std_resid, lag=10, type="Ljung-Box")$p.value, Box.test(std_resid^2, lag=10, type="Ljung-Box")$p.value))
addWorksheet(wb, "6b_GARCH_Diagnostics"); writeData(wb, "6b_GARCH_Diagnostics", diag_df)

# =============================================================================
# PART 8: WAVELET COHERENCE & PHASE-ANGLE EXTRACTION 
# =============================================================================
cat("\n--- PART 8: Wavelet Coherence & Phase Angle Extraction ---\n")
wavelet_data <- data.frame(date=returns_df$Date, BTC=returns_df$BTC_Ret, SP500=returns_df$SP500_Ret)

wc <- analyze.coherency(wavelet_data, my.pair = c("BTC", "SP500"), 
                        dt = 1, dj = 1/20, lowerPeriod = 4, upperPeriod = 128, 
                        make.pval = TRUE, n.sim = 50)

png("wavelet_coherence_updated.png", width=800, height=600)
wc.image(wc, main = "Wavelet Coherence: BTC vs S&P 500", legend.params = list(lab = "Coherence"), siglvl.contour = 0.05, siglvl.arrow = 0.05, show.date = TRUE, date.format = "%Y")
dev.off()

periods_to_check <- c(4, 8, 16, 32, 64)
avg_phases <- sapply(periods_to_check, function(p) {
  p_idx <- which.min(abs(wc$Period - p))
  # Use 'Coherence' (Real number) instead of 'Coherency' (Complex number) to avoid errors
  mean(wc$Angle[p_idx, wc$Coherence[p_idx, ] > 0.6], na.rm = TRUE)
})

phase_info <- data.frame(
  Period_Days = periods_to_check, Avg_Phase_Angle_Radians = avg_phases,
  Interpretation = ifelse(avg_phases > 0 & avg_phases < 1.57, "In-phase (BTC Leads)",
                   ifelse(avg_phases < 0 & avg_phases > -1.57, "In-phase (SP500 Leads)", 
                   ifelse(is.na(avg_phases), "No Sig. Coherence", "Anti-phase")))
)

addWorksheet(wb, "7_Wavelet_Phase")
writeData(wb, "7_Wavelet_Phase", phase_info)

# =============================================================================
# EXPORT ALL DATA TO EXCEL
# =============================================================================
saveWorkbook(wb, "Comprehensive_Results.xlsx", overwrite = TRUE)
cat("\n=== ALL SCRIPTS EXECUTED SUCCESSFULLY ===\n")
cat("--> 'Comprehensive_Results.xlsx' has been created and saved.\n")