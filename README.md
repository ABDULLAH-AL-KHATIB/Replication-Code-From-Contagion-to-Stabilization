# Replication-Code-From-Contagion-to-Stabilization
Replication Code: From Contagion to Stabilization
Paper Title: From Contagion to Stabilization: Spot Bitcoin ETFs and the Regime Shift in Crypto-Equity Integration
This repository contains the complete, reproducible R code used in the econometric analysis of the integration between Bitcoin and traditional equity markets. The script automatically fetches live market data, executes all statistical models, and exports the results into a formatted Excel workbook and high-resolution plots.
📊 Methodologies Included in this Script:
Multivariate Bai-Perron Structural Break Test (Controlling for VIX, 10Y Yields, DXY)
Quantile Regression & Wald Slope Equality Tests (τ=0.05 to 0.95)
Institutional Mechanism Testing (using BlackRock IBIT ETF volume) Linear Vector Autoregression (VAR) & Granger Causality
Nonlinear Rényi Transfer Entropy (1,000 bootstraps, q=0.5)
Fat-Tailed DCC-GARCH (Multivariate Student-t Distribution)
Wavelet Coherence & Phase-Angle Extraction
Rolling Beta Robustness Checks (60, 90, 120-day windows and NASDAQ benchmark)
⚙️ Environment Setup & Prerequisites
This code is optimized to run effortlessly in Google Colab using the R runtime.
Because advanced financial packages (rmgarch, RTransferEntropy) require heavy C++ math libraries that are not installed by default on basic Linux environments, the script includes automated system commands (apt-get) to install the required dependencies (libgmp3-dev, libmpfr-dev).
Required R Packages:
quantmod, strucchange, quantreg, vars, rmgarch, roll, ggplot2, dplyr, WaveletComp, zoo, gridExtra, openxlsx, RTransferEntropy, future, tseries
🚀 How to Run the Code
Open Google Colab.
Go to Runtime > Change runtime type and select R.
Copy the entire contents of Master_Script.R into a new code cell.
Click Run.
Note: Because the Rényi Transfer Entropy utilizes 1,000 bootstraps for mathematical rigor, the script will take approximately 10–15 minutes to finish processing. Please let it run uninterrupted.
📂 Outputs Generated
Once the script finishes executing, it will generate the following files which can be downloaded directly from the Colab file explorer:
Comprehensive_Results.xlsx (Contains 9 sheets with all model parameters, p-values, Wald tests, and diagnostics).
bai_perron_multivariate.png (BIC and RSS structural break plots).
robustness_checks.png (Rolling window and benchmark sensitivity plots).
wavelet_coherence_updated.png (Time-frequency coherence heat map).
📄 Data Sources
Data is pulled dynamically using the quantmod API from:
Yahoo Finance: BTC-USD, ^GSPC (S&P 500), ^IXIC (NASDAQ), IBIT (BlackRock ETF)
FRED / CBOE: ^VIX (Volatility Index), ^TNX (10-Year Treasury Yield), DX-Y.NYB (U.S. Dollar Index)
📝 License & Citation
This code is open-source. If you use this script in your research, please cite the original paper and the Zenodo archive.
