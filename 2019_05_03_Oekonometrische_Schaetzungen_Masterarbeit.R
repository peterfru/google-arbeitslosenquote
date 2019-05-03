# R-Code der Masterarbeit

# Installation von Packages
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("tidyverse")
usePackage("ggplot2")
usePackage("scales")
usePackage("gridExtra")
usePackage("Metrics")
usePackage("dplyr")
usePackage("mlr")
usePackage("tidyverse")
usePackage("gtrendsR")
usePackage("prophet")
usePackage("forecast")
usePackage("colortools")
usePackage("tidyverse")
usePackage("zoo")
usePackage("pastecs")
usePackage("tseries")
usePackage("dplyr")
usePackage("sandwich")
usePackage("lmtest")
usePackage("dynlm")
usePackage("jtools")
usePackage("car")
usePackage("installr")
usePackage("devtools")
usePackage("gets")
usePackage("ggseas")
usePackage("MLmetrics")

# Directory setzen

### Datenimport ###

# Import Arbeitslosendaten
Arbeitslosenquote <- read.csv2('2019_01_04_Arbeitslosenquote.csv')

# Import Google-Variable
Google_Variable <- read.csv2("2019_01_04_Google_Daten.csv")


# Spaltentypen umformen

# Arbeitslosenquote
Arbeitslosenquote$Monat <- as.yearmon(as.character(Arbeitslosenquote$Monat), "%Y-%m") 
Arbeitslosenquote$Monat <- as.Date(Arbeitslosenquote$Monat)
Arbeitslosenquote$Arbeitslosenquote <- as.numeric(Arbeitslosenquote$Arbeitslosenquote)

# Google-Variable
Google_Variable$Monat <- as.yearmon(as.character(Google_Variable$Monat), "%Y-%m") 
Google_Variable$Monat <- as.Date(Google_Variable$Monat)
Google_Variable <- Google_Variable %>% mutate(Normalisierte_Werte=as.numeric(Normalisierte.Werte)) %>% 
                                       mutate(Normalisierte.Werte=NULL)


### Deskripitve Analyse ###

# Arbeitslosendaten Deskriptive Analyse
Arbeitslosendaten_Summary <- summary(Arbeitslosenquote$Arbeitslosenquote)
Arbeitslosendaten_Summary
stat.desc(Arbeitslosenquote$Arbeitslosenquote, basic=F)

# Google-Variable Deskriptive Analyse
Google_Variable_Summary <- summary(Google_Variable$Normalisierte_Werte)
Google_Variable_Summary
stat.desc(Google_Variable$Normalisierte_Werte, basic=F)


### Zeitreihenmodelle ###

# Umwandlung des Arbeitslosenobjekts in ein Zeitreihenobjekt
Arbeitslosenquote_ts <- ts(Arbeitslosenquote$Arbeitslosenquote, frequency=12, start=c(2009, 1))

# Umwandlung des Google_Variable-Objekts in ein Zeitreihenobjekt
Google_Variable_ts <- ts(Google_Variable$Normalisierte_Werte, frequency=12, start=c(2009, 1))

# Zeitreihenzerlegung der Arbeitslosenquote
Arbeitslosenquote_ts %>% decompose %>% autoplot

# Zeitreihenzerlegung der Google_Variable
Google_Variable_ts %>% decompose %>% autoplot

# Test auf Stationarität (Augmented Dickey-Fuller Test)
adf.test((Arbeitslosenquote_ts), alternative="stationary", k=0)
ggAcf(Arbeitslosenquote_ts)


### Datenimport der Dateien inklusive Time-Lags ###

# Import Arbeitslosendaten inklusive Time-Lag
Arbeitslosenquote_inkl_lag <- read.csv2('2019_01_27_Arbeitslosenquote_inkl_time_lag.csv')

# Import Google-Variable inklusive Time-Lag
Google_Variable_inkl_lag <- read.csv2("2019_01_27_Google_Daten_inkl_time_lag.csv")

# Spaltentypen umformen
# Arbeitslosenquote inklusive Time-Lag
Arbeitslosenquote_inkl_lag$Monat <- as.yearmon(as.character(Arbeitslosenquote_inkl_lag$Monat), "%Y-%m") 
Arbeitslosenquote_inkl_lag$Monat <- as.Date(Arbeitslosenquote_inkl_lag$Monat)
Arbeitslosenquote_inkl_lag$Arbeitslosenquote <- as.numeric(Arbeitslosenquote_inkl_lag$Arbeitslosenquote)

# Google-Variable inklusive Time-Lag
Google_Variable_inkl_lag$Monat <- as.yearmon(as.character(Google_Variable_inkl_lag$Monat), "%Y-%m") 
Google_Variable_inkl_lag$Monat <- as.Date(Google_Variable_inkl_lag$Monat)
Google_Variable_inkl_lag <- Google_Variable_inkl_lag %>% mutate(Normalisierte_Werte=as.numeric(Normalisierte.Werte)) %>% 
  mutate(Normalisierte.Werte=NULL)

# Umwandlung des Arbeitslosenobjekts in ein Zeitreihenobjekt
Arbeitslosenquote_inkl_lag_ts <- ts(Arbeitslosenquote_inkl_lag$Arbeitslosenquote, frequency=12, start=c(2008, 1))

# Umwandlung des Google_Variable-Objekts in ein Zeitreihenobjekt
Google_Variable_inkl_lag_ts <- ts(Google_Variable_inkl_lag$Normalisierte_Werte, frequency=12, start=c(2008, 1))

# Schätzen des Basismodells # Autoregressive Distributed Lag Model
lm_basis <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12),
            start = c(2009, 1), 
            end = c(2018, 6))

# Summary, R2, BIC, HAC
summ_lm_basis <- summary(lm_basis)
lm_basis_aic <- AIC(lm_basis)
lm_basis_bic <- BIC(lm_basis)

# HAC-Standardfehler - Newey-West
n <- length(Arbeitslosenquote_inkl_lag_ts)
l <- floor(0.75 * n^(1/3))

vcov_nw_basis <- NeweyWest(lm_basis, lag = l - 1, prewhite = FALSE, adjust = TRUE)

# Teststatstatistik inkl. HAC-Standardfehler
lm_basis_coeftest <- coeftest(lm_basis, vcov = vcov_nw_basis)

# ACF der Residuen
acf_residuen <- ggAcf(resid(lm_basis))

# Ljung-Box-Test
Box.test(resid(lm_basis), type="Ljung-Box")


# Schätzen des Erweiterten Modells (Google_Variable ohne Lag)
lm_erweitert_ohne_lag <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12) + 
                         Google_Variable_inkl_lag_ts,
                         start = c(2009, 1), 
                         end = c(2018, 6))

# Summary, R2, BIC, HAC
summ_lm_erweitert_ohne_lag <- summary(lm_erweitert_ohne_lag)
lm_erweitert_ohne_lag_aic <- AIC(lm_erweitert_ohne_lag)
lm_erweitert_ohne_lag_bic <- BIC(lm_erweitert_ohne_lag)

# HAC-Standardfehler - Newey-West
n <- length(Arbeitslosenquote_inkl_lag_ts)
l <- floor(0.75 * n^(1/3))

vcov_nw_erweitert_ohne_lag <- NeweyWest(lm_erweitert_ohne_lag, lag = l - 1, prewhite = FALSE)

# Teststatstatistik inkl. HAC-Standardfehler
lm_erweitert_ohne_lag_coeftest <- coeftest(lm_erweitert_ohne_lag, vcov = vcov_nw_erweitert_ohne_lag)


# Schätzen des Erweiterten Modells (Google_Variable mit Lag 3)
lm_erweitert <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12) + 
            L(Google_Variable_inkl_lag_ts,3),
            start = c(2009, 1), 
            end = c(2018, 6))

# Summary, R2, BIC, HAC
summ_lm_erweitert <- summary(lm_erweitert)
lm_erweitert_aic <- AIC(lm_erweitert)
lm_erweitert_bic <- BIC(lm_erweitert)

# HAC-Standardfehler - Newey-West
n <- length(Arbeitslosenquote_inkl_lag_ts)
l <- floor(0.75 * n^(1/3))

vcov_nw_erweitert <- NeweyWest(lm_erweitert, lag = l - 1, prewhite = FALSE)

# Teststatstatistik inkl. HAC-Standardfehler
lm_erweitert_coeftest <- coeftest(lm_erweitert, vcov = vcov_nw_erweitert)

# Schätzen des Erweiterten Modells (Google_Variable mit und ohne Lag)
lm_erweitert_ohne_mit_lag <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12) + 
                             Google_Variable_inkl_lag_ts + L(Google_Variable_inkl_lag_ts,3),
                             start = c(2009, 1), 
                             end = c(2018, 6))

# Summary, R2, BIC, HAC
summ_lm_erweitert_ohne_mit_lag <- summary(lm_erweitert_ohne_mit_lag)
lm_erweitert_ohne_mit_lag_aic <- AIC(lm_erweitert_ohne_mit_lag)
lm_erweitert_ohne_mit_lag_bic <- BIC(lm_erweitert_ohne_mit_lag)

# HAC-Standardfehler - Newey-West
n <- length(Arbeitslosenquote_inkl_lag_ts)
l <- floor(0.75 * n^(1/3))

vcov_nw_erweitert_ohne_mit_lag <- NeweyWest(lm_erweitert_ohne_mit_lag, lag = l - 1, prewhite = FALSE)

# Teststatstatistik inkl. HAC-Standardfehler
lm_erweitert_ohne_mit_lag_coeftest <- coeftest(lm_erweitert_ohne_mit_lag, vcov = vcov_nw_erweitert_ohne_mit_lag)


### Pseudo-out-of-sample-Forecast ###

# Pseudo-out-of-sample-Forecast des Basismodells
data <- cbind(Arbeitslosenquote_inkl_lag_ts, Google_Variable_inkl_lag_ts)
Periodenbeginn <- seq(2009, 2014.5-1/12, 1/12)
Periodenende <- seq(2013-1/12, 2018.5-2/12, 1/12)

# Initialisieren der Forecast-Matrix
forecasts <- matrix(nrow = 1, ncol = length(Periodenende))

# Schleife für Pseudo-out-of-Sample-Forecasts
for(i in 1:length(Periodenende)) {
  
  # Modell (lm_erweitert)
  m <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12), 
             start = Periodenbeginn[i], 
             end = Periodenende[i])
  
  # Daten für den Forecast für jeweils eine Periode
  d <- window(data, Periodenende[i] - 1, Periodenende[i])
  
  # Forecast
  forecasts[i] <- coef(m) %*% c(1, d[13,1], d[2,1]) 
}

# Umformen der Vektoren
forecasts_t <- t(forecasts)

# Erzeugen des Windows für die Arbeitslosenquote
Arbeitslosenquote_window_ts <- window(Arbeitslosenquote_inkl_lag_ts, c(2013,1), c(2018,6))

# MAE
mae_lm_basis <- mean(abs(as.numeric(forecasts_t) - as.numeric(Arbeitslosenquote_window_ts)))

# Fehler für Plot
fehler_lm_basis_plot_daten <- (as.numeric(forecasts_t) - as.numeric(Arbeitslosenquote_window_ts))
write.csv2(fehler_lm_basis_plot_daten, file = "fehler_lm_basis_plot_daten.csv", row.names=FALSE)

# RMSE
rmse_lm_basis <- rmse(Arbeitslosenquote_window_ts, forecasts_t)

# MAPE
mape_lm_basis <- mape(Arbeitslosenquote_window_ts, forecasts_t)
mape_lm_basis_plot_daten <- (abs((as.numeric(Arbeitslosenquote_window_ts) - as.numeric(forecasts_t))/as.numeric(Arbeitslosenquote_window_ts)))
write.csv2(mape_lm_basis_plot_daten, file = "mape_lm_basis_plot_daten.csv", row.names=FALSE)

# Pseudo-out-of-sample-Forecast des erweiterten Modells
data <- cbind(Arbeitslosenquote_inkl_lag_ts, Google_Variable_inkl_lag_ts)
Periodenbeginn <- seq(2009, 2014.5-1/12, 1/12)
Periodenende <- seq(2013-1/12, 2018.5-2/12, 1/12)

# Initialisieren der Forecast-Matrix
forecasts <- matrix(nrow = 1, ncol = length(Periodenende))

# Schleife für Pseudo-out-of-Sample-Forecasts
for(i in 1:length(Periodenende)) {
  
  # Modell (lm_erweitert)
  m <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12) + 
             L(Google_Variable_inkl_lag_ts,3), 
             start = Periodenbeginn[i], 
             end = Periodenende[i])
  
  # Daten für den Forecast für jeweils eine Periode
  d <- window(data, Periodenende[i] - 1, Periodenende[i])
  
  # Forecast
  forecasts[i] <- coef(m) %*% c(1, d[13,1], d[2,1], d[11,2]) 
}

# Umformen der Vektoren
forecasts_t <- t(forecasts)

# Erzeugen des Windows für die Arbeitslosenquote
Arbeitslosenquote_window_ts <- window(Arbeitslosenquote_inkl_lag_ts, c(2013,1), c(2018,6))

# MAE
mae_lm_erweitert <- mean(abs(as.numeric(forecasts_t) - as.numeric(Arbeitslosenquote_window_ts)))

# RMSE
rmse_lm_erweitert <- rmse(Arbeitslosenquote_window_ts, forecasts_t)

# MAPE
mape_lm_erweitert <- mape(Arbeitslosenquote_window_ts, forecasts_t)


# Pseudo-out-of-sample-Forecast des erweiterten Modells ohne Lag der Google-Variable
data <- cbind(Arbeitslosenquote_inkl_lag_ts, Google_Variable_inkl_lag_ts)
Periodenbeginn <- seq(2009, 2014.5-1/12, 1/12)
Periodenende <- seq(2013-1/12, 2018.5-2/12, 1/12)

# Initialisieren der Forecast-Matrix
forecasts <- matrix(nrow = 1, ncol = length(Periodenende))

# Schleife für Pseudo-out-of-Sample-Forecasts
for(i in 1:length(Periodenende)) {
  
  # Modell (lm_erweitert)
  m <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12) + 
             Google_Variable_inkl_lag_ts, 
             start = Periodenbeginn[i], 
             end = Periodenende[i])
  
  # Daten für den Forecast für jeweils eine Periode
  d <- window(data, Periodenende[i] - 1, Periodenende[i]+1/12)
  
  # Forecast
  forecasts[i] <- coef(m) %*% c(1, d[13,1], d[2,1], d[14,2]) 
}

# Umformen der Vektoren
forecasts_t <- t(forecasts)

# Erzeugen des Windows für die Arbeitslosenquote
Arbeitslosenquote_window_ts <- window(Arbeitslosenquote_inkl_lag_ts, c(2013,1), c(2018,6))

# MAE
mae_lm_erweitert_ohne_lag <- mean(abs(as.numeric(forecasts_t) - as.numeric(Arbeitslosenquote_window_ts)))

# RMSE
rmse_lm_erweitert_ohne_lag <- rmse(Arbeitslosenquote_window_ts, forecasts_t)

# MAPE
mape_lm_erweitert_ohne_lag <- mape(Arbeitslosenquote_window_ts, forecasts_t)


# Pseudo-out-of-sample-Forecast des erweiterten Modells mit und ohne Lag der Google-Variable
data <- cbind(Arbeitslosenquote_inkl_lag_ts, Google_Variable_inkl_lag_ts)
Periodenbeginn <- seq(2009, 2014.5-1/12, 1/12)
Periodenende <- seq(2013-1/12, 2018.5-2/12, 1/12)

# Initialisieren der Forecast-Matrix
forecasts <- matrix(nrow = 1, ncol = length(Periodenende))

# Schleife für Pseudo-out-of-Sample-Forecasts
for(i in 1:length(Periodenende)) {
  
  # Modell (lm_erweitert)
  m <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12) + 
             Google_Variable_inkl_lag_ts + L(Google_Variable_inkl_lag_ts,3), 
             start = Periodenbeginn[i], 
             end = Periodenende[i])
  
  # Daten für den Forecast für jeweils eine Periode
  d <- window(data, Periodenende[i] - 1, Periodenende[i]+1/12)
  
  # Forecast
  forecasts[i] <- coef(m) %*% c(1, d[13,1], d[2,1], d[14,2], d[11,2]) 
}

# Umformen der Vektoren
forecasts_t <- t(forecasts)

# Erzeugen des Windows für die Arbeitslosenquote
Arbeitslosenquote_window_ts <- window(Arbeitslosenquote_inkl_lag_ts, c(2013,1), c(2018,6))

# MAE
mae_lm_erweitert_ohne_mit_lag <- mean(abs(as.numeric(forecasts_t) - as.numeric(Arbeitslosenquote_window_ts)))

# Fehler für Plot
fehler_lm_erweitert_ohne_mit_lag_plot_daten <- (as.numeric(forecasts_t) - as.numeric(Arbeitslosenquote_window_ts))
write.csv2(fehler_lm_erweitert_ohne_mit_lag_plot_daten, file = "fehler_lm_erweitert_ohne_mit_lag_plot_daten.csv", row.names=FALSE)

# RMSE
rmse_lm_erweitert_ohne_mit_lag <- rmse(Arbeitslosenquote_window_ts, forecasts_t)

# MAPE
mape_lm_erweitert_ohne_mit_lag <- mape(Arbeitslosenquote_window_ts, forecasts_t)
mape_lm_erweitert_ohne_mit_lag_plot_daten <- (abs((as.numeric(Arbeitslosenquote_window_ts) - as.numeric(forecasts_t))/as.numeric(Arbeitslosenquote_window_ts)))
write.csv2(mape_lm_erweitert_ohne_mit_lag_plot_daten, file = "mape_lm_erweitert_ohne_mit_lag_plot_daten.csv", row.names=FALSE)


### Berechnung unterschiedlicher Perioden ###

# Periode 1: 2011/1 bis 2013/6

# Pseudo-out-of-sample-Forecast des Basismodells
data <- cbind(Arbeitslosenquote_inkl_lag_ts, Google_Variable_inkl_lag_ts)
Periodenbeginn <- seq(2009, 2011.5-1/12, 1/12)
Periodenende <- seq(2011-1/12, 2013.5-2/12, 1/12)

# Initialisieren der Forecast-Matrix
forecasts <- matrix(nrow = 1, ncol = length(Periodenende))

# Schleife für Pseudo-out-of-Sample-Forecasts
for(i in 1:length(Periodenende)) {
  
  # Modell (lm_erweitert)
  m <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12), 
             start = Periodenbeginn[i], 
             end = Periodenende[i])
  
  # Daten für den Forecast für jeweils eine Periode
  d <- window(data, Periodenende[i] - 1, Periodenende[i])
  
  # Forecast
  forecasts[i] <- coef(m) %*% c(1, d[13,1], d[2,1]) 
}

# Umformen der Vektoren
forecasts_t <- t(forecasts)

# Erzeugen des Windows für die Arbeitslosenquote
Arbeitslosenquote_window_ts <- window(Arbeitslosenquote_inkl_lag_ts, c(2011,1), c(2013,6))

# MAE
periode_1_mae_lm_basis <- mean(abs(as.numeric(forecasts_t) - as.numeric(Arbeitslosenquote_window_ts)))

# RMSE
periode_1_rmse_lm_basis <- rmse(Arbeitslosenquote_window_ts, forecasts_t)

# MAPE
periode_1_mape_lm_basis <- mape(Arbeitslosenquote_window_ts, forecasts_t)


# Pseudo-out-of-sample-Forecast des erweiterten Modells (mit und ohne Lag der Google-Variable)
data <- cbind(Arbeitslosenquote_inkl_lag_ts, Google_Variable_inkl_lag_ts)
Periodenbeginn <- seq(2009, 2011.5-1/12, 1/12)
Periodenende <- seq(2011-1/12, 2013.5-2/12, 1/12)

# Initialisieren der Forecast-Matrix
forecasts <- matrix(nrow = 1, ncol = length(Periodenende))

# Schleife für Pseudo-out-of-Sample-Forecasts
for(i in 1:length(Periodenende)) {
  
  # Modell (lm_erweitert)
  m <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12) + 
             Google_Variable_inkl_lag_ts + L(Google_Variable_inkl_lag_ts,3), 
             start = Periodenbeginn[i], 
             end = Periodenende[i])
  
  # Daten für den Forecast für jeweils eine Periode
  d <- window(data, Periodenende[i] - 1, Periodenende[i] + 1/12)
  
  # Forecast
  forecasts[i] <- coef(m) %*% c(1, d[13,1], d[2,1], d[14,2], d[11,2]) 
}

# Umformen der Vektoren
forecasts_t <- t(forecasts)

# Erzeugen des Windows für die Arbeitslosenquote
Arbeitslosenquote_window_ts <- window(Arbeitslosenquote_inkl_lag_ts, c(2011,1), c(2013,6))

# MAE
periode_1_mae_lm_erweitert <- mean(abs(as.numeric(forecasts_t) - as.numeric(Arbeitslosenquote_window_ts)))

# RMSE
periode_1_rmse_lm_erweitert <- rmse(Arbeitslosenquote_window_ts, forecasts_t)

# MAPE
periode_1_mape_lm_erweitert <- mape(Arbeitslosenquote_window_ts, forecasts_t)


# Periode 2: 2013/7 bis 2015/12

# Pseudo-out-of-sample-Forecast des Basismodells
data <- cbind(Arbeitslosenquote_inkl_lag_ts, Google_Variable_inkl_lag_ts)
Periodenbeginn <- seq(2011.5, 2014-1/12, 1/12)
Periodenende <- seq(2013.5-1/12, 2016-2/12, 1/12)

# Initialisieren der Forecast-Matrix
forecasts <- matrix(nrow = 1, ncol = length(Periodenende))

# Schleife für Pseudo-out-of-Sample-Forecasts
for(i in 1:length(Periodenende)) {
  
  # Modell (lm_erweitert)
  m <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12), 
             start = Periodenbeginn[i], 
             end = Periodenende[i])
  
  # Daten für den Forecast für jeweils eine Periode
  d <- window(data, Periodenende[i] - 1, Periodenende[i])
  
  # Forecast
  forecasts[i] <- coef(m) %*% c(1, d[13,1], d[2,1]) 
}

# Umformen der Vektoren
forecasts_t <- t(forecasts)

# Erzeugen des Windows für die Arbeitslosenquote
Arbeitslosenquote_window_ts <- window(Arbeitslosenquote_inkl_lag_ts, c(2013,7), c(2015,12))

# MAE
periode_2_mae_lm_basis <- mean(abs(as.numeric(forecasts_t) - as.numeric(Arbeitslosenquote_window_ts)))

# RMSE
periode_2_rmse_lm_basis <- rmse(Arbeitslosenquote_window_ts, forecasts_t)

# MAPE
periode_2_mape_lm_basis <- mape(Arbeitslosenquote_window_ts, forecasts_t)


# Pseudo-out-of-sample-Forecast des erweiterten Modells (mit und ohne Lag der Google-Variable)
data <- cbind(Arbeitslosenquote_inkl_lag_ts, Google_Variable_inkl_lag_ts)
Periodenbeginn <- seq(2011.5, 2014-1/12, 1/12)
Periodenende <- seq(2013.5-1/12, 2016-2/12, 1/12)

# Initialisieren der Forecast-Matrix
forecasts <- matrix(nrow = 1, ncol = length(Periodenende))

# Schleife für Pseudo-out-of-Sample-Forecasts
for(i in 1:length(Periodenende)) {
  
  # Modell (lm_erweitert)
  m <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12) + 
               Google_Variable_inkl_lag_ts + L(Google_Variable_inkl_lag_ts,3), 
             start = Periodenbeginn[i], 
             end = Periodenende[i])
  
  # Daten für den Forecast für jeweils eine Periode
  d <- window(data, Periodenende[i] - 1, Periodenende[i] + 1/12)
  
  # Forecast
  forecasts[i] <- coef(m) %*% c(1, d[13,1], d[2,1], d[14,2], d[11,2]) 
}

# Umformen der Vektoren
forecasts_t <- t(forecasts)

# Erzeugen des Windows für die Arbeitslosenquote
Arbeitslosenquote_window_ts <- window(Arbeitslosenquote_inkl_lag_ts, c(2013,7), c(2015,12))

# MAE
periode_2_mae_lm_erweitert <- mean(abs(as.numeric(forecasts_t) - as.numeric(Arbeitslosenquote_window_ts)))

# RMSE
periode_2_rmse_lm_erweitert <- rmse(Arbeitslosenquote_window_ts, forecasts_t)

# MAPE
periode_2_mape_lm_erweitert <- mape(Arbeitslosenquote_window_ts, forecasts_t)


# Periode 3: 2016/1 bis 2018/6

# Pseudo-out-of-sample-Forecast des Basismodells
data <- cbind(Arbeitslosenquote_inkl_lag_ts, Google_Variable_inkl_lag_ts)
Periodenbeginn <- seq(2014, 2016.5-1/12, 1/12)
Periodenende <- seq(2016-1/12, 2018.5-2/12, 1/12)

# Initialisieren der Forecast-Matrix
forecasts <- matrix(nrow = 1, ncol = length(Periodenende))

# Schleife für Pseudo-out-of-Sample-Forecasts
for(i in 1:length(Periodenende)) {
  
  # Modell (lm_erweitert)
  m <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12), 
             start = Periodenbeginn[i], 
             end = Periodenende[i])
  
  # Daten für den Forecast für jeweils eine Periode
  d <- window(data, Periodenende[i] - 1, Periodenende[i])
  
  # Forecast
  forecasts[i] <- coef(m) %*% c(1, d[13,1], d[2,1]) 
}

# Umformen der Vektoren
forecasts_t <- t(forecasts)

# Erzeugen des Windows für die Arbeitslosenquote
Arbeitslosenquote_window_ts <- window(Arbeitslosenquote_inkl_lag_ts, c(2016,1), c(2018,6))

# MAE
periode_3_mae_lm_basis <- mean(abs(as.numeric(forecasts_t) - as.numeric(Arbeitslosenquote_window_ts)))

# RMSE
periode_3_rmse_lm_basis <- rmse(Arbeitslosenquote_window_ts, forecasts_t)

# MAPE
periode_3_mape_lm_basis <- mape(Arbeitslosenquote_window_ts, forecasts_t)


# Pseudo-out-of-sample-Forecast des erweiterten Modells (mit und ohne Lag der Google-Variable)
data <- cbind(Arbeitslosenquote_inkl_lag_ts, Google_Variable_inkl_lag_ts)
Periodenbeginn <- seq(2014, 2016.5-1/12, 1/12)
Periodenende <- seq(2016-1/12, 2018.5-2/12, 1/12)

# Initialisieren der Forecast-Matrix
forecasts <- matrix(nrow = 1, ncol = length(Periodenende))

# Schleife für Pseudo-out-of-Sample-Forecasts
for(i in 1:length(Periodenende)) {
  
  # Modell (lm_erweitert)
  m <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12) + 
             Google_Variable_inkl_lag_ts + L(Google_Variable_inkl_lag_ts,3), 
             start = Periodenbeginn[i], 
             end = Periodenende[i])
  
  # Daten für den Forecast für jeweils eine Periode
  d <- window(data, Periodenende[i] - 1, Periodenende[i] + 1/12)
  
  # Forecast
  forecasts[i] <- coef(m) %*% c(1, d[13,1], d[2,1], d[14,2], d[11,2]) 
}

# Umformen der Vektoren
forecasts_t <- t(forecasts)

# Erzeugen des Windows für die Arbeitslosenquote
Arbeitslosenquote_window_ts <- window(Arbeitslosenquote_inkl_lag_ts, c(2016,1), c(2018,6))

# MAE
periode_3_mae_lm_erweitert <- mean(abs(as.numeric(forecasts_t) - as.numeric(Arbeitslosenquote_window_ts)))

# RMSE
periode_3_rmse_lm_erweitert <- rmse(Arbeitslosenquote_window_ts, forecasts_t)

# MAPE
periode_3_mape_lm_erweitert <- mape(Arbeitslosenquote_window_ts, forecasts_t)


### Robustheitscheck ###
# Suchwort "Oesterreich" anstatt der arbeitsmarktrelevanten Suchwoerter (AMS + Job + Jobs - Steve - Apple)

# Datenimport der Dateien inklusive Time-Lags

# Import Google-Variable inklusive Time-Lag
Fake_Oesterreich_Google_Variable_inkl_lag <- read.csv2("2019_03_15_Fake_Oesterreich_Google_Daten_inkl_time_lag.csv")

# Spaltentypen umformen
# Fake-Google-Variable inklusive Time-Lag
Fake_Oesterreich_Google_Variable_inkl_lag$Monat <- as.yearmon(as.character(Fake_Oesterreich_Google_Variable_inkl_lag$Monat), "%Y-%m") 
Fake_Oesterreich_Google_Variable_inkl_lag$Monat <- as.Date(Fake_Oesterreich_Google_Variable_inkl_lag$Monat)
Fake_Oesterreich_Google_Variable_inkl_lag <- Fake_Oesterreich_Google_Variable_inkl_lag %>% mutate(Normalisierte_Werte=as.numeric(Normalisierte.Werte)) %>% 
  mutate(Normalisierte.Werte=NULL)

# Umwandlung des Google_Variable-Objekts in ein Zeitreihenobjekt
Fake_Oesterreich_Google_Variable_inkl_lag_ts <- ts(Fake_Oesterreich_Google_Variable_inkl_lag$Normalisierte_Werte, frequency=12, start=c(2008, 1))


# Schätzen des Erweiterten Modells (Fake-Google_Variable ohne Lag)
lm_fake_Oesterreich_erweitert_ohne_lag <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12) + 
                               Fake_Oesterreich_Google_Variable_inkl_lag_ts,
                               start = c(2009, 1), 
                               end = c(2018, 6))

# Summary, R2, BIC, HAC
summ_lm_fake_Oesterreich_erweitert_ohne_lag <- summary(lm_fake_Oesterreich_erweitert_ohne_lag)
lm_fake_Oesterreich_erweitert_ohne_lag_aic <- AIC(lm_fake_Oesterreich_erweitert_ohne_lag)
lm_fake_Oesterreich_erweitert_ohne_lag_bic <- BIC(lm_fake_Oesterreich_erweitert_ohne_lag)

# HAC-Standardfehler - Newey-West
n <- length(Arbeitslosenquote_inkl_lag_ts)
l <- floor(0.75 * n^(1/3))

vcov_nw_fake_Oesterreich_erweitert_ohne_lag <- NeweyWest(lm_fake_Oesterreich_erweitert_ohne_lag, lag = l - 1, prewhite = FALSE)

# Teststatstatistik inkl. HAC-Standardfehler
lm_fake_Oesterreich_erweitert_ohne_lag_coeftest <- coeftest(lm_fake_Oesterreich_erweitert_ohne_lag, vcov = vcov_nw_fake_Oesterreich_erweitert_ohne_lag)


# Schätzen des Erweiterten Modells (Google_Variable mit Lag 3)
lm_fake_Oesterreich_erweitert <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12) + 
                      L(Fake_Oesterreich_Google_Variable_inkl_lag_ts,3),
                      start = c(2009, 1), 
                      end = c(2018, 6))

# Summary, R2, BIC, HAC
summ_lm_fake_Oesterreich_erweitert <- summary(lm_fake_Oesterreich_erweitert)
lm_fake_Oesterreich_erweitert_aic <- AIC(lm_fake_Oesterreich_erweitert)
lm_fake_Oesterreich_erweitert_bic <- BIC(lm_fake_Oesterreich_erweitert)

# HAC-Standardfehler - Newey-West
n <- length(Arbeitslosenquote_inkl_lag_ts)
l <- floor(0.75 * n^(1/3))

vcov_nw_fake_Oesterreich_erweitert <- NeweyWest(lm_fake_Oesterreich_erweitert, lag = l - 1, prewhite = FALSE)

# Teststatstatistik inkl. HAC-Standardfehler
lm_fake_Oesterreich_erweitert_coeftest <- coeftest(lm_fake_Oesterreich_erweitert, vcov = vcov_nw_fake_Oesterreich_erweitert)

# Schätzen des Erweiterten Modells (Google_Variable mit und ohne Lag)
lm_fake_Oesterreich_erweitert_ohne_mit_lag <- dynlm(Arbeitslosenquote_inkl_lag_ts ~ L(Arbeitslosenquote_inkl_lag_ts, 1) + L(Arbeitslosenquote_inkl_lag_ts, 12) + 
                                   Fake_Oesterreich_Google_Variable_inkl_lag_ts + L(Fake_Oesterreich_Google_Variable_inkl_lag_ts,3),
                                   start = c(2009, 1), 
                                   end = c(2018, 6))

# Summary, R2, BIC, HAC
summ_lm_fake_Oesterreich_erweitert_ohne_mit_lag <- summary(lm_fake_Oesterreich_erweitert_ohne_mit_lag)
lm_fake_Oesterreich_erweitert_ohne_mit_lag_aic <- AIC(lm_fake_Oesterreich_erweitert_ohne_mit_lag)
lm_fake_Oesterreich_erweitert_ohne_mit_lag_bic <- BIC(lm_fake_Oesterreich_erweitert_ohne_mit_lag)

# HAC-Standardfehler - Newey-West
n <- length(Arbeitslosenquote_inkl_lag_ts)
l <- floor(0.75 * n^(1/3))

vcov_nw_fake_Oesterreich_erweitert_ohne_mit_lag <- NeweyWest(lm_fake_Oesterreich_erweitert_ohne_mit_lag, lag = l - 1, prewhite = FALSE)

# Teststatstatistik inkl. HAC-Standardfehler
lm_fake_Oesterreich_erweitert_ohne_mit_lag_coeftest <- coeftest(lm_fake_Oesterreich_erweitert_ohne_mit_lag, vcov = vcov_nw_fake_Oesterreich_erweitert_ohne_mit_lag)