# Load required packages
library(tidyverse)
library(readxl)
library(readr)
library(Metrics)
library(rsm)
library(h2o)
library(DiagrammeR)

#---------------------------
# Import the functions
# 1. Function to fit Gaussian Process hyperparameter tuned model (gp_master_smallDOE())
# 2. Function to fit DL and gridsearch (run_DOE_ANN_full())
# 3. Function to save the results (save_model_metrics())

source('./Research2026-002 data/utils.R')

# LOAD the date_sets

####################################################################
# DSD1 train coded, test not coded Also CCD24
####################################################################
train_dsd1 <- read.table('./Research2026-002 data/DSD -1 train.txt', header = TRUE)

#------------------------
# Validate
test_dsd1 <- read.table('./Research2026-002 data/DSD-1 validation.txt', header = TRUE)

#----------------------------------
# Ensemble modeling for dsd1
train_data <- train_dsd1
test_data <- test_dsd1

responses <- c("y")
predictors <- c('x2', 'x3', 'x7', 'x8')
 
rsm_formulas <- list(
  y = y ~ FO(x2, x3, x7, x8) + x2:x3
)

# MUST supply factor_ranges — the original natural-unit bounds.
factor_ranges <- list(
  x2 = c(5, 11),
  x3 = c(60, 120),
  x7 = c(100, 500),
  x8 = c(2, 8)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "DSD",
  factor_ranges = factor_ranges,     # <-- required here
  excel_file="Metrics.xlsx"
)

####################################################################
# DSD2
####################################################################
train_dsd2 <- read.table('./Research2026-002 data/DSD-2 data.txt', header = TRUE)

#---------------------------
# Validate
#---------------------------
test_dsd2 <- read.table('./Research2026-002 data/DSD-2 test.txt', header = TRUE)

#---------------------------
# Convert seb to L1:-1,L2:1
train_dsd2$SEBc <- ifelse(train_dsd2$SEB == "L1", -1, 1)
test_dsd2$SEBc <- ifelse(test_dsd2$SEB == "L1", -1, 1)

#----------------------------------
# Ensemble modeling for dsd2
train_data <- train_dsd2
test_data <- test_dsd2

responses <- c('OA')
predictors <- c("SEBc","DPF", 'Time', 'Temperature', 'Inoculation', 'TCP', 'AS')
 
rsm_formulas <- list(
  OA = OA ~ FO(SEBc, DPF, Time, Temperature, Inoculation, TCP, AS)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "DSD",
  excel_file="Metrics.xlsx"
)

####################################################################
# DSD3 No paper hence no model
####################################################################
dsd3 <- read.table('./Research2026-002 data/DSD-3 data.txt', header = TRUE)

#---------------------------------
# Validate
train_dsd3 <- dsd3[1:24, , drop = FALSE]
test_dsd3 <- dsd3[25:28,, drop = FALSE]

#----------------------------------
# Ensemble modeling for dsd3
train_data <- train_dsd3
test_data <- test_dsd3

responses <- c("Yield")
predictors <- c('A', 'B', 'C', 'D', 'E', 'Fa', 'G', 'H', 'I', 'J')

rsm_formulas <- list(
  Yield = Yield ~ SO(A,	B,	C,	D,	E,	Fa,	G,	H,	I,	J)
)
factor_ranges <- list(
  A = c(-1, 1),
  B = c(-1, 1),
  C = c(-1, 1),
  D = c(-1, 1),
  E = c(-1, 1),
  Fa = c(-1, 1),
  G = c(-1, 1),
  H = c(-1, 1),
  I = c(-1, 1),
  J = c(-1, 1)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "DSD",
  factor_ranges = factor_ranges,     # <-- required here
  excel_file="Metrics.xlsx"
)

####################################################################
# DSD4
####################################################################
dsd4 <- read.csv('./Research2026-002 data/DSD-4 data.csv')

#---------------------------------
# Train and test sets
dsd4 <- as.data.frame(dsd4)
train_dsd4 <- sample(dsd4[1:14,])
test_dsd4 <- sample(dsd4[15:31,])

#----------------------------------
# Ensemble modeling for dsd4
train_data <- train_dsd4
test_data <- test_dsd4[1:12,,drop = FALSE]
test_datap <- test_dsd4[c(1,13:17),,drop = FALSE]

responses <- c("ErP")
# responsesb <- c("ErA")
predictors <- c('Tcrys','poxy','TDew','Dwell','Heating','Rotation')

rsm_formulas <- list(
  ErP = ErP ~ FO(Tcrys, poxy, TDew) + I(poxy^2)
)
rsm_formulasp <- list(
  ErA = ErA ~ FO(Tcrys, TDew) + I(Tcrys^2)
)
results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "DSD",
  excel_file="Metrics.xlsx"
)
results <- doe_meta_model(
  train_data = train_data,
  test_data = test_datap,
  responses = c("ErA"),
  predictors = predictors,
  rsm_formulas = rsm_formulasp,
  design_type = "DSD",
  excel_file="Metrics.xlsx"
)

####################################################################
# DSD5 Also BBD13 and TAG3
####################################################################
train_dsd5 <- read.table('./Research2026-002 data/DSD-5 data.txt', header = TRUE)

#--------------------------
# Validate
test_dsd5 <- read.table('./Research2026-002 data/BBD-13 dsd5tag4 test.txt', header = TRUE)

#----------------------------------
# Ensemble modeling for dsd5
train_data <- train_dsd5
test_data <- test_dsd5

responses <- c("Hardness")
predictors <- c('LT', 'ID', 'NT', 'PS')

rsm_formulas <- list(
  Hardness = Hardness ~ FO(LT, ID, NT, PS) + I(LT^2) + I(ID^2) + I(NT^2) + I(PS^2)
)

# MUST supply factor_ranges — the original natural-unit bounds.
factor_ranges <- list(
  LT = c(100, 300),
  ID = c(50, 100),
  NT = c(240, 260),
  PS = c(60, 120)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "DSD",
  factor_ranges = factor_ranges,     # <-- required here
  excel_file="Metrics.xlsx"
)

####################################################################
# DSD6
####################################################################
dsd6 <- read.table('./Research2026-002 data/DSD-6 data.txt', header = TRUE)

#---------------------------------
# Train and test sets
dsd6 <- as.data.frame(dsd6)
train_dsd6 <- dsd6 %>% filter(Type == 'Training')
test_dsd6 <- dsd6 %>% filter(Type == 'Validation')

#----------------------------------
# Ensemble modeling for dsd6
train_data <- train_dsd6
test_data <- test_dsd6

responses <- c("Actual")
predictors <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8')

rsm_formulas <- list(
  Actual = Actual ~ FO(X1, X2, X3, X4, X5, X6, X7, X8)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "DSD",
  excel_file="Metrics.xlsx"
)

####################################################################
# DSD7
####################################################################
dsd7 <- read.table('./Research2026-002 data/DSD-7 data.txt', header = TRUE)

#---------------------------------
#Validate
train_dsd7 <- dsd7[1:15, , drop = FALSE]
test_dsd7 <- dsd7[16, , drop = FALSE]

#----------------------------------
# Ensemble modeling for dsd7
train_data <- train_dsd7
test_data <- test_dsd7

responses <- c("LengthDiff","DiameterDiff")
predictors <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6')

rsm_formulas <- list(
  LengthDiff = LengthDiff ~ FO(X1, X2, X3, X4, X5, X6) + I(X1^2) + X1:X6 + X3:X4 + X5:X6,
  DiameterDiff = DiameterDiff ~ FO(X1, X2, X3, X4, X5, X6) + I(X2^2) + X1:X3 + X2:X4 + X2:X5 + X5:X6
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "DSD",
  excel_file="Metrics.xlsx"
)

####################################################################
# DSD8
####################################################################
dsd8 <- read.table('./Research2026-002 data/DSD-8 data.txt', header = TRUE)

#---------------------------------
# Validate
train_dsd8 <- dsd8[1:50, , drop = FALSE]
test_dsd8 <- dsd8[51, , drop = FALSE]

#----------------------------------
# Ensemble modeling for dsd8
train_data <- train_dsd8
test_data <- test_dsd8

responses <- c("Ra","TS", 'FS')
predictors <- c('LT',	'NP',	'ID',	'FA',	'PS',	'ET',	'BT',	'BO')

rsm_formulas <- list(
  Ra = Ra ~ FO(LT, ID, FA, PS, ET, BO) + I(ID^2) + I(PS^2) + LT:BO + ET:BO,
  TS = TS ~ FO(LT, ID, FA, PS, ET, BT, BO) + I(ET^2) + I(BT^2) + ID:BO + FA:PS + FA:ET,
  FS = FS ~ FO(LT, ID, FA, PS, ET, BT, BO) + I(ET^2) + I(BT^2) + ID:BO + FA:PS + FA:ET
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "DSD",
  excel_file="Metrics.xlsx"
)

#--------------------------------------------------------------------
# Optional: Clean up H2O logs that are more than 7 days old. 
# Set dry_run = FALSE to actually delete the files.
cleanup_h2o_logs(max_age_days = 7, dry_run = TRUE)