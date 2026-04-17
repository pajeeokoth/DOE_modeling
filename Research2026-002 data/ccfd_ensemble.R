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

# LOAD the data sets

####################################################################
# CCRD1
####################################################################
train_ccrd1 <- read.table('./Research2026-002 data/CCRD-1 data.txt', header = TRUE)

#---------------------------
# Validate
test_ccrd1 <- read.table('./Research2026-002 data/CCRD-1 test.txt', header = TRUE)

#----------------------------------
# Ensemble modeling for ccrd1
train_data <- train_ccrd1
test_data <- test_ccrd1

responses <- c("Lipase")
predictors <- c('Temp', 'Medium','size','Agitation','Incubation','pH')
 
rsm_formulas <- list(
  Lipase = Lipase ~ SO(Temp, Medium, size, Agitation, Incubation, pH) + Temp:Agitation:Incubation
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCRD",
  excel_file="Metrics.xlsx"
)

####################################################################
# CCFD1
####################################################################
train_ccfd1 <- read.table('./Research2026-002 data/CCFD-1 data.txt', header = TRUE)

#---------------------------
# Validate
test_ccfd1 <- read.table('./Research2026-002 data/CCFD-1 validate.txt', header = TRUE)

#----------------------------------
# Ensemble modeling for ccfd1
train_data <- train_ccfd1
test_data <- test_ccfd1

responses <- c("Yield_Exp")
predictors <- c('X1_actual', 'X2_actual','X3_actual','X4_actual')

rsm_formulas <- list(
  Yield_Exp = Yield_Exp ~ SO(X1_actual, X2_actual, X3_actual, X4_actual)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCFD",
  excel_file="Metrics.xlsx"
)

# ####################################################################
# # FCCD1 Also CCD4
# ####################################################################
# fccd1 <- read.csv('./Research2026-002 data/CCD-4 design.txt'
#                         , header = FALSE
#                         , skip = 3
#                         , sep = ""
#                         , fill = TRUE
#                         , stringsAsFactors = FALSE
#                         , fileEncoding = "UTF-8")

# # Define the column names
# colnames(fccd1) <- c("Run", "pulse_on", "pulse_off",  "serve_volt"
#                           ,'peak_cur', 'wire', "kerf_width",'asr','mrr')
# #--------------------------
# # Validate

# test_fccd1 <- read.table('./Research2026-002 data/CCD-4 validate.txt'
#                         , header = FALSE
#                         , skip = 3
#                         , sep = ""
#                         , fill = TRUE
#                         , stringsAsFactors = FALSE
#                         , fileEncoding = "UTF-8")

# # Define the column names
# colnames(test_fccd1) <- c("Run", "pulse_on", "pulse_off",  "serve_volt", 'peak_cur'
#                          ,'wire', "kerf_width",'asr','mrr','rsm_pk','rsm_pr'
#                          ,'rsm_pm','ann_pk','ann_pr','ann_pm','rsm_ke','rsm_re'
#                          ,'rsm_me','ann_ke','ann_re','ann_me')

# #----------------------------------
# # Ensemble modeling for fccd1
# train_data <- train_fccd1
# test_data <- test_fccd1

# responses <- c("kerf_width",'asr','mrr')
# predictors <- c("pulse_on", "pulse_off",  "serve_volt",'peak_cur', 'wire')

# rsm_formulas <- list(
#   kerf_width = kerf_width ~ FO(pulse_on, pulse_off, serve_volt, peak_cur, wire) + I(pulse_off^2) + pulse_on:serve_volt + pulse_off:serve_volt + peak_cur:wire,
#   asr = asr ~ FO(pulse_on, pulse_off, serve_volt, peak_cur, wire) + I(pulse_off^2) + I(serve_volt^2) + pulse_on:pulse_off + pulse_off:serve_volt + peak_cur:wire,
#   mrr = mrr ~ FO(pulse_on, pulse_off, serve_volt, peak_cur, wire) + I(pulse_on^2) + I(serve_volt^2) +I(peak_cur^2) + I(wire^2) + pulse_on:serve_volt +  pulse_off:serve_volt + peak_cur:wire
# )

# results <- doe_meta_model(
#   train_data = train_data,
#   test_data = test_data,
#   responses = responses,
#   predictors = predictors,
#   rsm_formulas = rsm_formulas,
#   design_type = "FCCD",
#   excel_file="Metrics.xlsx"
# )

####################################################################
# FFD1 (Fractional Factorial Design) text -pg48(52)
####################################################################
ffd1 <- read.csv('./Research2026-002 data/FFD-1 data.csv', header = TRUE)

#--------------------------
# Validate
train_ffd1 <- ffd1[1:16, , drop = FALSE]
test_ffd1 <- ffd1[17:24, , drop = FALSE]

#----------------------------------
# Ensemble modeling for ffd1
train_data <- train_ffd1
test_data <- test_ffd1

responses <- c("Weight",'Cell')
predictors <- c('N', 'P','L','M','A','F','S')

rsm_formulas <- list(
  Weight = Weight ~ FO(N,P,L,M,A,F,S) + N:A + P:F + M:F,
  Cell = Cell ~ FO(N,P,L,M,A,F,S) + N:A + P:F + A:F
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "FFD",
  excel_file="Metrics.xlsx"
)

####################################################################
# FFD2
####################################################################
ffd2 <- read.table('./Research2026-002 data/FFD-2 data.txt', header = TRUE)

#--------------------------
# Validate
train_ffd2 <- ffd2[1:16, , drop = FALSE]
test_ffd2 <- ffd2[17:24, , drop = FALSE]

#----------------------------------
# Ensemble modeling for ffd1
train_data <- train_ffd1
test_data <- test_ffd1

responses <- c("Ra")
predictors <- c('vc', 'f','alpha')

rsm_formulas <- list(
  Ra = Ra ~ SO(vc, f, alpha)
  )

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "FFD",
  excel_file="Metrics.xlsx"
)

####################################################################
# 2FD1 (Two-Level Factorial Design)
####################################################################
tlfd1 <- read.csv('./Research2026-002 data/2FD-1 data.csv', header = TRUE)

# Create the transformed response column
tlfd1$inv_sqrt_avg <- 1 / sqrt(tlfd1$Average)

#--------------------------
# Validate
train_2fd1 <- tlfd1[1:20, , drop = FALSE]
test_2fd1 <- tlfd1[21:28, , drop = FALSE]

#----------------------------------
# Ensemble modeling for 2fd1
train_data <- train_2fd1
test_data <- test_2fd1

responses <- c("inv_sqrt_avg")
predictors <- c("B", "C", "D")

rsm_formulas <- list(
  inv_sqrt_avg = inv_sqrt_avg ~ FO(B, C, D) + B:C
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "2LFD",
  excel_file="Metrics.xlsx"
)

####################################################################
# 2FD2 (Two-Level Factorial Design)
####################################################################
tlfd2 <- read.csv('./Research2026-002 data/2FD-2 data.txt', header = TRUE)

#--------------------------
# Validate
train_2fd2 <- tlfd2[1:13, , drop = FALSE]
test_2fd2 <- tlfd2[14, , drop = FALSE]

#----------------------------------
# Ensemble modeling for 2fd2
train_data <- train_2fd2
test_data <- test_2fd2

responses <- c("y7", "y28")
predictors <- c("A", "B")

rsm_formulas <- list(
  y7 = y7 ~ SO(A, B),
  y28 = y28 ~ SO(A, B)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "2LFD",
  excel_file="Metrics.xlsx"
)

####################################################################
# 3FD1 (Three-Level Factorial Design)
####################################################################
tlfd1 <- read.table('./Research2026-002 data/3FD-1 data.txt', header = TRUE)

#--------------------------
# Validate
train_fd1 <- fd1[1:13, , drop = FALSE]
test_fd1 <- fd1[14:22, , drop = FALSE]

#----------------------------------
# Ensemble modeling for ccfd1
train_data <- train_fd1
test_data <- test_fd1

responses <- c("yield")
predictors <- c('x1', 'x2','x3')
 
rsm_formulas <- list(
  yield = yield ~ FO(x1,x2, x3) + I(x1^2) + I(x3^2) + x1:x3
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "3LFD",
  excel_file="Metrics.xlsx"
)

####################################################################
# FCCD2 (CCD Full Factorial Face Centered)
####################################################################
fffcd2 <- read.table('./Research2026-002 data/FCCD-2 data.txt', header = TRUE)

#--------------------------
# Validate
train_fffcd2 <- fffcd2[1:27, , drop = FALSE]
test_fffcd2 <- fffcd2[28, , drop = FALSE]

#----------------------------------
# Ensemble modeling for fccd2
train_data <- train_fffcd2
test_data <- test_fffcd2

responses <- c("UTS")
predictors <- c("Temp","HT","CSA")
 
rsm_formulas <- list(
  UTS = UTS ~ FO(Temp, HT, CSA) + I(Temp^2) + Temp:HT + Temp:CSA + HT:CSA
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "FCCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# FCCD3 (CCD Full Factorial Face Centered)
####################################################################
fffcd3 <- read.table('./Research2026-002 data/FCCD-3 data.txt', header = TRUE)

#--------------------------
# Validate
train_fffcd3 <- fffcd3[1:30, , drop = FALSE]
test_fffcd3 <- fffcd3[31:34, , drop = FALSE]

#----------------------------------
# Ensemble modeling for fccd3
train_data <- train_fffcd3
test_data <- test_fffcd3

responses <- c("SF","E")
predictors <- c("A","B","C","D")
 
rsm_formulas <- list(
  SF = SF ~ FO(A, B, C, D) + I(B^2) + I(D^2) + A:B + B:C + C:D,
  E = E ~ FO(A, B, C, D) + I(B^2) + A:B
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "FCCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# FCCD4 (CCD Full Factorial Face Centered)
####################################################################
fffcd4 <- read.table('./Research2026-002 data/FCCD-4 data.txt', header = TRUE)

#--------------------------
# Validate
train_fffcd4 <- fffcd4[1:20, , drop = FALSE]
test_fffcd4 <- fffcd4[21:22, , drop = FALSE]

#----------------------------------
# Ensemble modeling for fccd4
train_data <- train_fffcd4
test_data <- test_fffcd4

responses <- c("Ra")
predictors <- c("A","B","C")
 
rsm_formulas <- list(
  Ra = Ra ~ SO(A, B, C)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "FCCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# PBD1 taking last as test, test not provided Also BBD2-paper
####################################################################
# Note that the two responses have different factors, this will affect the 
# GP and ANN performance, but the RSM will be unaffected since it's
# fit separately for each response. The 1 validation run does not have 
# the G and F factors, so using last observation as test.
####################################################################
pbd1 <- read.table('./Research2026-002 data/PBD-1 data.txt', header = TRUE)

#---------------------------
# Validate
train_pbd1 <- pbd1[1:13, , drop = FALSE]
test_pbd1 <- pbd1[14, , drop = FALSE]

#----------------------------------
# Ensemble modeling for PBD1
train_data <- train_pbd1
test_data <- test_pbd1

responses <- c("Biomass","CaCO3")
predictors <- c("A", "B", "D", "F", "G")

rsm_formulas <- list(
  Biomass = Biomass ~ FO(A, B, D, F),
  CaCO3 = CaCO3 ~ FO(A, B, D, F, G)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "PBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# PBD2 validation x1-low
####################################################################
pbd2 <- read.table('./Research2026-002 data/PBD-2 data.txt', header = TRUE)

#---------------------------
# Validate
train_pbd2 <- pbd2[1:12, , drop = FALSE]
test_pbd2 <- pbd2[13:14, , drop = FALSE]

#----------------------------------
# Ensemble modeling for PBD2
train_data <- train_pbd2
test_data <- test_pbd2

responses <- c("y1", "y2", "y3", "y4")
predictors <- c("x1", "x2", "x4", "x5", "x6", "x7", "x8", "x9", "x11")

rsm_formulas <- list(
  y1 = y1 ~ FO(x1, x2, x4, x5, x6, x7, x8, x9, x11),
  y2 = y2 ~ FO(x1, x2, x4, x5, x6, x7, x8, x9, x11),
  y3 = y3 ~ FO(x1, x2, x4, x5, x6, x7, x8, x9, x11),
  y4 = y4 ~ FO(x1, x2, x4, x5, x6, x7, x8, x9, x11)
)

results <- doe_meta_model(
  train_data = train_data,+
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "PBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# PBD3 Also BBD8-paper
####################################################################
# Note validation data does not have all the train factors, only 
# using the factors available in validation set.
####################################################################
pbd3 <- read.table('./Research2026-002 data/PBD-3 data.txt', header = TRUE)

#-------------------------------
# Validate
test_pbd3 <- read.table('./Research2026-002 data/BBD-8 test.txt', header = TRUE)

#----------------------------------
# Ensemble modeling for PBD3
train_data <- train_pbd3
test_data <- test_pbd3

responses <- c("Y2","Y5","Y6")
predictors <- c("A", "C",  "D")

rsm_formulas <- list(
  Y2 = Y2 ~ FO(A, C, D),
  Y5 = Y5 ~ FO(A, C, D),
  Y6 = Y6 ~ FO(A, C, D)
)

# Train is coded (-1,0,1), test is in natural units.
# factor_ranges = natural-unit bounds derived from test data:
#   A: 2→-1, 4→0, 6→+1  →  c(2, 6)
#   C: 20→-1, 40→0, 60→+1 → c(20, 60)
#   D: 2→-1, 4→0, 6→+1  →  c(2, 6)
factor_ranges <- list(
  A = c(2, 6),
  C = c(20, 60),
  D = c(2, 6)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "PBD",
  factor_ranges = factor_ranges,     # <-- required here
  excel_file="Metrics.xlsx"
)

####################################################################
# PBD4 Also BBD9-paper
####################################################################
# Note validation data does not have all the train factors, only 
# using the factors available in validation set. The 5 validation 
# runs do not have the E and F factors, so using only A, B, F 
# for modeling. Using last observation as test since it's the only 
# one with the all factors.
####################################################################
pbd4 <- read.table('./Research2026-002 data/PBD-4 data.txt', header = TRUE)

#---------------------------------
# Validate
train_pbd4 <- pbd4[1:14, , drop = FALSE]
test_pbd4 <- pbd4[15, , drop = FALSE]

#----------------------------------
# Ensemble modeling for PBD4
train_data <- train_pbd4
test_data <- test_pbd4

responses <- 'Y'
predictors <- c("A", "B",'F')

rsm_formulas <- list(
  Y = Y ~ FO(A, B, F)
)

# You MUST supply factor_ranges — the original natural-unit bounds.
# Without them, there's no way to know that -1 maps to 20 and +1 maps to 25.
# factor_ranges <- list(
#   A = c(20, 25),
#   B = c(25, 31),
#   F = c(3, 3.75)
# )

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "PBD",
  # factor_ranges = factor_ranges,     # <-- required here
  excel_file="Metrics.xlsx"
)

####################################################################
# PBD5 Also BBD11-paper
####################################################################
# Note validation data does not have all the train factors, only 
# using the factors available in validation set. 
# Using last observation as test since it's the only one with the all factors.
####################################################################
pbd5 <- read.table('./Research2026-002 data/PBD-5 data.txt', header = TRUE)

#-------------------------------
# Validate
train_pbd5 <- pbd5[1:11, , drop = FALSE]
test_pbd5 <- pbd5[12, , drop = FALSE]

#----------------------------------
# Ensemble modeling for PBD5
train_data <- train_pbd5
test_data <- test_pbd5

responses <- c("Colchicine")
predictors <- c("Power", "Time", "Particle", "Solvent", "Solid", "Extract", "pH")

rsm_formulas <- list(
  Colchicine = Colchicine ~ FO(Power, Time, Particle, Solvent, Solid, Extract, pH)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "PBD",
  excel_file="Metrics.xlsx"
)



#--------------------------------------------------------------------
# Optional: Clean up H2O logs that are more than 7 days old. 
# Set dry_run = FALSE to actually delete the files.
cleanup_h2o_logs(max_age_days = 7)