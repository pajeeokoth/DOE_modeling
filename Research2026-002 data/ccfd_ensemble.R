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
train_ccrd1 <- read.table('./Research2026-002 data/CCRD-1 data.txt'
                          , header = TRUE
                          , skip = 0
                          , sep = ""
                          , fill = TRUE
                          , stringsAsFactors = FALSE
                          , fileEncoding = "UTF-8")

#---------------------------
# Validate
test_ccrd1 <- read.table('./Research2026-002 data/CCRD-1 test.txt'
                         , header = TRUE
                         , skip = 0
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

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
train_ccfd1 <- read.table('./Research2026-002 data/CCFD-1 data.txt'
                          , header = TRUE
                          , skip = 0
                          , sep = ""
                          , fill = TRUE
                          , stringsAsFactors = FALSE
                          , fileEncoding = "UTF-8")

#---------------------------
# Validate
test_ccfd1 <- read.table('./Research2026-002 data/CCFD-1 validate.txt'
                         , header = TRUE
                         , skip = 0
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

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
# # FCCD1
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
# FFD1
####################################################################
ffd1 <- read.csv('./Research2026-002 data/FFD-1 data.csv')

#--------------------------
# Validate
#---------------------------------
# Train and test sets
train_ffd1 <- as.data.frame(ffd1)[1:16, ]
test_ffd1 <- as.data.frame(ffd1)[17:24, ]

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
# ffd2 <- read.csv('./Research2026-002 data/2FD-1 data.csv')
# 
# #--------------------------
# # Validate
# #---------------------------------
# # Train and test sets
# train_ffd2 <- as.data.frame(ffd2)[1:16, ]
# test_ffd2 <- as.data.frame(ffd2)[17:24, ]
# 
# #----------------------------------
# # Ensemble modeling for ffd2
# train_data <- train_ffd2
# test_data <- test_ffd2
# 
# responses <- c("weight",'cell')
# predictors <- c('N', 'P','L','M','A','F','S')

# rsm_formulas <- list(
#   weight = weight ~ FO(N,P,L,M,A,F,S) + N:A + P:F + M:F,
#   cell = cell ~ FO(N,P,L,M,A,F,S) + N:A + P:F + A:F
# )

# results <- doe_meta_model(
#   train_data = train_data,
#   test_data = test_data,
#   responses = responses,
#   predictors = predictors,
#   rsm_formulas = rsm_formulas,
#   design_type = "FFD",
#   excel_file="Metrics.xlsx"
# )

####################################################################
# FD1
####################################################################
fd1 <- read.table('./Research2026-002 data/3FD-1 data.txt'
                    , header = TRUE
                    , skip = 0
                    , sep = ""
                    , fill = TRUE
                    , stringsAsFactors = FALSE
                    , fileEncoding = "UTF-8")

#--------------------------
# Validate
#---------------------------------
# Train and test sets
train_fd1 <- as.data.frame(fd1)[1:13, ]
test_fd1 <- as.data.frame(fd1)[14:22, ]

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
  design_type = "FD",
  excel_file="Metrics.xlsx"
)

####################################################################
# PBD1 taking last as test, test not provided BBD2-paper
####################################################################
pbd1 <- read.table('./Research2026-002 data/PBD-1 data.txt'
                   , header = TRUE
                   , skip = 0
                   , sep = ""
                   , fill = TRUE
                   , stringsAsFactors = FALSE
                   , fileEncoding = "UTF-8")

#---------------------------
# Validate
#---------------------------
pbd1 <- as.data.frame(pbd1)
train_pbd1 <- sample(pbd1[1:13,])
test_pbd1 <- pbd1[sample(14:14),]

#----------------------------------
# Ensemble modeling for PBD1
train_data <- train_pbd1
test_data <- test_pbd1

responses <- c("Biomass","CaCO3")
predictors <- c("A", "B", "C", "D", "F", "G", "H")

rsm_formulas <- list(
  Biomass = Biomass ~ FO(A, B, C, D, F, G, H),
  CaCO3 = CaCO3 ~ FO(A, B, C, D, F, G, H)
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
# PBD3 Optional
####################################################################
pbd3 <- read.table('./Research2026-002 data/PBD-3 data.txt'
                   , header = TRUE
                   , skip = 0
                   , sep = ""
                   , fill = TRUE
                   , stringsAsFactors = FALSE
                   , fileEncoding = "UTF-8")

#---------------------------
# Validate
pbd3 <- as.data.frame(pbd3)
train_pbd3 <- sample(pbd3[1:15,])
test_pbd3 <- pbd3[sample(16:17),]

#----------------------------------
# Ensemble modeling for PBD3
train_data <- train_pbd3
test_data <- test_pbd3

responses <- c("Y1","Y2","Y3","Y4","Y5","Y6")
predictors <- c("A", "C",  "D")

rsm_formulas <- list(
  Y1 = Y1 ~ FO(A, C, D) + I(A^2) + I(C^2) + I(D^2),
  Y2 = Y2 ~ FO(A, C, D) + I(A^2) + I(C^2) + I(D^2),
  Y3 = Y3 ~ FO(A, C, D) + I(A^2) + I(C^2) + I(D^2),
  Y4 = Y4 ~ FO(A, C, D) + I(A^2) + I(C^2) + I(D^2),
  Y5 = Y5 ~ FO(A, C, D) + I(A^2) + I(C^2) + I(D^2),
  Y6 = Y6 ~ FO(A, C, D) + I(A^2) + I(C^2) + I(D^2)
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
# PBD4 Optional BBD9-paper
####################################################################
train_pbd4 <- read.table('./Research2026-002 data/PBD-4 data.txt'
                    , header = FALSE
                    , skip = 1
                    , sep = ""
                    , fill = TRUE
                    , stringsAsFactors = FALSE
                    , fileEncoding = "UTF-8")
# Define the column names                    
colnames(pbd4) <- c("Run", "A", "B", "C", "D", "E", "F", "Y")

#---------------------------------
# Validate
pbd4 <- read.table('./Research2026-002 data/BBD-9 data.txt'
                    , header = TRUE
                    , skip = 0
                    , sep = ""
                    , fill = TRUE
                    , stringsAsFactors = FALSE
                    , fileEncoding = "UTF-8")



pbd4 <- as.data.frame(pbd4)
train_pbd4 <- sample(pbd4[18:22,])

#----------------------------------
# Ensemble modeling for PBD4
train_data <- train_pbd4
test_data <- test_pbd4

responses <- 'Y'
predictors <- c("A", "B",'C')

rsm_formulas <- list(
  Y = Y ~ FO(A, B, C)
)

# You MUST supply factor_ranges — the original natural-unit bounds.
# Without them, there's no way to know that -1 maps to 20 and +1 maps to 25.
factor_ranges <- list(
  A = c(20, 25),
  B = c(25, 31),
  C = c(3, 3.75)
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
# PBD5 Optional BBD11-paper
####################################################################
pbd5 <- read.table('./Research2026-002 data/PBD-5 data.txt'
                   , header = TRUE
                   , skip = 0
                   , sep = ""
                   , fill = TRUE
                   , stringsAsFactors = FALSE
                   , fileEncoding = "UTF-8")

#-------------------------------
# Validate
pbd5 <- as.data.frame(pbd5)
train_pbd5 <- sample(pbd5[1:12,])
test_pbd5 <- pbd5[sample(13:14,)]

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