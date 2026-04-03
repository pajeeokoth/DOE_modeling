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

###################
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

###################
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
  Yield_Exp = Yield_Exp ~ SO(X1_actual, X2_actual, X3_actual, X4_actual) + X1_actual:X2_actual:X3_actual
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
