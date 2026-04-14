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
# TAG1
####################################################################
train_tag1 <- read.table('./Research2026-002 data/TAG-1 data.txt'
                         , header = TRUE)

#------------------------
# Validate
test_tag1 <- read.table('./Research2026-002 data/TAG-1 test.txt'
                        , header = TRUE)

#----------------------------------
# Ensemble modeling for TAG1
train_data <- train_tag1
test_data <- test_tag1

responses <- c("Resp")
predictors <- c('Conc', 'Times','Temp','Agit')

rsm_formulas <- list(
  Resp = Resp ~ FO(Conc, Times, Temp, Agit)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "TAG",
  excel_file="Metrics.xlsx"
)

####################################################################
# TAG2
####################################################################
tag2 <- read.table('./Research2026-002 data/TAG-2 data.txt', header = TRUE)

#---------------------------
# Validate
#---------------------------
tag2 <- as.data.frame(tag2)
train_tag2 <- sample(tag2[1:25,])
test_tag2 <- sample(tag2[25:26,])

#----------------------------------
# Ensemble modeling for TAG2
train_data <- train_tag2
test_data <- test_tag2

responses <- c("SB","TY", 'ET', 'FB', 'EF')
predictors <- c('SW', 'AINF', 'TL', 'PS', 'TN', 'TB')
 
rsm_formulas <- list(
  SB = SB ~ FO(SW, AINF,TL,PS,TN,TB) + I(SW^2) + I(AINF^2) + I(TL^2) + I(PS^2) + I(TN^2) + I(TB^2) + SW:AINF + SW:TL + SW:PS + SW:TN + AINF:PS,
  TY = TY ~ SO(SW, AINF,TL,PS,TN,TB) + I(SW^2) + I(AINF^2) + I(TL^2) + I(PS^2) + I(TN^2) + I(TB^2) + SW:AINF + SW:TL + SW:PS + SW:TN + AINF:PS,
  ET = ET ~ FO(SW, AINF,TL,PS,TN,TB) + I(SW^2) + I(AINF^2) + I(TL^2) + I(PS^2) + I(TN^2) + I(TB^2) + SW:AINF + SW:TL + SW:PS + SW:TN + AINF:PS,
  FB = FB ~ FO(SW, AINF,TL,PS,TN,TB) + I(SW^2) + I(AINF^2) + I(TL^2) + I(PS^2) + I(TN^2) + I(TB^2) + SW:AINF + SW:TL + SW:PS + SW:TN + AINF:PS,
  EF = EF ~ FO(SW, AINF,TL,PS,TN,TB) + I(SW^2) + I(AINF^2) + I(TL^2) + I(PS^2) + I(TN^2) + I(TB^2) + SW:AINF + SW:TL + SW:PS + SW:TN + AINF:PS
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "TAG",
  excel_file="Metrics.xlsx"
)

####################################################################
# TAG3 no test set given
####################################################################
tag3 <- read.table('./Research2026-002 data/TAG-3 data.txt', header = TRUE)

#---------------------------------
# Train and test sets
tag3 <- as.data.frame(tag3)
train_tag3 <- sample(tag3[1:24,])
test_tag3 <- sample(tag3[25:27,])

#----------------------------------
# Ensemble modeling for TAG3
train_data <- train_tag3
test_data <- test_tag3

responses <- c("y")
predictors <- c('f1', 'f2', 'f3', 'f4')

rsm_formulas <- list(
  y = y ~ FO(f1, f2, f3, f4) + I(f1^2) + I(f2^2) + I(f3^2) + I(f4^2) + f1:f2 + f1:f4 + f2:f4
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "TAG",
  excel_file="Metrics.xlsx"
)

####################################################################
# TAG4
####################################################################
train_tag4 <- read.table('./Research2026-002 data/TAG-4 data.txt'
                         , header = TRUE)

#--------------------------
# Validate
test_tag4 <- read.table('./Research2026-002 data/BBD-13 dsd5tag4 test.txt'
                        , header = TRUE)

#----------------------------------
# Ensemble modeling for TAG4
train_data <- train_tag4
test_data <- test_tag4

responses <- c('Hardness')
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
  design_type = "TAG",
  factor_ranges = factor_ranges,     # <-- required here
  excel_file="Metrics.xlsx"
)

####################################################################
# TAG5 train coded, test set not coded
####################################################################
train_tag5 <- read.table('./Research2026-002 data/TAG-5 data.txt'
                         , header = TRUE)

#--------------------------
# Validate
tag5 <- read.table('./Research2026-002 data/CCD-18 data.txt', header = TRUE)

#----------------------------------
# Ensemble modeling for TAG5
train_data <- train_tag5[1:30, , drop = FALSE]
test_data <- tag5[31, , drop = FALSE]
test_data_b <- tag5[32, , drop = FALSE]

responses <- c("COD","Decol")
predictors <- c('Dye', 'DyeFe', 'H2O2Fe',  'pH')

rsm_formulas <- list(
  COD = COD ~ SO(Dye, DyeFe, H2O2Fe,  pH)
)
rsm_formulas_b <- list(
  Decol = Decol ~ SO(Dye, DyeFe, H2O2Fe,  pH)
)

# MUST supply factor_ranges — the original natural-unit bounds.
factor_ranges <- list(
  Dye = c(100, 300),
  DyeFe = c(10, 50),
  H2O2Fe = c(5, 25),
  pH = c(2, 9)
)

results <- doe_meta_model(
      train_data = train_data,
      test_data = test_data,
      responses = responses,
      predictors = predictors,
      rsm_formulas = rsm_formulas,
      design_type = "TAG",
      factor_ranges = factor_ranges,     # <-- required here
      excel_file="Metrics.xlsx"
  )

results <- doe_meta_model(
      train_data = train_data,
      test_data = test_data_b,
      responses = responses,
      predictors = predictors,
      rsm_formulas = rsm_formulas_b,
      design_type = "TAG",
      factor_ranges = factor_ranges,     # <-- required here
      excel_file="Metrics.xlsx"
)

####################################################################
# TAG6
####################################################################
tag6 <- read.table('./Research2026-002 data/TAG-6 data.txt'
                   , header = TRUE
                   , skip = 0
                   , sep = ""
                   , fill = TRUE
                   , stringsAsFactors = FALSE
                   , fileEncoding = "UTF-8")

#---------------------------------
# Train and test sets
train_tag6 <- as.data.frame(tag6)[1:16, ]
test_tag6 <- as.data.frame(tag6)[17:17, ]

#----------------------------------
# Ensemble modeling for TAG6
train_data <- train_tag6
test_data <- test_tag6

responses <- c("TS")
predictors <- c('A', 'B', 'C', 'D')

rsm_formulas <- list(
  TS = TS ~ FO(A, B, C, D)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "TAG",
  excel_file="Metrics.xlsx"
)

####################################################################
# TAG7
####################################################################
train_tag7 <- read.table('./Research2026-002 data/TAG-7 data.txt'
                   , header = TRUE
                   , skip = 0
                   , sep = ""
                   , fill = TRUE
                   , stringsAsFactors = FALSE
                   , fileEncoding = "UTF-8")

test_tag7 <- read.table('./Research2026-002 data/TAG-7 FFD2 test.txt'
                        , header = TRUE
                        , skip = 0
                        , sep = ""
                        , fill = TRUE
                        , stringsAsFactors = FALSE
                        , fileEncoding = "UTF-8")
#----------------------------------
# Ensemble modeling for TAG7
train_data <- train_tag7
test_data <- test_tag7

responses <- c("Ra")#,"MRR") #MRR has no validation data
predictors <- c('vc', 'f', 'alpha')

rsm_formulas <- list(
  Ra = Ra ~ SO(vc, f, alpha),
  MRR = MRR ~ SO(vc, f, alpha)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "TAG",
  excel_file="Metrics.xlsx"
)

####################################################################
# TAG8
####################################################################
tag8 <- read.table('./Research2026-002 data/TAG-8 data.txt'
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
train_tag8 <- as.data.frame(tag8)[1:23, ]
test_tag8 <- as.data.frame(tag8)[24:25, ]
#----------------------------------
# Ensemble modeling for TAG8
train_data <- train_tag8
test_data <- test_tag8

responses <- c('Co')
predictors <- c('Acid', 'Leach', 'Temp', 'Perc', 'SMBS')

rsm_formulas <- list(
  Co = Co ~ SO(Acid, Leach, Temp, Perc, SMBS)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "TAG",
  excel_file="Metrics.xlsx"
)

####################################################################
# TAG11
####################################################################
tag11 <- read.table('./Research2026-002 data/TAG-11 data.txt'
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
train_tag11 <- as.data.frame(tag11)[1:14, ]
test_tag11 <- as.data.frame(tag11)[15:18, ]
#----------------------------------
# Ensemble modeling for TAG11
train_data <- train_tag11
test_data <- test_tag11

responses <- c('Deformations')
predictors <- c('A', 'B', 'C', 'D', 'E', 'F')

rsm_formulas <- list(
  Deformations = Deformations ~ FO(A, B, C, D, E, F) + I(A^2) + I(B^2) + I(C^2) + I(D^2) + I(E^2) + I(F^2)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "TAG",
  excel_file="Metrics.xlsx"
)

####################################################################
# TAG15
####################################################################
tag15 <- read.table('./Research2026-002 data/TAG-15 data.txt'
                    , header = TRUE
                    , skip = 0
                    , sep = ""
                    , fill = TRUE
                    , stringsAsFactors = FALSE
                    , fileEncoding = "UTF-8")

#---------------------------------
# Train and test sets
tag15 <- as.data.frame(tag15)
train_tag15 <- sample(tag15[1:9,])
test_tag15 <- sample(tag15[9:9,])

#----------------------------------
# Ensemble modeling for TAG15
train_data <- train_tag15
test_data <- test_tag15

responses <- "Y"
predictors <- c("A", "B", "C", "D")

rsm_formulas <- list(
  Y = Y ~ SO(A, B, C, D)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "TAG",
  excel_file="Metrics.xlsx"
)