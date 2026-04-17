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
train_tag1 <- read.table('./Research2026-002 data/TAG-1 data.txt', header = TRUE)

#------------------------
# Validate
test_tag1 <- read.table('./Research2026-002 data/TAG-1 test.txt', header = TRUE)

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
train_tag2 <- tag2[1:25,, drop = FALSE]
test_tag2 <- tag2[26:27,, drop = FALSE]

#----------------------------------
# Ensemble modeling for TAG2
train_data <- train_tag2
test_data <- test_tag2

responses <- c("SB","TY", 'ET', 'FB', 'EF')
predictors <- c('SW', 'AINF', 'TL', 'PS', 'TN', 'TB')
 
rsm_formulas <- list(
  SB = SB ~ FO(SW, AINF,TL,PS,TN,TB) + I(SW^2) + I(AINF^2) + I(TL^2) + I(PS^2) + I(TN^2) + I(TB^2),
  TY = TY ~ SO(SW, AINF,TL,PS,TN,TB) + I(SW^2) + I(AINF^2) + I(TL^2) + I(PS^2) + I(TN^2) + I(TB^2),
  ET = ET ~ FO(SW, AINF,TL,PS,TN,TB) + I(SW^2) + I(AINF^2) + I(TL^2) + I(PS^2) + I(TN^2) + I(TB^2),
  FB = FB ~ FO(SW, AINF,TL,PS,TN,TB) + I(SW^2) + I(AINF^2) + I(TL^2) + I(PS^2) + I(TN^2) + I(TB^2),
  EF = EF ~ FO(SW, AINF,TL,PS,TN,TB) + I(SW^2) + I(AINF^2) + I(TL^2) + I(PS^2) + I(TN^2) + I(TB^2)
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
# TAG2A
####################################################################
tag2A <- read.csv('./Research2026-002 data/TAG-2a data.csv', header = TRUE)

#---------------------------------
# Validate
train_tag2A <- tag2A[1:9,, drop = FALSE]
test_tag2A <- tag2A[10,, drop = FALSE]

#----------------------------------
# Ensemble modeling for TAG2A
train_data <- train_tag2A
test_data <- test_tag2A

responses <- c("Ra")
predictors <- c("Spindle", "Feed", "Depth")

rsm_formulas <- list(
  Ra = Ra ~ FO(Spindle, Feed, Depth)
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
# TAG3 also BBD15 and CCD17 no test set
####################################################################
tag3 <- read.table('./Research2026-002 data/TAG-3 data.txt', header = TRUE)

#---------------------------------
# Validate
train_tag3 <- tag3[1:22, , drop = FALSE]
test_tag3 <- tag3[23:27, , drop = FALSE]

#----------------------------------
# Ensemble modeling for TAG3
train_data <- train_tag3
test_data <- test_tag3

responses <- "y"
predictors <- c("f1", "f2",  "f3", "f4")

rsm_formulas <- list(
  y = y ~ FO(f1, f2, f3, f4) + I(f1^2) + I(f2^2) + I(f3^2) + I(f4^2) + f1:f4 +f2:f4
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
# TAG4 Also BBD13 and DSD5
####################################################################
train_tag4 <- read.table('./Research2026-002 data/TAG-4 data.txt', header = TRUE)

#--------------------------
# Validate
test_tag4 <- read.table('./Research2026-002 data/BBD-13 dsd5tag4 test.txt', header = TRUE)

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
# TAG5 Also CCD18 train coded, test set not coded
####################################################################
train_tag5 <- read.table('./Research2026-002 data/TAG-5 data.txt', header = TRUE)

#--------------------------
# Validate
tag5 <- read.table('./Research2026-002 data/CCD-18 data.txt', header = TRUE)

#----------------------------------
# Ensemble modeling for TAG5
train_data <- train_tag5
test_data <- tag5[31, , drop = FALSE]
test_data_b <- tag5[32, , drop = FALSE]

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
      responses = c("COD"),
      predictors = predictors,
      rsm_formulas = rsm_formulas,
      design_type = "TAG",
      factor_ranges = factor_ranges,     # <-- required here
      excel_file="Metrics.xlsx"
  )

results <- doe_meta_model(
      train_data = train_data,
      test_data = test_data_b,
      responses = c("Decol"),
      predictors = predictors,
      rsm_formulas = rsm_formulas_b,
      design_type = "TAG",
      factor_ranges = factor_ranges,     # <-- required here
      excel_file="Metrics.xlsx"
)

####################################################################
# TAG6
####################################################################
tag6 <- read.table('./Research2026-002 data/TAG-6 data.txt', header = TRUE)

#---------------------------------
# Validate
train_tag6 <- tag6[1:16, , drop = FALSE]
test_tag6 <- tag6[17, , drop = FALSE]

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
# TAG7 Also FFD2 #MRR has no validation data
####################################################################
train_tag7 <- read.table('./Research2026-002 data/TAG-7 data.txt', header = TRUE)

test_tag7 <- read.table('./Research2026-002 data/TAG-7 FFD2 test.txt', header = TRUE)

#----------------------------------
# Ensemble modeling for TAG7
train_data <- train_tag7
test_data <- test_tag7

responses <- c("Ra")
predictors <- c('vc', 'f', 'alpha')

rsm_formulas <- list(
  Ra = Ra ~ SO(vc, f, alpha)
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
tag8 <- read.table('./Research2026-002 data/TAG-8 data.txt', header = TRUE)

#--------------------------
# Validate
#---------------------------------
# Train and test sets
train_tag8 <- tag8[1:25, , drop = FALSE]
test_tag8 <- tag8[26:27, , drop = FALSE]
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
# TAG9
####################################################################
tag9 <- read.table('./Research2026-002 data/TAG-9 data.txt', header = TRUE)

#---------------------------
# Convert CE to WET:-1,MQL:0,DRY2:1
tag9$CEc <- ifelse(tag9$CE == "WET", -1, ifelse(tag9$CE == "MQL", 0, 1))
# Convert tool_type to UNCOATED:-1,PVD:0,CVD:1
tag9$TT <- ifelse(tag9$Tool_type == "UNCOATED", -1, ifelse(tag9$Tool_type == "PVD", 0, 1))

#--------------------------
# Validate
train_tag9 <- tag9[1:27, , drop = FALSE]
test_tag9 <- tag9[28, , drop = FALSE]

#----------------------------------
# Ensemble modeling for TAG9
train_data <- train_tag9
test_data <- test_tag9

responses <- c('TW')
predictors <- c('CEc', 'NR', 'FR', 'DOC', 'TT')

rsm_formulas <- list(
  TW = TW ~ FO(CEc, NR, FR, DOC, TT) + CEc:NR + CEc:FR + CEc:TT
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
# TAG10
####################################################################
tag10 <- read.table('./Research2026-002 data/TAG-10 data.txt', header = TRUE)

#--------------------------
# Validate
train_tag10 <- tag10[1:9, , drop = FALSE]
test_tag10 <- tag10[10, , drop = FALSE]

#----------------------------------
# Ensemble modeling for TAG10
train_data <- train_tag10
test_data <- test_tag10

responses <- c("COF", "Ws")
predictors <- c('L', 'S', 'D')

rsm_formulas <- list(
  COF = COF ~ SO(L, S, D),
  Ws = Ws ~ SO(L, S, D)
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
tag11 <- read.table('./Research2026-002 data/TAG-11 data.txt', header = TRUE)

#--------------------------
# Validate
train_tag11 <- tag11[1:15, , drop = FALSE]
test_tag11 <- tag11[16:18, , drop = FALSE]

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
# TAG12 Need to read the paper to understand factors and responses
####################################################################
tag12 <- read.table('./Research2026-002 data/TAG-12 data.txt', header = TRUE)

#--------------------------
# Validate
train_tag12 <- tag12[1:16, , drop = FALSE]
test_tag12 <- tag12[17, , drop = FALSE]

#----------------------------------
# Ensemble modeling for TAG12
train_data <- train_tag12
test_data <- test_tag12

responses <- c("WGRG")
predictors <- c("Density", "Elongation", "Aspect_ratio", "Dilution")

rsm_formulas <- list(
  WGRG = WGRG ~ FO(Density, Elongation, Aspect_ratio, Dilution)
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
# TAG13
####################################################################
tag13 <- read.table('./Research2026-002 data/TAG-13 data.txt', header = TRUE)

#--------------------------
# Validate
train_tag13 <- tag13[1:32, , drop = FALSE]
test_tag13 <- tag13[33, , drop = FALSE]

#----------------------------------
# Ensemble modeling for TAG13
train_data <- train_tag13
test_data <- test_tag13

responses <- c("Impact")
predictors <- c("X1", "X2", "X3")

rsm_formulas <- list(
  Impact = Impact ~ FO(X1, X2, X3) + I(X1^2) + I(X2^2) + I(X3^2)
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
# TAG14 Also CCD21
####################################################################
train_tag14 <- read.table('./Research2026-002 data/TAG-14 data.txt', header = TRUE)
#--------------------------
# Validate
test_tag14 <- read.table('./Research2026-002 data/CCD-21 test.txt', header = TRUE)

#----------------------------------
# Ensemble modeling for TAG14
train_data <- train_tag14
test_data <- test_tag14

responses <- c("Kerf")
predictors <- c("A", "B", "C", "D", "E")

rsm_formulas <- list(
  Kerf = Kerf ~ FO(A, B, C, D, E) + TWI(A, B, C, D, E)
)

# MUST supply factor_ranges — the original natural-unit bounds.
factor_ranges <- list(
  A = c(1800, 2000),
  B = c(550, 700),
  C = c(1700, 1850),
  D = c(80, 85),
  E = c(1.5, 3)
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
# TAG15 Also BBD19
####################################################################
train_tag15 <- read.table('./Research2026-002 data/TAG-15 data.txt', header = TRUE)

#---------------------------------
# Validate
tag15_test <- read.table('./Research2026-002 data/BBD-19 data.txt', header = TRUE)
test_tag15 <- tag15_test[28, , drop = FALSE]

#----------------------------------
# Ensemble modeling for TAG15
train_data <- train_tag15
test_data <- test_tag15

responses <- "Y"
predictors <- c("Power",  "Pressure", "Frequency", "Speed")

rsm_formulas <- list(
  Y = Y ~ SO(Power, Pressure, Frequency, Speed)
)

# MUST supply factor_ranges — the original natural-unit bounds.
factor_ranges <- list(
  Power = c(1900, 2000),
  Pressure = c(15, 25),
  Frequency = c(19000, 20000),
  Speed = c(2.5, 5.5)
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

#--------------------------------------------------------------------
# Optional: Clean up H2O logs that are more than 7 days old. 
# Set dry_run = FALSE to delete the files.
cleanup_h2o_logs(max_age_days = 7, dry_run = FALSE)