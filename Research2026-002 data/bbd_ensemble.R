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
# BBD1
####################################################################
train_bbd1 <- read.table('./Research2026-002 data/BBD-1 data.txt', header = TRUE)
#---------------------------
# Validate
#---------------------------
test_bbd1  <- read.table('./Research2026-002 data/BBD-1 test.txt', header = TRUE)

#----------------------------------
# Ensemble modeling for BBD1
train_data <- train_bbd1
test_data <- test_bbd1

responses <- c("t","KGM", 'WI')
predictors <- c('DT', 'DV', 'MT')

rsm_formulas <- list(
  t = t ~ SO(DT, DV, MT),
  KGM = KGM ~ SO(DT, DV, MT),
  WI = WI ~ SO(DT, DV, MT)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD2 Optional
####################################################################
bbd2 <- read.table('./Research2026-002 data/BBD-2 data.txt'
                   , header = TRUE
                   , skip = 0
                   , sep = ""
                   , fill = TRUE
                   , stringsAsFactors = FALSE
                   , fileEncoding = "UTF-8")

#---------------------------
# Validate
#---------------------------
bbd2 <- as.data.frame(bbd2)
train_bbd2 <- sample(bbd2[1:17,])
test_bbd2 <- bbd2[sample(18:18),]

#----------------------------------
# Ensemble modeling for BBD2
train_data <- train_bbd2
test_data <- test_bbd2

responses <- c("Biomass","CaCO3")
predictors <- c("A", "B",  "F")

rsm_formulas <- list(
  Biomass = Biomass ~ SO(A, B, F),
  CaCO3 = CaCO3 ~ SO(A, B, F)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD3 Optional
####################################################################
bbd3 <- read.table('./Research2026-002 data/BBD-3 data.txt'
                   , header = TRUE
                   , skip = 0
                   , sep = ""
                   , fill = TRUE
                   , stringsAsFactors = FALSE
                   , fileEncoding = "UTF-8")

#---------------------------
# Validate
bbd3 <- as.data.frame(bbd3)
train_bbd3 <- sample(bbd3[1:17,])
test_bbd3 <- bbd3[sample(18:18),]

#----------------------------------
# Ensemble modeling for BBD3
train_data <- train_bbd3
test_data <- test_bbd3

responses <- c("Yield")
predictors <- c("A", "B",  "C")

rsm_formulas <- list(
  Yield = Yield ~ SO(A, B, C)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD4
####################################################################
bbd4 <- read.table('./Research2026-002 data/BBD-4 data.txt'
                         , header = TRUE
                         , skip = 0
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

#---------------------------
# Validate
bbd4 <- as.data.frame(bbd4)
train_bbd4 <- sample(bbd4[1:15,])
test_bbd4 <- bbd4[sample(16:17),]

#----------------------------------
# Ensemble modeling for BBD4
train_data <- train_bbd4
test_data <- test_bbd4

responses <- c("WS","PT", 'EPC', "B", 'E', "Tt", "SPE", "SPP")
predictors <- c("TN", "TS",  "LH")

rsm_formulas <- list(
  WS = WS ~ FO(TN, TS, LH) + I(TN^2) + I(TS^2) + I(LH^2),
  PT = PT ~ FO(TN, TS, LH) + I(TN^2) + I(TS^2) + I(LH^2),
  EPC = EPC ~ FO(TN, TS, LH) + I(TN^2) + I(TS^2) + I(LH^2),
  B = B ~ FO(TN, TS, LH) + I(TN^2) + I(TS^2) + I(LH^2),
  E = E ~ FO(TN, TS, LH),
  Tt = Tt ~ FO(TN, TS, LH),
  SPE = SPE ~ FO(TN, TS, LH) + I(TN^2) + I(TS^2) + I(LH^2),
  SPP = SPP ~ FO(TN, TS, LH) + I(TN^2) + I(TS^2) + I(LH^2)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD5
####################################################################
bbd5 <- read.table('./Research2026-002 data/BBD-5 data.txt'
                    , header = TRUE
                    , skip = 0
                    , sep = ""
                    , fill = TRUE
                    , stringsAsFactors = FALSE
                    , fileEncoding = "UTF-8")

#---------------------------
# Validate
train_bbd5 <- sample(bbd5[1:17,])
test_bbd5 <- sample(bbd5[18:19,])

#----------------------------------
# Ensemble modeling for BBD5
train_data <- train_bbd5
test_data <- test_bbd5

responses <- "Decol"
predictors <- c("A", "B", "C")

rsm_formulas <- list(
  Decol = Decol ~ SO(A, B, C)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD6
####################################################################
train_bbd6 <- read.table('./Research2026-002 data/BBD-6 data.txt'
                         , header = TRUE
                         , skip = 0
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

#-----------------
# Validate
test_bbd6 <- read.table('./Research2026-002 data/BBD-6 test.txt'
                         , header = TRUE
                         , skip = 0
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

#----------------------------------
# Ensemble modeling for BBD6
train_data <- train_bbd6
test_data <- test_bbd6

responses <- c("PWP","Porosity")
predictors <- c('PES','PVP','BF','Airgap')

rsm_formulas <- list(
  PWP = PWP ~ SO(PES, PVP, BF, Airgap),
  Porosity = Porosity ~ SO(PES, PVP, BF, Airgap)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD7 Optional
####################################################################
bbd7 <- read.table('./Research2026-002 data/BBD-7 data.txt'
                   , header = TRUE
                   , skip = 0
                   , sep = ""
                   , fill = TRUE
                   , stringsAsFactors = FALSE
                   , fileEncoding = "UTF-8")

#-------------------------------
# Validate
bbd7 <- as.data.frame(bbd7)
train_bbd7 <- sample(bbd7[1:15,])
test_bbd7 <- bbd7[sample(16:16),]

#----------------------------------
# Ensemble modeling for BBD7
train_data <- train_bbd7
test_data <- test_bbd7

responses <- c("Weight","PrintTime", "sB", 'EPC', 'SPE', 'SPP', "E", "Toughness")
predictors <- c("LT", "NT",  "PS")

rsm_formulas <- list(
  Weight = Weight ~ SO(LT, NT, PS),
  PrintTime = PrintTime ~ SO(LT, NT, PS),
  sB = sB ~ SO(LT, NT, PS),
  EPC = EPC ~ SO(LT, NT, PS),
  SPE = SPE ~ SO(LT, NT, PS),
  SPP = SPP ~ SO(LT, NT, PS),
  E = E ~ SO(LT, NT, PS),
  Toughness = Toughness ~ SO(LT, NT, PS)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD8 Optional Data issue with test (limits are outside train set)
####################################################################
train_bbd8 <- read.table('./Research2026-002 data/BBD-8 data.txt'
                   , header = TRUE
                   , skip = 0
                   , sep = ""
                   , fill = TRUE
                   , stringsAsFactors = FALSE
                   , fileEncoding = "UTF-8")


#-------------------------------
# Validate
test_bbd8 <- read.table('./Research2026-002 data/BBD-8 test.txt'
                   , header = TRUE
                   , skip = 0
                   , sep = ""
                   , fill = TRUE
                   , stringsAsFactors = FALSE
                   , fileEncoding = "UTF-8")

#----------------------------------
# Ensemble modeling for BBD8
train_data <- train_bbd8
test_data <- test_bbd8

responses <- c("Y2","Y6")
predictors <- c("A", "C",  "D")

rsm_formulas <- list(
  Y2 = Y2 ~ FO(A, C, D) + I(A^2) + I(C^2) + I(D^2),
  Y6 = Y6 ~ FO(A, C, D) + I(A^2) + I(C^2) + I(D^2)
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
  design_type = "BBD",
  factor_ranges = factor_ranges,     # <-- required here
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD9 No test set for response U
####################################################################
bbd9 <- read.table('./Research2026-002 data/BBD-9 data.txt'
                    , header = TRUE
                    , skip = 0
                    , sep = ""
                    , fill = TRUE
                    , stringsAsFactors = FALSE
                    , fileEncoding = "UTF-8")

#---------------------------------
# Validate
bbd9 <- as.data.frame(bbd9)
train_bbd9 <- bbd9[1:17,]
test_bbd9 <- bbd9[18:22, ]

#----------------------------------
# Ensemble modeling for BBD9
train_data <- train_bbd9
test_data <- test_bbd9

responses <- 'Y'
predictors <- c("A", "B",'C')

rsm_formulas <- list(
  Y = Y ~ FO(A, B, C) + I(A^2) + I(B^2) + I(C^2) + A:B
)

# Train is coded (-1,0,1), test is in natural units.
# Derive ranges from test data extremes:
#   A: 20–24 → c(20, 24)
#   B: 27.64–31 → c(27.64, 31)
#   C: 2.5–2.74 → c(2.5, 2.74)
factor_ranges <- list(
  A = c(20, 24),
  B = c(27.64, 31),
  C = c(2.5, 2.74)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  factor_ranges = factor_ranges,     # <-- required here
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD10 Optional
####################################################################
bbd10 <- read.table('./Research2026-002 data/BBD-10 data.txt'
                   , header = FALSE
                   , skip = 1
                   , sep = ""
                   , fill = TRUE
                   , stringsAsFactors = FALSE
                   , fileEncoding = "UTF-8")

# Define the column names
colnames(bbd10) <- c("Run","x1","x2","x3",'y1','y2','y3','y4','y5')


bbd10 <- as.data.frame(bbd10)
train_bbd10 <- sample(bbd10[1:15,])
test_bbd10 <- bbd10[sample(16:16),]

#----------------------------------
# Ensemble modeling for BBD10
train_data <- train_bbd10
test_data <- test_bbd10

responses <- c("y1","y2","y3","y4","y5")
predictors <- c("x1", "x2",  "x3")

rsm_formulas <- list(
  y1 = y1 ~ FO(x1, x2, x3),
  y2 = y2 ~ FO(x1, x2, x3) + TWI(x1, x2, x3) + I(x1^2) + I(x2^2) + I(x3^2),
  y3 = y3 ~ FO(x1, x2, x3),
  y4 = y4 ~ FO(x1, x2, x3) + TWI(x1, x2, x3) + I(x1^2) + I(x2^2) + I(x3^2),
  y5 = y5 ~ FO(x1, x2, x3) + TWI(x1, x2, x3) + I(x1^2) + I(x2^2) + I(x3^2)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD11 Optional no test set provided
####################################################################
bbd11 <- read.table('./Research2026-002 data/BBD-11 data.txt'
                   , header = FALSE
                   , skip = 1
                   , sep = ""
                   , fill = TRUE
                   , stringsAsFactors = FALSE
                   , fileEncoding = "UTF-8")

# Define the column names
colnames(bbd11) <- c("Run","power","irradiation", "solvent","pH","Colchicine")

#-------------------------------
# Validate
bbd11 <- as.data.frame(bbd11)
train_bbd11 <- sample(bbd11[1:21,])
test_bbd11 <- bbd11[sample(22:27),]

#----------------------------------
# Ensemble modeling for BBD11
train_data <- train_bbd11
test_data <- test_bbd11

responses <- c("Colchicine")
predictors <- c("power", "irradiation", "solvent", "pH")

rsm_formulas <- list(
  Colchicine = Colchicine ~ SO(power, irradiation, solvent, pH)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD12 from text-pg105
####################################################################
bbd12 <- read.table('./Research2026-002 data/BBD-12 data.txt'
                    , header = TRUE
                    , skip = 0
                    , sep = ""
                    , fill = TRUE
                    , stringsAsFactors = FALSE
                    , fileEncoding = "UTF-8")
#------------------
# Validate
#------------------
train_bbd12 <- bbd12[1:15, , drop = FALSE]
test_bbd12 <- bbd12[16:18, , drop = FALSE]

#----------------------------------
# Ensemble modeling for BBD12
train_data <- train_bbd12
test_data <- test_bbd12

responses <- 'Experimental'
predictors <- c("A", "B",'C')

rsm_formulas <- list(
  Experimental = Experimental ~ SO(A, B, C)
)

# MUST supply factor_ranges — the original natural-unit bounds.
factor_ranges <- list(
  A = c(-1, 1),
  B = c(0.5, 1.5),
  C = c(4, 12)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  factor_ranges = factor_ranges,     # <-- required here
  excel_file = "Metrics.xlsx"
)

####################################################################
# BBD13
####################################################################
train_bbd13 <- read.table('./Research2026-002 data/BBD-13 data.txt'
                          , header = TRUE)

test_bbd13 <- read.table('./Research2026-002 data/BBD-13 dsd5tag4 test.txt'
                         , header = TRUE
                         , skip = 0
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

#----------------------------------
# Ensemble modeling for BBD13
train_data <- train_bbd13
test_data <- test_bbd13

responses <- 'Hardness'
predictors <- c('LT',	'ID',	'NT',	'PS')

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
  design_type = "BBD",
  factor_ranges = factor_ranges,     # <-- required here
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD15 Optional no test set provided
####################################################################
bbd15 <- read.table('./Research2026-002 data/BBD-15 data.txt'
                   , header = TRUE
                   , skip = 0
                   , sep = ""
                   , fill = TRUE
                   , stringsAsFactors = FALSE
                   , fileEncoding = "UTF-8")

#---------------------------------
# Validate
bbd15 <- as.data.frame(bbd15)
train_bbd15 <- sample(bbd15[1:22,])
test_bbd15 <- bbd15[sample(23:27),]

#----------------------------------
# Ensemble modeling for BBD15
train_data <- train_bbd15
test_data <- test_bbd15

responses <- "y"
predictors <- c("f1", "f2",  "f3", "f4")

rsm_formulas <- list(
  y = y ~ FO(f1, f2, f3, f4) + I(f1^2) + I(f2^2) + I(f3^2) + I(f4^2) + f3:f4
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD16 
####################################################################
bbd16 <- read.table('./Research2026-002 data/BBD-16 data.txt'
                    , header = TRUE
                    , skip = 0
                    , sep = ""
                    , fill = TRUE
                    , stringsAsFactors = FALSE
                    , fileEncoding = "UTF-8")

#---------------------------------
# Train and test sets
bbd16 <- as.data.frame(bbd16)
train_bbd16 <- sample(bbd16[1:15,])
test_bbd16 <- sample(bbd16[16:19,])

#----------------------------------
# Ensemble modeling for BBD16
train_data <- train_bbd16
test_data <- test_bbd16

responses <- c("Y1","Y2", 'Y3', 'Y4', 'Y5')
predictors <- c("A", "B",  "C")

rsm_formulas <- list(
  Y1 = Y1 ~ SO(A, B,  C),
  Y2 = Y2 ~ SO(A, B, C),
  Y3 = Y3 ~ SO(A, B, C),
  Y4 = Y4 ~ SO(A, B, C),
  Y5 = Y5 ~ SO(A, B, C)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD17
####################################################################
bbd17 <- read.table('./Research2026-002 data/BBD-17 data.txt'
                    , header = TRUE
                    , skip = 0
                    , sep = ""
                    , fill = TRUE
                    , stringsAsFactors = FALSE
                    , fileEncoding = "UTF-8")

#---------------------------------
# Train and test sets
bbd17 <- as.data.frame(bbd17)
train_bbd17 <- sample(bbd17[1:27,])
test_bbd17 <- sample(bbd17[28:30,])

#----------------------------------
# Ensemble modeling for BBD17
train_data <- train_bbd17
test_data <- test_bbd17

responses <- 'Moisture'
predictors <- c('x1',	'x2',	'x3',	'x4')

rsm_formulas <- list(
  Moisture = Moisture ~ SO(x1,x2,x3,x4)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD18
####################################################################
bbd18 <- read.table('./Research2026-002 data/BBD-18 data.txt'
                    , header = TRUE
                    , skip = 0
                    , sep = ""
                    , fill = TRUE
                    , stringsAsFactors = FALSE
                    , fileEncoding = "UTF-8")

#---------------------------------
# Train and test sets
bbd18 <- as.data.frame(bbd18)
train_bbd18 <- bbd18[1:27, , drop = FALSE]
test_bbd18 <- bbd18[28,, drop = FALSE]

#----------------------------------
# Ensemble modeling for BBD18
train_data <- train_bbd18
test_data <- test_bbd18

responses <- c("MRR","EWR")
predictors <- c("Ip",  "Ton", "Tau", "SV")

rsm_formulas <- list(
  MRR = MRR ~ SO(Ip, Ton, Tau, SV),
  EWR = EWR ~ SO(Ip, Ton, Tau, SV)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

####################################################################
# BBD19
####################################################################
bbd19 <- read.table('./Research2026-002 data/BBD-19 data.txt'
                    , header = TRUE
                    , skip = 0
                    , sep = ""
                    , fill = TRUE
                    , stringsAsFactors = FALSE
                    , fileEncoding = "UTF-8")

#---------------------------------
# Train and test sets
bbd19 <- as.data.frame(bbd19)
train_bbd19 <- bbd19[1:27, , drop = FALSE]
test_bbd19 <- bbd19[28,, drop = FALSE]

#----------------------------------
# Ensemble modeling for BBD19
train_data <- train_bbd19
test_data <- test_bbd19

responses <- "Y"
predictors <- c("Power",  "Pressure", "Frequency", "Speed")

rsm_formulas <- list(
  Y = Y ~ SO(Power, Pressure, Frequency, Speed)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
  excel_file="Metrics.xlsx"
)

#--------------------------------------------------------------------
# Optional: Clean up H2O logs that are more than 7 days old. 
# Set dry_run = FALSE to actually delete the files.
cleanup_h2o_logs(max_age_days = 7, dry_run = FALSE) 