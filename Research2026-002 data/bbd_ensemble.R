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
# BBD4
####################################################################
bbd4 <- read.table('./Research2026-002 data/BBD-4 data.txt'
                         , header = FALSE
                         , skip = 1
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

# Define the column names
colnames(bbd4) <- c("Run", "tn", "ts",  "lh"
                          ,'ws','w1','w1a'
                          ,'pt','p1','p1a'
                          ,'epc','e1','e1a'
                          , "sh",'s1','s1a')


bbd4 <- as.data.frame(bbd4)
train_bbd4 <- sample(bbd4[1:15,])
test_bbd4 <- bbd4[sample(16:17),]

#----------------------------------
# Ensemble modeling for BBD4
train_data <- train_bbd4
test_data <- test_bbd4

responses <- c("ws","pt", 'epc', 'sh')
predictors <- c("tn", "ts",  "lh")

rsm_formulas <- list(
  ws = ws ~ FO(tn, ts, lh) + I(tn^2) + I(ts^2) + I(lh^2),
  pt = pt ~ FO(tn, ts, lh) + I(tn^2) + I(ts^2) + I(lh^2),
  epc = epc ~ FO(tn, ts, lh) + I(tn^2) + I(ts^2) + I(lh^2),
  sh = sh ~ FO(tn, ts, lh) + I(tn^2) + I(ts^2) + I(lh^2)
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
                    , header = FALSE
                    , skip = 1
                    , sep = ""
                    , fill = TRUE
                    , stringsAsFactors = FALSE
                    , fileEncoding = "UTF-8")

# Define the column names
colnames(bbd5) <- c("Run", "dye", "yea",  "ph", 'dec','rsmp','annp')

train_bbd5 <- sample(bbd5[1:17,])
test_bbd5 <- sample(bbd5[18:19,])

#----------------------------------
# Ensemble modeling for BBD5
train_data <- train_bbd5
test_data <- test_bbd5

responses <- 'dec'
predictors <- c("dye", "yea",'ph')

rsm_formulas <- list(
  dec = dec ~ SO(dye,yea, ph)
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
                         , header = FALSE
                         , skip = 1
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

# Define the column names
colnames(train_bbd6) <- c("Run", "v1", 'pes','pvp','bf','airg','pwp','por')


###################
# Validate
test_bbd6 <- read.table('./Research2026-002 data/BBD-6 test.txt'
                         , header = FALSE
                         , skip = 1
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

# Define the column names
colnames(test_bbd6) <- c("Run", "v1", 'pes','pvp','bf','airg','pwp','por')

#----------------------------------
# Ensemble modeling for BBD6
train_data <- train_bbd6
test_data <- test_bbd6

responses <- c("pwp","por")
predictors <- c('pes','pvp','bf','airg')

rsm_formulas <- list(
  pwp = pwp ~ SO(pes, pvp, bf, airg),
  por = por ~ SO(pes, pvp, bf, airg)
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
# BBD9
####################################################################
bbd9 <- read.table('./Research2026-002 data/BBD-9 data.txt'
                    , header = TRUE
                    , skip = 0
                    , sep = ""
                    , fill = TRUE
                    , stringsAsFactors = FALSE
                    , fileEncoding = "UTF-8")

###################
# Train and test sets
bbd9 <- as.data.frame(bbd9)
train_bbd9 <- sample(bbd9[1:17,])
test_bbd9 <- as.data.frame(bbd9)[18:22, ]

#----------------------------------
# Ensemble modeling for BBD9
train_data <- train_bbd9
test_data <- test_bbd9

responses <- 'Y'
predictors <- c("A", "B",'C')

rsm_formulas <- list(
  Y = Y ~ FO(A, B, C) + I(A^2) + I(B^2) + I(C^2) + A:B
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
# BBD12
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
  Experimental = Experimental ~ SO(A, B, C) + I(A^2)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "BBD",
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
train_bbd18 <- sample(bbd18[1:27,])
test_bbd18 <- sample(bbd18[28:29,])

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