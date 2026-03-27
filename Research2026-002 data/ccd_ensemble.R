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

source('./Research2026-002 data/utils.R') # save all results

# LOAD the data sets

####################################################################
# CCD1
####################################################################
train_ccd1 <- read.table('./Research2026-002 data/CCD-1 data.txt'
                         , header = TRUE
                         , stringsAsFactors = FALSE)
#------------------
# Validate
# -----------------
test_ccd1 <- read.table('./Research2026-002 data/CCd-1 validate data.txt'
                        , header = TRUE
                        , stringsAsFactors = FALSE)


#------------------
# Ensemble modeling for CCD1
train_data <- train_ccd1
test_data <- test_ccd1

responses <- 'Observed'
predictors <- c("A", "B",'C')

rsm_formulas <- list(
  Observed = Observed ~ SO(A, B, C) + A:B:C + I(A^2):B + I(A^2):C + A:I(B^2)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file = "Metrics.xlsx"
)

####################################################################
# CCD2
####################################################################
train_ccd2 <- read.csv('./Research2026-002 data/CCD-2 data.txt'
                         , header = TRUE)
#------------------
# Validate
#------------------
test_ccd2 <- read.table('./Research2026-002 data/CCD-2 validate data.txt', header = TRUE)

#------------------
# Ensemble modeling for CCD2
train_data <- train_ccd2
test_data <- test_ccd2

responses <- 'Experimental'
predictors <- c("X1a", "X2a",'X3a')

rsm_formulas <- list(
  Experimental = Experimental ~ SO(X1a, X2a,X3a)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file = "Metrics.xlsx"
)

####################################################################
# CCD3
####################################################################
train_ccd3 <- read.table('./Research2026-002 data/CCD-3 data.txt'
                         , header = FALSE
                         , skip = 2
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")
# Define the column names
colnames(train_ccd3) <- c("Run", "Ac", "Bc",  "Cc", 'Aa','Ba','Ca', "Removal")

#------------------
# Validate
#------------------
test_ccd3 <- read.table('./Research2026-002 data/CCD-3 validate data.txt'
                        , header = FALSE
                        , skip = 3
                        , sep = ""
                        , fill = TRUE
                        , stringsAsFactors = FALSE
                        , fileEncoding = "UTF-8")
# Define the column names
colnames(test_ccd3) <- c("Run", 'Aa','Ba','Ca', "Removal",'rsm_res', "ann_res")

#------------------
# Ensemble modeling for CCD3
train_data <- train_ccd3
test_data <- test_ccd3

responses <- 'Removal'
predictors <- c("Aa", "Ba",'Ca')

rsm_formulas <- list(
  Removal = Removal ~ SO(Aa, Ba,Ca)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file = "Metrics.xlsx"
)

####################################################################
# CCD4
####################################################################
train_ccd4 <- read.table('./Research2026-002 data/CCD-4 design.txt'
                         , header = FALSE
                         , skip = 5
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")
# Define the column names
colnames(train_ccd4) <- c("Run", "pulse_on", "pulse_off",  "serve_volt"
                          ,'peak_cur', 'wire', "kerf_width",'asr','mrr')
#------------------
# Validate
#------------------
test_ccd4 <- read.table('./Research2026-002 data/CCD-4 validate.txt'
                        , header = FALSE
                        , skip = 3
                        , sep = ""
                        , fill = TRUE
                        , stringsAsFactors = FALSE
                        , fileEncoding = "UTF-8")

# Define the column names
colnames(test_ccd4) <- c("Run", "pulse_on", "pulse_off",  "serve_volt", 'peak_cur'
                         ,'wire', "kerf_width",'asr','mrr','rsm_pk','rsm_pr'
                         ,'rsm_pm','ann_pk','ann_pr','ann_pm','rsm_ke','rsm_re'
                         ,'rsm_me','ann_ke','ann_re','ann_me')

##------------------
# Ensemble modeling for CCD4
train_data <- train_ccd4
test_data <- test_ccd4

responses <- c("kerf_width","asr", 'mrr')
predictors <- c("pulse_on", "pulse_off",  "serve_volt", 'peak_cur','wire')

rsm_formulas <- list(
  kerf_width = kerf_width ~ FO(pulse_on, pulse_off, serve_volt, peak_cur, wire) + I(pulse_off^2) + pulse_on:serve_volt + pulse_off:serve_volt + peak_cur:wire,
  asr = asr ~ FO(pulse_on, pulse_off, serve_volt, peak_cur, wire) + I(pulse_off^2) + I(serve_volt^2) + pulse_on:pulse_off + pulse_off:serve_volt + peak_cur:wire,
  mrr = mrr ~ FO(pulse_on, pulse_off, serve_volt, peak_cur, wire) + I(pulse_on^2) + I(serve_volt^2) +I(peak_cur^2) + I(wire^2) + pulse_on:serve_volt +  pulse_off:serve_volt + peak_cur:wire
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD5
####################################################################
train_ccd5 <- read.table('./Research2026-002 data/CCD-5 design.txt'
                         , header = FALSE
                         , skip = 3
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

# Define the column names
colnames(train_ccd5) <- c("Run", "x1", "x2",  "x3", 'x4','experiment', "rsm_p",'rsm_d','ann_p','ann_d')

#------------------
# Validate
#------------------
test_ccd5 <- read.table('./Research2026-002 data/CCD-5 validate data.txt'
                        , header = FALSE
                        , skip = 3
                        , sep = ""
                        , fill = TRUE
                        , stringsAsFactors = FALSE
                        , fileEncoding = "UTF-8")

# Define the column names
colnames(test_ccd5) <- c("Run", "x1", "x2",  "x3", 'x4','experiment', "rsm_p",'rsm_d','ann_p','ann_d')

#------------------
# Ensemble modeling for CCD5
train_data <- train_ccd5
test_data <- test_ccd5

responses <- 'experiment'
predictors <- c("x1", "x2",  "x3", 'x4')

rsm_formulas <- list(
  experiment = experiment ~ SO(x1, x2,x3,x4)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD6
####################################################################
train_ccd6 <- read.table('./Research2026-002 data/CCD-6 data.txt'
                         , header = FALSE
                         , skip = 1
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

# Define the column names
colnames(train_ccd6) <- c("Run","temp","t",'ph',"enz",'experiment','pred',"ann")

#------------------
# Validate
#------------------
test_ccd6 <- read.table('./Research2026-002 data/CCD-6 validate.txt'
                         , header = FALSE
                         , skip = 1
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

# Define the column names
colnames(test_ccd6) <- c("Run","temp","t",'ph',"enz",'experiment','pred')

#------------------
# Ensemble modeling for CCD6
train_data <- train_ccd6
test_data <- test_ccd6

responses <- 'experiment'
predictors <- c("temp","t",'ph',"enz")

rsm_formulas <- list(
  experiment = experiment ~ SO(temp,t,ph,enz)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file="DOE_Metrics.xlsx"
)

####################################################################
# CCD7
####################################################################
train_ccd7 <- read.table('./Research2026-002 data/CCD-7 train.txt'
                         , header = FALSE
                         , skip = 2
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

# Define the column names
colnames(train_ccd7) <- c("Run", "x1", "x2",'con', "x3_l","bl", "d80")

# Convert x3 to A:-1,B:0,C:1
train_ccd7$x3 <- ifelse(train_ccd7$x3_l == "A", -1
                       , ifelse(train_ccd7$x3_l == "B", 0, 1))

#------------------
# Validate
#------------------
test_ccd7a <- read.table('./Research2026-002 data/CCD-7 validate part 1.txt'
                        , header = FALSE
                        , skip = 2
                        , sep = ""
                        , fill = TRUE
                        , stringsAsFactors = FALSE
                        , fileEncoding = "UTF-8")

# Define the column names
colnames(test_ccd7a) <- c("Run", "x1", "x2","x3_l","bl", "d80",'des')

test_ccd7b <- read.table('./Research2026-002 data/CCD-7 validate part 2 need to combine with part 1.txt'
                        , header = FALSE
                        , skip = 2
                        , sep = ""
                        , fill = TRUE
                        , stringsAsFactors = FALSE
                        , fileEncoding = "UTF-8")

# Define the column names
colnames(test_ccd7b) <- c("Run", "x1", "x2","x3_l","bl", "d80",'des')

# combine testa with testb
test_ccd7 <- rbind(test_ccd7a,test_ccd7b)

# Convert x3 to A:-1,B:0,C:1
test_ccd7$x3 <- ifelse(test_ccd7$x3_l == "A", -1
                           , ifelse(test_ccd7$x3_l == "B", 0, 1))

#----------------------------------
# Ensemble modeling for CCD7
train_data <- train_ccd7
test_data <- test_ccd7

responses <- c("bl","d80")
predictors <- c('x1', 'x2', 'x3')

rsm_formulas <- list(
  bl = bl ~ SO(x1, x2) + x3,
  d80 = d80 ~ SO(x1, x2) + x3
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD8
####################################################################
train_ccd8 <- read.csv('./Research2026-002 data/CCD-8.csv')

# Convert MP and  UF to Type I:-1, Type II:0, Type III:1
train_ccd8$MP <- recode(train_ccd8$MP_l,
                           "Type I" = -1,
                           "Type II" =  0,
                           "Type III" =  1)
train_ccd8$UF <- recode(train_ccd8$UF_l,
                            "Type I" = -1,
                            "Type II" =  0,
                            "Type III" =  1)

#------------------
# Validate
#------------------
test_ccd8 <- read.csv('./Research2026-002 data/CCD-8test.csv')

# Convert MP and  UF to Type I:-1, Type II:0, Type III:1
test_ccd8$MP <- recode(test_ccd8$MP_l,
                            "Type I" = -1,
                            "Type II" =  0,
                            "Type III" =  1)
test_ccd8$UF <- recode(test_ccd8$UF_l,
                            "Type I" = -1,
                            "Type II" =  0,
                            "Type III" =  1)

#----------------------------------
# Ensemble modeling for CCD8
train_data <- train_ccd8
test_data <- test_ccd8

responses <- c("CI","COI")
predictors <- c('EP', 'Noise', 'MP', 'UF')

rsm_formulas <- list(
  CI = CI ~ FO(EP,Noise) + I(EP^2) + I(Noise^2) + EP:I(Noise^2) + MP + UF,
  COI = COI ~ FO(EP,Noise) + I(EP^2) + I(Noise^2) + EP:I(Noise^2) + MP + UF
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD9
####################################################################
train_ccd9 <- read.table('./Research2026-002 data/CCD-9 data.txt'
                         , header = FALSE
                         , skip = 2
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

# Define the column names
colnames(train_ccd9) <- c("Run","A","B",'RS',"PGI",'GL',"rsmr",'annr'
                          ,'rsmp','annp','rsmg','anng')
#------------------
# Validate
#------------------
test_ccd9 <- read.table('./Research2026-002 data/CCD-9 test.txt'
                         , header = FALSE
                         , skip = 1
                         , sep = ""
                         , fill = TRUE
                         , stringsAsFactors = FALSE
                         , fileEncoding = "UTF-8")

# Define the column names
colnames(test_ccd9) <- c("Run","A","B",'RSp',"PGIp",'GLp',"RS",'PGI','GL')

#----------------------------------
# Ensemble modeling for CCD9
train_data <- train_ccd9
test_data <- test_ccd9

responses <- c("RS","PGI", 'GL')
predictors <- c('A', 'B')

rsm_formulas <- list(
  RS = RS ~ FO(A, B) + I(A^2) + I(B^2) + A:B,
  PGI = PGI ~ SO(A, B),
  GL = GL ~ FO(A, B) + I(B^2)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD10
####################################################################
train_ccd10 <- read.csv('./Research2026-002 data/CCD-10 data.txt'
                         , header = TRUE
                         , check.names = FALSE
                         )

# Define the column names
colnames(train_ccd10) <- c("Run","x1","x2",'x3',"x4",'experiment',"rsm",'ann')

#------------------
# Validate
#------------------
test_ccd10 <- read.csv('./Research2026-002 data/CCD-10 test.txt')

# Define the column names
colnames(test_ccd10) <- c("Run","x1","x2",'x3',"x4",'experiment')

#----------------------------------
# Ensemble modeling for CCD10
train_data <- train_ccd10
test_data <- test_ccd10

responses <- 'experiment'
predictors <- c("x1","x2",'x3',"x4")

rsm_formulas <- list(
  experiment = experiment ~ SO(x1,x2, x3, x4)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD11
####################################################################
ccd11 <- read.csv('./Research2026-002 data/CCD-11 data.txt', header = TRUE)

#------------------
# Validate
#------------------
train_ccd11 <- ccd11[1:20, , drop = FALSE]
test_ccd11 <- ccd11[21:23, , drop = FALSE]

#----------------------------------
# Ensemble modeling for CCD11
train_data <- train_ccd11
test_data <- test_ccd11

responses <- 'TSFL_kN'
predictors <- c("N","P",'D')

rsm_formulas <- list(
  TSFL_kN = TSFL_kN ~ SO(N, P, D)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD15
####################################################################
ccd15 <- read.csv('./Research2026-002 data/CCD-15 data.txt'
                  , header = FALSE
                  , skip = 1
                  , sep = ""
                  , fill = TRUE
                  , stringsAsFactors = FALSE
                  , fileEncoding = "UTF-8")

# Define the column names
colnames(ccd15) <- c('stdno',"Run","A","B",'C','surf',"tang")

#------------------
# Validate
#------------------
train_ccd15 = as.data.frame(ccd15)[1:16, ]
test_ccd15 = as.data.frame(ccd15)[17:22, ]

#----------------------------------
# Ensemble modeling for CCD15
train_data <- train_ccd15
test_data <- test_ccd15

responses <- c("surf","tang")
predictors <- c('A', 'B', 'C')

rsm_formulas <- list(
  surf = surf ~ FO(A) + I(C^2) + B:C,
  tang = tang ~ SO(A, B, C) + I(C^2) + TWI(B,C)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD16
####################################################################
ccd16 <- read.csv('./Research2026-002 data/CCD-16 data.txt'
                  , header = FALSE
                  , skip = 1
                  , sep = ""
                  , fill = TRUE
                  , stringsAsFactors = FALSE
                  , fileEncoding = "UTF-8")

# Define the column names
colnames(ccd16) <- c("Run","x1","x2",'x3','x4',"wh")

#------------------
# Validate
#------------------
train_ccd16 = as.data.frame(ccd16)[1:25, ]
test_ccd16 = as.data.frame(ccd16)[26:30, ]

#----------------------------------
# Ensemble modeling for CCD16
train_data <- train_ccd16
test_data <- test_ccd16

responses <- 'wh'
predictors <- c("x1","x2",'x3',"x4")

rsm_formulas <- list(
  wh = wh ~ SO(x1, x2, x3, x4)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD17
####################################################################
ccd17 <- read.table('./Research2026-002 data/CCD-17 data.txt'
                  , header = FALSE
                  , skip = 1
                  , sep = ""
                  , fill = TRUE
                  , stringsAsFactors = FALSE
                  , fileEncoding = "UTF-8")

# Define the column names
colnames(ccd17) <- c("Run","x1","x2",'x3','x4',"y",'rsm','error')

#------------------
# Validate
#------------------
train_ccd17 = as.data.frame(ccd17)[1:25, ]
test_ccd17 = as.data.frame(ccd17)[26:30, ]

#----------------------------------
# Ensemble modeling for CCD17
train_data <- train_ccd17
test_data <- test_ccd17

responses <- 'y'
predictors <- c("x1","x2",'x3',"x4")

rsm_formulas <- list(
  y = y ~ FO(x1,x2,x3,x4) + I(x1^2) + I(x2^2) + I(x3^2) + I(x4^2) + TWI(x3,x4)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD19
####################################################################
ccd19 <- read.table('./Research2026-002 data/CCD-19 data.txt', header = TRUE)

#------------------
# Train and test split
train_ccd19 = as.data.frame(ccd19)[1:30, ]
test_ccd19 = as.data.frame(ccd19)[31:33, ]

#----------------------------------
# Ensemble modeling for CCD19
train_data <- train_ccd19
test_data <- test_ccd19

responses <- 'Yield'
predictors <- c("A","B",'C',"D")

rsm_formulas <- list(
  Yield = Yield ~ SO(A,B,C,D)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD21
####################################################################
ccd21 <- read.table('./Research2026-002 data/CCD-21 data.txt', header = TRUE)
#------------------
# Validate
#------------------
test_ccd21 <- read.table('./Research2026-002 data/CCD-21 test.txt', header = TRUE)

#----------------------------------
# Ensemble modeling for CCD21
train_data <- ccd21
test_data <- test_ccd21

responses <- 'Kerf'
predictors <- c("A","B",'C',"D",'E')

rsm_formulas <- list(
  Kerf = Kerf ~ FO(A,B,C,D,E) + TWI(A,B,C,D,E)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)