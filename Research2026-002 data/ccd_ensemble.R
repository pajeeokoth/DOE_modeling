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
  Experimental = Experimental ~ SO(X1a, X2a, X3a)
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
  Removal = Removal ~ SO(Aa, Ba, Ca)
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
test_ccd6 <- test_ccd6[1:6, , drop = FALSE] # only use the first 6 rows for testing

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
  excel_file="Metrics.xlsx"
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
  RS = RS ~ FO(A, B) + I(B^2) + A:B,
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
  experiment = experiment ~ SO(x1, x2, x3, x4)
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
# CCD11 train coded, test could be
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
# CCD12
####################################################################
ccd12 <- read.csv('./Research2026-002 data/CCD-12 data.csv')

#--------------------
# Validate
#---------------------
train_ccd12 <- ccd12[1:20, , drop = FALSE]
test_ccd12 <- ccd12[21:22, , drop = FALSE]

#----------------------------------
# Ensemble modeling for CCD12
train_data <- train_ccd12
test_data <- test_ccd12

responses <- c('COD_removal', 'Biogas')
predictors <- c("HRT","Vup","Influent_COD")

rsm_formulas <- list(
  COD_removal = COD_removal ~ FO(HRT, Vup, Influent_COD) + I(HRT^2) + HRT:Vup + HRT:Influent_COD,
  Biogas = Biogas ~ FO(HRT, Vup, Influent_COD) + I(HRT^2) + I(Vup^2) + I(Influent_COD^2) + HRT:Influent_COD + Vup:Influent_COD + I(HRT^2):Vup + HRT:I(Vup^2)
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
# CCD13 train coded, test not coded
####################################################################
ccd13 <- read.csv('./Research2026-002 data/CCD-13 data.csv')

#------------------
# Validate
#------------------
train_ccd13 <- ccd13[1:20, , drop = FALSE]
test_ccd13 <- ccd13[21:23, , drop = FALSE]

#----------------------------------
# Ensemble modeling for CCD13
train_data <- train_ccd13
test_data <- test_ccd13

responses <- c('NTU', 'SVI')
predictors <- c("x1","x2","x3")

rsm_formulas <- list(
  NTU = NTU ~ SO(x1, x2, x3),
  SVI = SVI ~ SO(x1, x2, x3)
)

# MUST supply factor_ranges — the original natural-unit bounds.
factor_ranges <- list(
  x1 = c(0, 2000),
  x2 = c(0, 32),
  x3 = c(2, 10)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  factor_ranges = factor_ranges,     # <-- required here
  excel_file = "Metrics.xlsx"
)

####################################################################
# CCD14
####################################################################
ccd14 <- read.csv('./Research2026-002 data/CCD-14 data.txt'
                  , header = FALSE
                  , skip = 1
                  , sep = ""
                  , fill = TRUE
                  , stringsAsFactors = FALSE
                  , fileEncoding = "UTF-8")

# Define the column names
colnames(ccd14) <- c("Run","A","B",'C',"D",'therm',"pres")

#------------------
# Validate
train_ccd14 <- ccd14[1:30, , drop = FALSE]
test_ccd14 <- ccd14[31:33, , drop = FALSE]

#----------------------------------
# Ensemble modeling for CCD14
train_data <- train_ccd14
test_data <- test_ccd14

responses <- c('therm', 'pres')
predictors <- c("A","B",'C',"D")

rsm_formulas <- list(
  therm = therm ~ FO(A, B, C, D) + A:B,
  pres = pres ~ FO(A, B, C, D) + C:D
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
train_ccd15 <- ccd15[1:16, , drop = FALSE]
test_ccd15 <- ccd15[17:22, , drop = FALSE]

#----------------------------------
# Ensemble modeling for CCD15
train_data <- train_ccd15
test_data <- test_ccd15

responses <- c("surf","tang")
predictors <- c('A', 'B', 'C')

rsm_formulas <- list(
  surf = surf ~ FO(B) + I(C^2) + B:C,
  tang = tang ~ FO(A, B, C) + I(C^2) + TWI(B,C)
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
train_ccd16 <- ccd16[1:25, , drop = FALSE]
test_ccd16 <- ccd16[26:30, , drop = FALSE]

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
# CCD17 Already coded, no test set given
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
train_ccd17 <- ccd17[1:25, , drop = FALSE]
test_ccd17 <- ccd17[26:30, , drop = FALSE]

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
# CCD18 Also TAG5
####################################################################
ccd18 <- read.table('./Research2026-002 data/CCD-18 data.txt'
                  , header = TRUE
                  , skip = 0
                  , sep = ""
                  , fill = TRUE
                  , stringsAsFactors = FALSE
                  , fileEncoding = "UTF-8")

#------------------
# Validate
train_ccd18 <- ccd18[1:30, , drop = FALSE]
test_ccd18 <- ccd18[31, , drop = FALSE]
test_ccd18b <- ccd18[32, , drop = FALSE]

#----------------------------------
# Ensemble modeling for CCD18
train_data <- train_ccd18
test_data <- test_ccd18
test_data_b <- test_ccd18b

responses <- c("COD","Decol")
predictors <- c("Dye","DyeFe","H2O2Fe","pH")

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = list(COD   = COD ~ SO(Dye,DyeFe,H2O2Fe,pH)),
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data_b,
  responses = responses,
  predictors = predictors,
  rsm_formulas = list(Decol = Decol ~ SO(Dye,DyeFe,H2O2Fe,pH)),
  design_type = "CCD",
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD19 Coding off for B in test set
####################################################################
ccd19 <- read.table('./Research2026-002 data/CCD-19 data.txt', header = TRUE)

#------------------
# Train and test split
train_ccd19 <- ccd19[1:30, , drop = FALSE]
test_ccd19 <- ccd19[31:33, , drop = FALSE]

#----------------------------------
# Ensemble modeling for CCD19
train_data <- train_ccd19
test_data <- test_ccd19

responses <- 'Yield'
predictors <- c("A","B",'C',"D")

rsm_formulas <- list(
  Yield = Yield ~ SO(A,B,C,D)
)

# MUST supply factor_ranges — the original natural-unit bounds.
factor_ranges <- list(
  A = c(1, 5),
  B = c(10, 30),
  C = c(40, 200),
  D = c(60, 300)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  factor_ranges = factor_ranges,     # <-- required here
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD20
####################################################################
ccd20 <- read.table('./Research2026-002 data/CCD-20 data.txt'
                  , header = TRUE
                  , skip = 0
                  , sep = ""
                  , fill = TRUE
                  , stringsAsFactors = FALSE
                  , fileEncoding = "UTF-8")

#------------------
# Validate
#------------------
train_ccd20 <- ccd20[1:20, , drop = FALSE]
test_ccd20 <- ccd20[21:21, , drop = FALSE]

#----------------------------------
# Ensemble modeling for CCD20
train_data <- train_ccd20
test_data <- test_ccd20

responses <- "wear"
predictors <- c("Load","speed",'distance')

rsm_formulas <- list(
  wear = wear ~ FO(Load,speed,distance) + I(Load^2) + I(speed^2) + TWI(distance,Load) + TWI(speed,distance)
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
# CCD21 train coded, test not coded
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
  design_type = "CCD",
  factor_ranges = factor_ranges,     # <-- required here
  excel_file="Metrics.xlsx"
)

####################################################################
# CCD22
####################################################################
ccd22 <- read.table('./Research2026-002 data/CCD-22 data.txt', header = TRUE)

#------------------
# Validate
#------------------
train_ccd22 <- ccd22[1:22, , drop = FALSE]
test_ccd22 <- ccd22[23:24, , drop = FALSE]

#----------------------------------
# Ensemble modeling for CCD22
train_data <- train_ccd22
test_data <- test_ccd22

responses <- c('HRY', 'BR')
predictors <- c("steam","dryer","temp")

rsm_formulas <- list(
  HRY = HRY ~ FO(steam,dryer,temp) + TWI(steam,dryer) + TWI(dryer,temp) + I(steam^2),
  BR = BR ~ FO(steam,dryer,temp) + TWI(steam,dryer) + TWI(dryer,temp) + I(steam^2)
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
# CCD23 Not sure which paper this is from
####################################################################
ccd23 <- read.table('./Research2026-002 data/CCD-23 data.txt', header = TRUE)

#------------------
# Validate
#------------------
train_ccd23 <- ccd23[1:27, , drop = FALSE]
test_ccd23 <- ccd23[28:33, , drop = FALSE]

#----------------------------------
# Ensemble modeling for CCD23
train_data <- train_ccd23
test_data <- test_ccd23

responses <- c('SF_MPa', 'E_GPa')
predictors <- c("A","B","C","D")

rsm_formulas <- list(
  SF_MPa = SF_MPa ~ SO(A,B,C,D),
  E_GPa = E_GPa ~ SO(A,B,C,D)
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
# CCD24 Also DSD1
####################################################################
train_ccd24 <- read.table('./Research2026-002 data/CCD-24 data.txt', header = TRUE)

#------------------------
# Validate
test_ccd24 <- read.table('./Research2026-002 data/DSD-1 validation.txt', header = TRUE)

#----------------------------------
# Ensemble modeling for ccd24
train_data <- train_ccd24
test_data <- test_ccd24

responses <- c("y")
predictors <- c("x3", 'x7', 'x8')
 
rsm_formulas <- list(
  y = y ~ FO(x3, x7, x8) + x3:x7 + x3:x8 + I(x7^2) + I(x8^2)
)

# MUST supply factor_ranges — the original natural-unit bounds.
factor_ranges <- list(
  x3 = c(39.55, 140.45),
  x7 = c(65.91, 234.09),
  x8 = c(0.45, 10.54)
)

results <- doe_meta_model(
  train_data = train_data,
  test_data = test_data,
  responses = responses,
  predictors = predictors,
  rsm_formulas = rsm_formulas,
  design_type = "CCD",
  factor_ranges = factor_ranges,     # <-- required here
  excel_file="Metrics.xlsx"
)

#--------------------------------------------------------------------
# Optional: Clean up H2O logs that are more than 7 days old. 
# Set dry_run = FALSE to actually delete the files.
cleanup_h2o_logs(max_age_days = 7, dry_run = FALSE)  # Set to FALSE to delete files