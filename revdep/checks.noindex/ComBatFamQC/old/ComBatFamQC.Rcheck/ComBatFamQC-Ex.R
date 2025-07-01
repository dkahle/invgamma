pkgname <- "ComBatFamQC"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ComBatFamQC')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("adni")
### * adni

flush(stderr()); flush(stdout())

### Name: adni
### Title: Harmonization Data
### Aliases: adni
### Keywords: datasets

### ** Examples

data(adni)
head(adni)



cleanEx()
nameEx("age_df")
### * age_df

flush(stderr()); flush(stdout())

### Name: age_df
### Title: Age Trajectory Data
### Aliases: age_df
### Keywords: datasets

### ** Examples

data(age_df)
head(age_df)



cleanEx()
nameEx("age_list_gen")
### * age_list_gen

flush(stderr()); flush(stdout())

### Name: age_list_gen
### Title: Age Trend Estimates Generation
### Aliases: age_list_gen

### ** Examples

sub_df <- age_df[,c("Volume_1", "age", "sex", "ICV_baseline")] |> na.omit()
colnames(sub_df) <- c("Volume_1", "age", "sex", "icv")
age_list_gen(sub_df = sub_df)



cleanEx()
nameEx("age_save")
### * age_save

flush(stderr()); flush(stdout())

### Name: age_save
### Title: Export Brain ROI Age Trends
### Aliases: age_save

### ** Examples

if(interactive()){
sub_df <- age_df[,c("Volume_1", "age", "sex", "ICV_baseline")] |> na.omit()
colnames(sub_df) <- c("Volume_1", "age", "sex", "icv")
age_list <- list("Volume_1" = age_list_gen(sub_df = sub_df))

temp_dir <- tempfile()
dir.create(temp_dir)
age_save(temp_dir, age_list)
message("Age trend table saved to: ", temp_dir)
unlink(temp_dir, recursive = TRUE)
}
## Don't show: 
if (exists("temp_dir")) unlink(temp_dir, recursive = TRUE)
## End(Don't show)



cleanEx()
nameEx("age_shiny")
### * age_shiny

flush(stderr()); flush(stdout())

### Name: age_shiny
### Title: Lifespan Age Trends
### Aliases: age_shiny

### ** Examples

sub_df <- age_df[,c("Volume_1", "age", "sex", "ICV_baseline")] |> na.omit()
colnames(sub_df) <- c("Volume_1", "age", "sex", "icv")
age_list <- list("Volume_1" = age_list_gen(sub_df = sub_df))
quantile_type <- c("quantile_25", "median", "quantile_75")
if(interactive()){
  age_shiny(age_list = age_list, features = "Volume_1", quantile_type = quantile_type)
}



cleanEx()
nameEx("age_table_gen")
### * age_table_gen

flush(stderr()); flush(stdout())

### Name: age_table_gen
### Title: Generate Age Trend Summary Table
### Aliases: age_table_gen

### ** Examples

sub_df <- age_df[,c("Volume_1", "age", "sex", "ICV_baseline")] |> na.omit()
colnames(sub_df) <- c("Volume_1", "age", "sex", "icv")
age_list <- list("Volume_1" = age_list_gen(sub_df = sub_df))
result <- age_list[[1]]
if(interactive()){
 age_table_gen(result, q = "median", s = "F")
}




cleanEx()
nameEx("age_trend_plot")
### * age_trend_plot

flush(stderr()); flush(stdout())

### Name: age_trend_plot
### Title: Generate Age Trend Plot
### Aliases: age_trend_plot

### ** Examples

sub_df <- age_df[,c("Volume_1", "age", "sex", "ICV_baseline")] |> na.omit()
colnames(sub_df) <- c("Volume_1", "age", "sex", "icv")
age_list <- list("Volume_1" = age_list_gen(sub_df = sub_df))
customized_results <- cus_result_gen(age_list, customized_q = 0.75, f = "Volume_1")

if(interactive()){
 age_trend_plot(
   age_list = age_list,
   f = "Volume_1",
   s = "F",
   q = "customization",
   cus_list = customized_results,
   use_plotly = TRUE
 )
}



cleanEx()
nameEx("combat_harm")
### * combat_harm

flush(stderr()); flush(stdout())

### Name: combat_harm
### Title: ComBatFamily Harmonization
### Aliases: combat_harm

### ** Examples

combat_harm(features = colnames(adni)[43:53], batch = "manufac",
covariates = c("AGE", "SEX", "DIAGNOSIS"), df = head(adni,100), type = "lm")




cleanEx()
nameEx("combat_plot_gen")
### * combat_plot_gen

flush(stderr()); flush(stdout())

### Name: combat_plot_gen
### Title: Generate Diagnostic Plots for Batch Effect Analysis
### Aliases: combat_plot_gen

### ** Examples

if(interactive()){
 result <- visual_prep(type = "lm", features = "thickness.left.cuneus",
 batch = "manufac", covariates = "AGE", df = adni[1:100, ], mdmr = FALSE, cores = 1)
 combat_plot_gen(result, f = "thickness.left.cuneus", plot_name = "batch_density")
 combat_plot_gen(result, f = "thickness.left.cuneus", c = "AGE", plot_name = "cov_feature")
}




cleanEx()
nameEx("combat_table_gen")
### * combat_table_gen

flush(stderr()); flush(stdout())

### Name: combat_table_gen
### Title: Generate Diagnostic Tables for Batch Effect Analysis
### Aliases: combat_table_gen

### ** Examples

if(interactive()){
 result <- visual_prep(type = "lm", features = "thickness.left.cuneus",
 batch = "manufac", covariates = "AGE", df = adni[1:100, ], mdmr = FALSE, cores = 1)
 combat_table_gen(result, table_name = "cov_table", c = "AGE")
 combat_table_gen(result, table_name = "pc_variance", PC1 = "PC1", PC2 = "PC2")
 }




cleanEx()
nameEx("comfam")
### * comfam

flush(stderr()); flush(stdout())

### Name: comfam
### Title: ComBat Family Harmonization
### Aliases: comfam

### ** Examples

comfam(iris[,1:2], iris$Species)
comfam(iris[,1:2], iris$Species, iris[3:4], lm, y ~ Petal.Length + Petal.Width)



cleanEx()
nameEx("comfam_shiny")
### * comfam_shiny

flush(stderr()); flush(stdout())

### Name: comfam_shiny
### Title: Batch Effect Interactive Visualization
### Aliases: comfam_shiny

### ** Examples

result_lm <- visual_prep(type = "lm", features = colnames(adni)[43:53],
batch = "manufac", covariates = c("AGE", "SEX", "DIAGNOSIS"),
df = head(adni, 500), cores = 1)
if (interactive()) {
  comfam_shiny(result = result_lm)
}



cleanEx()
nameEx("covfam")
### * covfam

flush(stderr()); flush(stdout())

### Name: covfam
### Title: CovBat Family Harmonization
### Aliases: covfam

### ** Examples

covfam(iris[,1:2], iris$Species)
covfam(iris[,1:2], iris$Species, iris[3:4], lm, y ~ Petal.Length + Petal.Width)



cleanEx()
nameEx("cus_result_gen")
### * cus_result_gen

flush(stderr()); flush(stdout())

### Name: cus_result_gen
### Title: Generate Customized Predicted Quantiles List
### Aliases: cus_result_gen

### ** Examples

sub_df <- age_df[,c("Volume_1", "age", "sex", "ICV_baseline")] |> na.omit()
colnames(sub_df) <- c("Volume_1", "age", "sex", "icv")
age_list <- list("Volume_1" = age_list_gen(sub_df = sub_df))
cus_result_gen(age_list, customized_q = 0.75, f = "Volume_1")



cleanEx()
nameEx("customize_percentile")
### * customize_percentile

flush(stderr()); flush(stdout())

### Name: customize_percentile
### Title: Generate Predicted Quantiles for Age Trends
### Aliases: customize_percentile

### ** Examples

sub_df <- age_df[,c("Volume_1", "age", "sex", "ICV_baseline")] |> na.omit()
colnames(sub_df) <- c("Volume_1", "age", "sex", "icv")
age_list <- list("Volume_1" = age_list_gen(sub_df = sub_df))
customize_percentile(age_list, feature = "Volume_1", q = 0.5, s = "F")



cleanEx()
nameEx("data_prep")
### * data_prep

flush(stderr()); flush(stdout())

### Name: data_prep
### Title: Data Preparation
### Aliases: data_prep

### ** Examples

data_prep(stage = "harmonization", result = NULL, features = colnames(adni)[43:53],
batch = "manufac", covariates = "AGE", df = head(adni, 100), type = "lm", random = NULL,
smooth = NULL, interaction = NULL, smooth_int_type = NULL, predict = FALSE, object = NULL)




cleanEx()
nameEx("diag_save")
### * diag_save

flush(stderr()); flush(stdout())

### Name: diag_save
### Title: Export Batch Effect Diagnosis Results
### Aliases: diag_save

### ** Examples

if(interactive()){
  result <- visual_prep(type = "lm", features = "thickness.left.cuneus",
  batch = "manufac", covariates = "AGE", df = adni[1:100, ], mdmr = FALSE, cores = 1)
  temp_dir <- tempfile()
  dir.create(temp_dir)
  diag_save(temp_dir, result, quarto = FALSE)
  message("Diagnostics saved to: ", temp_dir)
  unlink(temp_dir, recursive = TRUE)  # Clean up the temporary directory
}
## Don't show: 
# Ensure temp_dir exists before attempting cleanup
if (exists("temp_dir")) unlink(temp_dir, recursive = TRUE)
## End(Don't show)



cleanEx()
nameEx("dot-biweight_midvar")
### * dot-biweight_midvar

flush(stderr()); flush(stdout())

### Name: .biweight_midvar
### Title: Biweight Midvariance Calculation
### Aliases: .biweight_midvar

### ** Examples

data <- c(1, 2, 3, 4, 100)
biweight_var <- .biweight_midvar(data)
print(biweight_var)



cleanEx()
nameEx("eb_check")
### * eb_check

flush(stderr()); flush(stdout())

### Name: eb_check
### Title: EB Assumption Check
### Aliases: eb_check

### ** Examples

eb_check(data = adni[1:500,43:53], bat = as.factor(adni$manufac[1:500]),
covar = adni[1:500, c("AGE", "SEX")], model = lm, formula = y ~ AGE + SEX)




cleanEx()
nameEx("form_gen")
### * form_gen

flush(stderr()); flush(stdout())

### Name: form_gen
### Title: ComBatFamily Model Formula Generations
### Aliases: form_gen

### ** Examples

covariates <- adni[, c("AGE", "SEX")]
form_gen(x = "lm", c = covariates)




cleanEx()
nameEx("getQuantileRefactored")
### * getQuantileRefactored

flush(stderr()); flush(stdout())

### Name: getQuantileRefactored
### Title: Compute Quantile Functions for a Predictor in a GAMLSS Model
### Aliases: getQuantileRefactored

### ** Examples

if (requireNamespace("gamlss", quietly = TRUE)) {
  library(gamlss)
  sub_df <- data.frame(
    age = seq(1, 20, length.out = 100),
    height = 50 + 2.5 * seq(1, 20, length.out = 100) + rnorm(100, 0, 5)
  )

  mdl <- gamlss(height ~ pb(age), data = sub_df, family = NO())

  quantile_function <- getQuantileRefactored(
    obj = mdl,
    term = "age",
    quantile = c(0.25, 0.5, 0.75),
    data = sub_df
  )
 }else{
 message("The 'gamlss' package is not installed. Please install it to run this example.")
 }



cleanEx()
nameEx("interaction_gen")
### * interaction_gen

flush(stderr()); flush(stdout())

### Name: interaction_gen
### Title: Interaction Term Generation
### Aliases: interaction_gen

### ** Examples

interaction_gen(type = "lm", covariates = c("AGE", "SEX", "DIAGNOSIS"),
interaction = "AGE,DIAGNOSIS")

interaction_gen(type = "gam", covariates = c("AGE", "SEX", "DIAGNOSIS"),
smooth = "AGE", smooth_int_type = "linear", interaction = "AGE,DIAGNOSIS")




cleanEx()
nameEx("model_gen")
### * model_gen

flush(stderr()); flush(stdout())

### Name: model_gen
### Title: Model Generations
### Aliases: model_gen

### ** Examples

model_gen(y = "thickness.left.caudal.anterior.cingulate", type = "lm",
batch = "manufac", covariates = c("AGE", "SEX"), df = adni)




cleanEx()
nameEx("predict.comfam")
### * predict.comfam

flush(stderr()); flush(stdout())

### Name: predict.comfam
### Title: Apply Harmonization to New Data
### Aliases: predict.comfam

### ** Examples

com_out <- comfam(iris[1:75,1:2], iris$Species[1:75])

# out-of-sample with new batch
out_pred <- predict(com_out, iris[76:150,1:2], iris$Species[76:150])

# in-sample
in_pred <- predict(com_out, iris[1:25,1:2], iris$Species[1:25])
max(in_pred$dat.combat - com_out$dat.combat[1:25,])



cleanEx()
nameEx("residual_gen")
### * residual_gen

flush(stderr()); flush(stdout())

### Name: residual_gen
### Title: Post Harmonization Residual Generation
### Aliases: residual_gen

### ** Examples

features <- colnames(adni)[43:53]
residual_gen(type = "lm", features = features,
covariates = c("AGE", "SEX", "DIAGNOSIS"), df = adni, rm = c("AGE", "SEX"), cores = 1)



cleanEx()
nameEx("visual_prep")
### * visual_prep

flush(stderr()); flush(stdout())

### Name: visual_prep
### Title: Batch Effect Diagnostic Visualization Preparation
### Aliases: visual_prep

### ** Examples

visual_prep(type = "lm", features = colnames(adni)[43:53], batch = "manufac",
covariates = c("AGE", "SEX", "DIAGNOSIS"), df = head(adni, 500), cores = 1)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
