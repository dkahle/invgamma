chains <- 1
iter <- 1000
warmup <- 500

# GLMs
glm_gaussian_data <- mock_glm_gaussian_data()
cat_init_glm_gaussian <- mock_cat_glm_gaussian_initialization(glm_gaussian_data)
glm_binomial_data <- mock_glm_binomial_data()
cat_init_glm_binomial <- mock_cat_glm_binomial_initialization(glm_binomial_data)

cat_glm_model_gaussian <- cat_glm(formula = ~., cat_init = cat_init_glm_gaussian)
cat_glm_model_binomial <- cat_glm(formula = ~., cat_init = cat_init_glm_binomial)

cat_glm_tune_model_gaussian <- cat_glm_tune(formula = ~., cat_init = cat_init_glm_gaussian)
cat_glm_tune_model_binomial <- cat_glm_tune(formula = ~., cat_init = cat_init_glm_binomial)

suppressWarnings(
  cat_glm_bayes_model_gaussian <- cat_glm_bayes(
    formula = ~.,
    cat_init = cat_init_glm_gaussian,
    chains = chains,
    iter = iter,
    warmup = warmup
  )
)
suppressWarnings(
  cat_glm_bayes_model_binomial <- cat_glm_bayes(
    formula = ~.,
    cat_init = cat_init_glm_binomial,
    chains = chains,
    iter = iter,
    warmup = warmup
  )
)
suppressWarnings(
  cat_glm_bayes_joint_model_gaussian <- cat_glm_bayes_joint(
    formula = ~.,
    cat_init = cat_init_glm_gaussian,
    chains = chains,
    iter = iter,
    warmup = warmup
  )
)
suppressWarnings(
  cat_glm_bayes_joint_model_binomial <- cat_glm_bayes_joint(
    formula = ~.,
    cat_init = cat_init_glm_binomial,
    chains = chains,
    iter = iter,
    warmup = warmup
  )
)

cat_glm_bayes_joint_gibbs_model_binomial <- cat_glm_bayes_joint_gibbs(
  formula = ~.,
  cat_init = cat_init_glm_binomial
)

# COX
cox_data <- mock_cox_data()
cat_init_cox <- mock_cat_cox_initialization(cox_data)

cat_cox_model_cre <- cat_cox(formula = ~., cat_init = cat_init_cox, method = "CRE")
cat_cox_model_wme <- cat_cox(formula = ~., cat_init = cat_init_cox, method = "WME")

cat_cox_tune_model_cre <- cat_cox_tune(
  formula = ~.,
  cat_init = cat_init_cox,
  method = "CRE",
  cross_validation_fold_num = 2
)
cat_cox_tune_model_wme <- cat_cox_tune(
  formula = ~.,
  cat_init = cat_init_cox,
  method = "WME",
  cross_validation_fold_num = 2
)

suppressWarnings(
  cat_cox_bayes_model <- cat_cox_bayes(
    formula = ~.,
    cat_init = cat_init_cox,
    chains = chains,
    iter = iter,
    warmup = warmup
  )
)
suppressWarnings(
  cat_cox_bayes_joint_model <- cat_cox_bayes_joint(
    formula = ~.,
    cat_init = cat_init_cox,
    chains = chains,
    iter = iter,
    warmup = warmup
  )
)

# LMM
lmm_data <- mock_lmm_data()
cat_init_lmm <- mock_cat_lmm_initialization(lmm_data)

cat_lmm_model <- cat_lmm(cat_init = cat_init_lmm)

cat_lmm_tune_model <- cat_lmm_tune(cat_init = cat_init_lmm)


test_that("predict.cat_glm works correctly", {
  # cat_glm
  predict_result_cat_glm_gaussian <- predict(cat_glm_model_gaussian)
  predict_result_cat_glm_gaussian_new_data <- predict(cat_glm_model_gaussian, newdata = glm_gaussian_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_glm_gaussian), nrow(cat_glm_model_gaussian$cat_init$obs_x))
  expect_equal(predict_result_cat_glm_gaussian[1], predict_result_cat_glm_gaussian_new_data)

  predict_result_cat_glm_binomial <- predict(cat_glm_model_binomial, type = "response")
  predict_result_cat_glm_binomial_new_data <- predict(cat_glm_model_binomial, newdata = glm_binomial_data[1, , drop = FALSE], type = "response")
  expect_equal(length(predict_result_cat_glm_binomial), nrow(cat_glm_model_binomial$cat_init$obs_x))
  expect_equal(predict_result_cat_glm_binomial[1], predict_result_cat_glm_binomial_new_data)

  # cat_glm_tune
  predict_result_cat_glm_tune_gaussian <- predict(cat_glm_tune_model_gaussian)
  predict_result_cat_glm_tune_gaussian_new_data <- predict(cat_glm_tune_model_gaussian, newdata = glm_gaussian_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_glm_tune_gaussian), nrow(cat_glm_tune_model_gaussian$cat_init$obs_x))
  expect_equal(predict_result_cat_glm_tune_gaussian[1], predict_result_cat_glm_tune_gaussian_new_data)

  predict_result_cat_glm_tune_binomial <- predict(cat_glm_tune_model_binomial, type = "response")
  predict_result_cat_glm_tune_binomial_new_data <- predict(cat_glm_tune_model_binomial, newdata = glm_binomial_data[1, , drop = FALSE], type = "response")
  expect_equal(length(predict_result_cat_glm_tune_binomial), nrow(cat_glm_tune_model_binomial$cat_init$obs_x))
  expect_equal(predict_result_cat_glm_tune_binomial[1], predict_result_cat_glm_tune_binomial_new_data)

  # cat_glm_bayes
  predict_result_cat_glm_bayes_gaussian <- predict(cat_glm_bayes_model_gaussian)
  predict_result_cat_glm_bayes_gaussian_new_data <- predict(cat_glm_bayes_model_gaussian, newdata = glm_gaussian_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_glm_bayes_gaussian), nrow(cat_glm_bayes_model_gaussian$cat_init$obs_x))
  expect_equal(predict_result_cat_glm_bayes_gaussian[1], predict_result_cat_glm_bayes_gaussian_new_data)

  predict_result_cat_glm_bayes_binomial <- predict(cat_glm_bayes_model_binomial)
  predict_result_cat_glm_bayes_binomial_new_data <- predict(cat_glm_bayes_model_binomial, newdata = glm_binomial_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_glm_bayes_binomial), nrow(cat_glm_bayes_model_binomial$cat_init$obs_x))
  expect_equal(predict_result_cat_glm_bayes_binomial[1], predict_result_cat_glm_bayes_binomial_new_data)

  # cat_glm_bayes_joint
  predict_result_cat_glm_bayes_joint_gaussian <- predict(cat_glm_bayes_joint_model_gaussian)
  predict_result_cat_glm_bayes_joint_gaussian_new_data <- predict(cat_glm_bayes_joint_model_gaussian, newdata = glm_gaussian_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_glm_bayes_joint_gaussian), nrow(cat_glm_bayes_joint_model_gaussian$cat_init$obs_x))
  expect_equal(predict_result_cat_glm_bayes_joint_gaussian[1], predict_result_cat_glm_bayes_joint_gaussian_new_data)

  predict_result_cat_glm_bayes_joint_binomial <- predict(cat_glm_bayes_joint_model_binomial)
  predict_result_cat_glm_bayes_joint_binomial_new_data <- predict(cat_glm_bayes_joint_model_binomial, newdata = glm_binomial_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_glm_bayes_joint_binomial), nrow(cat_glm_bayes_joint_model_binomial$cat_init$obs_x))
  expect_equal(predict_result_cat_glm_bayes_joint_binomial[1], predict_result_cat_glm_bayes_joint_binomial_new_data)

  # cat_glm_bayes_joint_gibbs
  predict_result_cat_glm_bayes_joint_gibbs_binomial <- predict(cat_glm_bayes_joint_gibbs_model_binomial)
  predict_result_cat_glm_bayes_joint_gibbs_binomial_new_data <- predict(cat_glm_bayes_joint_gibbs_model_binomial, newdata = glm_binomial_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_glm_bayes_joint_gibbs_binomial), nrow(cat_glm_bayes_joint_gibbs_model_binomial$cat_init$obs_x))
  expect_equal(predict_result_cat_glm_bayes_joint_gibbs_binomial[1], predict_result_cat_glm_bayes_joint_gibbs_binomial_new_data)
})

test_that("predict.cat_cox works correctly", {
  # cat_cox
  predict_result_cat_cox_cre <- predict(cat_cox_model_cre)
  predict_result_cat_cox_cre_new_data <- predict(cat_cox_model_cre, newdata = cox_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_cox_cre), nrow(cat_cox_model_cre$cat_init$obs_x))
  expect_equal(predict_result_cat_cox_cre[1], predict_result_cat_cox_cre_new_data)

  predict_result_cat_cox_wme <- predict(cat_cox_model_wme)
  predict_result_cat_cox_wme_new_data <- predict(cat_cox_model_wme, newdata = cox_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_cox_wme), nrow(cat_cox_model_wme$cat_init$obs_x))
  expect_equal(predict_result_cat_cox_wme[1], predict_result_cat_cox_wme_new_data)

  # cat_cox_tune
  predict_result_cat_cox_tune_cre <- predict(cat_cox_tune_model_cre)
  predict_result_cat_cox_tune_cre_new_data <- predict(cat_cox_tune_model_cre, newdata = cox_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_cox_tune_cre), nrow(cat_cox_tune_model_cre$cat_init$obs_x))
  expect_equal(predict_result_cat_cox_tune_cre[1], predict_result_cat_cox_tune_cre_new_data)

  predict_result_cat_cox_tune_wme <- predict(cat_cox_tune_model_wme)
  predict_result_cat_cox_tune_wme_new_data <- predict(cat_cox_tune_model_wme, newdata = cox_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_cox_tune_wme), nrow(cat_cox_tune_model_wme$cat_init$obs_x))
  expect_equal(predict_result_cat_cox_tune_wme[1], predict_result_cat_cox_tune_wme_new_data)

  # cat_cox_bayes
  predict_result_cat_cox_bayes <- predict(cat_cox_bayes_model)
  predict_result_cat_cox_bayes_new_data <- predict(cat_cox_bayes_model, newdata = cox_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_cox_bayes), nrow(cat_cox_bayes_model$cat_init$obs_x))
  expect_equal(predict_result_cat_cox_bayes[1], predict_result_cat_cox_bayes_new_data)

  # cat_cox_bayes_joint
  predict_result_cat_cox_bayes_joint <- predict(cat_cox_bayes_joint_model)
  predict_result_cat_cox_bayes_joint_new_data <- predict(cat_cox_bayes_joint_model, newdata = cox_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_cox_bayes_joint), nrow(cat_cox_bayes_joint_model$cat_init$obs_x))
  expect_equal(predict_result_cat_cox_bayes_joint[1], predict_result_cat_cox_bayes_joint_new_data)
})

test_that("predict.cat_lmm works correctly", {
  # cat_lmm
  predict_result_cat_lmm <- predict(cat_lmm_model)
  predict_result_cat_lmm_new_data <- predict(cat_lmm_model, newdata = lmm_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_lmm), nrow(cat_lmm_model$cat_init$obs_x))
  expect_equal(predict_result_cat_lmm[1], predict_result_cat_lmm_new_data)

  # cat_lmm_tune
  predict_result_cat_lmm_tune <- predict(cat_lmm_tune_model)
  predict_result_cat_lmm_tune_new_data <- predict(cat_lmm_tune_model, newdata = lmm_data[1, , drop = FALSE])
  expect_equal(length(predict_result_cat_lmm_tune), nrow(cat_lmm_tune_model$cat_init$obs_x))
  expect_equal(predict_result_cat_lmm_tune[1], predict_result_cat_lmm_tune_new_data)
})
