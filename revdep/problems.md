# BayesFBHborrow

<details>

* Version: 2.0.2
* GitHub: NA
* Source code: https://github.com/cran/BayesFBHborrow
* Date/Publication: 2024-09-16 11:00:06 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::revdep_details(, "BayesFBHborrow")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BayesFBHborrow-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: BayesFBHborrow
    > ### Title: BayesFBHborrow: Run MCMC for a piecewise exponential model
    > ### Aliases: BayesFBHborrow
    > 
    > ### ** Examples
    > 
    > set.seed(123)
    ...
    +                           "cprop_beta" = 3.25,
    +                           "alpha" = 0.4)
    >                           
    > # Set hyperparameters to default, with the borrowing model "mix"
    > out <- BayesFBHborrow(data = piecewise_exp_cc, data_hist = piecewise_exp_hist,
    +                       model_choice = 'mix', tuning_parameters = tuning_parameters,
    +                       iter = 2, warmup_iter = 0)
    Error in if (shape <= 0.01) { : the condition has length > 1
    Calls: BayesFBHborrow ... GibbsMH -> GibbsMH.WBorrow -> .tau_update -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          warning("`rinvgamma()` is unreliable for `shape` <= .01.", 
              call. = FALSE, immediate. = TRUE)
      }`: the condition has length > 1
      Backtrace:
          ▆
       1. ├─BayesFBHborrow::BayesFBHborrow(...) at test-run_mcmc.R:267:3
       2. └─BayesFBHborrow:::BayesFBHborrow.WBorrow(...)
       3.   ├─BayesFBHborrow::GibbsMH(...)
       4.   └─BayesFBHborrow:::GibbsMH.WBorrow(...)
       5.     └─BayesFBHborrow:::.tau_update(...)
       6.       └─invgamma::rinvgamma(n = J + 1, shape = ac[call], rate = bd[call])
      
      [ FAIL 11 | WARN 0 | SKIP 0 | PASS 572 ]
      Error: Test failures
      Execution halted
    ```

