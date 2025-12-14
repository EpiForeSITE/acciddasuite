## Packages listed in the repo

1. [`fable`](https://fable.tidyverts.org/)\
A collection of time series forecasting models.
Provides tools to evaluate, visualise, and combine models in a workflow consistent with the `tidyverse`.  
Example code:  
```r
   library(fable)
   some_data |>
     model(
       ets = ETS(box_cox(Turnover, 0.3)),
       arima = ARIMA(log(Turnover)),
       snaive = SNAIVE(Turnover)
     ) |>
     forecast(h = "4 weeks")
```
I can also think of `tidymodels` and `modeltime` here.
Note: [`tgsc`](https://www.jbs.cam.ac.uk/centres/health/tsgc/) looks promising.

2. [epipredict](https://cmu-delphi.github.io/epipredict/)\
A wrapper package for `tidymodels` with convenience functions for epidemiological forecasting.
Example code:

    ``` r
    library(epipredict)
      four_week_ahead <- arx_forecaster(
          training_data,
          outcome = "death_rate",
          predictors = c("case_rate", "death_rate"),
          args_list = arx_args_list(
              lags = list(c(0, 1, 2, 3, 7, 14), c(0, 7, 14)),
              ahead = 4 * 7,
              quantile_levels = c(0.1, 0.25, 0.5, 0.75, 0.9)
          )
      )
    ```

3.  [`epiestim`](https://mrc-ide.github.io/EpiEstim/index.html)\
Estimates time varying reproduction numbers (Rt) from incidence time series. This shouldn't be used for forecasting directly, but Rt estimates can be used as inputs to [`projections`](https://github.com/reconhub/projections) packages for forecasting.\

Example code:

``` r
library(EpiEstim)
library(projections)

Rt <- estimate_R(
  incid = incidence_data,
  method = "parametric_si",
  config = make_config(
    mean_si = 14.2,
    std_si = 9.6,
    t_start = length(incidence_data$counts) - 14,
    t_end = length(incidence_data$counts)
  )
)

proj <- project(
  incidence_data,
  R = Rt$R$`Median(R)`,
  si = Rt$si_distr[-1],
  n_sim = 1000,
  n_days = 30,
  R_fix_within = TRUE
)

```

4.  [EpiNow2](https://epiforecasts.io/EpiNow2/index.html)\
Estimates Rt and incoportates nowcasting (i.e. adjusting for reporting delays) and forecasting in a single package.\
Example code:

``` r
library(EpiNow2)
estimates <- epinow(
  data = reported_cases,
  generation_time = gt_opts(example_generation_time),
  delays = delay_opts(example_incubation_period + reporting_delay),
  rt = rt_opts(prior = LogNormal(mean = 2, sd = 0.2)),
  stan = stan_opts(cores = 4),
  verbose = interactive()
)
```

5. [`hubverse`](https://hubverse-org.r-universe.dev/hubverse)\
   A collection of packages to to standardise epidemic forecasts.\
   `hubEnsembles` to aggregate forecasts from multiple models.
   `hubEvals` to evaluate forecasts.


It is clear and simple, but it can be slightly tightened and made more direct. Here is a cleaner version with improved flow and consistency, without changing the meaning.

## Discussion

**Maintenance burden:**
This would involve wrapping fast moving, complex packages maintained by different teams. Any upstream API change would risk breaking the package. We would inherit technical debt without adding new core methodology.

**Functionality overlap:**
`epipredict`, `fable`, `tidymodels`, and `modeltime` already provide general forecasting frameworks that can be readily applied to epidemiological data, including ensemble methods and model evaluation.

**Suggested approach:**
If needed, we could provide a very minimal example notebook demonstrating how to use existing tools, for example fitting three models to forecast influenza cases four weeks ahead and evaluating their performance.

There are already comprehensive resources from [Delphi](https://cmu-delphi.github.io/delphi-tooling-book/sliding-forecasters.html) and the [EpiR Handbook](https://www.epirhandbook.com/en/new_pages/epidemic_models.html), and `tidymodels` itself is supported by high quality vignettes and books.

A more useful contribution may be to provide a GitHub based support platform with office hours, where we answer questions, support teams, and help debug or improve their code.
