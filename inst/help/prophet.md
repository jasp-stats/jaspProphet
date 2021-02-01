Prophet
===

Prophet allows the user to predict time series on different scales taking into account seasonal effects and changepoints.

### Assumptions
- Residuals are normally distributed.

### Input
---
#### Assignment Box
- Dependent Variable: Time series variable to be predicted.
- Time: Variable containing the date/time stamp for every observation. Combined date and time values should be in the standard format "YYYY-MM-DD HH:MM:SS", where seconds (":SS") can also be omitted. Date-only values should be in the format "YYYY-MM-DD".
- Changepoints: Logical variable (only 0 or 1) indicating at which time stamps Prophet should set a trend changepoint (i.e., change in growth).
- Carrying Capacity: Time series variable containing the maximum values Prophet will predict at a given time stamp. This variable is only used for logistic growth (see *Growth*). Must be larger than the saturating minimum. Note that values of this variable must be supplied for predictions.
- Saturating Minimum: Time series variable containing the minimum values Prophet will predict at a given time stamp. This variable is only used for logistic growth (see *Growth*). Must be smaller than the carrying capacity. Note that values of this variable must be supplied for predictions.
- Covariates: Time series variables that are associated with the dependent variable. Note that values of covariates must be supplied for predictions.
- Include in Training: Logical variable (only 0 or 1) indicating which cases should be used for estimating Prophet (= 1) and which cases should be predicted (= 0). This variable is necessary for making predictions when a carrying capacity, a saturating minimum, or covariates are supplied. 

#### History Plot
- This option plots the dependent variable over time. Only plots cases that are selected by the history indicator variable.
    - Add line: Connects the cases in the history plot with a line.
    - Start: A text string at which time stamp the history plot should start its x-axis. Needs to be in the same format as the time variable.
    - End: A text string at which time stamp the history plot should end its x-axis. Needs to be in the same format as the time variable.

#### Growth
- Linear: When this option is selected, Prophet uses linear growth for prediction.
- Logistic: When this option is selected, Prophet uses logistic growth for prediction. This option requires that either a carrying capacity variable or a constant carrying capacity is supplied.
    - Constant Carrying Capacity: The maximum value Prophet will predict for all time stamps. Must be larger than the saturating minimum. This value is automatically used for predictions.
    - Constant Saturating Minimum: The minimum value Prophet will predict for all time stamps. Must be smaller than the carrying capacity. This value is automatically used for predictions.

### Model
#### Automatic Changepoints
- Max. changepoints: The maximum number of trend changepoints prophet will set and estimate. If enough data is available, Prophet will set the maximum number of changepoints.
- Changepoint range: Proportion of the data, where Prophet will set trend changepoints. For example, with the default of 0.8, Prophet will set changepoints within the first 80% of the time series.
- Changepoint prior $\tau$: Scale of the prior Laplace distribution for the magnitude of trend changepoints. The distribution has a location at 0. Larger values lead to larger changes and smaller values to smaller changes in the trend. If set to 0, no changepoints will be estimated.

#### Estimation
- Maximum a posteriori: Parameters are estimated using maximum a posteriori estimation. This approach is usually fast.
- Markov chain Monte Carlo (MCMC): Parameters are estimated using MCMC sampling. With this setting, 1 prediction interval sample is drawn for each MCMC sample. This approach can become very slow for long time series and complex models. Before running a model with many samples, it is best to run a model with very few samples (e.g., 10) to check if all settings are specified correctly and whether any errors occur.
    - Samples: Number of MCMC samples used for parameter estimation and prediction.

#### Uncertainty
- Prediction interval level: Level of prediction intervals that Prophet will estimate (displayed in forecast plots). Indicates the percentage of predicted samples that lie within the displayed intervals.
- Prediction interval samples: Number of predicted samples drawn to estimate prediction intervals. Can only be set for maximum a posteriori estimation. For MCMC estimation, this is always set to 1.
- Credible interval level: Level of equal-tailed credible intervals for estimated parameters. For example, the default of 95% means that the interval includes 95% of the posterior samples, with 2.5% of the lowest/highest values liying outside each boundary of the interval.

#### Covariates
- Displays the covariates selected in the assignment box.
    - Normal prior σ: Scale of the prior normal distribution that determines the influence of the covariate on the prediction. The distribution has a location at 0. Larger values will lead to stronger influence of the covariate.
    - Standardize: This option indicates whether the covariate will be standardized before parameter estimation. This is recommended for non-binary covariates.
    - Mode: This option indicates whether the covariate will be added as an additive or multiplicative term to the model.

#### Seasonalities
- In this box, seasonality terms can be created and added to the model.
    - Name: Name of the seasonality. Names must only contain letters, number, dots, or underscores and must start with letters or dots that are not followed by a number.
    - Period: Period of the seasonality (i.e., number of units after which the seasonality will be repeated).
    - Unit: Unit of the seasonality period. The unit should match the scale of the time series (e.g., time series measured in days should have a seasonality in days, weeks, or years).
    - Normal prior σ: Scale of the prior normal distribution that determines the influence of the seasonality on the prediction. The distribution has a location at 0. Larger values will lead to stronger influence of the seasonality.
    - Fourier order: Order of the fourier series used to approximate the seasonal effect. Larger values will lead to more flexible seasonalities (i.e., more changes).
    - Mode: This option indicates whether the seasonality will be added as an additive or multiplicative term to the model.

### Prediction
- Periodical: This option lets Prophet make periodical predictions. This includes predictions for the time series itself. Note that, when covariates, a carrying capacity, or a saturating minimum are supplied, they also need to be included for predictions. For these situations, the variable "Include in Training" can separate cases to be used for estimation from those for prediction.
    - Number of periods: The number of periods that Prophet will predict.
    - Unit: The unit of the periods that Prophet will predict. The unit should match the scale of the time series (e.g., time series measured in days should be predicted in days, weeks, or years). Note that Prophet makes only one prediction per single unit (e.g., when the unit is "weeks", only one case per week will be predicted).
- Nonperiodical: This option lets Prophet make nonperiodical predictions. Note that, when covariates, a carrying capacity, or a saturating minimum are supplied, they also need to be included for predictions. For these situations, the variable "Include in Training" can separate cases to be used for estimation from those for prediction.
    - Start: A text string at which the prediction interval will start. Needs to be in the same format as the time variable.
    - End: A text string at which the prediction interval will end. Needs to be in the same format as the time variable.
    - Unit: The unit of the interval that Prophet will predict. The unit should match the scale of the time series (e.g., time series measured in days should be predicted in days, weeks, or years). Note that Prophet makes only one prediction per single unit (e.g., when the unit is "weeks", only one case per week will be predicted).
- Save predictions: Save the predictions in a .csv-file.

### Evaluation
- Simulated historical forecasts: Performs a type of cross-validation for time series. Repeatedly estimates Prophet on a subset of the time series and makes predictions on future "test" cases. Divides the time series in subsets by placing cutoffs between a specified interval. Uses all cases before each cutoff to estimate Prophet. The estimated model then predicts cases within a future horizon. Finally, the predictions are compared to the true case values and performance metrics can be computed by averaging over "test sets".
    - Unit: The unit of the horizon and period between cutoffs. The unit should match the scale of the time series (e.g., time series measured in days should be predicted in days, weeks, or years). Note that Prophet makes only one prediction per single unit (e.g., when the unit is "weeks", only one case per week will be predicted).
    - Horizon: Horizon of the cross-validation (i.e., number of future cases to be predicted).
    - Period between cutoffs: Number of units between cutoffs.
    - Initial training time: Number of units that are always used to estimate Prophet and will never be predicted.
- Performance metrics:
    - Mean squared error (MSE): Display the MSE for each unit in the horizon.
    - Root mean squared error (RMSE): Display the RMSE for each unit in the horizon.
    - Mean absolute percentage error (MAPE): Display the MAPE for each unit in the horizon.
- Changepoint table: Display the estimated changepoints in a table.

### Plots
#### Forecast Plots
- Overall: Plots the overall forecast by Prophet for the time interval specified in prediction.
    - Show data points: Adds the data points to the plot.
    - Show changepoints: Adds the changepoints to the plot.
    - Show carrying capacity: Adds the carrying capacity to the plot. Only enabled for logistic growth.
    - Show saturating minimum: Adds the saturating minimum to the plot. Only enabled for logistic growth.
    - Start: A text string at which time stamp the plot should start its x-axis. Needs to be in the same format as the time variable.
    - End: A text string at which time stamp the plot should end its x-axis. Needs to be in the same format as the time variable.
- Trend: Plots the trend forecast by Prophet for the time interval specified in prediction.
    - Show changepoints: Adds the changepoints to the plot.
    - Start: A text string at which time stamp the plot should start its x-axis. Needs to be in the same format as the time variable.
    - End: A text string at which time stamp the plot should end its x-axis. Needs to be in the same format as the time variable.
- Seasonality Plots: Plots the estimated seasonalities.
- Covariate Plots: Plots the covariate time series.

#### Performance Plots
- Mean squared error (MSE): Plots the MSE for each unit in the horizon.
- Root mean squared error (RMSE): Plots the RMSE for each unit in the horizon.
- Mean absolute percentage error (MAPE): Plots the MAPE for each unit in the horizon.

#### Parameter Plots
- Changepoint plot: Plots the estimated magnitude against the time stamp of each changepoint.
- Posterior distributions: Plots the posterior distributions with credible intervals for each estimated parameter.
    - Credible interval level: Level of the displayed credible intervals. For example, the default of 95% means that the interval includes 95% of the posterior samples, with 2.5% of the lowest/highest values liying outside each boundary of the interval.

### Output
---
#### History Plot
Time stamps on the x-axis and the dependent variable on the y-axis. Displays only cases used to train Prophet (i.e., the history).

#### Posterior Summary
- Parameter: The parameter for which estimates are displayed.
- Mean: Mean of the posterior distribution.
- SD: Standard deviation of the posterior distribution.
- % CI: Credible interval with equal tails of the posterior distribution.
    - Lower: Lower bound of the credible interval.
    - Upper: Upper bound of the credible interval.
- R-hat: Convergence diagnostic comparing within and between chains estimates of model parameters. Values larger than 1.01 indicates possible convergence problems.
- ESS (bulk): Estimated sample size in the middle of the distribution. Small values render the parameter estimates unprecise.
- ESS (tail): Estimated sample size in the tails of the distribution. Small values render the bounds of credible intervals unprecise.

#### Changepoint Posterior Summary
- Changepoint: Time stamp of each changepoint that is displayed.
- Change in growth rate ($\delta$): Magnitude of the change in the growth rate at each changepoint.
    - Mean: Mean of the posterior distribution for each changepoint.
    - SD: Standard deviation of the posterior distribution for each changepoint.
- % CI: Credible interval with equal tails of the posterior distribution for each changepoint.
    - Lower: Lower bound of the credible interval.
    - Upper: Upper bound of the credible interval.

#### Simulated Historical Forecasts
- Horizon: Number of units in the future where cases are predicted in each "test set". For example, a horizon of 3 means that cases 3 units ahead are predicted in each "test set".
- MSE: Mean squared error for each horizon.
- RMSE: Root mean squared error for each horizon.
- MAPE: Mean absolute percentage error for each horizon.

#### Forecast Plots
- Overall: Time stamps on the x-axis and the dependent variable on the y-axis. Points represent the data. The blue line is the overall prediction by Prophet and the blue-shaded area  the prediction interval. The vertical dashed line marks the end of the data set and the start of predictions for future cases. Vertical full lines indicate changepoints in the trend. Horizontal lines are the carrying capacity (upper) and saturating (minimum) for logistic growth.
- Trend: Time stamps on the x-axis and the dependent variable on the y-axis. The blue line is the predicted trend by Prophet and the blue-shaded area the prediction interval. The vertical dashed line marks the end of the data set and the start of predictions for future cases. Vertical full lines indicate changepoints in the trend.

#### Seasonality Plots
Time stamps on the x-axis and the dependent variable on the y-axis. The blue line is the predicted seasonality by Prophet and the blue-shaded area the prediction interval.

#### Covariate Plots
Time stamps on the x-axis and covariates on the y-axis. Points represent covariate data.

#### Performance Plots
Horizon on the x-axis and performance metrics on the y-axis. The red line indicates the performance metric. Points represent prediction errors for individual "test sets" (without averaging). For example, the red line could represent the MSE and the points the individual squared errors.

#### Parameter Plots
- Changepoint plot: Time stamps of changepoints on the x-axis and change in growth rate ($\delta$) on the y-axis. Points indicate individual changepoints (mean estimates of magnitude).
- Parameter plots: Parameter values on the x-axis and posterior densities on the y-axis. Segments indicate credible intervals.

### References
- Taylor, S. J. & Letham, B. (2018). Forecasting at scale. *The American Statistician, 72*(1), 37-45.

### R Packages
- prophet
- ggplot2
- rstan

### Examples
