## Sensitivity analysis

### Key points

1. Remember to specify an `End time` under `Run options`.
2. You must click on the variable to visualise (e.g. S) in the plot window (legend on right hand side) after running the model to visualise outputs.


### Model parameters

1. Parameter values can be changed under `Model parameters`, then click `Run model` again (or click the `Auto run` option).
2. Parameter values can be uploaded or downloaded using the `Download` or `Browse` button respectively. This is under the `Model parameters` section.

### Locked parameter set

`Locked parameter set` will let you compare outputs with different parameter values.

- "lock" the current parameter values.
- Change a parameter.
- Run model again.
- Dashed lines are based on the "locked" (or older) parameter values

### Vary parameter

Here you can explore how varying parameter values impacts on your model output.

1. Select parameter to vary.
2. You can choose an "arithmetic" or "logarithmic" scale to vary over from the dropdown `Scale type`.
3. `Variation type` can be:
    - "percentage": % change from central value. You need to then specify `Variation (%)`.
    - "range": specify min and max range to vary over. You need to then specify `From` and `To`.
4. For both cases type in how many runs you want to explore using `Number of runs`.

### Plot options

Here you can specify what output you would like to see using `Type of plot`.
1. Trace over time: how variables vary over time with each parameter value.
2. Value at a single time: the value of e.g. S, I, R, at a specific time point which can be specific using `Time to use`.
3. Value at its min/max = the min/max value of e.g. S, I, R as the parameter value varies.
4. Time at value's min/max = the time at which e.g. S, I, R reaches its minimum or maximum as the parameter value varies.

### Code

1. Expanding `Model code` underneath the plot window will show you your model code.

### Visualisation options

1. Click on the legend (e.g. S, I, or R) to show or hide variables.
2. Double click on the plot to zoom back out.
3. Hover over the top right hand corner of the plot window for additional plot options.
4. `Graph settings` option under the plot window gives you options for plotting on the log scale or on secondary axes.

### Download options

Underneath the plot window there is a `Download` option.

1. Modelled = downloads the modelled outputs e.g. the value of S, I, R over time.
2. Parameters = downloads your current parameters values.
