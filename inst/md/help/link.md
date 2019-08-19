## Link

### Key points

You must have data (in the correct format) and correctly compiled model code for the linkage to work.

### Data

If your dataset has been correctly loaded, you should see a green tick.
The number of rows in your data will also be shown.

### Model

If your model has correctly compiled, you should see a green tick.
The number of paramaters and number of variables/outputs will also be shown.

### Link

Here you need to specify which variables in your model corresponds to the columns in your uploaded dataset.
E.g in your model you might have a variable `date_onset` which you want to link to the column `date_of_symptom_onset` in your dataset.
Once linked, you should see a green tick and the configurations you have selected.

Note that the interface will *not* know if you have correctly linked the right variables to the right data!
E.g. check that you have not linked `date_onset` in your model to `date_of_symptom_onset` in your dataset.
