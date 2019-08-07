The data must have a header row, and all columns must contain only numbers.  Missing data is allowed (blank entries or the string `NA`).

The file must include a column that represents a time variable - in this column the data must be strictly increasing.  The application will try and detect the time variable automatically and if more than one variable is possible you must select it from the drop-down below.

Only strictly increasing variables will be included in the drop-down - if your column ever decreases or even stays the same then it is not suitable as a time column and will not be included.
