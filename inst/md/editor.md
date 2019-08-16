## Write odin code

Write code in this editor and press "Compile" and the odin model will be created, which will be available for use in the other tabs.

## Key bits to remember

```r
deriv(X) <- ...
```

Specifies that `X` is a *variable* that will change over time, but that we can only describe `X` in terms of its rates of change.  Every `deriv()` call must be paired with an `initial()` call that describes the initial conditions

```r
initial(X) <- ...
```

The code here looks like R but is not R.

To create other quantities, use `<-` to bind a name (e.g., `a`) to a mathematical expression (e.g., `X + 1`) such as:

```r
a <- X + 1
```

Standard mathematical operators (`*`, `/`, `+`, `-`) are supported, you can also use `a^b` for "`a` to the power of `b`".

You can also add comments and notes to your code by using the `#` symbol.

``` r
# This is a comment and will be ignored
b <- 2 * a # the text after the '#' is a comment and will be ignored
```

## Validating the code

Select `auto validate` to validate as you type.  This may get annoying and/or slow.

## Further help

A more detailed user guide can be found <a href="https://mrc-ide.github.io/infectiousdiseasemodels-2019/guide/" target="_blank">here</a> (opens in new page).
