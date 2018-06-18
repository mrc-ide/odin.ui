## Write odin code

Write code in this editor and press "Compile" and the odin model will be created!  A new tab will be opened corresponding to the title of your model.  If the tab exists then the previous version will be replaced without warning

## Key bits to remember

```r
deriv(X) <- ...
```

Specifies that `X` is a *variable* will change over time, but that we can only describe `X` in terms of its rates of change.  Every `deriv()` call must be paired with an `initial()` call that describes the initial conditions

```r
initial(X) <- ...
```

The code here looks like R but is not R.  Not everything will work

## Validating the code

Select `auto validate` to validate as you type.  This may get annoying and/or slow.
