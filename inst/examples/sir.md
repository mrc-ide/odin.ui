## A simple SIR model

An SIR model is a simple compartmental model.

In `odin` code, the `deriv` function indicates that something is a _variable_ and that we will work with derivatives of that variable with respect to time:


```r
deriv(S) <- ...
deriv(I) <- ...
deriv(R) <- ...
```

...and so on
