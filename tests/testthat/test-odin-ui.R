context("odin.ui")


test_that("parameter inteface generates as expected", {
  gen <- odin::odin({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user(1)
    K <- user(100)
    r <- user(0.5)
  })

  p <- odin_ui_parameters(gen)
  expect_is(p, "list")
  expect_equal(length(p), 4L)
  id <- vapply(p[-1], function(x) x$children[[2]]$attribs$id, "")
  expect_equal(id, paste0("pars_", coef(gen)$name))
})
