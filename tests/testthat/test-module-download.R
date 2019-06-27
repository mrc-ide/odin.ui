context("module: download")

test_that("download filename", {
  re <- "^odin-prefix-modelled-[0-9]{8}-[0-9]{6}.csv$"
  names <- download_names(filename = c("modelled", "combined", "parameters"),
                          display = c("Modelled", "Combined", "Parameters"),
                          data = c("smooth", "combined", "user"))

  expect_match(download_filename(NULL, "prefix", "Modelled", names), re)
  expect_match(download_filename(NA, "prefix", "Modelled", names), re)
  expect_match(download_filename("", "prefix", "Modelled", names), re)

  expect_equal(download_filename("foo", "prefix", "Modelled", names),
               "foo.csv")
  expect_equal(download_filename("foo.csv", "prefix", "Modelled", names),
               "foo.csv")
})


test_that("download data", {
  path <- tempfile()
  on.exit(unlink(path))
  simulation <- list(smooth = data_frame(x = 1),
                     combined = data_frame(x = 2),
                     user = data_frame(x = 3))
  names <- download_names(c("modelled", "combined", "parameters"),
                          data = c("smooth", "combined", "user"))

  download_data(path, simulation, "modelled", names)
  expect_equal(read_csv(path), simulation$smooth)

  download_data(path, simulation, "combined", names)
  expect_equal(read_csv(path), simulation$combined)

  download_data(path, simulation, "parameters", names)
  expect_equal(read_csv(path), simulation$user)
})
