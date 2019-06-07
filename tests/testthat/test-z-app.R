context("selenium: app")

## TODO: this can be factored out into a test for the csv module but I
## do need a driver for that.
test_that("integration", {
  dr <- selenium_driver()

  app <- launch_prototype()

  dr$navigate(app$url)
  title <- dr$getTitle()[[1]]
  expect_equal(title, "odin editor")

  upload <- retry_until_element_exists(dr, shiny::NS("odin_csv", "filename"))

  ## Upload data into the app:
  path <- path_remote("anne/data/combined.csv")
  upload$sendKeysToElement(list(path))

  ## We should automatically set the time variable field now
  Sys.sleep(0.5) # not sure about this; poll for non empty?
  summary <- dr$findElement("id", shiny::NS("odin_csv", "summary"))
  expect_equal(
    summary$getElementText()[[1]],
    "Uploaded 56 rows and 3 columns. Response variables: cases, deaths")

  ## For now, just check that there is actually a plot produced.
  ## Unfortunately I don't see the svg element here though.
  plot <- dr$findElement("id", shiny::NS("odin_csv", "data_plot"))
  expect_equal(length(plot$findChildElements("xpath", "*[1]")), 1)

  table <- dr$findElement("id", shiny::NS("odin_csv", "data_table"))
  table_head <- table$findChildElement("xpath", ".//thead/tr")
  expect_equal(vcapply(table_head$findChildElements("tag name", "th"),
                       function(x) x$getElementText()[[1]]),
               c("day", "cases", "deaths"))

  status <- dr$findElement("id", "status")
  icons <- status$findChildElements("tag name", "i")
  expect_equal(
    vcapply(icons, function(x) x$getElementAttribute("class")[[1]]),
    c("fa fa-table text-success",
      "fa fa-edit text-danger",
      "fa fxa-random text-danger"))
})
