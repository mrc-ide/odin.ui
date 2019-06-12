module_status <- function(class, title, body) {
  list(ui = simple_panel(class, title, body),
       class = class,
       ok = class == "success")
}


show_module_status_if_not_ok <- function(x) {
  if (!isTRUE(x$ok)) {
    x$ui
  }
}


text_module_status <- function(x) {
  paste0("text-", x$class %||% "danger")
}


get_inputs <- function(input, ids, names) {
  set_names(lapply(ids, function(x) input[[x]]), names)
}


odin_colours <- function(model, data, link) {
  col_model <- odin_colours_model(model)
  col_data <- odin_colours_data(data)

  if (length(link) > 0L) {
    link <- list_to_character(link, TRUE)
    col_model[link] <- col_data[names(link)]
  }

  list(model = col_model, data = col_data)
}


odin_colours_data <- function(data) {
  set_names(odin_ui_palettes("brewer_set1")(length(data)), data)
}


## It'll be hard to get the same colours here without creating
## dependencies on the link that are unfortunate.
odin_colours_model <- function(model) {
  set_names(odin_ui_palettes("odin")(length(model)), model)
}


odin_y2 <- function(y2_model, name_data, link) {
  y2_data <- set_names(rep(FALSE, length(name_data)), name_data)
  y2_data[names(link)] <- y2_model[list_to_character(link)]
  list(model = list_to_logical(y2_model, TRUE), data = y2_data)
}
