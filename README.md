# odin.ui

[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)
[![Travis build status](https://travis-ci.org/mrc-ide/odin.ui.svg?branch=master)](https://travis-ci.org/mrc-ide/odin.ui)
[![codecov.io](https://codecov.io/github/mrc-ide/odin.ui/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/odin.ui?branch=master)

A shiny UI for [`odin`](https://github.com/mrc-ide/odin)


## Usage

Create a dynamic `odin` model editor:

```r
odin.ui::odin_ui_editor_app()
```

Create an interface for a particular odin model:

```r
odin.ui::odin_ui_app(model, default_time = 10)
```

Code and docs side by side

```r
odin.ui::odin_ui_doc_app(system.file("examples/sir.yml", package = "odin.ui"))
```

Example for teaching

```r
odin.ui::odin_ui_explore("inst/examples/sir.yml")
```

## Docker


```
docker pull mrcide/odin.ui
docker run --rm -p 3838:3838 mrcide/odin.ui
```

Then visit http://localhost:3838/odin.ui/
