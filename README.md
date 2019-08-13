# odin.ui

[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)
[![Travis build status](https://travis-ci.org/mrc-ide/odin.ui.svg?branch=master)](https://travis-ci.org/mrc-ide/odin.ui)
[![codecov.io](https://codecov.io/github/mrc-ide/odin.ui/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/odin.ui?branch=master)

A shiny UI for [`odin`](https://github.com/mrc-ide/odin)

## Docker

```
docker pull mrcide/odin.ui
docker run --rm -p 3838:3838 mrcide/odin.ui
```

Then visit http://localhost:3838/odin.ui/

## Testing

Start a selenium server with either

```
./scripts/selenium-start
```

or

```
./scripts/selenium-start --debug
```

(with the latter you can connect over VNC to `localhost:5900`, which is useful for writing the tests and debugging failures, but not needed for running the suite - the password is `secret` [see the docker-selenium repo](https://github.com/SeleniumHQ/docker-selenium)).

The package *must* be installed before running the tests because we use `callr::r_bg` to create a backgrounded R process that will run the shiny app.
