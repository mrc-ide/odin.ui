FROM rocker/shiny:latest
COPY docker/bin /usr/local/bin/

## These are the deps pulled separately for faster builds
RUN install_packages --repo "https://mrc-ide.github.io/drat" \
  dde \
  dygraphs \
  odin \
  remotes \
  shinyAce

## This will change fairly frequently
RUN install_remote \
  mrc-ide/odin@development

COPY docker/app /srv/shiny-server

## And this one will change all the time
COPY . /src
RUN R CMD INSTALL /src && rm -rf /src
