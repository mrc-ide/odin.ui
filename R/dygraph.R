dygraph_multi <- function(dat, include, cols, mean, interval) {
  is_multi <- vapply(dat, is.array, logical(1))
  order <- unique(c(names(interval), include, mean))

  ## I don't think this is the most efficient way of doing this but it
  ## will work at least.
  use <- data.frame(time = dat[[1L]])
  series <- list()

  ## so, first pull in all the intervals, because these are the
  ## hardest.
  dat_interval <- lapply(names(interval), dygraph_interval, dat, interval)
  ser_interval <- Map(function(d, c)
    list(names = colnames(d), col = c, width = 2, n = 3),
    dat_interval, cols[names(interval)])

  mean <- setdiff(mean, names(interval))
  dat_mean <- lapply(dat[mean], rowMeans)
  ser_mean <- Map(function(v, c)
    list(names = v, col = c, width = 2, n = 1),
    mean, cols[mean])

  ## Then we pull in the direct series too; this is different for
  ## multi and non-multi
  if (length(include) > 0L) {
    dat_individual <- dat[include]
    i <- vlapply(dat_individual, is.array)
    for (v in include[i]) {
      colnames(dat_individual[[v]]) <-
        sprintf("%s[%d]", v, seq_len(ncol(dat_individual[[v]])))
    }

    ser_individual <- lapply(include, function(v) {
      d <- dat_individual[[v]]
      if (is.array(d)) {
        list(names = colnames(d), col = transp(cols[[v]], .3),
             width = 1, n = ncol(d))
      } else {
        list(names = v, col = cols[[v]], width = 2, n = 1)
      }
    })
    if (all(i)) {
      names(dat_individual) <- NULL
    } else {
      names(dat_individual)[i] <- ""
    }
  } else {
    ser_individual <- dat_individual <- NULL
  }

  dat_use <- as.data.frame(c(list(time = dat[[1L]]),
                             dat_interval,
                             dat_mean,
                             dat_individual),
                           check.names = FALSE)
  ser <- c(ser_interval, ser_mean, ser_individual)

  ## The function can be broken in two here; above is data assembly,
  ## below is plotting
  cols_use <- lapply(ser, function(s) rep(s$col, s$n))

  out <- dygraphs::dygraph(dat_use, xlab = "Time")
  out <- dygraphs::dyOptions(out,
                             colors = unlist(cols_use, use.names = FALSE),
                             strokeWidth = 1,
                             labelsKMB = TRUE,
                             animatedZooms = TRUE,
                             drawGrid = FALSE)
  for (s in ser_interval) {
    out <- dygraphs::dySeries(out, s$names, color = s$col,
                              strokeWidth = s$width)
  }
  for (s in ser_mean) {
    out <- dygraphs::dySeries(out, s$names, color = s$col,
                              strokeWidth = s$width)
  }
  for (s in ser_individual) {
    if (length(s$names) == 1L && s$names %in% include) {
      out <- dygraphs::dySeries(out, s$names, color = s$col,
                                strokeWidth = s$width)
    }
  }

  out <- dygraphs::dyHighlight(out,
                               highlightSeriesOpts = list(strokeWidth = 2),
                               highlightCircleSize = 3,
                               highlightSeriesBackgroundAlpha = 1,
                             hideOnMouseOut = TRUE)
  out <- dygraphs::dyCSS(out, "dygraph.css")
  out
}


dygraph_interval <- function(v, dat, intervals) {
  r <- apply(dat[[v]], 1, quantile, intervals[[v]])
  m <- rowMeans(dat[[v]])
  ret <- cbind(r[1, ], m, r[2, ])
  colnames(ret) <- sprintf(c("%s[lower]", "%s", "%s[upper]"), v)
  ret
}
