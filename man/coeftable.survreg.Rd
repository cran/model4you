% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-pmtree.R
\name{coeftable.survreg}
\alias{coeftable.survreg}
\title{Table of coefficients for survreg model}
\usage{
coeftable.survreg(model, confint = TRUE, digits = 2, intree = FALSE)
}
\arguments{
\item{model}{model of class \code{\link[survival]{survreg}}}

\item{confint}{should a confidence interval be computed? Default: TRUE}

\item{digits}{integer, used for formating numbers. Default: 2}

\item{intree}{is the table plotted within a tree? Default: FALSE}
}
\value{
None.
}
\description{
This function is mostly useful for plotting a pmtree.
The generic plotting does not show the estimate and confidence interval
of the scale parameter. This one does.
}
\examples{
if(require("survival") & require("TH.data")) {
  ## Load data
  data(GBSG2, package = "TH.data")

  ## Weibull model
  bmod <- survreg(Surv(time, cens) ~ horTh, data = GBSG2, model = TRUE)

  ## Coefficient table
  grid.newpage()
  coeftable.survreg(bmod)

  ## partitioned model
  tr <- pmtree(bmod)

  ## plot
  plot(tr, terminal_panel = node_pmterminal(tr, plotfun = survreg_plot,
    confint = TRUE, coeftable = coeftable.survreg))
}

}
