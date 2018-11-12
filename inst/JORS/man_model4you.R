## ----setup, echo=FALSE, message=FALSE------------------------------------
library("knitr")
knitr::opts_chunk$set(cache = TRUE, message = FALSE, fig.align = "center")

# knit_hooks$set(inline = function(x) {
#   if (is.numeric(x)) return(knitr:::format_sci(x, 'latex'))
#   highr:::hi_latex(x)
# })

library("dplyr")
library("stringr")

knit_hooks$set(inline = function(x) {
  if (is.numeric(x)) return(knitr:::format_sci(x, 'latex'))
  highr:::hi_latex(x) %>%  str_replace("hlopt", "textalltt")
  })

## ----eval=FALSE----------------------------------------------------------
## base_model <- model(response ~ treatment, data)

## ----eval=FALSE----------------------------------------------------------
## strat_models <- pmtree(base_model)

## ----eval=FALSE----------------------------------------------------------
## pm_forest <- pmforest(base_model)
## pers_models <- pmodel(pm_forest)

## ----prep----------------------------------------------------------------
library("model4you")
set.seed(2017)

library("ggplot2")
theme_set(theme_classic())
library("ggbeeswarm")
library("gridExtra")

## ----math_data-----------------------------------------------------------
data("MathExam14W", package = "psychotools")

## scale points achieved to [0, 100] percent
MathExam14W$tests <- 100 * MathExam14W$tests/26
MathExam14W$pcorrect <- 100 * MathExam14W$nsolved/13

## select variables to be used
MathExam <- MathExam14W[ , c("pcorrect", "group", "tests", "study",
                             "attempt", "semester", "gender")]

## ----math_bmod-----------------------------------------------------------
bmod_math <- lm(pcorrect ~ group, data = MathExam)

## ------------------------------------------------------------------------
cbind(estimate = coef(bmod_math), confint(bmod_math))

## ----math_bmod_vis, fig.height=2.5, fig.width=5, out.width="0.7\\textwidth", fig.cap = "Density estimates of base model for the Mathematics Exam data."----
lm_plot(bmod_math)

## ----math_tree, fig.width = 13, fig.cap = "Personalised model tree for the Mathematics Exam datam."----
tr_math <- pmtree(bmod_math, control = ctree_control(maxdepth = 2))
plot(tr_math, terminal_panel = node_pmterminal(tr_math,
                                               plotfun = lm_plot))

## ----math_pmodl----------------------------------------------------------
forest_math <- pmforest(bmod_math)
pmods_math <- pmodel(forest_math)

## model parameters of first 6 students
head(pmods_math)

## ----math_dp1, fig.width=4, fig.height=3, out.width='.48\\linewidth', fig.cap="Dependence plot for percentage of tests successfully solved.", fig.subcap=c("Scatter plot with loess curve.", "Beeswarm plot.")----
dpdat_math <- cbind(pmods_math, MathExam)

ggplot(dpdat_math, aes(x = tests, y = group2)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(fill = NA, method = "loess") +
  theme(legend.position = "none") +
  ylab("estimated individual\nexam group effect")

ggplot(dpdat_math, aes(x = tests, y = group2, color = tests)) +
  geom_quasirandom(alpha = 0.5, size = 1) +
  theme(legend.position = "none") +
  ylab("estimated individual\nexam group effect")

## ----math_dp2, fig.width=4, fig.height=3, out.width='.48\\linewidth', fig.cap="Dependence plots for the number of previous attempts and gender.", fig.subcap=c('Gender.', 'Number of previous attempts and gender.')----
ggplot(dpdat_math, aes(x = gender, y = group2, color = gender)) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.5) +
  theme(legend.position = "none") +
  ylab("estimated individual\nexam group effect")

ggplot(dpdat_math, aes(x = attempt, y = group2, color = gender)) +
  geom_quasirandom(alpha = 0.5) +
  ylab("estimated individual\nexam group effect")

