## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(knitr)
library(ggplot2)

## ---- message = FALSE,include = TRUE------------------------------------------
library(scipub)

## -----------------------------------------------------------------------------
apastat(stats::cor.test(psydat$Age, psydat$Height), ci = TRUE)

## -----------------------------------------------------------------------------
apastat(stats::t.test(Height ~ Sex, data = psydat))

## -----------------------------------------------------------------------------
apastat(stats::lm(data = psydat, Height ~ Age + Sex))

## -----------------------------------------------------------------------------
apastat(stats::lm(data = psydat, Height ~ Age + Sex), var = "Age")

## ----results="asis"-----------------------------------------------------------
correltable(data = psydat, vars = c("Age", "Height", "iq"),tri="lower",html=TRUE)

## ----results="asis"-----------------------------------------------------------
correltable(data = psydat, vars = c("Age", "Height", "iq"), var_names = c("Age (months)", "Height (inches)", "IQ"), tri = "upper", colnum = TRUE,html=TRUE)

## ----results="asis"-----------------------------------------------------------
correltable(data = psydat, vars = c("Age", "Height", "iq"), var_names = c("Age (months)", "Height (inches)", "IQ"), tri = "upper", method = "spearman", use = "complete", cutempty = TRUE, colnum = TRUE,html=TRUE)

## ----results="asis"-----------------------------------------------------------
correltable(data = psydat, vars = c("Age", "Height", "iq"), var_names = c("Age (months)", "Height (inches)", "IQ"), vars2 = c("depressT", "anxT"), var_names2 = c("Depression T", "Anxiety T"),html=TRUE)

## ----results="asis"-----------------------------------------------------------
correltable(data = psydat, html=TRUE)

## ----results="asis"-----------------------------------------------------------
FullTable1(data = psydat, vars = c("Age", "Sex","Height", "depressT"),  html=TRUE)

## ----results="asis"-----------------------------------------------------------
FullTable1(data = psydat, vars = c("Age", "Height", "depressT"), strata = "Sex", html=TRUE)

## ----results="asis"-----------------------------------------------------------
FullTable1(data = psydat, vars = c("Age", "Sex","Height", "depressT"), var_names = c("Age (months)", "Sex","Height (inches)", "Depression T"), strata = "Income", stars = "stat",p_col = FALSE, html=TRUE)

## ----results="asis"-----------------------------------------------------------
FullTable1(data = psydat, strata = "Sex",stars = "name",p_col = FALSE, html=TRUE)

## ----results="asis"-----------------------------------------------------------
temp <- data.frame(iq=psydat$iq, iq_winsor=winsorZ(psydat$iq), iq_outlier = winsorZ_find(psydat$iq))
summary(temp)
ggplot(temp[!is.na(temp$iq),], aes(x=iq, y=iq_winsor)) + geom_point(aes(color=iq_outlier),alpha=.7) + geom_line() + theme_bw()

