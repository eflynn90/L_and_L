#!/usr/bin/Rscript
##
##  EDF 7/30/21
##

library(dplyr)
library(ggplot2)

setwd("~/Downloads/")


## Read in expression and genotype data
expr_gt = read.table("APBB1IPexpr_gt.txt",
                     header=TRUE,sep='\t')
names(expr_gt)
head(expr_gt)


## Plot expression and genotype data
expr_gt %>%
  ggplot(aes(chr10_26434585_G_A_b38, ABPP1IP_expr)) +
  geom_point()

expr_gt %>%
  ggplot(aes(as.factor(chr10_26434585_G_A_b38), 
             log10(ABPP1IP_expr))) +
  geom_point() +
  geom_boxplot()


## Calculate linear model on expression and genotype data
summary(lm(data = expr_gt,
   ABPP1IP_expr ~ chr10_26434585_G_A_b38))
summary(lm(data = expr_gt,
           log10(ABPP1IP_expr) ~ chr10_26434585_G_A_b38))
lm_var = summary(lm(data = expr_gt,
                    log10(ABPP1IP_expr) ~ chr10_26434585_G_A_b38))
lm_var$coefficients


## Optional: plot data with linear regression line
expr_gt %>%
  ggplot(aes(chr10_26434585_G_A_b38, 
             log10(ABPP1IP_expr))) +
  geom_point(position=position_jitter(width=.2)) +
  geom_abline(slope = lm_var$coefficients[2,1],
              intercept = lm_var$coefficients[1,1])
expr_gt %>%
  ggplot(aes(as.factor(chr10_26434585_G_A_b38), 
             log10(ABPP1IP_expr))) +
  geom_point() +
  geom_boxplot() +
  geom_abline(slope = lm_var$coefficients[2,1],
              intercept = lm_var$coefficients[1,1] -
                lm_var$coefficients[2,1])






