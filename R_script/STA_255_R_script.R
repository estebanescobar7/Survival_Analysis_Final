

library(tidyverse)
library(kableExtra)
library(table1)
library(ggplot2)
library(patchwork)
library(survival)
library(Epi)

# helper function for tables and data analysis
source( "/Users/Esteban/Desktop/Schools/my_functions/helper_function.R" )
# ------------------------------------------------------------------------------
# ----------------- Setting up and cleaning data    -----------------------------
# ------------------------------------------------------------------------------

# read in data 
usrds <- read.csv( "/Users/Esteban/Desktop/Schools/UCI/Courses/2022-2023/Winter_2022/STA\ 255/Exam/Final/data/usrdsData.csv")
usrds_long <- read.csv( "/Users/Esteban/Desktop/Schools/UCI/Courses/2022-2023/Winter_2022/STA\ 255/Exam/Final/data/LongitudinalAlbumin.csv")

# --------------------------------------------------------------------------- 
# ----------------------Let look at  baseline     ---------------------------
# --------------------------------------------------------------------------- 

# clean up data 

usrds_baseline <- usrds %>% 
  mutate(low_albumin = case_when(albumin.0 < 3.6 ~ "Yes", 
                                 albumin.0 >= 3.5 ~ "No"), 
         low_albumin = factor(low_albumin,
                              levels = c("No", "Yes"))) %>% 
  mutate(bmi.category = case_when( bmi < 18.5 ~ "Underweight", 
                                   bmi >= 18.5 & bmi < 25 ~ "Healthy Weight", 
                                   bmi >= 25 & bmi < 30 ~ "Overwight", 
                                   bmi >= 30 ~ "Obese" ), 
         bmi.category = factor(bmi.category,
                               levels = c("Healthy Weight", "Underweight", "Overwight", "Obese"))) %>% 
  rename(sex = female ) %>% 
  mutate(sex = factor(sex,
                      levels = c(0,1), 
                      labels = c("Male", "Female" ))) %>% 
  mutate(racegrp = factor(racegrp,
                          levels = c(1,2,3), 
                          labels = c("Caucasian", "African American", "Other"))) %>% 
  mutate(smokegrp = factor(smokegrp, 
                           levels = c(1,2,3), 
                           labels = c( "Never", "Former","Current"))) %>% 
  mutate(hist.cvd = factor(hist.cvd, 
                           levels = c(0,1), 
                           labels = c("No", "Yes" ))) %>% 
  mutate(diabetes = factor(diabetes, 
                           levels = c(0,1), 
                           labels = c("No", "Yes"))) %>% 
  mutate(undnour = factor(undnour, 
                          levels = c(0,1), 
                          labels = c("No", "Yes"))) %>% 
  relocate(low_albumin, .after = "albumin.0" ) %>% 
  relocate(bmi.category, .after = "bmi" ) %>% 
  relocate( c(albumin.0, low_albumin, hist.cvd), .after = "usrds.id"  ) %>% 
  rename(albumin = albumin.0  ) 

# --------------------------------------------------------------------------- 
# ----------------------Let look at longitudinal   --------------------------
# --------------------------------------------------------------------------- 

# merge data
usrds_all <- merge(usrds_long ,usrds, by = "usrds.id")

# make data ready for analysis 
usrds_all <- usrds_all %>% 
  group_by(usrds.id) %>% 
  mutate(start = measday, 
         stop = lead(measday, n = 1  ), 
         stop = ifelse(is.na(stop) == TRUE, tdeath, stop), 
         death = ifelse(death == 1, c(rep(0,length(usrds.id)-1),1), death)) 

# cleaning up the data 
usrds_all <- usrds_all %>% 
  mutate(low_albumin = case_when(albumin < 3.6 ~ "Yes", 
                                 albumin >= 3.5 ~ "No"), 
         low_albumin = factor(low_albumin,
                              levels = c("No", "Yes"))) %>% 
  mutate(bmi.category = case_when( bmi < 18.5 ~ "Underweight", 
                                   bmi >= 18.5 & bmi < 25 ~ "Healthy Weight", 
                                   bmi >= 25 & bmi < 30 ~ "Overwight", 
                                   bmi >= 30 ~ "Obese" ), 
         bmi.category = factor(bmi.category,
                               levels = c("Healthy Weight", "Underweight", "Overwight", "Obese"))) %>% 
  rename(sex = female ) %>% 
  mutate(sex = factor(sex,
                      levels = c(0,1), 
                      labels = c("Male", "Female" ))) %>% 
  mutate(racegrp = factor(racegrp,
                          levels = c(1,2,3), 
                          labels = c("Caucasian", "African American", "Other"))) %>% 
  mutate(smokegrp = factor(smokegrp, 
                           levels = c(1,2,3), 
                           labels = c( "Never", "Former","Current"))) %>% 
  mutate(hist.cvd = factor(hist.cvd, 
                           levels = c(0,1), 
                           labels = c("No", "Yes" ))) %>% 
  mutate(diabetes = factor(diabetes, 
                           levels = c(0,1), 
                           labels = c("No", "Yes"))) %>% 
  mutate(undnour = factor(undnour, 
                          levels = c(0,1), 
                          labels = c("No", "Yes"))) %>% 
  relocate(low_albumin, .after = "albumin" ) %>% 
  relocate(bmi.category, .after = "bmi" ) %>% 
  relocate( c(start, stop ), .after = measday )

# --------------------------------------------------------
# -----------------   EDA    -----------------------------
# --------------------------------------------------------

# focus on baseline data 

#length(unique(usrds_baseline$usrds.id)) # 1979

#summary(usrds_baseline)

# I Google and I think having a 7 level in Serum albumin is okay 

p1 <- ggplot(usrds_baseline) + 
  aes(x = albumin, y =  ..density..   ) + 
  geom_histogram(color = "black", 
                 fill = "white", 
                 binwidth = 0.2) + 
  geom_density( alpha = .10, fill = "red") +
  xlab('Serum Albumin') +
  ggtitle("Histogram of Serum Albumin At Baseline") +
  theme_minimal() 

p2 <- ggplot(usrds_baseline) + 
  aes(x = albumin) + 
  geom_boxplot() + 
  xlab('Serum Albumin') +
  ggtitle("Boxplot of Serum Albumin At Baseline") +
  theme_minimal() 

ggsave("histogram.pdf",
       plot = p1,
       device = "pdf",
       path = "/Users/Esteban/Desktop/Schools/UCI/Courses/2022-2023/Winter_2022/STA\ 255/Exam/Final/Figures",
       dpi = "retina")

pdf(file="/Users/Esteban/Desktop/Schools/UCI/Courses/2022-2023/Winter_2022/STA\ 255/Exam/Final/Figures/survial_curve.pdf", 
    width = 9, 
    height = 9)
par(mfrow = c(2,2))
fit <- survfit( Surv(tdeath, death) ~ low_albumin, data=usrds_baseline)  
plot( fit, xscale = 365.25, lty=1:2, col = 1:2, main= "Albumin", cex.main = 0.8, xlab = "Time (Years)", ylab = "Survival" )
legend( 0, 0.5, lty=1:2, col = 1:2, legend= c( "High Albumin", "Low Albumin" ), bty="n" )
fit <- survfit( Surv(tdeath, death) ~ hist.cvd, data=usrds_baseline)  
plot( fit, xscale = 365.25, lty=1:2, col = 1:2, main = "History of Cardiovascular Disease", cex.main = 0.8, xlab = "Time (Years)", ylab = "Survival"  )
legend( 0, 0.5, lty=1:2, col = 1:2, legend= c( "No", "Yes" ), bty="n" )
fit <- survfit( Surv(tdeath, death) ~ low_albumin + hist.cvd, data=usrds_baseline)  
plot( fit, xscale = 365.25, lty=1:4, col = 1:4, main = "Serum Albumin with History of CVD", cex.main = 0.8, xlab = "Time (Years)", ylab = "Survival"  )
legend( 0, 0.5, lty=1:4, col = 1:4, legend= c( "Low w/o cvd", "High w/ cvd", "High w/o cvd", "Low w/ cvd" ), bty="n" )
dev.off()

# fit <- survfit( Surv(tdeath, death) ~ bmi.category, data=usrds_baseline)  
# plot( fit, xscale = 365.25, lty=1:4, col = 1:4  )
# legend( 100, 0.5, lty=1:4, col = 1:4, legend= c( "Healthy Weight", "Underweight", "Overwight", "Obese" ), bty="n" )

pdf(file="/Users/Esteban/Desktop/Schools/UCI/Courses/2022-2023/Winter_2022/STA\ 255/Exam/Final/Figures/survial_curve_other.pdf", 
    width = 9, 
    height = 9)
par(mfrow = c(2,2))
fit <- survfit( Surv(tdeath, death) ~ racegrp, data=usrds_baseline)  
plot( fit, xscale = 365.25, lty=1:3, col = 1:3, main = "Race Group", cex.main = 0.8, xlab = "Time (Years)", ylab = "Survival"  )
legend( 0, 0.6, lty=1:3, col = 1:3, legend= c( "Caucasian", "African American", "Other"), bty="n" )
fit <- survfit( Surv(tdeath, death) ~ diabetes, data=usrds_baseline)  
plot( fit, xscale = 365.25, lty=1:2, col = 1:2, main = "Diabetes Status", cex.main = 0.8, xlab = "Time (Years)", ylab = "Survival"  )
legend( 0, 0.6, lty=1:2, col = 1:2, legend= c( "No", "Yes"), bty="n" )
fit <- survfit( Surv(tdeath, death) ~ smokegrp, data=usrds_baseline)  
plot( fit, xscale = 365.25, lty=1:3, col = 1:3, main = "Smoking Status", cex.main = 0.8, xlab = "Time (Years)", ylab = "Survival"  )
legend( 0, 0.6, lty=1:3, col = 1:3, legend= c( "Never", "Former", "Current"), bty="n" )
fit <- survfit( Surv(tdeath, death) ~ sex, data=usrds_baseline)  
plot( fit, xscale = 365.25, lty=1:2, col = 1:2, main = "Sex", cex.main = 0.8, xlab = "Time (Years)", ylab = "Survival"  )
legend( 0, 0.6, lty=1:2, col = 1:2, legend= c( "Male", "Female"), bty="n" )
dev.off()

# variables to look at 
# covarites of interest: albumin
# response: Total mortality among hemodialysis patients
# confounders:  age, sex, smokegrp, bmi, esrdtime, hist.cvd
# precision variables: undnour, racegrp, diabetes 

# tables to look at 
table1(~ albumin + low_albumin + hist.cvd +  age + sex + smokegrp + bmi + bmi.category + esrdtime  + undnour + racegrp + diabetes | death, data = usrds_baseline  )
table1(~  hist.cvd + age + sex + smokegrp + bmi + bmi.category + esrdtime + undnour + racegrp + diabetes | low_albumin, data = usrds_baseline  )
table1(~   albumin + low_albumin + age + sex + smokegrp + bmi + bmi.category + esrdtime + undnour + racegrp + diabetes | hist.cvd, data = usrds_baseline  )
table1(~  albumin + low_albumin + hist.cvd + age + sex + smokegrp + bmi + bmi.category + esrdtime  + undnour + racegrp + diabetes , data = usrds_baseline  )


# --------------------------------------------------------
# -----------------   Aim 1    ---------------------------
# --------------------------------------------------------

# a. Quantify the association between baseline serum albumin and the risk of mortality.

# here we will fit the following model 

# keeping albumin continuous: 

fit1 <- coxph( Surv(tdeath,death) ~ albumin + hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5) + esrdtime + undnour + racegrp + diabetes, data = usrds_baseline )
summary( fit1 )

adj_table.coxph( Surv(tdeath,death) ~ albumin + hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5) + esrdtime + undnour + racegrp + diabetes, 
                 data = usrds_baseline, 
                 fileout = "/Users/Esteban/Desktop/Schools/UCI/Courses/2022-2023/Winter_2022/STA\ 255/Exam/Final/Latex/model1a.tex", 
                 digts = 3, 
                 se = TRUE)

# fit2 <- coxph( Surv(tdeath, death ) ~ albumin + hist.cvd + I(age/10) + sex + smokegrp + bmi.category  + esrdtime  + undnour + racegrp + diabetes  , data = usrds_baseline )
# summary( fit2 )

# # Keeping albumin dichotomous: 
fit3 <- coxph(Surv(tdeath,death) ~ low_albumin + hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5) + esrdtime + undnour + racegrp + diabetes, data = usrds_baseline )
summary( fit3 )

# adjusting for all other covarites we estimate the relative risk of mortality among hemodialysis patients is estimated to be 57.33% higher comparing two sub-population
# one of which are people with low Serum Albumin and the other normal level of serum albumin. 

# fit4 <- coxph( Surv(tdeath, death ) ~ low_albumin  + I(age/10) + sex + smokegrp + bmi.category  + esrdtime + hist.cvd + undnour + racegrp + diabetes  , data = usrds_baseline )
# summary( fit4 )

# b) Quantify the potential difference in the association between baseline albumin 
#    and the risk of mortality comparing patients with and without a history of cardiovascular disease.

# we want to fit an interaction and test it: consider the following: 

fit.full <- coxph(Surv(tdeath,death) ~ albumin + hist.cvd + albumin*hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5) + esrdtime + undnour + racegrp + diabetes, data = usrds_baseline )
summary( fit.full )

adj_table.coxph( Surv(tdeath,death) ~ albumin + hist.cvd + albumin*hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5) + esrdtime + undnour + racegrp + diabetes, 
                 data = usrds_baseline, 
                 fileout = "/Users/Esteban/Desktop/Schools/UCI/Courses/2022-2023/Winter_2022/STA\ 255/Exam/Final/Latex/model1b.tex", 
                 digts = 3, 
                 se = TRUE)

fit.reduce <- coxph( Surv(tdeath, death ) ~ albumin + hist.cvd   + I(age/10) + sex + smokegrp + I(bmi/5)  + esrdtime + undnour + racegrp + diabetes  , data = usrds_baseline )
anova(  fit.reduce, fit.full )
linContr.coxph(model = fit.full, 
               contr.names = c( "albumin", "albumin:hist.cvdYes"), 
               contr.coef = c(1,1))



# --------------------------------------------------------
# -----------------   Aim 2    ---------------------------
# --------------------------------------------------------

# Using data on repeated albumin measurements: Repeat your analysis above to
# by considering albumin as a time-dependent covariate and compare/contrast 
# your results commenting on the likely reasons for any diﬀerences you observe.

# now consider the full data 
summary(usrds_all)
#length(unique(usrds_all$usrds.id)) # 1979

# a Quantify the association between serum albumin and the risk of mortality.

fit <- coxph(Surv(start,stop,death) ~ albumin + hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5)  + esrdtime + undnour + racegrp + diabetes , data=usrds_all )
summary(fit)
adj_table.coxph( Surv(start,stop,death) ~ albumin + hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5)  + esrdtime + undnour + racegrp + diabetes , 
                 data=usrds_all,
                 fileout = "/Users/Esteban/Desktop/Schools/UCI/Courses/2022-2023/Winter_2022/STA\ 255/Exam/Final/Latex/model2a.tex", 
                 digts = 3, 
                 se = TRUE )


# b) Quantify the potential difference in the association between baseline albumin 
#    and the risk of mortality comparing patients with and without a history of cardiovascular disease.

# consider the following: 

fit.full <- coxph(Surv(start,stop,death) ~ albumin + hist.cvd + albumin*hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5)  + esrdtime + undnour + racegrp + diabetes , data=usrds_all )
summary(fit.full)
adj_table.coxph( Surv(start,stop,death) ~ albumin + hist.cvd + albumin*hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5)  + esrdtime + undnour + racegrp + diabetes, 
                 data=usrds_all,
                 fileout = "/Users/Esteban/Desktop/Schools/UCI/Courses/2022-2023/Winter_2022/STA\ 255/Exam/Final/Latex/model2b.tex", 
                 digts = 3, 
                 se = TRUE)

fit.reduce <- coxph( Surv( start, stop, death) ~  albumin + hist.cvd  + I(age/10) + sex + smokegrp + I(bmi/5)  + esrdtime + undnour + racegrp + diabetes , data=usrds_all )
anova(  fit.reduce, fit.full )
linContr.coxph(model = fit.full, 
               contr.names = c( "albumin", "albumin:hist.cvdYes"), 
               contr.coef = c(1,1) )

# --------------------------------------------------------
# -----------------   Aim 3    ---------------------------
# --------------------------------------------------------

# Alternative sampling strategy: Repeat your analysis for 1(a) using a nested 
# case-control design with M = 4 controls per case. Compare your estimated 
# coeﬃcients to those obtained using the full cohort in 1(a) and comment on t
# the savings in total of individuals used in the analysis relative to the 
# increased variance observed for the primary question of interest. 

# This section can be placed in the Appendix of your report.

# we will consider the baseline data again. 

#summary(usrds_baseline)

# Recall our model from before: 

fit1 <- coxph( Surv(tdeath, death ) ~ albumin + hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5)  + esrdtime  + undnour + racegrp + diabetes  , data = usrds_baseline )
summary( fit1 )

#####	Sample with 4:1 Controls:Cases
##
set.seed(1006752954)
tx.cc <- ccwc( exit=tdeath, fail=death, controls=4, data=usrds_baseline, include = list(low_albumin,hist.cvd, age, sex, smokegrp, bmi, esrdtime, undnour, racegrp, diabetes, tdeath ) )
head(tx.cc, 10)

##
#####	Plot time for cases and controls (control time >= case time)
##

pdf(file="/Users/Esteban/Desktop/Schools/UCI/Courses/2022-2023/Winter_2022/STA\ 255/Exam/Final/Figures/nested_case_study.pdf", 
    width = 10, 
    height = 5)
par(mfrow = c(1,2))
plot(tx.cc$Time[tx.cc$Fail==1], tx.cc$tdeath[tx.cc$Fail==1])
plot(tx.cc$Time[tx.cc$Fail==0], tx.cc$tdeath[tx.cc$Fail==0])
dev.off()

##
#####	Fit the model via clogit() (actually calling coxph...)
##

fit.cc <- clogit(Fail ~ low_albumin + hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5)  + esrdtime  + undnour + racegrp + diabetes + strata(Set), data = tx.cc)
summary(fit.cc)


fileout <- "/Users/Esteban/Desktop/Schools/UCI/Courses/2022-2023/Winter_2022/STA\ 255/Exam/Final/Latex/model3.tex"
digts <- 3

cat("\\begin{tabular}{", rep("c",5), "}", "\n",
    "\\toprule", "\n",
    file=fileout )
cat("\\textbf{Covariate} && \\multicolumn{3}{c}{\\textbf{Adjusted}}", " \\\\ ",  "\n",
    "\\hline ", '\n',
    '\\\\',
    "\n",
    file = fileout,
    append = TRUE)
cat( paste("&&", "RR", " \\quad ",  "(95\\% CI) & SE &  P-value", " \\\\", "\n",
           "\\\\",
           "\n"),
     file=fileout,
     append=TRUE )

result <- data.frame()

fit.all <- fit.cc
summ.fit.all <-  summary(fit.all); summ.fit.all
results.all <- as.data.frame( cbind(summ.fit.all$conf.int, se = summ.fit.all$coefficients[, "se(coef)" ], p_value  = summ.fit.all$coefficients[,"Pr(>|z|)"]))

colnames(results.all) <- lapply( c("RR", "exp(-coef)", "ci95.lo", "ci95.hi", "SE", "p_value") , paste, ".adj", sep = "" )
results.all <- tibble::rownames_to_column(results.all, "row.name") 
table <- results.all
write.table( paste(table$row.name,
                   " && ",
                   format(table$RR.adj, digits = digts, nsmall = digts),
                   " (", format(table$ci95.lo.adj, digits = digts, nsmall = digts), ", ", format(table$ci95.hi.adj, digits = digts, nsmall = digts), ")",
                   " & ",
                   round(table$SE.adj, digts ),
                   " & ",
                   ifelse(table$p_value.adj <.0001, "$<$.0001", round(table$p_value.adj,4) ),
                   "\\\\",
                   sep=""),
             file=fileout,
             quote=FALSE,
             row.names=FALSE,
             col.names=FALSE,
             append=TRUE )
cat( "\\bottomrule", "\n", "\\end{tabular}", file=fileout, append=TRUE )






# --------------------------------------------------------
# ----------------- Diagnostics    -----------------------
# --------------------------------------------------------



##
#####	Consider outliers (in the Y|X-space)
##


fit1 <- coxph( Surv(tdeath,death) ~ albumin + hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5) + esrdtime + undnour + racegrp + diabetes, data = usrds_baseline )
fit_tvc <- coxph(Surv(start,stop,death) ~ albumin + hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5)  + esrdtime + undnour + racegrp + diabetes , data=usrds_all )

pdf(file="/Users/Esteban/Desktop/Schools/UCI/Courses/2022-2023/Winter_2022/STA\ 255/Exam/Final/Figures/diagnostic_no_interaction.pdf", 
    width = 10, 
    height = 5)
par(mfrow = c(1,2))
# plot 1
dresids <- residuals(fit1, type="deviance" )
mresids <- residuals(fit1, type="martingale" )
lp <- predict( fit1, type="lp" )
plot( lp, dresids, xlab="Linear Predictor", ylab="Deviance Residual", main = "Baseline" )
abline( h=2.5, col="red" )
# plot 2
dresids <- residuals(fit_tvc, type="deviance" )
mresids <- residuals(fit_tvc, type="martingale" )
lp <- predict( fit_tvc, type="lp" )
plot( lp, dresids, xlab="Linear Predictor", ylab="Deviance Residual", main = "Repeated measure" )
abline( h=2.5, col="red" )
dev.off


fit.full <- coxph(Surv(tdeath,death) ~ albumin + hist.cvd + albumin*hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5) + esrdtime + undnour + racegrp + diabetes, data = usrds_baseline )
fit.full_tvc <- coxph(Surv(start,stop,death) ~ albumin + hist.cvd + albumin*hist.cvd + I(age/10) + sex + smokegrp + I(bmi/5)  + esrdtime + undnour + racegrp + diabetes, data=usrds_all )


pdf(file="/Users/Esteban/Desktop/Schools/UCI/Courses/2022-2023/Winter_2022/STA\ 255/Exam/Final/Figures/plot1", 
    width = 9, 
    height = 9)
par(mfrow = c(1,2))
# plot 1
dresids <- residuals(fit.full, type="deviance" )
mresids <- residuals(fit.full, type="martingale" )
lp <- predict( fit.full, type="lp" )
plot( lp, dresids, xlab="Linear Predictor", ylab="Deviance Residual", main = "Baseline" )
abline( h=2.5, col="red" )
# plot 2
dresids <- residuals(fit.full_tvc, type="deviance" )
mresids <- residuals(fit.full_tvc, type="martingale" )
lp <- predict( fit.full_tvc, type="lp" )
plot( lp, dresids, xlab="Linear Predictor", ylab="Deviance Residual", main = "Reapeated measurement" )
abline( h=2.5, col="red" )
dev.off



















