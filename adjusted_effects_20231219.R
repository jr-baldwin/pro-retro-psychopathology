################################################################################
#### Calculating Adjusted Effects for Prospective & Retrospective Measures #####
################################################################################

# Note: Not all studies included reported adjusted associations between prospective 
# and retrospective measures with psychopathology. Where studies reported both 
# (i) the agreement between prospective and retrospective measures, and 
# (ii) the unadjusted associations between prospective and retrospective measures 
# with psychopathology, we made correlation matrices incorporating prospective measures, 
# retrospective measures, and psychopathology. Using these correlation matrices 
# and the respective study sample size, we then ran structural equation models 
# to estimate the partial (adjusted) correlations between different measures of 
# maltreatment and psychopathology. The following script derives these partial 
# correlations for the following studies:
# - Mills et al. (2016)
# - Patten et al. (2015)
# - Scott et al. (2012)
# - Elwyn et al. (2013)
# - Smith et al. (2008)
# - Talmon & Widom (2022)
# - Dion et al. (2019)
# - Widom & Morris (1997)

## Load libraries ####
library(lavaan) # for SEMs
library(semPlot) # for SEMs
library(psych) # for deriving correlations
library(effectsize) # for converting other effect sizes to correlations

## Check method against results from Reuben (2018) to replicate the findings ####

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.4, 1,
0.23, 0.47, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=950, mimic="EQS") # Add sample size for "sample.nobs"
summary(fit, fit.measures=T, standardized=TRUE)

# Extract results
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
# Prospective (adjusted) correlation
(c(pro_EffectSize, pro_SE)) #r=0.054, SE=0.03 (reported in Table 3 as r=0.05)
# Retrospective (adjusted) correlation
(c(retro_EffectSize, retro_SE)) #r=0.375, SE=0.03 (reported in Table 3 as r=0.38)

## Mills et al. (2016) ####

# Because associations between prospective and retrospective measures with psychopathology
# are presented as odds ratios, we convert to correlations for inclusion in the SEM
# We also first derive the r between prospective and retrospective measures of sexual abuse.

## Sexual abuse with lifetime depressive disorder ###

# Derive correlation between prospective and retrospective measures of sexual abuse
# no pro or retro measure = 2756
# retro_only = 889
# pro_only = 40
# pro_retro = 54
table <- matrix(nrow=2, ncol=2, 
                c(54,40,889,2756))
tetrachoric(table) # r = 0.36

# Unadjusted prospective sexual abuse with lifetime_depressive_disorder
# OR = 1.88; convert to r 
oddsratio_to_r(OR=1.88, log = FALSE)
# r = 0.1714427

# Unadjusted retrospective sexual abuse with lifetime_depressive_disorder
# OR = 2.05; convert to r 
oddsratio_to_r(OR=2.05, log = FALSE)
# r = 0.1941188

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.1941188, 1,
0.1714427, 0.36, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=3739, mimic="EQS") # Add sample size for "sample.nobs"
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE))
(c(retro_EffectSize, retro_SE))

## Sexual abuse with lifetime anxiety ###
			
# Unadjusted prospective sexual abuse with lifetime_depressive_disorder
oddsratio_to_r(OR=1.75, log = FALSE)
# r = 0.1524627

# Unadjusted retrospective sexual abuse with lifetime_depressive_disorder
oddsratio_to_r(OR=2.66, log = FALSE)
# r = 0.2603866

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.2603866, 1,
0.1524627, 0.36, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=3739, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

## Sexual abuse with lifetime PTSD ###

# Unadjusted prospective sexual abuse with lifetime_ptsd
oddsratio_to_r(OR=4.34, log = FALSE)
# r = 0.3750963

# Unadjusted retrospective sexual abuse with lifetime_ptsd
oddsratio_to_r(OR=4.9, log = FALSE)
# r = 0.4012767

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.4012767, 1,
0.3750963, 0.36, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=3739, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

### Patten et al. (2016) ####

## ACEs and major_depression

# We first derive the correlation between prospective and retrospective measures
# based on the Ns reported for the different groups in the study
# n = 1896 
# pro_retro = 1896*0.228 = 432
# pro_only = 1896*0.10 = 190
# retro_only = 1896*0.30 = 569
# none = 1896*0.372 = 705
# 432+190+569+705 = 1896

# correlation between prospective and retrospective measures of sexual abuse
table <- matrix(nrow=2, ncol=2, 
                c(432,190,569,705))
tetrachoric(table) # r = 0.38 (equivalent to kappa=0.21, see below)

kappa <- function(table) {
  po <- sum(diag(table))/sum(table)
  p_yes <- (sum(table[1,])/sum(table)) * (sum(table[,1])/sum(table)) # probability of yes on both measures
  p_no <- (sum(table[2,])/sum(table)) * (sum(table[,2])/sum(table)) # probability of no on both measures
  pe <- p_yes + p_no
  kappa <- (po - pe) / (1 - pe)
  se <-  sqrt(  po*(1-po)  / (sum(table)*(1-pe)^2) )
  return(c(kappa, se))
}
kappa(table)

# Unadjusted prospective ACEs with depression
oddsratio_to_r(OR=2, log = FALSE)
# r = 0.1876806

# Unadjusted retrospective ACEs with depression
oddsratio_to_r(OR=5.5, log = FALSE)
# r = 0.4253155

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.4253155, 1,
0.1876806, 0.38, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=1896, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

## ACEs and high alcohol use

# Unadjusted prospective ACEs with alcohol
oddsratio_to_r(OR=1.2, log = FALSE)
# r = 0.05019621

# Unadjusted retrospective ACEs with alcohol
oddsratio_to_r(OR=1.3, log = FALSE)
# r = 0.07213608

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.07213608, 1,
0.05019621, 0.38, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=1896, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
# check whether prospective adjusted ES corresponds to that reported in paper (OR = 1.1) - yes
r_to_oddsratio(r=0.02662985, log = FALSE) 

(c(retro_EffectSize, retro_SE)) # retrospective

### Scott et al. (2012) ####

# First derive the correlation between prospective and retrospective measures
# the Ns are calculated from Scott, 2010 which uses the same sample (NZ Mental Health Survey)
# N=221 have prospective measure (Table 1, row 1)
# N=1923 have no prospective measure (table 1, row 1)
# Total N=221+1923 = 2144 (although only 1413 for analyses with MH outcome)
# 99 have pro & retro measures (table 3, last row)
# 358 have retro measures only (table 3, last row)
# 221-99 (N=122) have pro measures only
# 2144-(99+358+122) = N=1565 have no pro or retro measures

# pro_retro = 99
# pro_only = 122
# retro_only = 358
# none = 1565

table <- matrix(nrow=2, ncol=2, 
                c(99, 122, 358, 1565))
tetrachoric(table) # r = 0.39 (equivalent to kappa=0.18)
kappa(table)

## Maltreatment and lifetime_major_depressive_disorder

# Unadjusted prospective maltreatment with lifetime_major_depressive_disorder
oddsratio_to_r(OR=2.37, log = FALSE)
# r = 0.2314114

# Unadjusted retrospective maltreatment with lifetime_major_depressive_disorder
oddsratio_to_r(OR=2.51, log = FALSE)
# r = 0.2458998

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.2458998, 1,
0.2314114, 0.39, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=1413, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

## Maltreatment and lifetime_anxiety_disorder

# Unadjusted prospective maltreatment with lifetime_anxiety_disorder
oddsratio_to_r(OR=2.72, log = FALSE)
# r = 0.265908

# Unadjusted retrospective maltreatment with lifetime_anxiety_disorder
oddsratio_to_r(OR=3.10, log = FALSE)
# r = 0.2977421

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.2977421, 1,
0.265908, 0.39, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=1413, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

## Maltreatment and lifetime_alcohol_abuse_dependence

# Unadjusted prospective maltreatment with lifetime_alcohol_abuse_dependence
oddsratio_to_r(OR=2.98, log = FALSE)
# r = 0.2882302

# Unadjusted retrospective maltreatment with lifetime_alcohol_abuse_dependence
oddsratio_to_r(OR=3.36, log = FALSE)
# r = 0.3168728

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.3168728, 1,
0.2882302, 0.39, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=1413, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

## Maltreatment and lifetime_drug_abuse_dependence

# Unadjusted prospective maltreatment with lifetime_drug_abuse_dependence
oddsratio_to_r(OR=3.72, log = FALSE)
# r =  0.3405059

# Unadjusted retrospective maltreatment with lifetime_drug_abuse_dependence
oddsratio_to_r(OR=3.52, log = FALSE)
# r = 0.3277509

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.3277509, 1,
0.3405059, 0.39, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=1413, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

### Elwyn et al. (2013) ####

# Note: we directly calculated unadjusted rs for use in SEM
# We did not rely on converting unadjusted ORs to r as it seemed to induce some discrepancy

### Calculate r for agreement between prospective and retrospective maltreatment measures 
# Table 2 info:
# 43.98 % of those with prospective substantiated maltreatment had a retro memory of MT
# 25.15% of those with no prospective measure had a retro memory of MT
# Table 1 info:
# 244 with retro measure of maltreatment
# 198 with prospective (substantiated maltreatment)
# 846 with data on retro measures yes or no (total n)

# pro_retro = 198*0.4398 = 87
# pro_only = 198-87  = 111
# retro_only = 244-87 = 157
# none = 846-(87+111+157) = 491

table <- matrix(nrow=2, ncol=2, 
                c(87, 111, 157, 491))
tetrachoric(table) # r = 0.31 
# note, fig 1 gives 0.48 for path from official MT to memory, but this 
# is a probit coefficient - check it is the same when converting from probit to OR
# using formula here: https://stats.stackexchange.com/questions/19864/convert-probit-regression-coefficient-into-correlation

probit <- 0.48
logit <- 1.7 * probit
OR <- 2.71828^logit
# convert odd ratio to pearson correlation
# Bonett, D. G. (2007). Transforming odds ratios into correlations for meta-analytic research American Psychologist, 62(3), 254–255. doi:10.1037/0003-066X.62.3.254
# Two different approximations: Pearson and Digby
OR2cor <- function(OR) {
  return(list(
    pearson=cos(pi/(1 + sqrt(OR))),
    digby=(OR^.75 - 1)/(OR^.75 + 1)
  ))
}

OR2cor(OR)# r= 0.31, the same

# Unadjusted prospective maltreatment with drug_problems
oddsratio_to_r(OR=1.387949, log = FALSE)
# r =  0.09000351

# check consistent with tetrachoric r from 2x2 table
# Adult illegal drug use
# a = exposed on prospective measure only, cases with psychopathology
# b = exposed on prospective measure only, controls without psychopathology
# c = non-exposed on prospective measure, cases with psychopathology
# d = non-exposed on prospective measure, controls without psychopathology
a <- 156*0.3974
b <- 156-a
c <- 647*0.3221
d <- 647-c

table <- matrix(nrow=2, ncol=2, 
                c(a, b, c, d))
tetrachoric(table) # r = 0.11 (tetrachoric vs r=0.09 [OR convert to r])

# Unadjusted retrospective maltreatment with lifetime_drug_abuse_dependence
oddsratio_to_r(OR=2.2392602, log = FALSE)
# r = 0.2169337
a <- 228*0.4688
b <- 228-a
c <- 574*0.2827
d <- 574-c
table <- matrix(nrow=2, ncol=2, 
                c(a, b, c, d))
tetrachoric(table) # r = 0.29 (tetrachoric vs r=0.22 [OR convert to r])

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.29, 1,
0.11, 0.31, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=803, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

# Unadjusted prospective maltreatment with alcohol_problems
oddsratio_to_r(OR=1.3027022, log = FALSE)
# r = 0.07270399
a <- 156*0.1523
b <- 156-a
c <- 647*0.1212
d <- 647-c
table <- matrix(nrow=2, ncol=2, 
                c(a, b, c, d))
tetrachoric(table) # r = 0.082 (tetrachoric vs r=0.07 [OR convert to r])

# Unadjusted retrospective maltreatment with alcohol_problems
oddsratio_to_r(OR=2.290633, log = FALSE)
# r = 0.2227387
a <- 228*0.2009
b <- 228-a
c <- 574*0.0989
d <- 574-c
table <- matrix(nrow=2, ncol=2, 
                c(a, b, c, d))
tetrachoric(table) # r = 0.27 (tetrachoric vs r=0.22 [OR convert to r])

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.27, 1,
0.082, 0.31, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=803, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

### Smith et al. (2008) ####

# Note: in the main spreadsheet, we reported unadjusted ORs calculated from a 2x2 table 
# Here we directly correlate tetrachoric correlations for the unadjusted effects for the SEM

## Prospective and retrospective agreement for maltreatment
# maltreatment
# agreement yes = 88
# 49.8% (N=88) of those with objective reports (N=177) self-reported it
177*0.498
# objective yes subjective no = 89
# 50.2% (N=89) of those with objective reports (N=177) didn't self-reported it
177*0.502
# subjective yes objective no = 153
# 63.4% (N=153) of those with subjective reports (N=241) didn't have an official record
241*0.634
# agreement no = 516
# 77.2% (N=516) of those with no objective records (669) didn't self-report
669*0.772
# total N=846
88+89+153+516

table <- matrix(nrow=2, ncol=2, 
                c(88,	89,	153,	516))
library(psych)
tetrachoric(table) # r=0.41
fisher.test(table)

# Derive function for odds ratio and SE (of log odds ratio)
odds_ratio <- function(a, b, c, d) {
  or <- (a/c) / (b/d)
  se_log_or <- sqrt(1/a + 1/b + 1/c +1/d)
  return(c(or, se_log_or))
}

## Prospective maltreatment and drug use aged 14-18
a <- 185*0.546
b <- 185-a
c <- 695*0.341
d <- 695-c
sum(a,b,c,d)
odds_ratio(a,b,c,d)

## Retrospective maltreatment and drug use aged 14-18
a <- 241*0.532
b <- 241-a
c <- 594*0.32
d <- 594-c

sum(a,b,c,d)
odds_ratio(a,b,c,d)
table <- matrix(nrow=2, ncol=2, 
                c(a,b,c,d))
tetrachoric(table) # r=0.32

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.32, 1,
0.30, 0.41, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=850, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

### Drug use age 21-23

## Prospective maltreatment and drug use aged 21-23
a <- 176*0.542
b <- 176-a
c <- 657*0.424
d <- 657-c

sum(a,b,c,d)
odds_ratio(a,b,c,d)
table <- matrix(nrow=2, ncol=2, 
                c(a,b,c,d))
tetrachoric(table) # r=0.17

## Retrospective maltreatment and drug use aged 21-23
a <- 236*0.619
b <- 236-a
c <- 597*0.382
d <- 597-c

sum(a,b,c,d)
odds_ratio(a,b,c,d)
table <- matrix(nrow=2, ncol=2, 
                c(a,b,c,d))
tetrachoric(table) # r=0.35

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.35, 1,
0.17, 0.41, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=850, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective


## Talmon & Widom (2022) ####

# Correlations for the associations between prospective & retrospective measures are tetrachoric

# Child maltreatment and anorexia

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
-0.03, 1,
0.12, 0.4, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=807, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

# Physical abuse and anorexia

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.03, 1,
0.07, 0.36, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=807, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective\

# Sexual abuse and anorexia

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.12, 1,
0.25, 0.43, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=807, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

# Neglect and anorexia

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.08, 1,
0.05, 0.51, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=807, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

# Maltreatment and bulimia

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.35, 1,
-0.21, 0.4, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=807, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

# Physical abuse and bulimia

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.4, 1,
-0.03, 0.36, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=807, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

# Neglect and bulimia

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.2, 1,
-0.16, 0.51, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Specify the model
# regress psychopathology on retrospective and prospective measures as joint predictors
model <- 'psychopathology_outcome ~ retrospective + prospective'

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=807, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

### Dion et al. (2019) ####
# unadjusted correlation between prospective measures & psychopathology = 0.174
# unadjusted correlation between retrospective measures & psychopathology = 0.235
# correlation between prospective and retrospective measures = 0.291 (Table 2)
# Note SDs are also given in Table 2 (column 1, brackets) but including these does not give standardised results
# SD prospective maltreatment = 0.51
# SD retrospective maltreatment = 0.79
# SD psychological distress = 16.33

## Create a correlation matrix as follows:
# 1 should always be on the diagonal 
# column 1, row 2 = correlation between the outcome and the retrospective measure
# column 1, row 3 = correlation between the outcome and the prospective measure
# column 2, row 3 = correlation between the retrospective and the prospective measure
lowercor='
1,
0.235, 1,
0.174, 0.291, 1'

# Get full correlation matrix, adding the names
Fullcor <- getCov(lowercor, names=c("psychopathology_outcome", "retrospective", "prospective"))
Fullcor # see the results by just running the name of the created object

# Fit the model
fit <- sem(model, sample.cov=Fullcor, sample.nobs=605, mimic="EQS")
summary(fit, fit.measures=T, standardized=TRUE)

# Extract effect sizes and standard errors for objective and subjective effects on psychopathology
retro_EffectSize <- parameterEstimates(fit)$est[1]
retro_SE <- parameterEstimates(fit)$se[1]
pro_EffectSize <- parameterEstimates(fit)$est[2]
pro_SE <- parameterEstimates(fit)$se[2]
(c(pro_EffectSize, pro_SE)) # prospective effect
(c(retro_EffectSize, retro_SE)) # retrospective

## Derive SE for unadjusted correlations for Dion 
# Derive function to calculate SE for correlation
# See: https://stats.stackexchange.com/questions/73621/standard-error-from-correlation-coefficient/262893 and https://digital.library.adelaide.edu.au/dspace/bitstream/2440/15169/1/14.pdf
derive_SE_cor <- function(n, r) {
  SE <- (1 - r^2) / sqrt(n - 2)
  return(SE)
}
# prospective measure
derive_SE_cor(605, 0.174)

# retrospective measure
derive_SE_cor(605, 0.235)

### Widom & Morris (1997) ####
# Here we calculated adjusted odds ratios for women only using the data provided
# It was not necessary to use the correlation matrix/SEM technique as data were
# already provided to derive adjusted ORs

# a = exposed on prospective measure only, cases with psychopathology
# b = exposed on prospective measure only, controls without psychopathology
# c = non-exposed on prospective and retrospective measure, cases with psychopathology
# d = non-exposed on prospective and retrospective measure, controls without psychopathology

## Prospective measure adjusted effect

# n = 24 with prospective measure and no retro measure
# n = 275 with no prospective or retrospective measure
# 21% of those with prospective measure only had suicide attempt
# 8% of those with no prospective or retrospective measure had suicide attempt

a = 0.21*24 # percentage with psychopathology of those with pro measure only
b = 24-a # percentage without psychopathology of those with pro measure only
c = 0.08*275 # percentage with psychopathology of those with no prospective and retrospective measure
d = 275-c # percentage without psychopathology of those with no prospective and retrospective measure
sum(a,b,c,d) # only including those with pro measure only and neither pro nor retro measure (24+275)
odds_ratio(a,b,c,d)
# OR prospective = 3.06, SE=0.54

## Retrospective measure adjusted effect

# n = 226 with retrospective measure and no pro measure
# n = 275 with no prospective or retrospective measure
# 22% of those with prospective measure only had suicide attempt
# 8% of those with no prospective or retrospective measure had suicide attempt

a = 0.22*226 # percentage with psychopathology of those with retro measure only
b = 226-a # percentage without psychopathology of those with retro measure only
c = 0.08*275 # percentage with psychopathology of those with no prospective and retrospective measure
d = 275-c # percentage without psychopathology of those with no prospective and retrospective measure
sum(a,b,c,d) # only including those with pro measure only and neither pro nor retro measure (24+275)
odds_ratio(a,b,c,d)
# OR retrospective = 3.24, SE= 0.27
