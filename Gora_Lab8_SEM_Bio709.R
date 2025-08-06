#### BioStats Lab 8- Structural Equation Modeling ####
###  Gora_Lab8_SEM_BIO709.R
###  by Sarah Gora
###  Date created: April 12, 2022

# set WD 
setwd("~/Desktop/Biostats_2/StructuralEquationModeling")

# load packages
library(lavaan)
## This is lavaan 0.6-8
## lavaan is FREE software! Please report any bugs.
install.packages("semPlot")
library(semPlot)
?lavaan::sem


# read in the data 
library(readr)
dat  <- read_csv("SEM_02_1_Intro_to_Lavaan_data.csv")
summary(dat)
head(dat)



####### SEM Modeling ########3


# Step 1: Specify parts of the model
mod.1 = 'y1 ~ x1
         y2 ~ x1
         y3 ~ y1 + y2'

# Step 2: Estimate model
mod.1.fit <- sem(mod.1, data = dat)


varTable(mod.1.fit)


# fit variables to scale 
x1 <- dat$x1/100
y1 <- dat$y1/100
y2 <- dat$y2
y3 <- dat$y3/100

# create a new data frame of the transformed data ("t.dat")
t.dat <- data.frame(x1, y1, y2, y3)
summary(t.dat)



# Step 1: Specify model (again)
mod.1 = 'y1 ~ x1
         y2 ~ x1
         y3 ~ y1 + y2'

# Step 2: Estimate model
mod.1.fit <- sem(mod.1, data = t.dat)

# Step 3: Extract results
summary(mod.1.fit, standardized = TRUE, rsq = T)


# Is this a good model? NO! How can I tell?


# Step 4: what do we modify in the model
modindices(mod.1.fit)


# Step 5: Re-specify model, including 
mod.1.1 <- 'y1 ~ x1
            y2 ~ x1
            y3 ~ y1 + y2 + x1'

mod.1.1.fit <- sem(mod.1.1, data = t.dat)
summary(mod.1.1.fit, standardized = TRUE, rsq = T)



# SEM Diagnostics

# measures of FIT
# GFI > 0.95 indicates good fit
# RMSEA < 0.05 indicates a close fit

# Step 6: check fit indicies
fitMeasures(mod.1.1.fit)



### SEM Visualization


# Step 7: Take a look at the final reconstructed model
semPaths(mod.1.1.fit, what = 'std', layout = "tree3", 
         intercepts = FALSE, residuals = FALSE, 
         edge.label.cex = 1.25, curvePivot = FALSE, fade = FALSE, rotation = 3)

# - What is the direct effect of x1 on y3? 
# It’s simply the effect size = .4 - What is the indirect effect of x1 on y3 via y2? 
# Multiple the two effects together = .048 
# - What is the indirect effect of x1 on y3 via y1? 
# Multiple the two effects together = .138 
#- What is the total effect of x1 on y3? 
# = .4+.048+.138 =.586



# COMPLEX SEMs FOR REAL-WORLD DATA

# 1. Plant species composition data in 81 1m2 plots along a productivity gradient
comp <- read.csv("Niwot_Comp.csv", header = TRUE, row.names = 1)
head(comp)

dim(comp)
# 81  71 


# 2. Environmental data on soil depth, soil depth variability, soil moisture, nitrogen (N mineralization and total inorganic nitrogen), and NPP (or net primary production).
env <- read.csv("Niwot_Enviro.csv", header = TRUE, row.names = 1)
head(env)

dim(env)
# 81  6



# The Graphical Model
# First, sketch out the meta-model you want to test. 
# Remember, this is ALWAYS where you start.

# Curating Variables to Use
# Step 1: Isolate the variable of interest. Here, this means separating NPP from all the other env variables. 
NPP = env[,6]
summary(NPP)


env2 = env[,1:5]
head(env2)



# Step 2: Run a PCA of all other variables to turn 5 environmental variables into their component axes of variation.
envPCA = prcomp(env2, scale. = TRUE)
biplot(envPCA)



# What does this tell us?
summary(envPCA)

envPCA
# PC1 is related to soil depth, soil moisture, and total N.
# PC2 is related to N-mineralization and CV soil depth.
# Together these two PC axes explain 67% of the variation in the env data.
# Let’s pull out those PC scores and use them as new predictor variables in our SEM.

envPCAs = envPCA$x
PC1 = envPCAs[,1]
PC2 = envPCAs[,2]


# Species Richness as an Additional Variable
# Step 3: Let’s get our plant richness (at each site) calculated from the comp dataset.
install.packages("vegan")
library(vegan)


SR = specnumber(comp)
barplot(SR) # barplot of species richness, per site (number of spp.)
summary(SR)


#At this point, we should have all of our data ready. 
# The following variables will be included in the SEM:
#- NPP (net primary productivity, our response var.)
#- PC1 (PC axis 1 of env data)
#- PC2 (PC axis 2 of env data)
#- SR (species richness at each site)

dat1 = data.frame(NPP, PC1, PC2, SR)
cor(dat1)
pairs(dat1)

summary(dat1)




######### Lab 8 Homework ########
# 1. Draw the a priori model you will be testing.
#- This should be a slight modification of the above model 
# (2) – just including the 2 environmental variables (PC1 and PC2) we are using.
#- Include PC1 and PC2 both directly impacting NPP, but only include PC1 impacting richness.
#- Draw this model in MS Powerpoint or similar, 
# export as .pdf, and name + upload according to our convention listed in Canvas assignments.


read.pdf("~/Desktop/Biostats_2/StructuralEquationModeling/Gora_Lab8_Figure1-APrioriSEM.pdf")


# 2. Specify the model in R and run it.
#- IF you get a variance error, follow the steps above and correct what you need to. 
#  Then re-run.
#- Use the capture.output() function to save the model summary as a .txt file.

# variables for SEM 
# - NPP (net primary productivity, our response var.)
# - PC1 (PC axis 1 of env data)
# - PC2 (PC axis 2 of env data)
# - SR (species richness at each site)

dat2 = data.frame(NPP, PC1, PC2, SR)
cor(dat2)

# endogenous-influenced by other variables 
# exogenous-NOT influenced by other variables

# NPP = Y2 = endogenous response variable 1 (influenced by PC1,PC2,SR)
# SR =Y1 endogenous response & predictor variable 2 (influenced by PC1)
# PC1 = X1 = exogenous predictor variable 1 (not influenced by other variables)
# PC2 = X2= exogenous predictor variable 2 (not influenced by other variables)


# Step 1: Specify parts of the model
# mod_2 = 'Y1 ~ X1
#          Y2 ~ Y1 + X1 + X2'

mod_2 = 'SR ~ PC1
         NPP ~ SR + PC1 + PC2'


# Step 2: Estimate model
mod_2_fit <- sem(mod_2, data = dat2)
varTable(mod_2_fit)

# Variances are different
# NPP - huge number
# re-code variables to same scale by transformation

SR_scaled <- dat2$SR/2
NPP_scaled <- dat2$NPP/100
PC1 <- dat2$PC1
PC2 <- dat2$PC2


# create a new data frame of the transformed data ("t.dat")
t_dat2 <- data.frame(NPP_scaled, PC1, PC2, SR_scaled)
summary(t_dat2)
varTable(t_dat2)

###### RERUN MODEL WITH TRANSFORMED/SCALED DATA #######

# Step 1: Specify model (again)
t_mod_2 = 'SR_scaled ~ PC1
           NPP_scaled ~ SR_scaled + PC1 + PC2'

# Step 2: Estimate model (again)
t_mod_2_fit <- sem(t_mod_2, data = t_dat2)
varTable(t_mod_2_fit)

# Step 3: Extract results
summary_t_mod_2_fit <- summary(t_mod_2_fit, standardized = TRUE, rsq = T) 



capture.output(summary_t_mod_2_fit, file="~/Desktop/Biostats_2/StructuralEquationModeling/Final/Gora_Lab8_Table1-SEMSummary.txt")

# 3. Is the model a good fit for the data?
#- Use the fitMeasures() function to compute all measures of fit. 
# (How do you tell if something is a good fit on each one of those measures?)
#- Subset out the following measures from the named vector, into a separate named vector (Chi-square, RMSEA, GFI).
#- Use the write.table() function to save the new named vector as a .txt file.

# measures of FIT
# GFI > 0.95 indicates good fit
# RMSEA < 0.05 indicates a close fit

fitMeasures(t_mod_2_fit)

# Chi-square = 0.081
# RMSEA = 0.000
# GFI = 0.999

# create data frame
t_mod_2_fit_meas <- data.frame("ChiSquare"=c(0.081),
                                  "RMSEA"=c(0.000),
                                  "GFI"=c(0.999))
t_mod_2_fit_meas

write.table(t_mod_2_fit_meas, file="~/Desktop/Biostats_2/StructuralEquationModeling/Final/Gora_Lab8_Table2-MeasuresOfFit.txt")

getwd()


# 4. Seek to improve the SEM above
#- Specifically, add into the model a direct effect of PC2 on SR.
#- What happened? What’s the chi-square? And why?
#- Write 1-2 sentences explaining why this model doesnt hold 
# and save this explanation as .txt (eg in Notepad or TextEdit).

# use scaled data frame
t_dat2 <- data.frame(NPP_scaled, PC1, PC2, SR_scaled)

# Step 1: Specify model (again)
t_mod_4 = 'SR_scaled ~ PC1 
           SR_scaled ~ PC2                     # ADD direct effect of PC2 on SR
           NPP_scaled ~ SR_scaled + PC1 + PC2'

# Step 2: Estimate model (again)
t_mod_4_fit <- sem(t_mod_4, data = t_dat2)
varTable(t_mod_4_fit)

##### Is this a good model?
# measures of FIT
# GFI > 0.95 indicates good fit
# RMSEA < 0.05 indicates a close fit

fitMeasures(t_mod_4_fit)

# ChiSquared = 0.000 
# RMSEA = 0.000
# GFI = 1.000      

# why does this model not hold? 

# overfitted 
# PC2 is not important as a direct effect on SR 
# 

# 5. Graph the a priori SEM from above (Question 1)
#- Use the semPaths() function to plot the non-saturated model
#- What does this model tell you about what is driving ecosystem function? 
# What is the direct effect of PC1 on NPP and what is the indirect effect of PC1 on NPP? 
# What does that make the total effect of PC1 on NPP? 
# Does this dataset support the idea that biodiversity strongly leads to higher ecosystem function?
#- Save the full model plot as an image file.

# SEM Visualization

# unscaled unsaturated model
semPaths(mod_2_fit, what = 'std', layout = "tree3",
         intercepts = FALSE, residuals = FALSE, 
         edge.label.cex = 1.25, curvePivot = FALSE, 
         fade = FALSE, rotation = 3)

# scaled unsaturated model as tree **** best
semPaths(t_mod_2_fit, what = 'std', layout = "tree3",
         intercepts = FALSE, residuals = FALSE, 
         edge.label.cex = 1.25, curvePivot = FALSE, 
         fade = FALSE, rotation = 3)

# scaled unsaturated model as srping 
semPaths(t_mod_2_fit, what = 'std', layout = "spring",
         intercepts = FALSE, residuals = FALSE, 
         edge.label.cex = 1.25, curvePivot = FALSE, 
         fade = FALSE, rotation = 3)

# saturated 
semPaths(t_mod_4_fit, what = 'std', layout = "spring",
         intercepts = FALSE, residuals = FALSE, 
         edge.label.cex = 1.25, curvePivot = FALSE, 
         fade = FALSE)

# what is driving NPP 

# The direct effect of PC2 on NPP is 
# the indirect effect of PC1 on NPP (multiply) is 
# the direct effect of PC1 on NPP is 



# 6. Save all four files 
# 1 model in PPT   Gora_Lab8_
# 1 model summary (txt) 
# 1 fit measures summary
# 1 text description
# 1 plots Gora_Lab8_Table1
