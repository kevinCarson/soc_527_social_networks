###---------------------------------------------------------------------------------------###

#       Introduction to Exponential Random Graph Models (ERGMs)
#       University of Arizona School of Sociology SOC 527: Social Networks
#       Code Written by Kevin A. Carson (https://kevincarson.github.io/)
#       Last Updated: 10-08-2026

###---------------------------------------------------------------------------------------###
rm(list = ls()) #clearing the working memory
library(statnet)#reading the statnet package 
library(ergm)#reading the ergm package 
library(network)#reading the network package 
library(sna)#reading the sna package 
library(btergm)#reading the btergm package (used to estimate tergms)
library(ggnetwork)#reading the ggnetwork package (used to plot the sociograms) 
library(ergMargins)#reading the ergMargins package (used to estimate the VIFs and AMEs) 
library(netmediate)#reading the netmediate package used to estimate the micro-effects) 
library(ggplot2)
library(patchwork)
library(cowplot)
sessionInfo() #for complete replication
set.seed(3718)

# R version 4.5.2 (2025-10-31)
# Platform: aarch64-apple-darwin20
# Running under: macOS Tahoe 26.6.2
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: America/New_York
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] modelsummary_2.6.0    netmediate_1.1.1      ergMargins_1.6.1      btergm_1.11.1         ggnetwork_0.5.14      ggplot2_4.0.1        
# [7] statnet_2019.6        tsna_0.3.6            sna_2.8               statnet.common_4.12.0 ergm.count_4.1.3      tergm_4.2.2          
# [13] networkDynamic_0.11.5 ergm_4.10.1           network_1.19.0       
# 
# loaded via a namespace (and not attached):
# [1] gtable_0.3.6             networkLite_1.1.0        bayestestR_0.17.0        xfun_0.58                insight_1.4.4           
# [6] ergm.multi_0.3.0         rle_0.10.0               lattice_0.22-7           vctrs_0.6.5              tools_4.5.2             
# [11] Rdpack_2.6.4             sampling_2.11            generics_0.1.4           datawizard_1.3.0         parallel_4.5.2          
# [16] tibble_3.3.0             DEoptimR_1.1-4           pkgconfig_2.0.3          Matrix_1.7-4             tinytable_0.16.0        
# [21] checkmate_2.3.3          data.table_1.18.0        RColorBrewer_1.1-3       S7_0.2.1                 lifecycle_1.0.4         
# [26] compiler_4.5.2           farver_2.1.2             stringr_1.6.0            litedown_0.9             htmltools_0.5.9         
# [31] yaml_2.3.11              pillar_1.11.1            MASS_7.3-65              cachem_1.1.0             trust_0.1-8             
# [36] boot_1.3-32              nlme_3.1-168             robustbase_0.99-6        tidyselect_1.2.1         digest_0.6.39           
# [41] performance_0.16.0       stringi_1.8.7            dplyr_1.1.4              purrr_1.2.0              labeling_0.4.3          
# [46] fastmap_1.2.0            grid_4.5.2               cli_3.6.5                magrittr_2.0.4           ROCR_1.0-11             
# [51] withr_3.0.2              backports_1.5.0          scales_1.4.0             rmarkdown_2.30           igraph_2.2.1            
# [56] zoo_1.8-14               coda_0.19-4.1            memoise_2.0.1            lpSolveAPI_5.5.2.0-17.14 evaluate_1.0.5          
# [61] lpSolve_5.6.23           knitr_1.50               parameters_0.28.3        rbibutils_2.4            lmtest_0.9-40           
# [66] rlang_1.1.6              glue_1.8.0               intergraph_2.0-4         pkgload_1.4.1            rstudioapi_0.17.1       
# [71] R6_2.6.1                 tables_0.9.33  


###---------------------------------------------------------------------------------------###

# For this code script, we are going to use the Faux Desert High network datasets in the 
# statnet R package. Please see help("faux.desert.high") for more information on the dataset.
# Notes: simulated directed friendship relations from a high school in the US with 
#        107 adolescents. In addition to the friendship relations, the processed file 
#        additionally includes the race, grade, and gender of the adolescent. The simulated 
#        network is drawn from the Add Health dataset. 

###---------------------------------------------------------------------------------------###
data("faux.desert.high") #loading the network dataset into the R session
faux.desert.high #see the print output for more information
# making a plot of the network
socio.gram <- ggplot(ggnetwork(faux.desert.high), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black", curvature = 0.1,  arrow = arrow(length = unit(0.2, "lines"), 
                                                              type = "closed")) +
  geom_nodes(color=ifelse(get.vertex.attribute(faux.desert.high,"sex") == 1, "blue", "red"),size=2) +
  theme_blank()
socio.gram #it is often helpful to visualize the network structure

###---------------------------------------------------------------------------------------###

# Step 1. Fitting a Bernoulli Dyad-Independent ERGM with only the Edges Term

# Note: I highly suggest that one looks at the full list of possible ergm terms. The full list 
# can be provided via the below code: 
help("ergm-terms") #the full list will open in the help console

###---------------------------------------------------------------------------------------###
bernoulli.model <- ergm(faux.desert.high ~ edges) #fitting the model with just edges
summary(bernoulli.model) #printing the summary output of the model
#one can see, that when only the edges term is included, the estimated parameter is 
#the log odds of the graph density value
p.tie <- gden(faux.desert.high,mode="digraph") #computing the density
log.odds.p.tie <- log(p.tie/(1-p.tie)) #the log-odds of the density
data.frame(ours = log.odds.p.tie, estimated = bernoulli.model$coefficients) #compare the values

###---------------------------------------------------------------------------------------###

# Step 2. Fitting a Dyad-Independent ERGM with only the Edges Term + Exogenous Node Level Effects

# Note: I highly suggest that one looks at the full list of possible ergm terms for included
# actor-level covariates. Again, The full list can be provided via the below code: 
help("ergm-terms") #the full list will open in the help console

###---------------------------------------------------------------------------------------###
actor.model <-  ergm(faux.desert.high ~ edges + #intercept (density)
                       nodematch("race") + #same race
                       nodematch("sex") + #same sex
                       nodematch("grade") +  #same grade
                       nodeocov("sex") + #sex of ego
                       nodeicov("sex")) #sex of alter
summary(actor.model) #printing the summary output of the model

#here, one can show this model is nothing more than a logistic regression (this is 
#maximum psuedo likelihood estimation [MPLE]) where the dependent variable is the (i,j) tie 
adj.mat <- as.matrix(faux.desert.high) #the adjacency matrix
out.list <- list()
for(i in 1:nrow(adj.mat)){
  dat <- data.frame(tie = adj.mat[i,], 
             ego.id = i,
             alter.id = 1:nrow(adj.mat),
             ego.race = get.vertex.attribute(faux.desert.high,"race")[i],
             alter.race = get.vertex.attribute(faux.desert.high,"race"),
             ego.sex = get.vertex.attribute(faux.desert.high,"sex")[i],
             alter.sex = get.vertex.attribute(faux.desert.high,"sex"),
             ego.grade = get.vertex.attribute(faux.desert.high,"grade")[i],
             alter.grade = get.vertex.attribute(faux.desert.high,"grade"))
  dat <- dat[dat$ego.id != dat$alter.id,]
  dat$same.race <- as.numeric(dat$ego.race == dat$alter.race)
  dat$same.grade <- as.numeric(dat$ego.grade == dat$alter.grade)
  dat$same.sex <- as.numeric(dat$ego.sex == dat$alter.sex)
  out.list[[i]] <- dat
}
mple.data <- do.call(rbind,out.list)
#now, we estimate a logistic regression to the outcome tie variable
mple.model <- glm(tie ~ same.race + #node match race 
                    same.sex + #node match sex 
                    same.grade + #node match grade 
                    ego.sex + #sex for ego
                    alter.sex, #sex for alter
                  data = mple.data,
                  family = binomial(link="logit")) #logit model
summary(mple.model) #summary of the model
rbind(coef(mple.model),coef(actor.model)) #as you can see, they are the same

###---------------------------------------------------------------------------------------###

# Step 3. Fitting a Dyad-Dependent ERGM with the Edges Term + Exogenous Node Level Effects + Reciprocity

###---------------------------------------------------------------------------------------###
#
# the below model is fit via Monte Carlo Markov Chain Maximum Likelihood Estimation (MCMC-MLE)
# based upon the Metropolis-Hastings procedure
#
dyad.dependent.model <-  ergm(faux.desert.high ~ edges + #intercept (density)
                                mutual + #reciprocity term
                                nodematch("race") + #same race
                                nodematch("sex") + #same sex
                                nodematch("grade") +  #same grade
                                nodeocov("sex") + #sex of ego
                                nodeicov("sex")) #sex of alter
summary(dyad.dependent.model) #summarizing the model fit

###---------------------------------------------------------------------------------------###

# Step 4. Fitting the full ERGM to the Faux Desert High Dataset

###---------------------------------------------------------------------------------------###
fitted.model <- ergm(faux.desert.high ~ 
                           edges + #the intercept
                           mutual +  #reciprocity
                           #geometrically weighted edgewise shared partners with alpha set to 0.2
                           gwesp(decay=0.2, fixed=TRUE,type="OTP") + 
                           #geometrically weighted indegree distribution with alpha set to 0.2
                           gwidegree(decay=0.2, fixed=TRUE) +
                           #geometrically weighted outdegree distribution with alpha set to 0.2
                           gwodegree(decay=0.2, fixed=TRUE) + 
                           nodematch("race") +  #same race
                           nodematch("sex") + #same sex
                           nodematch("grade") + #same grade
                           nodeocov("sex") + #effect of sex on the outdegree distribution
                           nodeicov("sex"), #effect of sex on the indegree distribution
                   #the below parameters define the tuning parameters for the 
                   #Metropolis-Hastings algorithm
                   #[for publication models, the values should be much higher]
                   control = control.ergm(MCMC.samplesize = 10000, #the number of networks to sample from the chain
                                          MCMC.burnin = 10000, #the number of steps to make before starting the sample of the chain
                                          MCMC.interval = 1000, #the number of steps to make between sampling a proposed network
                                          MCMLE.maxit = 30, #the maximum number of iterations
                                          seed = 3718, #the random seed for replication
                                          parallel=3, #the number of cores to use for parallel computation
                                          parallel.type="PSOCK")) #the parallel type

summary(fitted.model) #summarizing the model fit

###---------------------------------------------------------------------------------------###

# Step 5. Interpret the model coefficients (odds ratios and average marginal effects)

#there are two ways to interpret the results of a fitted ERGM: global network level and individual
#tie level. From the global network perspective, a positive estimated effect indicates that the 
#count of a specific graph configuration is greater in the realized (observed) network than 
#expected by chance with graphs of the same number of nodes. For example, a positive mutual term 
#indicates that there are more reciprocal ties than expected, net of all other controls. 
#From the individual tie perspective, a positive estimated effect indicates that the log-odds of a 
#specific (i,j) tie being 1 is increased by beta for a one-unit increase in the network statistic. For 
#example, a positive mutual term with estimated value beta suggests that the (i,j) tie is exp(beta) 
#times more likely to be 1 if the (j,i) tie exists net of all other controls. However, as Duxbury (2022)
#points out, the estimated values are scaled by an unknown scaling constant (see Duxbury [2023]), therefore
#the estimates can not be interpreted as effect sizes (i.e., estimated beta = true beta/(scaling constant) [assuming no endogenity]).
#Since post-estimation probabilities are not affected by the scaling bias, one can interpret the parameters of a model from 
#the individual tie perspective by using average marginal effects. 

#The Problem of Scaling in Exponential Random Graph Models. Scott Duxbury. Sociological Methods & Research
###---------------------------------------------------------------------------------------###

# mutual (reciprocity) term
fitted.model$coefficients["mutual"] #estimate: 1.694203 
#the odds-ratio for the mutual term: 5.442305 
exp(fitted.model$coefficients["mutual"])
#the average marginal effect for the mutual term: 0.043383
ergm.AME(fitted.model,var1="mutual")
#the probability of the (i,j) tie being 1 is increased by 0.043383 if the (j,i) tie exists. Or, the
#probability of the (i,j) being 1 is 0.043383 greater than the probability of (i,j) being 0 if the 
#(j,i) tie exists. 

#the average marginal effects for all other model terms: 
ergm.AME(fitted.model,var1="gwesp.OTP.fixed.0.2") #for geometrically weighted edgewise shared partners
ergm.AME(fitted.model,var1="gwideg.fixed.0.2") #for geometrically weighted indegree distribution
ergm.AME(fitted.model,var1="gwodeg.fixed.0.2") #for geometrically weighted outdegree distribution
ergm.AME(fitted.model,var1="nodematch.race") #for same race
ergm.AME(fitted.model,var1="nodematch.sex") #for same gender
ergm.AME(fitted.model,var1="nodematch.grade") #for same grade
ergm.AME(fitted.model,var1="nodeocov.sex") #for sex outdegree effect
ergm.AME(fitted.model,var1="nodeicov.sex") #for sex indegree effect




###---------------------------------------------------------------------------------------###

# Step 6. Checking the model fit (degeneracy, multicollinearity, and goodness-of-fit)

###---------------------------------------------------------------------------------------###

# Step 6.a: Check model degeneracy

#first, we need to check for the degeneracy of the MCMC chains. 
pdf("fittedmodel_degeneracy_soc527.pdf") #this will create a PDF for you to look at (DEPENDS ON YOUR WORKING DIRECTORY)
mcmc.diagnostics(fitted.model) #from the plots, our model does not appear to be degenerate
dev.off() #resetting the device

# Step 6.b: Check model goodness-of-fit

model.gof <- ergm::gof(fitted.model) #now we check the model goodness of fit [this may take some time.....]
ergm:::print.gof(model.gof) #the btergm package has also a gof function that overrides it, so we have to call it this way
ergm:::plot.gof(model.gof) #instead of looking at the outputs, it is often easier to visualize the distributions

# Step 6.c: Check model multicollinearity

vif.ergm(fitted.model) #finally, one should check for multicollinearity (check the VIF; we are doing good!)
# Higher values indicate greater correlation.
# VIF > 20 is concerning, VIF > 100 indicates severe collinearity.
# mutual gwesp.OTP.fixed.0.2 gwideg.fixed.0.2 gwodeg.fixed.0.2 nodematch.race nodematch.sex
# [1,] 1.461773            2.678025         1.382497         1.496254       1.063089      1.056513
# nodematch.grade nodeocov.sex nodeicov.sex
# [1,]        1.696721     2.196364     2.172532




###---------------------------------------------------------------------------------------###

# Step 7. Simulate a network from our fitted ERGM

###---------------------------------------------------------------------------------------###
#simulate one networks from our fitted model (set MCMC burnin to 10000)! 
simulated.network <- simulate(fitted.model,nsim=1, control=control.simulate.ergm(MCMC.burnin = 10000)) 
#we can create the network plots using the ggnetwork package (realized network)
socio.gram <- ggplot(ggnetwork(faux.desert.high), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black", curvature = 0.1,  arrow = arrow(length = unit(0.2, "lines"), 
                                                              type = "closed")) +
  geom_nodes(color=ifelse(get.vertex.attribute(faux.desert.high,"sex") == 1, "blue", "red"),size=2) +
  theme_blank()

#we can create the network plots using the ggnetwork package (simulated network)
simulated.gram <- ggplot(ggnetwork(simulated.network), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black", curvature = 0.1,  arrow = arrow(length = unit(0.2, "lines"), 
                                                              type = "closed")) +
  geom_nodes(color=ifelse(get.vertex.attribute(simulated.network,"sex") == 1, "blue", "red"),size=2) +
  theme_blank()

#we can compare the two networks!
ggdraw() +
  draw_plot(socio.gram, x = 0.00, y = 0.2, width = 0.46, height = 0.6) +
  draw_plot(simulated.gram, x = 0.50, y = 0.2, width = 0.46, height = 0.6) +
  draw_label("Observed Desert High", x = 0.20, y = 0.90, size = 20, fontface = "bold") +
  draw_label("Simulated Desert High", x = 0.75, y = 0.90, size = 20, fontface = "bold")

#finally, we can use the netmediate package to test the micro-effects of grade homophily on 
#the macro-level structure of graph transitivity
mems <- MEMS(fitted.model, #out model
             micro_process = "nodematch.grade", #the micro process of interest
             macro_function = sna::gtrans, #our macro-level network structure
             nsim = 100) #the number of simulations the measure will be based on
mems #print out the results



###---------------------------------------------------------------------------------------###

# Possible Extensions. Temporal Exponential Random Graph Models (TERGMs) and Separable-TERGMS

# here we will use the Glasgow 'Teenage Friends and Lifestyle Study' dataset
# that we used for the  Stochastic Actor-Oriented Models lecture code. 
# The dataset is publicly available! The raw RData files can be downloaded from the following link:
# https://www.stats.ox.ac.uk/~snijders/siena/  (the siena website). I have processed the Glasgow data
# and stored the created network datasets for the class in an RData file.

###---------------------------------------------------------------------------------------###
setwd("~/Dropbox/Guest_Lecture_Slides_SOC_527/ergm") # CHANGE TO MATCH YOUR WORKING DIRECTORY
load("glasgow_network_datasets_processed.RData") #the network datasets
glasgow.networks #printing the network objects
#using the btergm R package, we estimate the Temporal Exponential Random Graph Models
#TERGMs are an ERGM fit to a block-matrix where the diagonal values are the panel networks
tergm.glasgow <- mtergm(glasgow.networks ~
                         edges + #the terms are the same from above, except memory() which captures within-state dependence
                         mutual + 
                         gwesp(decay=0.7, fixed=TRUE,type="OTP") + 
                         gwesp(decay=0.7, fixed=TRUE,type="ITP") + 
                         gwidegree(decay=0.7, fixed=TRUE) + 
                         gwodegree(decay=0.7, fixed=TRUE) +
                         nodematch("male") + 
                         nodeocov("male") + 
                         nodeicov("male") + 
                         memory(type = "stability"), #a lagged term
                         control = control.ergm(MCMC.samplesize = 10000, #the number of networks to sample from the chain
                                                MCMC.burnin = 10000, #the number of steps to make before starting the sample of the chain
                                                MCMC.interval = 1000, #the number of steps to make between sampling a proposed network
                                                MCMLE.maxit = 30, #the maximum number of iterations
                                                seed = 3718, #the random seed for replication
                                                parallel=3, #the number of cores to use for parallel computation
                                                parallel.type="PSOCK"))
summary(tergm.glasgow) #summary of the model
checkdegeneracy(tergm.glasgow,mycol="black") #checking the model degeneracy
fitted.gof <- gof(tergm.glasgow) #checking the model goodness-of-fit
plot(fitted.gof) #plotting the model goodness-of-fit
attr(tergm.glasgow,"aic") #the AIC for the model


#fitting an STERGM (Separable Temporal Exponential Random Graph Models)
#stergms are useful for modeling within-network changes, as we model separately the processes that 
#capture the factors associated with the creation of ties (Form operator) and the processes that 
#capture the factors associated with the persistence (maintenance) of ties across panels
stergm.glasgow <- tergm(glasgow.networks ~
                          #the specifications for the formation of the ties
                          Form(~edges + mutual + gwesp(decay=0.7, fixed=TRUE,type="OTP") +
                                gwidegree(decay=0.7, fixed=TRUE) + gwodegree(decay=0.7, fixed=TRUE) +
                                nodematch("male") +  nodeocov("male") + nodeicov("male")) +
                          #the specifications for the persistence of the ties
                          Persist(~edges + mutual + gwesp(decay=0.7, fixed=TRUE,type="OTP") +
                                  gwidegree(decay=0.7, fixed=TRUE) + gwodegree(decay=0.7, fixed=TRUE) +
                                  nodematch("male") +  nodeocov("male") + nodeicov("male")),
                        estimate="CMLE", #using conditional maximum likelihood estimation
                        control = control.tergm(CMLE.MCMC.burnin = 10000, #the number of steps to make before starting the sample of the chain
                                                CMLE.MCMC.interval = 1000, #the number of steps to make between sampling a proposed network
                                                CMLE.ergm = list(MCMC.samplesize = 10000,#the number of networks to sample from the chain
                                                                 MCMLE.maxit = 30),#the maximum number of iterations
                                                seed = 3718, #the random seed for replication
                                                parallel=4, #the number of cores to use for parallel computation
                                                parallel.type="PSOCK"))

summary(stergm.glasgow) #summary of the model


###---------------------------------------------------------------------------------------###
# Final Step: Export all of the results
###---------------------------------------------------------------------------------------###
setwd("~/Dropbox/Guest_Lecture_Slides_SOC_527/ergm") # CHANGE TO MATCH YOUR WORKING DIRECTORY
save.image(file = "soc527_introduction_to_ergms_111026.RData") #all of the results


# that is the end of the code script, if you have any questions about anything please feel free to 
# contact me at kacarson@arizona.edu

# Cheers!  (: 


