###---------------------------------------------------------------------------------------###

#       Introduction to Stochastic Actor-Oriented Models
#       University of Arizona School of Sociology SOC 527: Social Networks
#       Code Written by Kevin A. Carson (https://kevincarson.github.io/)
#       Last Updated: 10-08-2026

###---------------------------------------------------------------------------------------###
rm(list = ls()) #clearing the working memory
library(RSiena) #reading the RSiena package (for the estimation of SAOMs)
library(sna) #reading the sna package (for the computation of clustering)
library(network) #reading the network package
library(netmediate) #reading in the netmediate package (for the computation of MEMs)
library(networkDynamic) #reading the networkDynamic package to store the dynamic microstep movie
library(ndtv)  #reading the ndtv package to make the microstep video object
library(ggplot2) #reading in the ggplot2 to make some nice plots
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
# [1] ggplot2_4.0.1         ndtv_0.13.4           animation_2.8         networkDynamic_0.11.5 netmediate_1.1.1     
# [6] sna_2.8               network_1.19.0        statnet.common_4.12.0 RSiena_1.5.0         
# 
# loaded via a namespace (and not attached):
# [1] Rdpack_2.6.4             lpSolveAPI_5.5.2.0-17.14 rlang_1.1.6              magrittr_2.0.4          
# [5] compiler_4.5.2           vctrs_0.6.5              stringr_1.6.0            pkgconfig_2.0.3         
# [9] fastmap_1.2.0            labeling_0.4.3           rmarkdown_2.30           purrr_1.2.0             
# [13] xfun_0.58                cachem_1.1.0             ergm.multi_0.3.0         trust_0.1-8             
# [17] jsonlite_2.0.0           parallel_4.5.2           R6_2.6.1                 RColorBrewer_1.1-3      
# [21] stringi_1.8.7            pkgload_1.4.1            knitr_1.50               Matrix_1.7-4            
# [25] rle_0.10.0               tidyselect_1.2.1         rstudioapi_0.17.1        yaml_2.3.11             
# [29] lattice_0.22-7           tibble_3.3.0             withr_3.0.2              S7_0.2.1                
# [33] tsna_0.3.6               askpass_1.2.1            coda_0.19-4.1            evaluate_1.0.5          
# [37] networkLite_1.1.0        lpSolve_5.6.23           pillar_1.11.1            generics_0.1.4          
# [41] scales_1.4.0             xtable_1.8-4             sampling_2.11            glue_1.8.0              
# [45] scatterplot3d_0.3-44     tools_4.5.2              robustbase_0.99-6        ergm_4.10.1             
# [49] grid_4.5.2               rbibutils_2.4            ergMargins_1.6.1         base64_2.0.2            
# [53] nlme_3.1-168             tergm_4.2.2              cli_3.6.5                intergraph_2.0-4        
# [57] dplyr_1.1.4              gtable_0.3.6             DEoptimR_1.1-4           digest_0.6.39           
# [61] farver_2.1.2             htmlwidgets_1.6.4        memoise_2.0.1            htmltools_0.5.9         
# [65] lifecycle_1.0.4          openssl_2.3.4            MASS_7.3-65   


###---------------------------------------------------------------------------------------###


# Step 1: Loading and Processing the Network Dataset


# reading in the dataset for the example, here we will use the 'Teenage Friends and Lifestyle Study' data
# which is the most commonly used dataset for examples with Stochastic Actor-Oriented Models and
# the dataset is publically available! The RData files can be downloaded from the following link:
# https://www.stats.ox.ac.uk/~snijders/siena/  (the siena website)
#
# There are three waves of data, but for the ease of computation, for our examples we will only
# rely upon the first two waves of data.
###---------------------------------------------------------------------------------------###
setwd("~/Dropbox/Guest_Lecture_Slides_SOC_527/Glasgow_data") # CHANGE TO MATCH YOUR WORKING DIRECTORY
load("Glasgow-friendship.RData") #the friendship data
# per the siena website: 
# file Glasgow-friendship.RData contains the three friendship networks in the adjacency matrices 
# friendship.1, friendship.2, and friendship.3, for measurement points 1, 2, and 3, respectively. 
# The data are valued; code 1 stands for "best friend", code 2 for "just a friend", and code 0 
# for "no friend". Code 10 indicates structural absence of the tie, i.e., at least one of the 
# involved students was not yet part of the school cohort, or had already left the school 
# cohort at the given time point
friendship.1 #wave 1 friendship network (aij = 1 means i reported j as a friend)
friendship.2 #wave 2 friendship network (aij = 1 means i reported j as a friend)

load("Glasgow-demographic.RData") #the demographic data
sex.F #gender (1 for boys and 2 for girls)
#we are going to reverse code this to be 1 for boys and 0 for girls
gender <- ifelse(sex.F == 1, 1, 0)

load("Glasgow-substances.RData") #the substance usage data, for our examples, we focus only on alcohol consumption
tobacco #per the website: Tobacco use has the scores 1 (non), 2 (occasional) and 3 (regular, i.e. more than once per week).

#now, we need to do some data cleaning, since we only want those students that have no missing 
#information included in our example
load("Glasgow-selections.RData")
selection129 #per the website: variable selection129 identifies the students present at all three measurement times
smoking <- tobacco[selection129,] #only those adolescents in the 129 selection
gender.1 <- gender[selection129] #only those adolescents in the 129 selection
alcohol.data <- alcohol[selection129,] #subsetting the data object to only those 129 students
friendship.w1 <- friendship.1[selection129,selection129] #only those adolescents in the 129 selection
friendship.w2 <- friendship.2[selection129,selection129] #only those adolescents in the 129 selection
friendship.w3 <- friendship.3[selection129,selection129] #only those adolescents in the 129 selection
friendship.w1[friendship.w1 > 1] <- 1 #making all friendships = 1
friendship.w2[friendship.w2 > 1] <- 1 #making all friendships = 1
friendship.w3[friendship.w3 > 1] <- 1 #making all friendships = 1

###---------------------------------------------------------------------------------------###

# Step 2: Constructing the RSiena Data Objects

###---------------------------------------------------------------------------------------###
# coCovar = the actor-level covariate is constant across time
# varCovar = the actor-level covariate changes over time
# coDyadCovar = the dyadic covariate is constant across time (i.e., where two adolscents live)
# varDyadCovar = the dyadic covariate is constant across time (i.e., where two adolscents live)
smoking.siena <- coCovar(smoking[,1]) #the smoking variable for wave 1
gender.siena <- coCovar(gender.1) #the gender variable 
friendship.net <- sienaDependent(array(data=c(friendship.w1,friendship.w2),
                                       dim=c(nrow(friendship.w1),nrow(friendship.w1),2)),
                                 type="oneMode")
#the sienaDataCreate() function create the siena dataset
siena.data <- sienaDataCreate(friend=friendship.net,
                              smoking=smoking.siena,
                              gender=gender.siena)

###---------------------------------------------------------------------------------------###

# Step 3: Setting Up the Stochastic Actor-Oriented Models

###---------------------------------------------------------------------------------------###

# we need to set the siena algorithm used for the MOM estimation
# there are alot of options for the algorithm, so I suggest looking at the function documentation
# and the RSiena manual for more information on the algorithm parameters 
# The RSiena Manual: https://www.stats.ox.ac.uk/~snijders/siena/RSiena_Manual.pdf
siena.algo <- sienaAlgorithmCreate(projname="soc527_saom",
                                   nsub=4, #the number of subphases in Phase 2 (the estimation of the parameters)
                                   n3=2000, #the number of iterations in Phase 3 (the estimation of the standard errors)
                                   seed = 3718) #the seed for replication

# now we need to set the effects we are going to include in the model, the effects 
# define how and what variables are included in the model
my.effects <- getEffects(siena.data)
#we can look at all of the possible effects by doing this:
effectsDocumentation(my.effects) #it will open a html file in your browser
#we will include effects here using the includeEffects() function 
my.effects <- includeEffects(my.effects, #the effects objects
                             transTrip, #transitive triplets
                             transRecTrip, #transitive recip. triplets
                             outPopSqrt, #outdegree popularity (sqrt)
                             outActSqrt, #outdegree activity (sqrt)
                             inPopSqrt) #indegree popularity (sqrt)
my.effects <- includeEffects(my.effects,#the effects objects
                             interaction1 = "smoking", #the covariate of interest
                             egoX, #the effect of smoking on the outdegree distribution
                             altX, #the effect of smoking on the indegree distribution
                             simX) #smoking homophily
my.effects <- includeEffects(my.effects,#the effects objects
                             interaction1 = "gender", #the covariate of interest
                             egoX, #the effect of gender on the outdegree distribution
                             altX, #the effect of gender on the indegree distribution
                             sameX) #gender homophily
#printing the full effects object
my.effects


###---------------------------------------------------------------------------------------###

# Step 4: Estimating the Stochastic Actor-Oriented Model

###---------------------------------------------------------------------------------------###
saom.fit <- siena07(siena.algo, #the algorithm
                    data=siena.data, #the dataset
                    effects=my.effects, #the effects object
                    returnDeps=TRUE,#must be set to TRUE for the computation of the GOF
                    returnChains = TRUE) # returning the mirco-step chains. we use this later, but it can 
                                         # make the data object to large, so I would suggest, in general,
                                         # to keep it set to FALSE
summary(saom.fit) #the estimated MOM parameters
saom.fit


###---------------------------------------------------------------------------------------###

# Step 5: Checking the Model Goodness-of-Fit

# checklist:
#     the maximum overall convergence ratio is less than 0.25
#     the individual convergence t-ratios are less than 0.10 in absolute value
#     the Mahalanobis distance p-values are non-zero for the simulated networks

###---------------------------------------------------------------------------------------###

#first, look at the maximum overall convergence ratio  (you can also see this from the printed object)
saom.fit$tconv.max #0.2309, this is good!
#second, look at the individual convergence t-ratios (you can also see this from the printed object)
saom.fit$tconv #the individual t-ratios
all(abs(saom.fit$tconv) < 0.10) #TRUE, this is good!

#now, we need to check the goodness of fit of the simulated networks
#this is done via the sienaGOF() function, for our purposes, we will check the
#following distributions:
#   outdegree, indegree, and the triad census
outdegree.gof <- sienaGOF(saom.fit,OutdegreeDistribution,varName = "friend")
indegree.gof <- sienaGOF(saom.fit,IndegreeDistribution,varName = "friend")
triad.gof <- sienaGOF(saom.fit,TriadCensus,varName = "friend")
outdegree.gof #the p-value is 0.017 (we could improve the model fit)
plot(outdegree.gof) #where can we improve model fit
#it looks like our model is not fitting isolates well
#the red line is the observed network and the violin plots are 
#the simulated networks, we could potentially improve the fit by including 
#a term to capute the underestimated count of isolates
indegree.gof #the p-value is 0.76 (we are doing very good!)
plot(indegree.gof) #as shown here, we are doing pretty good capturing the indegree distribution
triad.gof #the p-value is 0.117  (we are doing relatively good!)
plot(triad.gof) #as shown here, we are doing pretty good capturing the MAN distribution


###---------------------------------------------------------------------------------------###

# Step 6: Extracting the Microsteps for Visualization

###---------------------------------------------------------------------------------------###

micro.chain <- saom.fit$chain[[500]][[1]][[1]] #the 500th microstep chain
#for the simulation, just the first 50 changes! (if you want the entire chain, comment out code)
micro.chain <- micro.chain[1:50] #the first 50 microsteps
datChain <- t(matrix(unlist(micro.chain),nc=length(micro.chain)))
#per the jimi adams (2018) code script: 
### Interpretation of the columns on the outputted df - 

# 1 - network or behavior function 
# 2 - same as 1, denoted as 0/1 
# 3 - same as 1/2, denoted using varname 
# 4 - ego ID (starting @0) 
# 5 - if network function - alter ID (starting @0) for changes 
# ego ID for no change 
# 0 if behavior function 
# 6 - 0 if network 
# -1, 0, 1 for change in behavior level behavior 
# Our aims don't make use of columns 7-10

# 11 - Designates stability (i.e., “True” means no change, “False” means change) 
chain.data <- as.data.frame(datChain, stringsAsFactors = F)
tie.toggles <- chain.data[,c(4,5,11)]
colnames(tie.toggles) <- c("ego", "alter", "stable")
#one issue is that the indexing is in c++, so we have to update by 1
tie.toggles$ego <- as.numeric(tie.toggles$ego) + 1
tie.toggles$alter <- as.numeric(tie.toggles$alter) + 1
tie.toggles$stable <- as.logical(tie.toggles$stable) 
#a list to store the created network attributes
net.microsteps <- vector("list",nrow(tie.toggles) + 1)
net.microsteps[[1]] <- network(friendship.w1)
set.vertex.attribute(net.microsteps[[1]],"gender",as.numeric(gender))
update.net <- friendship.w1
for(i in 1:nrow(tie.toggles)){
  idx <- i + 1 #to store the updated network attribute
  egox <- tie.toggles$ego[i]
  altx <- tie.toggles$alter[i]
  if(!tie.toggles$stable[i]){ #if there was a change!
    cur.edge <- update.net[egox,altx]
    new.edge <- 1 - cur.edge
    update.net[egox,altx] <- new.edge
  }
  net.microsteps[[idx]] <- network(update.net)
  set.vertex.attribute(net.microsteps[[idx]],"gender",as.numeric(gender))
}

sequence.chain <- networkDynamic(network.list = net.microsteps) #this may take some time to run.....
activate.vertex.attribute(sequence.chain, "active.col", ifelse(gender==1,"blue","red"),onset=0, terminus=Inf) # Which choice is to be evaluated per jimi adams
activate.vertex.attribute(sequence.chain, "active.size", 0.50,onset=0, terminus=Inf) # Which choice is to be evaluated per jimi adams
activate.vertex.attribute(sequence.chain, "active.sides", 50, onset=0, terminus=Inf) # Which choice is to be evaluated per jimi adams
for(i in 1:nrow(tie.toggles)){
  activate.vertex.attribute(sequence.chain, "active.col", "darkgreen", onset=i-1, terminus=i, v =  tie.toggles$ego[i])
  activate.vertex.attribute(sequence.chain, "active.size", 1.5, onset=i-1, terminus=i, v =  tie.toggles$ego[i])
  activate.vertex.attribute(sequence.chain, "active.sides", 4, onset=i-1, terminus=i, v =  tie.toggles$ego[i]) # Which choice is to be evaluated per jimi adams
}
palette(c("white", "gray75", "black"))

### This generates the layouts conforming to each interval (be sure to change the end back)
slice.par <- list(interval=1, aggregate.dur=1, start=0, end=nrow(tie.toggles), rule="all")
compute.animation(sequence.chain, slice.par=slice.par, displayisolates=T)

### This generates the video from that dynamic layout.
# as html
setwd("~/Dropbox/Guest_Lecture_Slides_SOC_527") # CHANGE TO MATCH YOUR WORKING DIRECTORY
render.d3movie(sequence.chain, 
               filename='soc527_saom_movie.html',
               launchBrowser=FALSE,
               edge.col="gray75",
               vertex.col="active.col", 
               vertex.cex = "active.size",
               displaylabels=FALSE, 
               main= NULL,
               vertex.sides = "active.sides",
               label.cex=.5,
               ani.options=list(interval=1),  
               script.type='embedded')

###---------------------------------------------------------------------------------------###

# Step 7: Micro-Level Effects on Marco-Structures

#here we are interested in what is the effect of gender homophily on the transitivity of the network? 

###---------------------------------------------------------------------------------------###
n.sims <- 100 #the number of simulations to conduct
#let's draw 100 beta values from the empirical distribution
thetas.empirical <- MASS::mvrnorm(n.sims, mu=saom.fit$theta, Sigma = saom.fit$covtheta)
rates.of.netchange <- rnorm(n.sims, saom.fit$rate, sd = saom.fit$vrate)
thetas.null <- thetas.empirical
thetas.null[,which(my.effects$effectName[my.effects$include==TRUE] == "same gender") - 1] <- 0 #setting the effect to 0
#transitivity values for null distribution
transitivity.null <- numeric(n.sims)
#transitivity values for empirical distribution
transitivity.empirical <- numeric(n.sims)

#recreating the algorithm for our simulations
simulation.algo <- sienaAlgorithmCreate(projname="soc527_simulations",
                                        cond = FALSE, #unconditional estimation
                                        nsub = 0, #no subphases in phase 3
                                        seed = 3718, #the seed for replication
                                        n3=100, #the number of simulations in phase 3 (a small value)
                                        simOnly = TRUE, #simulation only
                                        useStdInits=FALSE)

#conducting the simulations to estimate the effect of gender homophily on the transitivity of the network
for(i in 1:n.sims){
  my.effects$initialValue[my.effects$include==TRUE] <- c(rates.of.netchange[i], thetas.null[i,])
  simulated.null <- siena07(simulation.algo, data=siena.data,
                            effects=my.effects, returnDeps=TRUE)
  simulated.edges <- simulated.null$sims[[100]][[1]][[1]][[1]] #simulated network
  sim.net.null <- matrix(0,nrow(friendship.w1),nrow(friendship.w1))
  sim.net.null[simulated.edges[,1:2]] <- 1 #adding the simulated edges
  transitivity.null[i] <- gtrans(sim.net.null)
  
  my.effects$initialValue[my.effects$include==TRUE] <- c(rates.of.netchange[i], thetas.empirical[i,])
  simulated.empirical <- siena07(simulation.algo, data=siena.data,
                             effects=my.effects, returnDeps=TRUE)
  simulated.edges <- simulated.empirical$sims[[100]][[1]][[1]][[1]] #simulated network
  sim.net.empirical <- matrix(0,nrow(friendship.w1),nrow(friendship.w1))
  sim.net.empirical[simulated.edges[,1:2]] <- 1 #adding the simulated edges
  transitivity.empirical[i] <- gtrans(sim.net.empirical)
}

diff.data <- data.frame(null=transitivity.null,
                        empirical=transitivity.empirical,
                        diff=transitivity.empirical-transitivity.null)
mems.data <- data.frame(estimate = mean(diff.data$diff),
                        se = sd(diff.data$diff))
mems.data$p.value1 <- 2*pnorm(abs(mems.data$estimate/mems.data$se),lower.tail = FALSE)
mems.data$p.value2 <- sum(diff.data$diff < 0)/n.sims
mems.data #printing out the results
# estimate        se    p.value     p.value2
# 0.1763177 0.09320648 0.05853238     0.03
#proportion change in transitivity based on gender homophily
100*mems.data$estimate/sum(diff.data$empirical)
gplot.data <- data.frame(dis = c(rep("No Gender Homophily",n.sims), rep("Gender Homophily", n.sims)),
                         value = c(diff.data$null,diff.data$empirical))
full.plot <- ggplot(gplot.data,aes(x = "", y = value, fill=dis)) +
  geom_boxplot() +
  labs(x = "Simulated Values Based on Fitted SAOM",
       y = "Transitivity of Simulated Networks",
       fill = "Distribution:")+ 
  scale_fill_manual(values=c("Gender Homophily" = "#2171B5",
                            "No Gender Homophily" = "red")) + 
  scale_y_continuous(limits = c(0, 0.60), breaks = seq(0,0.60,0.05)) +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = "top") 

###---------------------------------------------------------------------------------------###
# Conversely, we could get similiar results (much faster) by using the netmediate R package
# the netmediate R package contains a function called MEMS() that allows users
# to specify various micro-effects on marco-structures across different modeling
# strategies and different research designs
###---------------------------------------------------------------------------------------###
mems <- MEMS(saom.fit, #out model
             micro_process = "same gender", #the micro process of interest
             macro_function = sna::gtrans, #our macro-level network structure
             nsim = 100, #the number of simulations the measure will be based on
              SAOM_data=siena.data) #the siena dataset that we have been using
mems


###---------------------------------------------------------------------------------------###

# Step 8: The Co-Evolution of Networks and Behavior (the real power of the SAOM)

# alongside allowing use to draw inferences about panel network dynamics, the 
# SAOM is powerful as a tool to model the co-evolution of network dynamics and 
# behavioral changes. This is related to the concern of the simultaneous process of 
# peer influence and peer selection. 

# in our example here, we estimate a co-evolution model to understand the evolution of
# friendship dynamics and drinking behavior
###---------------------------------------------------------------------------------------###
#creating the siena network data object
friendship <- sienaDependent(array(data=c(friendship.w1,friendship.w2,friendship.w3),
                                   dim = c(nrow(friendship.w1), nrow(friendship.w1), 3)))
#creating the siena diffusion variable data object
alcohol <- sienaDependent(alcohol.data, type = "behavior")
#creating the siena dataset
my.data <- sienaDataCreate(friend = friendship, 
                           alcohol = alcohol,
                           gender = gender.siena)
#creating the algorithm object
siena.algo <- sienaAlgorithmCreate(projname="glasgow_alcohol",
                                   n3=5000,nsub=6,seed=37)
my.effects <- getEffects(my.data) #getting the set of possible effects
#effectsDocumentation(my.effects)
my.effects <- includeEffects(my.effects,
                             transTrip,
                             transRecTrip, #transitive recip. triplets
                             outPopSqrt, #outdegree popularity (sqrt)
                             outActSqrt, #outdegree activity (sqrt)
                             inPopSqrt,
                             name="friend")
my.effects <- includeEffects(my.effects,sameX,egoX,altX,
                             name="friend",interaction1 = "gender")
my.effects <- includeEffects(my.effects,simX,egoX,altX,
                             name="friend",interaction1 = "alcohol")
my.effects <- includeEffects(my.effects,avSim,name="alcohol",
                             interaction1 = "friend")
#estimate the SAOM
drinking.model <- siena07(siena.algo,
                          data = my.data,
                          effects = my.effects,
                          returnDeps= TRUE,
                          useCluster = TRUE,
                          verbose = FALSE,
                          nbrNodes = 4, #four cores for faster computation
                          silent = TRUE)
drinking.model

                           
###---------------------------------------------------------------------------------------###

# Step 9: The Evolution of Networks and the Diffusion ofs Behavior (the real power of the SAOM)

# in our example here, we will replicate the co-evolution diffusion model published by 
# Greenan (2015).Diffusion of innovations in dynamic networks, where models the 
# evolution of friendship dynamics and the adoption of marijuana use across 129 students
# in the Glasgow dataset. 
###---------------------------------------------------------------------------------------###
setwd("~/Dropbox/Guest_Lecture_Slides_SOC_527/Glasgow_data") # CHANGE TO MATCH YOUR WORKING DIRECTORY
load("Glasgow-friendship.RData") #the network objects
load("Glasgow-substances.RData") #the dependent variable and exogenous covariates
load("Glasgow-demographic.RData") #the socio demographics of the adolescents
all.waves <- which(complete.cases(cannabis)) #checking who is present at every wave
alcohol.data <- alcohol[all.waves,] #subsetting the data object to only those 129 students
sex <- sex.F - 1 #girls = 1, boys = 0
sex.data <- sex[all.waves]#subsetting the data object to only those 129 students
cannabis.data <- cannabis[all.waves,]#subsetting the data object to only those 129 students
net1 <- friendship.1[all.waves,all.waves]#subsetting the data object to only those 129 students
net2 <- friendship.2[all.waves,all.waves]#subsetting the data object to only those 129 students
net3 <- friendship.3[all.waves,all.waves]#subsetting the data object to only those 129 students
net1[net1 > 0] <- 1 #if the tie is present, make it 1
net2[net2 > 0] <- 1 #if the tie is present, make it 1
net3[net3 > 0] <- 1 #if the tie is present, make it 1
#------------------------------------------------------------------------------------#
#Creating the Cannabis Diffusion Variable
#------------------------------------------------------------------------------------#
#a matrix to store the wave-specific diffusion value for each adolescent
cannabis.diffusion <- matrix(0,nrow = nrow(cannabis.data), ncol=3)
for(i in 1:nrow(cannabis.diffusion)){ #for each adolscent
  adol.data <- cannabis.data[i,] #the self-reported cannabis smoking values
  starting <- adol.data[1] #what is the wave 1 value? 
  if(starting == 1){ #if they did not use cannabis in the first wave
    if(adol.data[2] != 1){ #if the second wave did use cannabis
      diffuse.vec <- c(0,1,1)
    }else{#if not, 
      if(adol.data[3] != 1){ #did they use cannabis in the last wave? 
        diffuse.vec <- c(0,0,1)
      }else{
        diffuse.vec <- rep(0,3) #if they did not self-report cannabis smoking across the waves
      }
    }
  }else{ #if they did
    diffuse.vec <- rep(1,3) #all values are then 1
  }
  cannabis.diffusion[i,] <- diffuse.vec #add the value to the diffusion matrix
}
#creating the siena network data object
friendship <- sienaDependent(array(data=c(net1,net2,net3),
                                   dim = c(nrow(net1), nrow(net1), 3)))
#creating the siena diffusion variable data object
cannabis <- sienaDependent(cannabis.diffusion, type = "behavior")
alcohol <- varCovar(alcohol.data) #the alcohol use variable 
female <- coCovar(sex.data) #the gender variable
#creating the data object
my.data <- sienaDataCreate(net = friendship,
                           smoking = cannabis,
                           alcohol = alcohol,
                           female=female)
#creating the algorithm object
siena.algo <- sienaAlgorithmCreate(projname="Greenan_2015_Replication",
                                   n3=5000,nsub=6,firstg=0.05,seed=37)
my.effects <- getEffects(my.data) #getting the set of possible effects
#effectsDocumentation(my.effects)
#including all of the effects present in Greenan 2015
my.effects <- includeEffects(my.effects,nbrDist2, transTrip,
                             name="net")
my.effects <- includeEffects(my.effects,simX,
                             name="net",interaction1 = "female")
my.effects <- includeEffects(my.effects,simX,egoX,altX,
                             name="net",interaction1 = "smoking")
my.effects <- includeEffects(my.effects,avExposure,name="smoking",
                             interaction1 = "net",type = "rate")
my.effects <- includeEffects(my.effects,avExposure,name="smoking",
                             interaction1 = "net",type = "rate")
my.effects <- includeEffects(my.effects,RateX,name="smoking",
                             interaction1 = "female",type = "rate")
my.effects <- includeEffects(my.effects,RateX,name="smoking",
                             interaction1 = "alcohol",type = "rate")
my.effects <- includeEffects(my.effects,infectCovar,name="smoking",
                             interaction1 = "net",interaction2 = "female",
                             type = "rate")
#estimate the SAOM
diffuse.model <- siena07(siena.algo,
                         data = my.data,
                         effects = my.effects,
                         returnDeps= TRUE,
                         useCluster = TRUE,
                         verbose = FALSE,
                         nbrNodes = 4, #four cores for faster computation
                         silent = TRUE)
diffuse.model

###---------------------------------------------------------------------------------------###
# Final Step: Export all of the results
###---------------------------------------------------------------------------------------###
setwd("~/Dropbox/Guest_Lecture_Slides_SOC_527") # CHANGE TO MATCH YOUR WORKING DIRECTORY
save.image(file = "soc527_introduction_to_saoms_10082026.RData") #all of the results



# that is the end of the code script, if you have any questions about anything please feel free to 
# contact me at kacarson@arizona.edu

# Cheers! 

