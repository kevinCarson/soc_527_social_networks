###---------------------------------------------------------------------------------------###

#       Introduction to Relational Event Models (REMs)
#       University of Arizona School of Sociology SOC 527: Social Networks
#       Code Written by Kevin A. Carson (https://kevincarson.github.io/)
#       Last Updated: 10-16-26

###---------------------------------------------------------------------------------------###
rm(list = ls()) #clearing the working memory
library(relevent) #reading the relevent package 
library(dream) #reading the dream package
library(amorem) #reading the amorem package
library(remdata) #reading the remdata package
library(survival) #reading the survival package
library(rem)#reading the rem package
library(dplyr) #the dplyr package
library(coxme) #the coxme package for mixed effects cox models
library(remify)
library(remstimate)
library(remstats)
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
# [1] coxme_2.2-22          bdsmatrix_1.3-7       rem_1.3.1             survival_3.8-3        remdata_0.2.1         amorem_1.2.0         
# [7] relevent_1.2-1        coda_0.19-4.1         sna_2.8               network_1.19.0        statnet.common_4.12.0 trust_0.1-8          
# [13] dplyr_1.1.4           dream_2.1.5          
# 
# loaded via a namespace (and not attached):
# [1] Matrix_1.7-4      compiler_4.5.2    tidyselect_1.2.1  Rcpp_1.1.2        collapse_2.1.5    parallel_4.5.2    splines_4.5.2     yaml_2.3.11      
# [9] fastmap_1.2.0     lattice_0.22-7    R6_2.6.1          generics_0.1.4    knitr_1.50        iterators_1.0.14  tibble_3.3.0      pillar_1.11.1    
# [17] rlang_1.1.6       xfun_0.58         pkgload_1.4.1     doParallel_1.0.17 cli_3.6.5         magrittr_2.0.4    digest_0.6.39     foreach_1.5.2    
# [25] grid_4.5.2        rstudioapi_0.17.1 nlme_3.1-168      lifecycle_1.0.4   vctrs_0.6.5       evaluate_1.0.5    glue_1.8.0        data.table_1.18.0
# [33] codetools_0.2-20  rmarkdown_2.30    tools_4.5.2       pkgconfig_2.0.3   htmltools_0.5.9  

###---------------------------------------------------------------------------------------###

# For this code script, we are going to use the Copenhagen Networks Study in the 
# amorem R package. The Copenhagen Networks Study followed 700 university students over four weeks
# to understand the dynamics of interactions across multiple spaces and forms of communications.
# Please see help("copenhagen_calls") for more information on the dataset. For more information on the
# study, please see here for more information: https://www.nature.com/articles/s41597-019-0325-x 
# In our lecture example, we will focus on relational sequence of directed phone calls. 
# Notes: copenhagen_calls -> the temporally ordered set of dyadic directed phone calls at 
#        specific time points, where a row denotes a directed phone call from "sender" to 
#        "receiver" at t ("time")

copenhagen_calls #the temporally order set of phone call interactions
copenhagen_actors #information on the gender of the university students

#please note, for our classroom example, we remove dyadic interactions in which the university
#student has missing information on the gender variable.

good.ids <- copenhagen_actors$id #the ids of university students that have information on gender
copenhagen_calls <- subset(copenhagen_calls,copenhagen_calls$sender %in% good.ids & 
                                            copenhagen_calls$receiver %in% good.ids)

###---------------------------------------------------------------------------------------###

###---------------------------------------------------------------------------------------###

#Relational event models (REMs) have two different formulations that are based upon the 
#known timing of the relational events. If the exact timing is known, then the exact (temporal)
#likelihood can be used (see Butts 2008). Conversely, if only the temporal ordering of events is 
#known, then one can use the ordinal timing likelihood. A key difference between the two is 
#concerned with the interpretation of the parameter estimates. In exact timing (temporal) REMs,
#the parameters tell us how covariates increases (decreases) the instantaneous rate of relational events to 
#occur, whereas in ordinal timing REMs, the parameters tell us how covariates increase (decrease)
#the relative likelihood of a relational event to occur. 

#.  To estimate our REM, we are going to rely on the dream R package. Importantly, unlike 
#.  SAOMs and ERGMs, the researcher is required to (1) define the support set, (2) compute
#.  the endogenous and exogenous network covariates, and (3) estimate the REM! The beauty of 
#.  this is that (a) it is relatively easy to create and compute new endogenous statistics 
#.  and (b) allows for one to use a variety of estimation procedures to be used
#.  (e.g., random effects vs fixed effects).
#. 
#.  Within the R ecosystem, there are a variety of packages that can be used to fit
#.  relational event models. These are the: dream R package, the relevent R package, the amorem R package, 
#.  the rem R package, the llrem R package. In addition, a group of scholars have created the remverse 
#.  suite of R packages (remify, remstats, remstimate, remulate packages). Finally, within R, there is the 
#.  goldfish R package that fits actor-oriented relational event models. Outside of R, there is the
#.  eventnet Java application program that can create post-processing relational event sequences. 

# In general, the support set (the set of all potential dyadic events) for a a one-mode relational event model 
# is the full Cartesian product of all active actors. For instance, if we have 5 actors, then the support
# set at time t, is the set of all 20 dyads (excluding events where the sender and receiver is the same). We
# can visualize this by looking at the output from the create_res function in the dream R package. 

# Below is a hypothetical one-mode relational event sequence with 9 actors and 18 relational events
# The following actors are included: A, B, C, D, E, F, G, H, J
events <- data.frame(time = 1:18,
                     sender = c("A", "B", "C",
                                "A", "D", "E",
                                "F", "B", "A",
                                "F", "D", "B",
                                "G", "B", "D",
                                "H", "A", "D"),
                     target = c("B", "C", "D",
                                "E", "A", "F",
                                "D", "A", "C",
                                "G", "B", "C",
                                "H", "J", "A",
                                "F", "C", "B"))
# Creating the full one-mode relational risk set with p = 1.00 (all true events)
# based upon the ordinal timing relational event framework
full.process <- create_res(ordinal = TRUE,
                           type = "one-mode",
                           riskset = "fixed",
                           time = events$time,
                           sender = events$sender,
                           receiver = events$target,
                           p_samplingobserved = 1.00,
                           seed = 9999)
#all potential dyadic events at time 1!
filter(as.data.frame(full.process),time==1)

# Below, we are going to create the riskset for the Copenhagen Networks Study phone call interaction
# events. Here, for the case of our lecture examples, we ignore the timing of events and assume that 
# only the order of the events is known, therefore, we are going to estimate an Ordinal Timing Relational 
# Event Model framework. 

# Step 1. Process the realized relational event sequence and define the support set

# we use the create_res function in the dream package to create the support set
# in our example, we have to use the Case-Control Sampling Procedure since the number of possible events 
# at each time point is too large. In particular, the full number of possible events across time is 510*(510-1)*3385 = 878,712,150, 
# which is too large for a lecture example. 
rem.data <- create_res(time=1:nrow(copenhagen_calls), #the sequential event id
                       sender=copenhagen_calls$sender, #the initiator of the phone call (relational event sender)
                       receiver=copenhagen_calls$receiver, #the receiver of the phone call (relational event receiver)
                       type="one-mode", #a one-mode relational event sequence (actors can send and receive phone calls)
                       ordinal = TRUE, #ordinal timing relational event model
                       riskset="fixed", #in this function, this means that any dyadic phone call can occur at any time point
                       seed=67, #the random number seed fore replication
                       case_control=TRUE, #we use case control sample to sample null events 
                                          #[the full number of possible events across time is 510*(510-1)*3385 = 878,712,150]
                       n_controls=250) #the number of null events for each realized time point

#the print and summary functions relay important information about the post-processed sequence
summary(rem.data)
# From the printed output, we see that:
#     (a) there are 3,385 realized relational events
#     (b) there are 510 actors
#     (c) we sampled 250 control (null) events for each time point (realized event)
#     (d) the total number of relational events in our analysis is 849,635.


###---------------------------------------------------------------------------------------###

###---------------------------------------------------------------------------------------###

# Step 2. Computing Endogenous and Exogenous Statistics

# the next step in estimating a relational event model is to compute the 
# set of endogenous and exogenous statistics. Here, endogenous statistics capture
# the dependency from the history of prior relational events, whereas exogenous
# statistics capture how actor-level attributes and event-level attributes impact 
# the timing of relational events. In the dream package, there are numerous statistics
# that can be included to capture endogenous dependencies. One can also use a variety of 
# other packages to compute relational event model statistics, such as the rem R package. 
#
# When creating endogenous statistics, scholars typically decide between computing the 
# counts of past events or computing the weights of past events (the weights represented a weighted count
# where the weight of a past event decreases as the timing since the event increases)

# A note on the storing of statistics. Within the dream ecosytem, Diego and I created the package to 
# mirror the network R package, where the processed relational event sequence is stored as a dream 
# object and computed network statistics are added to the dream object. 

rem.data.stats <- rem.data %>%
  #computing the repetition (inertia) statistics (the counts of past dyadic events where the sender is the current
  #event sender and the receiver is the current event receiver)
  dreamstats_repetition(counts=TRUE) %>% 
  #computing the reciprocity statistics (the counts of past dyadic events where the sender is the current
  #event receiver and the receiver is the current event sender)
  dreamstats_reciprocity(counts=TRUE) %>%
  #computing the sender outdegree statistic (the counts of past dyadic events where the sender is the current
  #event sender)
  dreamstats_degree(formation="sender-outdegree",counts=TRUE) %>%
  #computing the receiver indegree statistic (the counts of past dyadic events where the receiver is the current
  #event receiver)
  dreamstats_degree(formation="receiver-indegree",counts=TRUE) %>%
  #computing the outgoing two-paths statistic (transitive triads)
  dreamstats_triads(formation="OTP",counts=TRUE) %>%
  #computing the incoming two-paths statistic (cyclical triads)
  dreamstats_triads(formation="ITP",counts=TRUE) 
summary(rem.data.stats) # the summary output shows the added statistics
  
#we add the female exogenous node-level attribute by first creating the dataset
#that stores the information for each actor
actor.data <- data.frame(actor_id = as.character(copenhagen_actors$id),
                         female = copenhagen_actors$female,
                         time_start = -Inf,
                         time_end = Inf)
#here, we are adding the node-level attribute, here we are adding three covariates based on
#female: sender is female, receiver is female, and if the sender and receiver are the same gender
rem.data.stats1 <- rem.data.stats %>%
  dreamstats_actor(var_name = "female", sender_ref = TRUE, actor_info = actor.data) %>%
  dreamstats_actor(var_name = "female", sender_ref = FALSE, actor_info = actor.data) %>%
  dreamstats_dyadic(var_name = "female",transformation = "same")
  
###---------------------------------------------------------------------------------------###

###---------------------------------------------------------------------------------------###

# Step 3. Fitting an Ordinal Timing Relational Event Model to a Post-Processing RES 

#to fit the ordinal timing relational event model to the post-processing we can use the 
#estimate_rem function in the dream package
# to start, we estimate a baseline exogenous attribute only model
fitted.rem.exo <-estimate_rem(~female.sender +  #the effect of female on sending relational events
                            female.receiver +  #the effect of female on receiving relational events
                            female.same, #the effect of female homophily on a relational events
                          data=rem.data.stats1) #the dream data object
summary(fitted.rem.exo) #summary of the fitted model
AIC(fitted.rem.exo) #The model AIC: 37344.47
summary(dream::gof_rem(fitted.rem.exo)) #the proportion of correctly specified realized events: 0.08035
plot(fitted.rem.exo) #this will plot the standardized deviance residuals

# we extend the above model by adding inertia and reciprocity (dyadic processes)
fitted.rem.dyad <-estimate_rem(~female.sender +  #the effect of female on sending relational events
                                female.receiver +  #the effect of female on receiving relational events
                                female.same + #the effect of female homophily on a relational events
                                repetition + #the effect of inertia
                                reciprocity, #the effect of reciprocity
                              data=rem.data.stats1) #the dream data object
summary(fitted.rem.dyad) #summary of the fitted model
AIC(fitted.rem.dyad) #The model AIC: 19044.84 (doing much better!)
summary(dream::gof_rem(fitted.rem.dyad)) #the proportion of correctly specified realized events: 0.71226

# we extend the above model by degree-based processes to capture sociality and popularity-based effects
fitted.rem.degree <-estimate_rem(~repetition + #the effect of inertia
                            reciprocity + #the effect of reciprocity
                            sender.outdegree +  #the effect of sender outdegree
                            receiver.indegree + #the effect of receiver indegree
                            female.sender +  #the effect of female on sending relational events
                            female.receiver +  #the effect of female on receiving relational events
                            female.same, #the effect of female homophily on a relational events
                  data=rem.data.stats1) #the dream data object
summary(fitted.rem.degree) #summary of the fitted model
AIC(fitted.rem.degree) #The model AIC: 19013.18 (doing better!)
summary(dream::gof_rem(fitted.rem.degree)) #the proportion of correctly specified realized events: 0.68538

# we extend the above model by triad processes 
fitted.rem.full <-estimate_rem(~repetition + #the effect of inertia
                                   reciprocity + #the effect of reciprocity
                                   sender.outdegree +  #the effect of sender outdegree
                                   receiver.indegree + #the effect of receiver indegree
                                   outgoing.two.paths + #transitive triad effects
                                   incoming.two.paths + #cyclical closure effects
                                   female.sender +  #the effect of female on sending relational events
                                   female.receiver +  #the effect of female on receiving relational events
                                   female.same, #the effect of female homophily on a relational events
                                 data=rem.data.stats1) #the dream data object
summary(fitted.rem.full) #summary of the fitted model
AIC(fitted.rem.full) #The model AIC: 18609.97 (doing better!)
plot(fitted.rem.full) #this will plot the standardized deviance residuals
coef(fitted.rem.full) #these are in log relative hazard ratios
#For repetition, the parameter estimate is: 1.251
#The parameter interpretation: reciprocating dyadic events are 3.492939 more likely to occur than 
#non-reciprocating dyadic events. 
#For female sender effect, the parameter estimate is: 0.246
#The parameter interpretation: relational events with a female sender are 1.2789 more likely to occur
#than dyadic events with a non-female sender. 

#we can also compare the probability of two events occur by comparing the hazard rates:
new.effects<-data.frame(matrix(0,nrow=1,ncol=length(coef(fitted.rem.full))))
colnames(new.effects) <- names(coef(fitted.rem.full))
hazard.rate.no.recip.male.sender <- predict(fitted.rem.full,newdata = new.effects)
new.effects<-data.frame(matrix(0,nrow=1,ncol=length(coef(fitted.rem.full))))
colnames(new.effects) <- names(coef(fitted.rem.full))
new.effects$reciprocity <- 1
new.effects$female.sender <- 1
hazard.rate.recip.female.sender <- predict(fitted.rem.full,newdata = new.effects)
hazard.rate.recip.female.sender/hazard.rate.no.recip.male.sender #2.507435
#The probability of a female actor reciprocating a prior dyadic relational event is 2.507 times
#more likely to occur than an relational event where a male sender initiates a relational 
#event to a non-reciprocating receiver. 
new.effects<-data.frame(matrix(0,nrow=10,ncol=length(coef(fitted.rem.full))))
colnames(new.effects) <- names(coef(fitted.rem.full))
new.effects$reciprocity <- 0:9
plot(x = 0:9,
     y = log(predict(fitted.rem.full,newdata = new.effects)),
     xlab="count of reciprocal events",
     ylab="log conditional hazard rate")
plot(x = 0:9,
     y = (predict(fitted.rem.full,newdata = new.effects)),
     xlab="count of reciprocal events",
     ylab="conditional hazard rate")

#something of importance is how well are model is doing in terms of predicting the correct dyad at 
#each time point
summary(dream::gof_rem(fitted.rem.full)) #the proportion of correctly specified realized events: 0.68301
#we can also do it by doing this:
rel.ranks.full <- rem.data.stats1$processed_sequence %>%
  mutate(hazards = predict(fitted.rem.full)) %>% #computing the predicted hazard rate for each dyadic event from the model
  group_by(time) %>%
  mutate(rank = rank(hazards), rel.rank = rank(hazards)/n()) %>%
  ungroup() %>%
  filter(observed == 1)
sub.sum <- paste0("Mean relative rank of fitted model: ", round(mean(rel.ranks.full$rel.rank),4))
plot(x=1:nrow(rel.ranks.full),
     y=rel.ranks.full$rel.rank,
     xlab="Event",
     ylab="Relative Rank (higher values are better)",
     main="Relative Rank of Realized Events for Full Model",
     cex=0.5)
abline(h=mean(rel.ranks.full$rel.rank),col="blue")
mtext(sub.sum, side = 3, line = 0.25,  adj = 1,  cex = 1.1)


rel.ranks.teacher <- rem.data.stats1$processed_sequence %>%
  mutate(hazards = predict(fitted.rem.exo)) %>% #computing the predicted hazard rate for each dyadic event from the model
  group_by(time) %>%
  mutate(rank = rank(hazards), rel.rank = rank(hazards)/n()) %>%
  ungroup() %>%
  filter(observed == 1)
sub.sum <- paste0("Mean relative rank of fitted model: ", round(mean(rel.ranks.teacher$rel.rank),4))
plot(x=1:nrow(rel.ranks.teacher),
     y=rel.ranks.teacher$rel.rank,
     xlab="Event",
     ylab="Relative Rank (higher values are better)",
     main="Relative Rank of Realized Events for Attribute-Only Model",
     cex=0.5)
abline(h=mean(rel.ranks.teacher$rel.rank),col="blue")
mtext(sub.sum, side = 3, line = 0.25,  adj = 1,  cex = 1.1)



#the beauty of relational event modeling is that we can use a variety of functions and estimation
#routines to estimate the REM to a post-processing dataset!
surv.data <- as.data.frame(rem.data.stats1) #exporting the data frame from the dream object
#we can use the coxph model for ordinal timing REMS
cox.model <- coxph(Surv(time = rep(1,nrow(surv.data)), event = surv.data$observed) ~ 
                 repetition + 
                 sender.outdegree + 
                 receiver.indegree + 
                 reciprocity + 
                 outgoing.two.paths + 
                 female.sender  + 
                 female.receiver + 
                 female.same+
                 strata(time),
               data=surv.data)
summary(cox.model) #printing the results

#we can additionally account for sender node-level heterogeneity in event sending rates
re.cox.model <- coxme(Surv(time = rep(1,nrow(surv.data)), event = surv.data$observed) ~ 
                    repetition + 
                    sender.outdegree + 
                    receiver.indegree + 
                    reciprocity + 
                    outgoing.two.paths + 
                    female.sender  + 
                    female.receiver + 
                    female.same+
                    (1|sender) +  #random effects for sender
                    strata(time),
                  data=surv.data)
summary(re.cox.model)
AIC(cox.model) - AIC(re.cox.model)  #a comparison of the AIC values: 808.3534 (we should account for node-level heterogeneity!)



###---------------------------------------------------------------------------------------###

# We can also fit an interval timing relational event model to our dataset! Here, we will use 
# the remverse suite of R packages to show how REMs can be fit using different R packages!

#here we will use the McFarland Classroom Event Dataset; please see
setwd("~/Dropbox/Guest_Lecture_Slides_SOC_527/rem") # CHANGE TO MATCH YOUR WORKING DIRECTORY
load("relevent_workshop.RData") #the RData file was taken from: https://statnet.org/workshop-relevent/
#processing the file
classroom.data <- create_res(time=Class$StartTime[1:(nrow(Class)-1)], #the sequential event id
                             sender=Class$FromId[1:(nrow(Class)-1)], #the initiator of the phone call (relational event sender)
                             receiver=Class$ToId[1:(nrow(Class)-1)], #the receiver of the phone call (relational event receiver)
                             type="one-mode", #a one-mode relational event sequence (actors can send and receive phone calls)
                             ordinal = FALSE, #ordinal timing relational event model
                             t=Class$StartTime[nrow(Class)], #the assume ending time point of the sequence
                             riskset="fixed") %>%  
  #event sender and the receiver is the current event receiver)
  dreamstats_repetition(counts=TRUE) %>% 
  #computing the reciprocity statistics (the counts of past dyadic events where the sender is the current
  #event receiver and the receiver is the current event sender)
  dreamstats_reciprocity(counts=TRUE) %>%
  #computing the sender outdegree statistic (the counts of past dyadic events where the sender is the current
  #event sender)
  dreamstats_degree(formation="sender-outdegree",counts=TRUE) %>%
  #computing the receiver indegree statistic (the counts of past dyadic events where the receiver is the current
  #event receiver)
  dreamstats_degree(formation="receiver-indegree",counts=TRUE) %>%
  #computing the outgoing two-paths statistic (transitive triads)
  dreamstats_triads(formation="OTP",counts=TRUE) %>%
  #computing the incoming two-paths statistic (cyclical triads)
  dreamstats_triads(formation="ITP",counts=TRUE) 

#here we will model the impact of being the teacher on receiving and sending communications
actor.data <- data.frame(actor_id = classroom_actors$id,
                         teacher = ifelse(classroom_actors$role == "instructor", 1,0),
                         time_start = -Inf,
                         time_end = Inf)
#adding the teacher sender and receiver effects
classroom.data <- classroom.data %>%
  dreamstats_actor(var_name = "teacher", sender_ref = TRUE, actor_info = actor.data) %>%
  dreamstats_actor(var_name = "teacher", sender_ref = FALSE, actor_info = actor.data)

#fitting a baseline interval timing relational event model with teacher effects
exact.rem.teacher <-estimate_rem(~teacher.sender + teacher.receiver,
                          data=classroom.data)
summary(exact.rem.teacher) #summary of the model
AIC(exact.rem.teacher) #The model AIC: 4946.678

#fitting an interval timing REM with endogenous effects
exact.rem.full <-estimate_rem(~teacher.sender + 
                                teacher.receiver +
                                repetition + 
                                reciprocity +
                                sender.outdegree + 
                                receiver.indegree + 
                                outgoing.two.paths +
                                incoming.two.paths,
                              data=classroom.data)
summary(exact.rem.full)#summary of the model
AIC(exact.rem.full) #The model AIC: 4812.994 (doing better!)
summary(dream::gof_rem(exact.rem.full)) #the proportion of correctly specified sender is: 0.17656 (could be doing better!)
plot(exact.rem.full) #this will plot the standardized deviance residuals
coef(exact.rem.full) #these are in log hazard rates
hazards <- predict(exact.rem.full)
#baseline is the baseline hazard rate impacting all event rates: -3.531681
#the baseline expected dyadic weighting time is: 1/exp(-3.531681)
#the baseline expected weighting time until the next dyadic event is: 1/(20*19*exp(-3.531681)) = 0.089951


#we can check the piece-wise hazard rate assumption by looking at the distribution of the 
#normalized weighting times, they should follow an exponential distribution
data.hazards <- classroom.data$processed_sequence
interevent.times <- data.frame(id = unique(classroom.data$processed_sequence$time),
                               times=classroom.data$interevent_times)
data.hazards$interevent.times <-  interevent.times$times[match(data.hazards$time,interevent.times$id)]
data.hazards$hazard.rates <- hazards
hist(data.hazards$hazard.rates*data.hazards$interevent.times)
interval_hazards <- data.hazards %>%
  group_by(time) %>%
  summarise(total_rate = sum(hazard.rates), interevent.time = mean(interevent.times), .groups = "drop") %>%
  arrange(time) %>%
  mutate(waititing.time = total_rate*interevent.time)
hist(interval_hazards$waititing.time,breaks = 40,probability = TRUE,
     main = "Normalized Weighting Times",xlab = "Normalized Weighting Times")
curve(dexp(x, rate = 1),add = TRUE,lwd = 2,col = "blue")


#Using the remverse packages to estimate the fitted model
classroom.reh <- remify(edgelist = Class[1:(nrow(Class)-1),], model = "tie", directed = TRUE) #processing the event sequence
classroom.stats <- remstats(reh = classroom.reh, tie_effects = ~ inertia(scaling="none") + reciprocity(scaling="none") + otp(scaling = "none") + 
                              itp(scaling="none")) #adding the endogenous statistics
classroom.exact <- remstimate(reh = classroom.reh, stats = classroom.stats) #estimating the interval timing model
summary(classroom.exact) #the summary of the model
#the model diagnostics
diagnostics(classroom.exact,reh = classroom.reh, stats = classroom.stats)
plot(diagnostics(classroom.exact,reh = classroom.reh, stats = classroom.stats))

###---------------------------------------------------------------------------------------###
# Final Step: Export all of the results
###---------------------------------------------------------------------------------------###
setwd("~/Dropbox/Guest_Lecture_Slides_SOC_527/rem") # CHANGE TO MATCH YOUR WORKING DIRECTORY
save.image(file = "soc527_introduction_to_rems_111726.RData") #all of the results


# that is the end of the code script, if you have any questions about anything please feel free to 
# contact me at kacarson@arizona.edu

# Cheers!  (: 

