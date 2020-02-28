setwd("C:/GitHub/gfmp/")

library("dplyr")
library("DLMtool")
library("MSEtool")
library("here")
library("purrr")
library("ggplot2")
library("reshape2")
library("cowplot")

library("future")
cores <- floor(future::availableCores()/2)
plan(multisession, workers = cores)

sc <- tibble::tribble(
  ~scenario, ~scenario_human, ~scenario_type,
  "ceq150", "Ceq. 150%", "Reference",
  "ceq200", "Ceq. 200%", "Reference",
  "high-m", "Higher M", "Reference",
  "high-h", "Higher steepness", "Reference",
  "sel1", "Lower selectivity", "Reference",
  # "oregon", "Oregon growth", "Reference",
  "no-cpue", "No CPUE Ceq. 250%", "Reference",
  "no-cpue-light", "No CPUE Ceq. 50%", "Reference",
  "inc-m", "M inc.", "Robustness",
  "ceq250", "Ceq. 250%", "Test",
  "ceq300", "Ceq. 300%", "Test"
)
sc <- mutate(sc, order = seq_len(n()))
saveRDS(sc, file = here("generated-data/rex-scenarios.rds"))

sra_rex <- purrr::map(sc$scenario, ~ {
  readRDS(here("generated-data", paste0("rex-sra-", .x, ".rds")))
})

names(sra_rex) <- sc$scenario
ceqsc <- c(1,2,9:10) #only want the 4 ceq scenarios with cpue

#Keep this code so I know how to query outputs
#slotNames(sra_rex[[1]])
#slotNames(sra_rex[[1]]@OM)

nSim <- sra_rex[[1]]@OM@nsim

#This is also a list
ceq150misc <- sra_rex[[1]]@Misc

#names(ceq150misc[[1]])
#ceq150misc[[1]]$q[1] #WCVI survey q
#ceq150misc[[2]]$q[1] #WCVI survey q

#Put all the survey q estimates into a dataframe
# ncol= number of scenarios nsc
# nrow = number of replicates (nsim) ... this varies by scenario

# Get a list of all the nsim values (different for each scenario)
nsc <- length(sra_rex)
nSims <- NULL
for(i in 1:nsc) nSims <- c(nSims,sra_rex[[i]]@OM@nsim)

####SURVEY q
q.all <- data.frame(matrix(ncol=nsc, nrow=max(nSims)))
colnames(q.all) <- sc$scenario
qmean <- data.frame(matrix(nrow=nsc, ncol=2))
qmean[,1] <- sc$scenario
colnames(qmean) <- c("Scenario", "Survey_q")

for(i in 1:length(sra_rex)) {
  sramisc <- sra_rex[[i]]@Misc  #get the misc object from sra scenario

  qsc <- sramisc %>%  #get q from all the replicates (returns 2, only want the 1st)
    purrr::map("q")

  if(!(i %in% 6:7)){
    qvec <- data.frame(matrix(unlist(qsc), nrow=length(qsc), byrow=T)) %>%
      dplyr::select(X1) %>%  #turn list into df Only take first col (WCVI survey)
      unlist() %>%
      as.vector()
  }

  if(i %in% 6:7){
    qvec <- data.frame(matrix(unlist(qsc), nrow=length(qsc), byrow=T)) %>%
      unlist() %>%
      as.vector()
  }

  # Add some NAs so can fill the dataframe
  nNA <- max(nSims)-nSims[i]
    if(nNA > 0) qvec <- c(qvec,rep(NA, nNA))
  q.all[,i] <- qvec #Add to dataframe
  qmean[i,2] <- sra_rex[[i]]@mean_fit$report$q[1] #Also get mean fitted value

}

#### R0
R0.all <- data.frame(matrix(ncol=nsc, nrow=max(nSims)))
colnames(R0.all) <- sc$scenario
R0mean <- data.frame(matrix(nrow=nsc, ncol=2))
R0mean[,1] <- sc$scenario
colnames(R0mean) <- c("Scenario", "R0")

for(i in 1:length(sra_rex)) {
  sramisc <- sra_rex[[i]]@Misc  #get the misc object from sra scenario

  R0sc <- sramisc %>%  #get R0 from all the replicates (returns 2, only want the 1st)
    purrr::map("R0")

   R0vec <- data.frame(matrix(unlist(R0sc), nrow=length(R0sc), byrow=T)) %>%
      unlist() %>%
      as.vector()

  # Add some NAs so can fill the dataframe
  nNA <- max(nSims)-nSims[i]
  if(nNA > 0) R0vec <- c(R0vec,rep(NA, nNA))
  R0.all[,i] <- R0vec #Add to dataframe
  R0mean[i,2] <- sra_rex[[i]]@mean_fit$report$R0 #Also get mean fitted value

}

#### B1
B1.all <- data.frame(matrix(ncol=nsc, nrow=max(nSims)))
colnames(B1.all) <- sc$scenario
B1mean <- data.frame(matrix(nrow=nsc, ncol=2))
B1mean[,1] <- sc$scenario
colnames(B1mean) <- c("Scenario", "B1")

for(i in 1:length(sra_rex)) {
  sramisc <- sra_rex[[i]]@Misc  #get the misc object from sra scenario

  B1sc <- sramisc %>%  #get Biomass from all the replicates (returns 2, only want the 1st)
    purrr::map("B") %>%
    map(~ .x[[1]]) #Get the first year of the hist period

  B1vec <- data.frame(matrix(unlist(B1sc), nrow=length(B1sc), byrow=T)) %>%
    unlist() %>%
    as.vector()

  # Add some NAs so can fill the dataframe
  nNA <- max(nSims)-nSims[i]
  if(nNA > 0) B1vec <- c(B1vec,rep(NA, nNA))
  B1.all[,i] <- B1vec #Add to dataframe

  B1mean[i,2] <- sra_rex[[i]]@mean_fit$report$B[1] #Also get mean fitted value

}

#### Steepness
h.all <- data.frame(matrix(ncol=nsc, nrow=max(nSims)))
colnames(h.all) <- sc$scenario
hmean <- data.frame(matrix(nrow=nsc, ncol=2))
hmean[,1] <- sc$scenario
colnames(hmean) <- c("Scenario", "Steepness")

for(i in 1:length(sra_rex)) {
  sramisc <- sra_rex[[i]]@Misc  #get the misc object from sra scenario

  hsc <- sramisc %>%  #get h from all the replicates (returns 2, only want the 1st)
    purrr::map("h")

  hvec <- data.frame(matrix(unlist(hsc), nrow=length(hsc), byrow=T)) %>%
    unlist() %>%
    as.vector()

  # Add some NAs so can fill the dataframe
  nNA <- max(nSims)-nSims[i]
  if(nNA > 0) hvec <- c(hvec,rep(NA, nNA))
  h.all[,i] <- hvec #Add to dataframe
  hmean[i,2] <- sra_rex[[i]]@mean_fit$report$h #Also get mean fitted value

}

#### F_equilibrium
Fe.all <- data.frame(matrix(ncol=nsc, nrow=max(nSims)))
colnames(Fe.all) <- sc$scenario
Femean <- data.frame(matrix(nrow=nsc, ncol=2))
Femean[,1] <- sc$scenario
colnames(Femean) <- c("Scenario", "F_equilibrium")

for(i in 1:length(sra_rex)) {
  sramisc <- sra_rex[[i]]@Misc  #get the misc object from sra scenario

  Fesc <- sramisc %>%  #get log_f_eq from all the replicates (returns 2, only want the 1st)
    purrr::map("log_F_equilibrium")

  Fevec <- data.frame(matrix(unlist(Fesc), nrow=length(Fesc), byrow=T)) %>%
    unlist() %>%
    as.vector()

  # Add some NAs so can fill the dataframe
  nNA <- max(nSims)-nSims[i]
  if(nNA > 0) Fevec <- c(Fevec,rep(NA, nNA))
  Fe.all[,i] <- Fevec #Add to dataframe
  Femean[i,2] <- sra_rex[[i]]@mean_fit$report$F_equilibrium #Also get mean fitted value

}


# Now plot
qmean <- qmean[ceqsc,]
R0mean <- R0mean[ceqsc,]
B1mean <- B1mean[ceqsc,]
hmean <- hmean[ceqsc,]
Femean <- Femean[ceqsc,]

qbox <- q.all %>%
  dplyr::select(all_of(ceqsc)) %>%
  melt() %>%
  rename("Scenario"=variable, "Survey_q"=value) %>%
  #dplyr::filter(!is.na(Survey_q)) %>%
  ggplot() +
    geom_boxplot(aes(x=Scenario, y=Survey_q), na.rm=TRUE)+
    geom_point(data=qmean, aes(x=Scenario, y=Survey_q), colour="red", fill="red", size=2.5)
print(qbox)

#R0
p <- R0.all %>%
  dplyr::select(all_of(ceqsc)) %>%
  melt() %>%
  rename("Scenario"=variable, "R0"=value) %>%
  dplyr::filter(!is.na(R0)) %>%
  mutate(logR0=log(R0))

R0box <-  ggplot(p) +
  geom_boxplot(aes(x=Scenario, y=logR0), na.rm=TRUE)+
  geom_point(data=R0mean, aes(x=Scenario, y=log(R0)), colour="red", fill="red", size=2.5)+
  coord_cartesian(ylim = quantile(p$logR0, c(0.025, 0.975)))
print(R0box)

#B1
p <- B1.all %>%
  dplyr::select(all_of(ceqsc)) %>%
  melt() %>%
  rename("Scenario"=variable, "B1"=value) %>%
  dplyr::filter(!is.na(B1)) %>%
  mutate(logB1=log(B1))

B1box <-  ggplot(p) +
  geom_boxplot(aes(x=Scenario, y=logB1), na.rm=TRUE)+
  geom_point(data=B1mean, aes(x=Scenario, y=log(B1)), colour="red", fill="red", size=2.5)+
  coord_cartesian(ylim = quantile(p$logB1, c(0.025, 0.975)))
print(B1box)

## Steepness
hbox <- h.all %>%
  dplyr::select(all_of(ceqsc)) %>%
  melt() %>%
  rename("Scenario"=variable, "Steepness"=value) %>%
  #dplyr::filter(!is.na(Survey_h)) %>%
  ggplot() +
  geom_boxplot(aes(x=Scenario, y=Steepness), na.rm=TRUE)+
  geom_point(data=hmean, aes(x=Scenario, y=Steepness), colour="red", fill="red", size=2.5)
print(hbox)

## F_equilibrium
p <- Fe.all %>%
  dplyr::select(all_of(ceqsc)) %>%
  melt() %>%
  rename("Scenario"=variable, "log_F_equilibrium"=value) %>%
  dplyr::filter(!is.na(log_F_equilibrium)) %>%
  mutate(F_equilibrium=exp(log_F_equilibrium))

Febox<-  ggplot(p) +
        geom_boxplot(aes(x=Scenario, y=F_equilibrium), na.rm=TRUE)+
        geom_point(data=Femean, aes(x=Scenario, y=F_equilibrium), colour="red", fill="red", size=2.5)
print(Febox)

#Plot on a grid
cowplot::plot_grid(qbox,B1box, R0box,Febox)

#Look at correlation coefficients between key parameters
corr <- list()
corr_derived_par <- list()

corr_R0_Feq <- corr_R0_q <- vector(length=length(ceqsc))

for(i in 1:length(ceqsc)) {
  scen <- ceqsc[i]

  print(scen)

  corr[[i]] <- cov2cor(sra_rex[[scen]]@mean_fit$SD$cov.fixed) %>% round(3) %>%
    structure(dimnames = list(names(sra_rex[[scen]]@mean_fit$SD$par.fixed),
                              names(sra_rex[[scen]]@mean_fit$SD$par.fixed)))

  corr_R0_Feq[i] <- corr[[i]]["log_R0","log_F_equilibrium"]

  SD_full <- TMB::sdreport(sra_rex[[scen]]@mean_fit$obj, sra_rex[[scen]]@mean_fit$opt$par,
                           getJointPrecision = TRUE) # Get correlation of derived parameters, by default joint precision = FALSE

  # Correlation between R0 and q = 0.69 (steepness is fixed so corr = 0)
  corr_derived_par[[i]] <- cov2cor(SD_full$cov) %>% round(3) %>%
    structure(dimnames = list(names(SD_full$value),
                              names(SD_full$value)))

  corr_R0_q[i] <- corr_derived_par[[i]][1,3]

}

corr_R0_Feq
corr_R0_q

corr <- data.frame(cbind(sc$scenario[ceqsc],corr_R0_Feq,corr_R0_q)) %>%
  rename(Scenario=V1,"R0 vs Feq"=corr_R0_Feq, "R0 vs Survey q" = corr_R0_q) %>%
  melt(id.vars="Scenario") %>%
  rename(Parameters=variable, r=value) %>%
  ggplot(aes(x=Scenario, y=r, group=Parameters)) +
  geom_point(aes(colour=Parameters), size=4)+
  ylab("Correlation between SRA parameters")+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  scale_color_manual(values=c('red','blue'))
corr
