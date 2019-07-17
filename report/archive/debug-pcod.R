library(DLMtool)
ompc <- readRDS(here::here("sra/pcod_no_comps_om.rds"))

ompc@nsim <- 3
ompc@proyears <- 50

pcod_mse <- DLMtool::runMSE(OM = ompc, MPs = "AvC", ntrials = 1000, PPD = TRUE)
AvC(1, pcod_mse@Misc$Data[[1]], reps = 1, plot = TRUE)
AvC(2, pcod_mse@Misc$Data[[1]], reps = 1, plot = TRUE)
AvC(3, pcod_mse@Misc$Data[[1]], reps = 1, plot = TRUE)

Islope1(1, pcod_mse@Misc$Data[[1]], reps = 1, plot = TRUE)

DD4010(1, pcod_mse@Misc$Data[[1]], reps = 1, plot = TRUE)

pcod_mse@TAC[1,,]
pcod_mse@B[1,,]
pcod_mse@B_BMSY[1,,]
pcod_mse@F_FMSY[1,,]
pcod_mse@FM[1,,]

names(ompc@cpars)
ompc@cpars$D
ompc@cpars$R0
.F <- median(ompc@cpars$Find)
.M <- median(ompc@cpars$M_ageArray)

.F/(.F + .M) * (1 - exp(-.F -.M)) * 100

# ompc@cpars$Find <- ompc@cpars$Find * 20
ompc@cpars$D <- ompc@cpars$D / 5
hist(ompc@cpars$D)

.F <- median(ompc@cpars$Find)
.M <- median(ompc@cpars$M_ageArray)
.F/(.F + .M) * (1 - exp(-.F -.M)) * 100
.F
.M
image(ompc@cpars$Find)

ompc@nsim <- 25
pcod_mse <- DLMtool::runMSE(OM = ompc, MPs = "AvC", ntrials = 1000, PPD = TRUE)
AvC(1, pcod_mse@Misc$Data[[1]], reps = 1, plot = TRUE)
pcod_mse@B[1,,]
pcod_mse@TAC[1,,]
pcod_mse@FM[1,,]
pcod_mse@F_FMSY[1,,]

plot(pcod_mse)

library(ggplot2)
pcod_hist <- DLMtool::runMSE(OM = ompc, MPs = "AvC", ntrials = 1000, Hist = T)
pcod_hist@TSdata$B %>%
  reshape2::melt() %>%
  dplyr::filter(Var1 %in% sample(unique(Var1), size = 3)) %>%
  ggplot(aes(Var2, value)) +
  geom_line() +
  facet_wrap(~Var1, scales = "free_y") +
  ylab("Simulated historical biomass") +
  xlab("Year")
