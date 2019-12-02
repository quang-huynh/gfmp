library(DLMtool)
library(MSEtool)
library(dplyr)
library(ggplot2)
library(gfdlm)

mp <- readr::read_csv(here::here("data", "mp.txt"), comment = "#")
mp_list <- split(mp, mp$type)

`LT P40` <- gfdlm::pm_factory("SBMSY", 0.4, c(36, 50))
`LT P80` <- gfdlm::pm_factory("SBMSY", 0.8, c(36, 50))
STY <- gfdlm::pm_factory("LTY", 0.5, c(6, 20))
LTY <- gfdlm::pm_factory("LTY", 0.5, c(36, 50))
PM <- c("LT P40", "LT P80", "STY", "LTY", "AAVY", "PNOF")

omrex_sra <- readRDS(here::here("generated-data", "rex-sra.rds"))
omrex <- omrex_sra@OM
omrex@nsim <- 48
omrex@TACSD <- c(0, 0)
omrex@interval <- 2

# rex_historical <- runMSE(omrex, Hist = TRUE, parallel = FALSE)

file_name <- here::here("generated-data", "rex-mse2.rds")
if (!file.exists(file_name)) {
  DLMtool::setup(cpus = floor(parallel::detectCores()/2))
  rex_mse <- runMSE(OM = omrex, MPs = mp$mp, parallel = TRUE)
  snowfall::sfStop()
  saveRDS(rex_mse, file = file_name)
} else {
  rex_mse <- readRDS(file_name)
}
# DLMtool::Converge(rex_mse)

rex_probs <- gfdlm::get_probs(rex_mse, PM)
gfdlm::plot_probs(rex_probs)

reference_mp <- c("NFref", "FMSYref")
rex_satisficed <- dplyr::filter(rex_probs, `LT P40` > 0.9, STY > 0.75) %>%
  arrange(-`LT P40`) %>%
  pull(MP)
rex_satisficed <- rex_satisficed[!rex_satisficed %in% reference_mp]
rex_satisficed_ref <- union(rex_satisficed, reference_mp)

rex_mse_sub <- DLMtool::Sub(rex_mse, MPs = rex_satisficed)
rex_mse_sub_ref <- DLMtool::Sub(rex_mse, MPs = rex_satisficed_ref)
rex_satisficed_df <- left_join(tibble(mp = rex_satisficed), mp, by = "mp")

g1 <- gfdlm::plot_projection_ts(rex_mse_sub_ref, type = c("SSB", "FM")) +
  coord_cartesian(expand = FALSE, ylim = c(0, 4.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4)) +
  theme(strip.text.y = element_blank())

catch <- readRDS(here::here("generated-data", "rex-catch.rds"))
g2 <- gfdlm::plot_projection_ts(rex_mse_sub_ref, type = c("C"), clip_ylim = 1.3, n_samples = 3) +
  theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank()) +
  theme(axis.title.y = element_blank()) +
  geom_hline(yintercept = mean(catch[23:23]), lty = 2, alpha = 0.4)

g <- cowplot::plot_grid(g1, g2, rel_widths = c(2, 1), align = "h")
ggsave("report/figure/rex-projections-base.png", width = 11, height = 12)

g <- gfdlm::plot_contours(rex_mse_sub_ref, xlim = c(0, 3.5),
  ylim = c(0, 3.5), alpha = c(0.1, 0.25, 0.50))
ggsave("report/figure/rex-kobe-base.png", width = 8, height = 7.5)

cols <- viridisLite::viridis(5)
names(cols) <- paste0("CC", seq(60, 100, 10))
g <- filter(mp, type == "Constant catch") %>%
  filter(grepl("^CC[0-9]+", mp)) %>%
  pull(mp) %>%
  DLMtool::Sub(rex_mse, .) %>%
  gfdlm::spider(pm_list = PM, palette = "Set2", lwd = 1.0) +
  scale_color_manual(values = cols) +
  labs(colour = "MP")
ggsave("report/figure/rex-spider-cc-base.png", width = 6, height = 6)

g <- rex_satisficed %>%
  DLMtool::Sub(rex_mse, .) %>%
  gfdlm::spider(pm_list = PM, palette = "Set2", lwd = 1.0) +
  scale_color_viridis_d() +
  # scale_color_manual(values =
  #     c(RColorBrewer::brewer.pal(length(mp_unique), "Set2"), "grey30", "grey65")) +
  labs(colour = "MP")
ggsave("report/figure/rex-spider-base.png", width = 6, height = 6)
