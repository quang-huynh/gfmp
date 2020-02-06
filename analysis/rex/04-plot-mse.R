# Must source `03-run-mse.R` first

# Tigure plots ----------------------------------------------------------------

g <- gfdlm::plot_tigure(pm_avg,
  satisficed = c("LT LRP" = LT_LRP_thresh, "STC" = STC_thresh))
.ggsave("pm-table-avg", 4.25, 6.5)
g <- gfdlm::plot_tigure(pm_min,
  satisficed = c("LT LRP" = LT_LRP_thresh, "STC" = STC_thresh),
)
.ggsave("pm-table-min", 4.25, 6.5)

g <- map(pm_df_list, dplyr::filter, MP %in% mp_sat) %>%
  set_names(scenarios_ref_human) %>%
  plot_tigure_facet(ncol = 2)
.ggsave("pm-tigures-ref-set", 7, 6.5)

g <- map(pm_df_list_rob, dplyr::filter, MP %in% mp_sat) %>%
  set_names(scenarios_rob_human) %>%
  plot_tigure_facet()
.ggsave("pm-tigures-rob-set", 7, 2.25)

# Convergence -----------------------------------------------------------------

walk(names(mse_sat_with_ref), ~ {
  g <- gfdlm::plot_convergence(mse_sat_with_ref[[.x]], PM, ylim = c(0.5, 1)) +
    scale_color_brewer(palette = "Set2")
  .ggsave(paste0("converge-", .x), 6.5, 6.5)
})

# Projections -----------------------------------------------------------------

walk(names(mse_sat_with_ref), ~ {
  g <- plot_main_projections(mse_sat_with_ref[[.x]],
    catch_breaks = catch_breaks,
    catch_labels = catch_labels)
  .ggsave(paste0("projections-satisficed-", .x), 7.5, 7.5)
})

# All not satisficed ones for "base":
g <- DLMtool::Sub(mse[[base_om]], MPs = mp_not_sat) %>%
  plot_main_projections(catch_breaks = catch_breaks,
    catch_labels = catch_labels)
.ggsave(paste0("projections-all-not-satisficed"), 7.5, 27)

# Example not satisficed ones for "base":
mp_eg_not_sat <- c(
  "CC_hist",
  "CC90",
  ".GB_slope8_0.66",
  ".Islope0.2_80",
  ".ICI2",
  ".IDX_smooth",
  ".IT5_hist",
  ".ITM_hist",
  ".SP6040_prior"
)
mp_eg_not_sat <- mp_eg_not_sat[mp_eg_not_sat %in% mp_not_sat]
g <- DLMtool::Sub(mse[[base_om]], MPs = mp_eg_not_sat) %>%
  plot_main_projections(catch_breaks = catch_breaks,
    catch_labels = catch_labels)
.ggsave(paste0("projections-eg-not-satisficed"), 8, 9.5)

# Kobe ------------------------------------------------------------------------

walk(names(mse_sat_with_ref), ~ {
  g <- gfdlm::plot_kobe(mse_sat[[.x]])
  .ggsave(paste0("kobe-", .x), 8, 7.5)
})

g <- mse_sat %>%
  set_names(scenarios_human) %>%
  gfdlm::plot_kobe_grid()
.ggsave("kobe-grid-satisficed2", 13, 7)
.ggsave("kobe-grid-satisficed", 7, 13)

# Radar plots -----------------------------------------------------------------

MPs <- union(mp_sat, reference_mp[reference_mp != "NFref"])

g <- pm_df_list %>% map(dplyr::filter, MP %in% MPs) %>%
  set_names(scenarios_ref_human) %>%
  plot_radar_facet(custom_pal = custom_pal)
.ggsave("spider-satisficed-panel-reference", 12, 11)

g <- pm_df_list_rob %>% map(dplyr::filter, MP %in% MPs) %>%
  set_names(scenarios_rob_human) %>%
  plot_radar_facet(custom_pal = custom_pal)
.ggsave("spider-satisficed-panel-robustness", 10, 5)

g <- pm_avg %>% dplyr::filter(MP %in% MPs) %>%
  plot_radar(custom_pal = custom_pal)
.ggsave("spider-satisficed-avg-reference", 6, 6)

g <- pm_min %>% dplyr::filter(MP %in% MPs) %>%
  plot_radar(custom_pal = custom_pal)
.ggsave("spider-satisficed-min-reference", 6, 6)

# Parallel coordinate plots ---------------------------------------------------

g <- pm_df_list %>% map(dplyr::filter, MP %in% MPs) %>%
  set_names(scenarios_ref_human) %>%
  gfdlm::plot_parallel_coords(type = "facet", custom_pal = custom_pal)
.ggsave("parallel-coordinates", 8, 6.6)

g <- pm_df_list_rob %>% map(dplyr::filter, MP %in% MPs) %>%
  set_names(scenarios_rob_human) %>%
  gfdlm::plot_parallel_coords(type = "facet", custom_pal = custom_pal)
.ggsave("parallel-coordinates", 8, 6.6)

g <- pm_df_list %>% map(dplyr::filter, MP %in% MPs) %>%
  gfdlm::plot_parallel_coords(type = "single", custom_pal = custom_pal)
.ggsave("parallel-coordinates-avg", 5, 3)

# Bivariate trade-off plots ---------------------------------------------------

g <- pm_df_list %>%
  map(dplyr::filter, MP %in% union(mp_sat, reference_mp[reference_mp != "NFref"])) %>%
  gfdlm::plot_tradeoff("LT LRP", "STC", custom_pal = custom_pal)
.ggsave("bivariate-trade-off-reference", 8, 6)

g <- pm_df_list_rob %>%
  map(dplyr::filter, MP %in% union(mp_sat, reference_mp[reference_mp != "NFref"])) %>%
  gfdlm::plot_tradeoff("LT LRP", "STC", custom_pal = custom_pal) +
  facet_wrap(~scenario, ncol = 2)
.ggsave("bivariate-trade-off-robustness", 6, 3)

# Psychedelic pyramid worms ---------------------------------------------------

walk(names(mse_sat_with_ref), ~{
  g <- plot_worms(mse_sat_with_ref[[.x]], this_year = this_year)
  .ggsave(paste0("neon-worms-", .x), 8, 6.6)
})

# Sensitivity plots -----------------------------------------------------------

slots <- c("D", "hs", "M", "ageM", "L50", "Linf", "K", "Isd")

g <- DLMtool::Sub(mse[[base_om]], MPs = mp_sat) %>%
  gfdlm::plot_sensitivity(`LT LRP`, slots = slots,
    ylab = expression(Mean~SSB/SSB[MSY]~"in"~years~36-50))
.ggsave("sensitivity-bbmsy-base", 12.5, 5)

g <- DLMtool::Sub(mse[[base_om]], MPs = mp_sat) %>%
  gfdlm::plot_sensitivity(`STY`, slots = slots,
    ylab = "Mean catch/reference catch in years 6-20")
.ggsave("sensitivity-yield-base", 12.5, 5)

g <- DLMtool::Sub(mse[[base_om]], MPs = mp_sat) %>%
  gfdlm::plot_sensitivity_trajectory("B_BMSY", slots = slots) +
  coord_cartesian(ylim = c(0, 4))
.ggsave("sensitivity-traj-bbmsy-base", 12.5, 5)

g <- DLMtool::Sub(mse[[base_om]], MPs = mp_sat) %>%
  gfdlm::plot_sensitivity_trajectory("F_FMSY", slots = slots) +
  coord_cartesian(ylim = c(0, 4))
.ggsave("sensitivity-traj-ffmsy-base", 12.5, 5)

# Optimize PNG files on Unix --------------------------------------------------

cores <- round(parallel::detectCores() / 2L)
files_per_core <- 5L
setwd(fig_dir)
if (!identical(.Platform$OS.type, "windows")) {
  system(paste0(
    "find -X . -name '*.png' -print0 | xargs -0 -n ",
    files_per_core, " -P ", cores, " optipng -strip all"
  ))
}
setwd(here::here())
