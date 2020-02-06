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

d <- pm_avg %>% inner_join(rename(mp, MP = mp), by = "MP") %>%
  split(.$type) %>%
  map(select, -type)
g <- d %>% map(plot_radar)
g <- cowplot::plot_grid(plotlist = g, ncol = 2, labels = names(d),
  hjust = 0, label_size = 11, align = "hv")
.ggsave("spider-all-avg-reference", 8.5, 8.5)

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

# FIXME: pull this into package:
d <- pm_avg %>% inner_join(rename(mp, MP = mp), by = "MP") %>%
  split(.$type) %>%
  map(select, -type)
g <- names(d) %>% map(~{
  gfdlm::plot_parallel_coords(d[.x], type = "single", rotate_labels = TRUE) +
    theme(strip.text = element_text(face = "bold", size = 11)) +
    guides(lty = FALSE, fill = FALSE) +
    scale_color_brewer(palette = "Set2") +
    coord_cartesian(ylim = c(-0.01, 1.01), expand = FALSE) +
    theme(plot.margin = grid::unit(c(1, .5, .5, .5), "lines")) +
    theme(
      panel.grid.major.y = element_line(colour = "grey85"),
      panel.grid.major.x = element_line(colour = "grey85"),
      panel.grid.minor.y = element_line(colour = "grey96")
    )
})
g2 <- cowplot::plot_grid(plotlist = g, ncol = 2, labels = names(d),
  hjust = 0, label_size = 11, vjust = 1, align="hv") +
  theme(plot.margin = grid::unit(c(1, 0, 1, 1), "lines"))
.ggsave("parallel-coordinates-all-avg-reference", 8.5, 8.5)

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

MPs <- union(mp_sat, reference_mp[reference_mp != "NFref"])
d <- purrr::map(scenarios_ref, ~ DLMtool::Sub(mse[[.x]], MPs = MPs)) %>%
  set_names(scenarios_ref_human)

g <- d %>% plot_worms_grid(this_year = this_year, include_historical = FALSE) +
  coord_fixed(xlim = c(0, 2.5), ylim = c(0, 2), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 1, 2)) +
  scale_y_continuous(breaks = c(0, 1))
.ggsave("neon-worms-projection", 10, 8.5)

g <- d %>% plot_worms_grid(this_year = this_year, include_historical = TRUE) +
  coord_fixed(xlim = c(0, 3), ylim = c(0, 3), expand = FALSE)
.ggsave("neon-worms-all", 10, 8.5)

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
