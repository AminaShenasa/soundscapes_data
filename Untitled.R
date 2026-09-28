# new script for final three way interaction model

# load packages
library(emmeans)
library(ggplot2)
library(patchwork)
library(scales)

# model
m_lme_pca <- lme(value ~ tide_temp_pc1 * elevation * TYPE,
                 random = ~ 1 | site,
                 correlation = corAR1(form = ~ hour_continuous | site),
                 data = combined_df_temp)

summary(m_lme_pca)
VarCorr(m_lme_pca)
Anova(m_lme_pca, type = "III")

# emmeans


# Use the observed range of the PC1 predictor rather than 0-10 tide ft
pc_range <- range(combined_df_temp$tide_temp_pc1, na.rm = TRUE)
pc_vals  <- seq(pc_range[1], pc_range[2], length.out = 30)

elev_vals <- c(-13.598, 2.924, 19.446, 35.967, 52.489)

rg <- ref_grid(m_lme_pca,
               at = list(tide_temp_pc1 = pc_vals,
                         elevation     = elev_vals))

preds <- as.data.frame(emmeans(rg, ~ tide_temp_pc1 * elevation * TYPE))

preds$elevation_lab <- factor(preds$elevation,
                              levels = elev_vals,
                              labels = paste0("Elevation: ", round(elev_vals, 1)))

p_emmeans <- ggplot(preds, aes(x = tide_temp_pc1, y = emmean,
                               color = TYPE, fill = TYPE)) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~ elevation_lab, nrow = 1) +
  scale_color_manual(name = "Habitat Type", values = hab_cols, labels = hab_labs) +
  scale_fill_manual(name = "Habitat Type", values = hab_cols, labels = hab_labs) +
  labs(x = "Tide/Temperature PC1", y = "Predicted value") +
  theme_urchin()

p_emmeans
ggsave("emmeans_tidetemp_type_elevation.png", p_emmeans,
       width = 12, height = 5, dpi = 600)


# 1. Model predictions
elev_vals <- c(-13.598, 2.924, 19.446, 35.967, 52.489)
elev_rng  <- range(elev_vals)

pc_range <- range(combined_df_temp$tide_temp_pc1, na.rm = TRUE)
pc_vals  <- seq(pc_range[1], pc_range[2], length.out = 30)

rg <- ref_grid(m_lme_pca,
               at = list(tide_temp_pc1 = pc_vals,
                         elevation     = elev_vals))

preds <- as.data.frame(emmeans(rg, ~ tide_temp_pc1 * elevation * TYPE))

preds$elevation_lab <- factor(preds$elevation,
                              levels = elev_vals,
                              labels = paste0("Sun: ", round(elev_vals, 1), "\u00B0"))


# 2. Main emmeans plot

p_emmeans <- ggplot(preds, aes(x = tide_temp_pc1, y = emmean,
                               color = TYPE, fill = TYPE)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~ elevation_lab, nrow = 1) +
  scale_x_continuous(
    breaks = c(-2, 0, 2),
    labels = c("-2\nCool", "0", "2\nWarm")
  ) +
  scale_color_manual(name = "Habitat Type", values = hab_cols, labels = hab_labs) +
  scale_fill_manual(name = "Habitat Type", values = hab_cols, labels = hab_labs) +
labs(x = "Tide/Temperature PC1<br><span style='font-size:10pt; color:grey40'>(high tide, cool \u2192 low tide, warm)</span>",
     y = "Predicted sea urchin activity PC1<br><span style='font-size:10pt; color:grey40'>(low noise \u2192 high noise)</span>") +
  theme_urchin() +
  theme(panel.spacing.x = unit(0.6, "lines"),
        axis.title.x = element_markdown(size = 12, lineheight = 1.2, margin = margin(t = 8)),
        axis.title.y = element_markdown(size = 12, lineheight = 1.2, margin = margin(r = 8)))
# 3. Solar-elevation gradient arrow

sun_pal <- gradient_n_pal(
  colours = c("#050B24", "#1B2A6B", "#4B3F9E", "#E8604C",
              "#F7A93B", "#FFD84D", "#FFF3B0"),
  values = rescale(c(-13.6, -12, -6, 0, 10, 30, 52.5), from = elev_rng)
)
elev_to_col <- function(e) {
  sun_pal(rescale(pmin(pmax(e, elev_rng[1]), elev_rng[2]), from = elev_rng))
}

elev_to_x <- function(e) 0.1 + (e - elev_rng[1]) / diff(elev_rng) * 0.8

n <- 300
arrow_df <- data.frame(x = seq(0.01, 0.95, length.out = n))
arrow_df$elev <- elev_rng[1] + (arrow_df$x - 0.1) / 0.8 * diff(elev_rng)
arrow_df$col  <- elev_to_col(arrow_df$elev)
w <- diff(arrow_df$x[1:2])

head_df <- data.frame(x = c(0.95, 0.95, 1), y = c(0.1, 0.9, 0.5))

dots_df <- data.frame(x = elev_to_x(elev_vals), col = elev_to_col(elev_vals))

tick_df <- data.frame(elev = 0, lab = "Sunrise/Sunset")
tick_df$x <- elev_to_x(tick_df$elev)

p_arrow <- ggplot() +
  geom_rect(data = arrow_df,
            aes(xmin = x, xmax = x + w * 1.05, ymin = 0.3, ymax = 0.7, fill = col)) +
  geom_polygon(data = head_df, aes(x, y), fill = elev_to_col(elev_rng[2])) +
  geom_segment(data = tick_df, aes(x = x, xend = x, y = 0.3, yend = 0.7),
               color = "white", linewidth = 0.6) +
  geom_text(data = tick_df, aes(x = x, y = 0.12, label = lab), size = 2.8) +
  geom_point(data = dots_df, aes(x, 0.5, fill = col),
             shape = 21, size = 4, color = "white", stroke = 1) +
  annotate("text", x = 0.1, y = 0.95, label = "Solar midnight",
           size = 3.5, fontface = "bold", vjust = 0) +
  annotate("text", x = 0.9, y = 0.95, label = "Solar noon",
           size = 3.5, fontface = "bold", vjust = 0) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.4), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(8, 5.5, 0, 30))


# 4. Combine and save

p_final <- p_arrow / p_emmeans +
  plot_layout(heights = c(0.16, 1))

p_final

ggsave("emmeans_tidetemp_type_elevation.png", p_final,
       width = 12, height = 6, dpi = 600)

##emtrends 

elev_vals <- c(-13.598, 2.924, 19.446, 35.967, 52.489)

# Slope of tide_temp_pc1 for each habitat at each solar elevation
trends <- emtrends(m_lme_pca,
                   ~ TYPE | elevation,
                   var = "tide_temp_pc1",
                   at  = list(elevation = elev_vals))

# Slopes with 95% CIs
summary(trends, infer = c(TRUE, TRUE))

# Is each slope different from zero? (t-tests are included above via infer)
# Do barren and forest slopes differ at each elevation?
pairs(trends)                      # forest - barren, within each elevation
summary(pairs(trends), infer = c(TRUE, TRUE))



# Slope of solar elevation for each habitat at cool, average, and warm conditions
elev_trends <- emtrends(m_lme_pca,
                        ~ TYPE | tide_temp_pc1,
                        var = "elevation",
                        at  = list(tide_temp_pc1 = c(-2, 0, 2)))

# Slopes with 95% CIs and tests against zero
summary(elev_trends, infer = c(TRUE, TRUE))

# Do barren and forest elevation slopes differ at each PC1 value?
summary(pairs(elev_trends), infer = c(TRUE, TRUE))




# 1. Model predictions: elevation on x, tide/temp PC1 as panels

pc_vals   <- c(-2, -1, 0, 1, 2)        # or use quantiles of your data (see notes)
elev_vals <- seq(-13.598, 52.489, length.out = 30)

rg <- ref_grid(m_lme_pca,
               at = list(tide_temp_pc1 = pc_vals,
                         elevation     = elev_vals))

preds <- as.data.frame(emmeans(rg, ~ elevation * tide_temp_pc1 * TYPE))

pc_labs <- c("-2" = "Cool, high tide\n(PC1 = -2)",
             "-1" = "PC1 = -1",
             "0"  = "PC1 = 0",
             "1"  = "PC1 = 1",
             "2"  = "Warm, low tide\n(PC1 = 2)")
preds$pc_lab <- factor(as.character(preds$tide_temp_pc1),
                       levels = names(pc_labs), labels = pc_labs)


# 2. Main plot

p_emmeans_swap <- ggplot(preds, aes(x = elevation, y = emmean,
                                    color = TYPE, fill = TYPE)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey60") +  # horizon
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~ pc_lab, nrow = 1) +
  scale_x_continuous(breaks = c(-12, 0, 20, 40, 52)) +
  scale_color_manual(name = "Habitat Type", values = hab_cols, labels = hab_labs) +
  scale_fill_manual(name = "Habitat Type", values = hab_cols, labels = hab_labs) +
  labs(x = "Solar elevation (\u00B0)<br><span style='font-size:10pt; color:grey40'>(sun below horizon \u2192 solar noon)</span>",
       y = "Predicted sea urchin activity PC1<br><span style='font-size:10pt; color:grey40'>(low noise \u2192 high noise)</span>") +
  theme_urchin() +
  theme(panel.spacing.x = unit(0.6, "lines"),
        axis.title.x = element_markdown(size = 12, lineheight = 1.2, margin = margin(t = 8)),
        axis.title.y = element_markdown(size = 12, lineheight = 1.2, margin = margin(r = 8)))


# 3. Gradient arrow above panels: tide/temp PC1 (cool/high tide -> warm/low tide)

arrow_df <- data.frame(x = seq(0.01, 0.95, length.out = 300))
arrow_df$col <- gradient_n_pal(c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"))(
  rescale(arrow_df$x, from = c(0.1, 0.9)) |> pmin(1) |> pmax(0))
w <- diff(arrow_df$x[1:2])
head_df <- data.frame(x = c(0.95, 0.95, 1), y = c(0.1, 0.9, 0.5))
dots_df <- data.frame(x = seq(0.1, 0.9, length.out = 5))
dots_df$col <- gradient_n_pal(c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"))(
  seq(0, 1, length.out = 5))

p_arrow <- ggplot() +
  geom_rect(data = arrow_df,
            aes(xmin = x, xmax = x + w * 1.05, ymin = 0.3, ymax = 0.7, fill = col)) +
  geom_polygon(data = head_df, aes(x, y), fill = "#D7191C") +
  geom_point(data = dots_df, aes(x, 0.5, fill = col),
             shape = 21, size = 4, color = "white", stroke = 1) +
  annotate("text", x = 0.1, y = 0.95, label = "Cool, high tide",
           size = 3.5, fontface = "bold", vjust = 0) +
  annotate("text", x = 0.9, y = 0.95, label = "Warm, low tide",
           size = 3.5, fontface = "bold", vjust = 0) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.4), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(8, 5.5, 0, 30))


# 4. Combine and save

p_final_swap <- p_arrow / p_emmeans_swap +
  plot_layout(heights = c(0.16, 1))

p_final_swap

ggsave("emmeans_elevation_by_tidetemp.png", p_final_swap,
      width = 12, height = 6, dpi = 600, device = ragg::agg_png)

library(emmeans)
library(ggplot2)
library(ggtext)

# ------------------------------------------------------------
# 1. Model predictions (same grid as before, no faceting this time)
# ------------------------------------------------------------
elev_vals <- c(-13.598, 2.924, 19.446, 35.967, 52.489)
pc_range  <- range(combined_df_temp$tide_temp_pc1, na.rm = TRUE)
pc_vals   <- seq(pc_range[1], pc_range[2], length.out = 30)

rg <- ref_grid(m_lme_pca,
               at = list(tide_temp_pc1 = pc_vals,
                         elevation     = elev_vals))

preds <- as.data.frame(emmeans(rg, ~ tide_temp_pc1 * elevation * TYPE))

preds$elev_lab <- factor(preds$elevation, levels = elev_vals,
                         labels = paste0(round(elev_vals, 1), "\u00B0"))

# labels to place at the right end of each line
end_labs <- preds[preds$tide_temp_pc1 == max(preds$tide_temp_pc1), ]

# ------------------------------------------------------------
# 2. Single-panel plot: all 10 slopes together
#    - color = habitat
#    - linewidth/alpha = solar elevation (darker/thicker = higher sun)
# ------------------------------------------------------------
p_all_slopes <- ggplot(preds, aes(x = tide_temp_pc1, y = emmean,
                                  color = TYPE, group = interaction(TYPE, elev_lab))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(aes(alpha = elevation, linewidth = elevation)) +
  geom_text(data = end_labs,
            aes(label = elev_lab), hjust = -0.1, size = 3,
            show.legend = FALSE, fontface = "bold") +
  scale_color_manual(name = "Habitat Type", values = hab_cols, labels = hab_labs) +
  scale_alpha_continuous(range = c(0.35, 1), guide = "none") +
  scale_linewidth_continuous(range = c(0.5, 1.3), guide = "none") +
  scale_x_continuous(breaks = c(-2, 0, 2),
                     labels = c("-2\nCool", "0", "2\nWarm"),
                     expand = expansion(mult = c(0.02, 0.12))) +   # room for end labels
  labs(x = "Tide/Temperature PC1<br><span style='font-size:10pt; color:grey40'>(high tide, cool \u2192 low tide, warm)</span>",
       y = "Predicted sea urchin activity PC1<br><span style='font-size:10pt; color:grey40'>(low noise \u2192 high noise)</span>",
       caption = "Line shade/thickness = solar elevation (lighter, thinner = lower sun; darker, thicker = higher sun).\nLabels at right show solar elevation for each line.") +
  theme_urchin() +
  theme(axis.title.x = element_markdown(size = 12, lineheight = 1.2, margin = margin(t = 8)),
        axis.title.y = element_markdown(size = 12, lineheight = 1.2, margin = margin(r = 8)),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 10)))

p_all_slopes

ggsave("emmeans_all_slopes_single_panel.png", p_all_slopes,
       width = 9, height = 6, dpi = 600, device = ragg::agg_png)


library(emmeans)
library(ggplot2)
library(ggtext)
library(scales)
library(ggrepel)

# ------------------------------------------------------------
# 1. Model predictions
# ------------------------------------------------------------
elev_vals <- c(-13.598, 2.924, 19.446, 35.967, 52.489)
pc_range  <- range(combined_df_temp$tide_temp_pc1, na.rm = TRUE)
pc_vals   <- seq(pc_range[1], pc_range[2], length.out = 30)

rg <- ref_grid(m_lme_pca,
               at = list(tide_temp_pc1 = pc_vals,
                         elevation     = elev_vals))

preds <- as.data.frame(emmeans(rg, ~ tide_temp_pc1 * elevation * TYPE))
preds$elev_lab <- factor(preds$elevation, levels = elev_vals,
                         labels = paste0(round(elev_vals, 1), "\u00B0"))

end_labs <- preds[preds$tide_temp_pc1 == max(preds$tide_temp_pc1), ]
end_labs$habitat_lab <- hab_labs[as.character(end_labs$TYPE)]

# ------------------------------------------------------------
# 2. Sun-elevation palette (same anchors as before)
# ------------------------------------------------------------
elev_rng <- range(elev_vals)
sun_pal <- gradient_n_pal(
  colours = c("#050B24", "#1B2A6B", "#4B3F9E", "#E8604C",
              "#F7A93B", "#FFD84D", "#FFF3B0"),
  values  = rescale(c(-13.6, -12, -6, 0, 10, 30, 52.5), from = elev_rng)
)

# ------------------------------------------------------------
# 3. Single panel: color = solar elevation, linetype = habitat
# ------------------------------------------------------------
p_all_slopes <- ggplot(preds, aes(x = tide_temp_pc1, y = emmean,
                                  color = elevation,
                                  linetype = TYPE,
                                  group = interaction(TYPE, elev_lab))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1) +
  geom_text_repel(data = end_labs,
                  aes(label = habitat_lab),
                  hjust = 0, direction = "y", nudge_x = 0.3,
                  segment.size = 0.3, size = 3, fontface = "italic",
                  show.legend = FALSE, color = "grey20") +
  scale_color_gradientn(colors = sun_pal(seq(0, 1, length.out = 100)),
                        name = "Solar elevation (\u00B0)",
                        breaks = elev_vals,
                        labels = round(elev_vals, 1)) +
  scale_linetype_manual(name = "Habitat Type",
                        values = c("Barren" = "solid", "Macro_Forest" = "22"),
                        labels = hab_labs) +
  scale_x_continuous(breaks = c(-2, 0, 2),
                     labels = c("-2\nCool", "0", "2\nWarm"),
                     expand = expansion(mult = c(0.02, 0.15))) +
  labs(x = "Tide/Temperature PC1<br><span style='font-size:10pt; color:grey40'>(high tide, cool \u2192 low tide, warm)</span>",
       y = "Predicted sea urchin activity PC1<br><span style='font-size:10pt; color:grey40'>(low noise \u2192 high noise)</span>") +
  theme_urchin() +
  theme(axis.title.x = element_markdown(size = 12, lineheight = 1.2, margin = margin(t = 8)),
        axis.title.y = element_markdown(size = 12, lineheight = 1.2, margin = margin(r = 8)),
        legend.key.width = unit(1.4, "lines"))

p_all_slopes

ggsave("emmeans_all_slopes_by_elevation.png", p_all_slopes,
       width = 10, height = 6.5, dpi = 600, device = ragg::agg_png)


library(mmrm)

m_mmrm <- mmrm(
  formula = value ~ tide_temp_pc1 * elevation * TYPE +
    ar1(hour_factor | site),
  data = combined_df_temp
)

summary(m_mmrm)

setdiff(levels(combined_df$hour_factor), levels(droplevels(combined_df_temp$hour_factor)))

combined_df_temp %>%
  filter(day_index == 3) %>%
  count(site) %>%
  arrange(n)



