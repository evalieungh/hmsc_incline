##############################################
#  pairwise trait differences versus omegas  #
##############################################

# script by Eva Lieungh
# started 2023-02-08

# read in data
setwd("C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/")

data <- read.csv("data_processed/fig3_omegas_traits_data.csv")

trait_diff_vector =  c("height_mm_diff",
                       "fresh_mass_g_diff",
                       "dry_mass_g_diff",
                       "leaf_area_cm2_diff",
                       "SLA_cm2_g_diff",
                       "LDMC_g_g_diff",
                       "leaf_thickness_diff",
                       "N_percent_diff",
                       "C_percent_diff",
                       "CN_ratio_diff")

# basic scatterplots between omegas and pairwise trait differences
{
  pdf(file = "results/figures/omegas_vs_traitdiffs.pdf",
      width = 7, # inches
      height = 15)
  par(mfrow = c(5, 2))
  for (trait in 1:length(trait_diff_vector)) {
    print(paste(trait, ":", trait_diff_vector[trait]))
    plot(data$omega ~ abs(data[, trait_diff_vector[trait]]),
         ylab = "omega",
         xlab = trait_diff_vector[trait])
  }
  dev.off()
}

# check for patterns with simple linear models
  # use absolute values of differences 
  # (because each species pair only occurs once per site, 
  # and it is random/alphabetic which species' value is 
  # subtracted from the other)
{ 
  # save directly as pdf
  pdf(file = "results/figures/omegas_vs_traitdiffs_lm.pdf",
      width = 10,
      height = 40)
  # each trait on a row
  par(mfrow = c(10, 4))
  for (trait in 1:length(trait_diff_vector)) {
    print(paste(trait, ":", trait_diff_vector[trait]))
    # make simple lm
    m = lm(data$omega ~ abs(data[, trait_diff_vector[trait]]))
    # print summary (estimates, p-value) to console
    print(summary(m))
    # make diagnostic plots (4 per model)
    plot(m,
         main = trait_diff_vector[trait])
    # add intercept and slope estimates as text below last plot
    mtext(
      paste(
        "intercept :",
        round(m$coefficients[1], 3),
        "\n",
        trait_diff_vector[trait],
        ":",
        round(m$coefficients[2], 3)
      ),
      side = 1, # position 1 = bottom
      line = 5, # lines from plot border
      cex = 0.6
    )
  }
  dev.off()
}
# no clear patterns, 
# no trait difference significantly explains co-occurrences.


# repeat plotting for separate sites
sites = unique(data$siteID)
{ 
  # save printed output to text file
  sink(file = "results/omega_traitdiff_comparison.txt")
  for (site in sites) {
    print(site)
    print(paste("mean omega:",
                mean(subset$omega)))
    # create subset for the current site
    subset = subset(data, data$siteID == site)
    # plot directly to pdf
    pdf(
      file = paste("results/figures/omegas_vs_traitdiffs_",
                   site,
                   ".pdf",
                   sep = ""),
      width = 7,
      height = 15
    )
    par(mfrow = c(5, 2))
    for (trait in 1:length(trait_diff_vector)) {
      print(paste("---", trait_diff_vector[trait], "---"))
      plot(subset$omega ~ abs(subset[, trait_diff_vector[trait]]),
           ylab = "co-occurrence estimate (-1,1)",
           xlab = trait_diff_vector[trait])
      traitcolumn = paste("spA_",
                          # remove last 5 characters ("_diff")
                          gsub('.{5}$', '', trait_diff_vector[trait]),
                          sep = "")
      print(paste("mean trait value:",
                  mean(na.omit(subset[, traitcolumn]))))
      trait_diff_column = paste(trait_diff_vector[trait],
                                sep = "")
      print(paste("mean trait difference:",
                  mean(abs(na.omit(subset[, trait_diff_column])))))
    }
    dev.off()
  }
  sink()
}

# still no obvious patterns, and seemingly small differences between sites.

# make simple plots with the mean omega, trait values, and differences

mean_df <- data.frame(
  site = c("Skjellingahaugen", "Gudmedalen", "Lavisdalen", "Ulvehaugen"),
  precipitation_level = c(4, 3, 2, 1),
  mean_omega = c(-0.007, 0.015, 0.028, -0.005),
  height_mm = c(128.34, 186.23, 126.27, 168.03),
  fresh_mass_g = c(0.088, 0.159, 0.123, 0.071),
  dry_mass_g = c(0.020, 0.035, 0.022, 0.020),
  leaf_area_cm2 = c(3.646, 6.776, 4.830, 3.186),
  SLA_cm2_g = c(260.04, 245.12, 297.16, 265.64),
  LDMC_g_g = c(0.331, 0.366, 0.288, 0.329),
  leaf_thickness = c(0.364, 0.329, 0.301, 0.290),
  N_percent = c(1.962, 2.154, 3.007, 2.240),
  C_percent = c(47.792, 48.829, 49.934, 48.128),
  CN_ratio = c(34.165, 33.076, 23.634, 31.991),
  height_mm_diff = c(0.76, 0.63, 0.70, 0.56),
  fresh_mass_g_diff = c(1.20, 1.38, 1.32, 1.08),
  dry_mass_g_diff = c(1.10, 1.22, 1.19, 1.04),
  leaf_area_cm2_diff = c(1.18, 1.37, 1.29, 1.08),
  SLA_cm2_g_diff = c(0.39, 0.42, 0.45, 0.45),
  LDMC_g_g_diff = c(0.37, 0.37, 0.44, 0.33),
  leaf_thickness_diff = c(0.38, 0.35, 0.38, 0.38),
  N_percent_diff = c(0.30, 0.30, 0.47, 0.34),
  C_percent_diff = c(0.05, 0.07, 0.08, 0.06),
  CN_Ratio_diff = c(0.32, 0.35, 0.42, 0.34)
)

# plot precipitation levels (1=low, 4=high) vs omegas
plot(mean_df$precipitation_level, 
     mean_df$mean_omega,
     xlim = c(1, 4), 
     xlab = "precipitation level (1 = low, 4 = high)",
     ylim = c(-0.1, 0.1),
     ylab = "mean co-occurrence estimate",
     pch = 16,
     xaxp = c(1,4,3)) # sets x axis ticks (start, end, number of 'regions')

# plot precipitation levels vs trait means
plot(x = mean_df$precipitation_level, 
     y = log(mean_df$height_mm),
     xaxp = c(1,4,3),
     xlab = "precipitation level (1 = low, 4 = high)",
     ylim = c(-5, 7),
     ylab = "log(mean trait value)",
     col = 3,
     pch = 16)
for (i in 5:12) {
  points(mean_df$precipitation_level, log(mean_df[, i]),
         col = i, pch = 16)
}
for (i in 4:12) {
  lines(mean_df$precipitation_level, log(mean_df[, i]),
         col = i, pch = 16)
}
