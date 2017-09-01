

## The stuff below is used for plotting, and model validation
bm_test$PREDICTED <- predict(rf_bm, bm_test)
bm_test$RESIDUALS <- bm_test$BIOMASS - bm_test$PREDICTED

bm_moran_dat <- bm_test %>%
  select(LAT, LON, PREDICTED, BIOMASS, RESIDUALS) %>%
  na.omit

bm_corr_res <- ncf::correlog(x = bm_moran_dat$LAT , y = bm_moran_dat$LON, z = bm_moran_dat$RESIDUALS,
                             increment = 1, resamp = 500, latlon = TRUE)

bm_corr_raw <- ncf::correlog(x = bm_moran_dat$LAT , y = bm_moran_dat$LON, z = bm_moran_dat$BIOMASS,
                             increment = 1, resamp = 500, latlon = TRUE)


td <- data.frame(bm_corr_res$correlation)

plot(bm_corr_res)

rf_rmse <- round(sqrt(tail(rf_bm$finalModel$mse, 1)), 2)

pl1 <- ggplot(bm_test, aes(x = PREDICTED, y = BIOMASS)) +
  geom_point() +
  geom_smooth(method='lm', formula = y ~ x, col = "grey40", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, col = "grey40") +
  theme_minimal() +
  labs(title = "",
       subtitle = paste0("RMSE = ", rf_rmse),
       x = expression(Model~predicted~biomass~(log[10])),
       y = expression(Observed~biomass~(log[10]))) +
  theme(aspect.ratio = 1)

## Maps and other sundrys
#
# us <- map_data("state")
#
# data(nw.atlantic)
# atl <- as.bathy(nw.atlantic)
# #
# gg <- ggplot()
# gg <- gg + geom_map(data = us, map = us,
#                     aes(x = long, y = lat,
#                         map_id = region),
#                     fill="grey70", color="grey90", size=0.15) +
#   geom_contour(data = atl, aes(x = x, y = y, z = z), color = "grey20", size = 0.1) +
#   coord_map(xlim = c(-80, -65),
#             ylim = c(37.5, 47.5)) +
#   theme_minimal()

# pl2 <- gg +
#   geom_point(data = bm_test, aes(x = LON, y = LAT, col = PREDICTED), alpha = 0.7, size = 0.75) +
#   scale_color_viridis() +
#   labs(x = "", y = "",
#        title = gsub("_", " ", name),
#        subtitle = expression(Predicted~biomass~(log[10])),
#        color = "") +
#   theme(legend.position = "none")

# both_fit <- cowplot::plot_grid(pl2, pl1, labels = NULL, align = 'hv', axis = "b")

# ggsave(both_fit, filename = "haddock_fit.pdf", height = 210, width = 297, units = "mm")

# # Use model to make probabilistic predictions
# probability.pa.pred <- predict(wf_abund, newdata = testing.data, "raw")
# probability.pa.pred <- data.frame(matrix(unlist(probability.pa.pred),
#                                          nrow = dim(testing.data)[1],
#                                          byrow = T))
#
# table(testing.data$PRESENCE, probability.pa.pred)



# This will produce a 2 column data frame with both probability of absence and presence.
# These values of probability of occurence can then be used to calculate AUC and calibration curves.
# wf_latlong <- wf_dat %>%
#   select(-one_of(na_count$COLS[na_count$NAs >= na_nums]),
#          -one_of(join_names),
#          LAT, LON) %>%
#   # mutate(PRESENCE = as.factor(recode(PRESENCE, `1` = "present", `0` = "absent"))) %>%
#   na.omit %>%
#   as.data.frame


# Make predictions on the scale of the response variable, in this case the binary presence-absence scale
testing.data$PREDICTED <- predict(wf_abund, newdata = testing.data, type="raw")

# These predicted response values can then be used to calculate kappa values
testing.data$residuals <- as.numeric(as.character(testing.data$PRESENCE)) - as.numeric(as.character(testing.data$PREDICTED))

### ADDITIONAL CODE ###

## look at correlation structure in data
# source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")
#
# ggcorr(trainX, geom = "blank", label = FALSE, hjust = .75, size = 3, layout.exp = 4) +
#   geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) >= 0.7)) +
#   scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
#   guides(color = FALSE, alpha = FALSE)
#
#

# # Plot the data distribution of complete cases
# ggplot(na_select, aes(x = NCOL, y = NROW)) +
#   geom_segment(x = max_ncols, y = 0,
#                xend = max_ncols,
#                yend = max_nrows,
#              col = "red") +
#   geom_segment(x = -Inf, y = max_nrows,
#                xend = max_ncols,
#                yend = max_nrows,
#                col = "red") +
#   geom_point() +
#   theme_bw() +
#   labs(y = "Complete cases (n)",
#        x = "Columns (n)",
#        title = paste0("Max complete cases = ", max_nrows , "\nMax columns = ", max_ncols))

