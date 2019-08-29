# plot the mean results (Figure 1)
plotdata %>%
  # no fama french factors
  filter(name %in% no_ff) %>%
  # group by model and get the mean and error bars to plot the original method
  group_by(name) %>% 
  ggplot() +
  # calculate mean and error bars
  geom_pointrange(mapping = aes(x = reorder(name, -val), y = val), 
                  stat = "summary",
                  fun.ymin = function(z) {mean(z) - 1.96*(sd(z)/sqrt(10000))},
                  fun.ymax = function(z) {mean(z) + 1.96*(sd(z)/sqrt(10000))},
                  fun.y = mean) + 
  geom_point(data = plotdata %>% filter(name == "MMPI"), aes(x = name, y = mean(val)), color = "red") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Model", y = "Average Normalized Mean Squared Error") + 
  scale_y_continuous(breaks = seq(-0.2, 0.5, 0.1)) + 
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = 1)) + 
  theme(axis.title = element_text(size = 16)) %>% 
  coord_flip(ylim = c(-0.2, 0.5))