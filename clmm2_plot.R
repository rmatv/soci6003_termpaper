# source: https://verbingnouns.github.io/notebooks/ordinal_data.html#Ordinal_regressions
library(tidyr)
clmm.graph <- model.int
vlines <- c(0, 
            clmm.graph$beta[[1]], clmm.graph$beta[[2]], clmm.graph$beta[[3]],
            clmm.graph$beta[[4]], clmm.graph$beta[[5]], clmm.graph$beta[[6]], 
            clmm.graph$beta[[7]], clmm.graph$beta[[8]], clmm.graph$beta[[9]],
            clmm.graph$beta[[10]], clmm.graph$beta[[11]], clmm.graph$beta[[12]], 
            clmm.graph$beta[[13]], clmm.graph$beta[[14]], clmm.graph$beta[[15]],
            clmm.graph$beta[[16]], clmm.graph$beta[[17]], clmm.graph$beta[[18]])

xaxis <- seq(min(vlines - .25), max(vlines + .25), length.out = 100)
yaxis <- rep(c(0, 1), 50)

tibble(xaxis, yaxis) %>% 
  mutate(Never  =  plogis(clmm.graph$Theta[1] - xaxis), 
         Seldom =  plogis(clmm.graph$Theta[2] - xaxis) - plogis(clmm.graph$Theta[1] - xaxis),
         Regularly =  1 - (plogis(clmm.graph$Theta[2] - xaxis))) %>%  
  gather(regularity, probability, 3:5) %>% 
  mutate(regularity = factor(regularity, levels=c("Never", "Seldom", "Regularly"))) %>%  
  ggplot(aes(x = xaxis, y = yaxis)) + 
  geom_hline(yintercept=0, lty = "dotted", lwd = .25) + 
  geom_hline(yintercept=1, lty = "dotted", lwd = .25) + 
  geom_line(aes(y = probability, linetype = regularity), lwd = .25, alpha = 1) +
  annotate("segment", x = vlines, y = 0, xend = vlines, yend = 1,
           lty = "solid", alpha = .5, lwd = .25) +
  annotate("text", x = vlines, y = .95, size = 1.5, hjust = 1,
           label = c("", "Vigorous PA", "", "Walking", "Sitting", 
                     "Sex (Female)", "", "Lower middle class", 
                     "Middle class", "Upper middle class", 
                     "Higher class", "Education (<15 yrs)", "", 
                     "Education (> 20 yrs)", "Education (Still studying)",
                     "", "", "Authorities", "Membership in sport club"), 
           angle=90, vjust=-0.2) + 
  annotate("text", x = vlines, y = .5, size = 1.5, hjust = 0,
           label = c("", "", "Moderate PA, Area, Sport clubs", "", "",
                     "", "Age, Education (16-19 yrs)", "", "", "", "", 
                     "", "", "", "", "", "", "", ""), 
           angle=90, vjust=-0.2) +
  scale_x_continuous(breaks = c(min(xaxis - .5), max(xaxis + .5))) + 
  scale_y_continuous(breaks = c(0, .25, .5, .75, 1)) +
  ylab("Probability") + xlab("") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = .2),
        text = element_text(size = 6),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.margin = margin(0,0,0,0),
        legend.box.margin=margin(-15, 0,0,0),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))

ggsave("probabilities.pdf", plot = test.plot, device = "pdf",
  path = "fig/",
  scale = 1,
  width = 4.25,
  height = 3,
  units = "in",
  dpi = 900,
  limitsize = TRUE)

