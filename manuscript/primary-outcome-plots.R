# Primary outcome
#################
## Authors: Peter Kamerman, Fiona Baker, and Stella Iacovides
## Contains script used to generate the plots used in the manuscript

### Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(grid)
library(scales)

### Load data
regression <- read_csv('./data/regression.csv')

# Process data
##############
# Convert id and period to factors
regression <- regression %>%
    mutate(id = factor(id),
           period = factor(period)) %>%
    tbl_df()

# Process data for plot
regression <- regression %>%
    group_by(id, period) %>%
    gather(key = time, value = score, 3:13) %>%
    mutate(time = as.numeric(as.character(time)))

## Plot
#######
ischaemia <- ggplot(regression,
       aes(x = time,
           y = score,
           colour = period,
           fill = period)) +
    geom_smooth(aes(linetype = period),
                method = 'loess',
                size = 1,
                alpha = 0.3) +
    labs(x = '\nDuration of ischaemia (minutes)',
         y = 'Pain intensity (0-100mm VAS)\n') +
    scale_y_continuous(limits = c(0, 100),
                       expand = c(0,0)) +
    scale_x_continuous(limits = c(0, 10),
                       expand = c(0, 0),
                       breaks = c(0, 2, 4, 6, 8, 10),
                       labels = c(0, 2, 4, 6, 8, 10)) +
    scale_linetype_manual(values = c('dotted', 'longdash', 'solid'),
                          labels = c("Baseline night",
                                   "Fragmentation night 1",
                                   "Fragmentation night 2")) +
    scale_colour_manual(values = c('#000000', '#000000', '#000000'),
                      labels = c("Baseline night",
                               "Fragmentation night 1",
                               "Fragmentation night 2")) +
    scale_fill_manual(values = c('#000000', '#000000', '#000000'),
                        labels = c("Baseline night",
                                 "Fragmentation night 1",
                                 "Fragmentation night 2")) +
    theme_cowplot() +
    theme(legend.position = c(0.80, 0.15),
          legend.background = element_rect(colour = '#000000'),
          legend.key.size = unit(30, "pt"),
          legend.key.width = unit(45, 'pt'),
          legend.text = element_text(size = 18),
          legend.title = element_blank(),
          plot.margin = unit(c(2,2,2,2), "lines"),
          axis.line = element_line(size = 0.9),
          axis.ticks = element_line(size = 0.9),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 20))

save_plot('./figures/figure-3.pdf', ischaemia,
          base_height = 8, base_aspect_ratio = 1.25)
