# Secondary outcome measures
############################
## Authors: Peter Kamerman, Fiona Baker, and Stella Iacovides
## Contains script used to generate the secondary
## outcome measure plots used in the manuscript

### Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(grid)
library(scales)

### Load data
data2 <- read_csv('./data/other.csv')

### Quick look
glimpse(data2)

### Process data
#### Convert id and period to factors
data2 <- data2 %>%
    mutate(id = factor(id),
           period = factor(period))

### Sleep quality plot
sleep_quality <- ggplot(data2,
       aes(x = period,
           y = sleep.quality,
           colour = period,
           fill = period)) +
    geom_boxplot(fatten = 2,
                 size = 0.9,
                 colour = '#000000',
                 fill = '#FFFFFF',
                 outlier.size = 2.5) +
    labs(x = '\nIntervention',
         y = 'Sleep quality\n(0-100mm VAS)',
         title = "") +
    scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
    scale_x_discrete(labels = c('Baseline\nnight',
                                'Fragmentation\nnight 1',
                                'Fragmentation\nnight 2')) +
    theme_cowplot() +
    theme(legend.position = 'none',
          plot.margin = unit(c(1,2,1,2), "lines"),
          axis.line = element_line(size = 0.9),
          axis.ticks = element_line(size = 0.9),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 20),
          axis.text.x = element_blank(),
          axis.title.x = element_blank())

### Morning vigilance
morning_vigilance <- ggplot(data2,
       aes(x = period,
           y = morning.vigilance,
           colour = period,
           fill = period)) +
    geom_boxplot(fatten = 2,
                 size = 0.9,
                 colour = '#000000',
                 fill = '#FFFFFF',
                 outlier.size = 2.5) +
    labs(x = '\nIntervention',
         y = 'Morning vigilance\n(0-100mm VAS)') +
    scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
    scale_x_discrete(labels = c('Baseline\nnight',
                                'Fragmentation\nnight 1',
                                'Fragmentation\nnight 2')) +
    theme_cowplot() +
    theme(legend.position = 'none',
          plot.margin = unit(c(1,2,1,2), "lines"),
          axis.line = element_line(size = 0.9),
          axis.ticks = element_line(size = 0.9),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 20),
          axis.text.x = element_blank(),
          axis.title.x = element_blank())

### Profile of Mood States
#### Process data
poms_plot <- data2 %>%
    group_by(id, period) %>%
    gather(key = time,
           value = score,
           poms.morning,
           poms.evening) %>%
    mutate(time = factor(time,
                         levels = c('poms.evening', 'poms.morning'),
                         labels = c('Evening', 'Morning')))
#### Plot
poms <- ggplot(poms_plot,
       aes(x = period,
           y = score,
           colour = time,
           fill = time)) +
    geom_boxplot(fatten = 2,
                 size = 0.9) +
    labs(x = '\nIntervention',
         y = 'Profile of mood states\n(0-200 score)') +
    scale_y_continuous(limits = c(0, 200),
                       expand = c(0,0)) +
    scale_x_discrete(labels = c('Baseline\nnight',
                                'Fragmentation\nnight 1',
                                'Fragmentation\nnight 2')) +
    scale_fill_manual(labels = c('Evening', 'Morning'),
                      values = c('#888888', '#FFFFFF')) +
    scale_colour_manual(labels = c('Evening', 'Morning'),
                        values = c('#000000', '#000000')) +
    theme_cowplot() +
    theme(legend.position = c(0.9, 0.92),
          legend.key.size = unit(30, "pt"),
          legend.text = element_text(size = 18),
          legend.title = element_blank(),
          plot.margin = unit(c(1,2,1,2), "lines"),
          axis.line = element_line(size = 0.9),
          axis.ticks = element_line(size = 0.9),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 20),
          axis.title.x = element_blank())

### Multi-panel plot
#### Mood, vigilance, sleep quality
stacked <- plot_grid(sleep_quality, morning_vigilance, poms,
                       align = 'none',
                       nrow = 3, ncol = 1,
                       labels = 'AUTO', label_size = 20,
                       rel_heights = c(1, 1, 1.12))
save_plot('./figures/figure-1.pdf', stacked,
          base_height = 11.6, base_width = 8.2)

### Pin-prick
pin_prick <- ggplot(data2,
                    aes(x = period,
                        y = pin.prick_mN,
                        colour = period,
                        fill = period)) +
    geom_boxplot(fatten = 2,
                 size = 0.9,
                 colour = '#000000',
                 fill = '#FFFFFF',
                 outlier.size = 2.5) +
    labs(y = 'Pin-prick threshold (mN)\n') +
    scale_y_continuous(limits = c(0, 600), expand = c(0,0)) +
    scale_x_discrete(labels = c('Baseline\nnight',
                                'Fragmentation\nnight 1',
                                'Fragmentation\nnight 2')) +
    theme_cowplot() +
    theme(legend.position = 'none',
          plot.margin = unit(c(2,2,2,2), "lines"),
          axis.line = element_line(size = 0.9),
          axis.ticks = element_line(size = 0.9),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 20),
          axis.title.x = element_blank())

save_plot('./figures/figure-2.pdf', pin_prick,
          base_height = 8, base_aspect_ratio = 1.25)
