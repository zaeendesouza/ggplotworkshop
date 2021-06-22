# Standalone code for all plots in the slides.
# Zaeen de Souza

library(pacman)
p_load(
  ggalt,
  ggthemes,
  ggplot2
  readxl,
  tidyverse,
  ggthemes,
  gridExtra,
  grid,
  gridtext,
  extrafont,
  extrafontdb,
  Hmisc,
  lubridate,
  ggalt,
  ggtext)


# ggplot theme
my_theme <- function(base_size = 12,
                     base_family = "") {
  theme_minimal(base_size = base_size,
                base_family = base_family) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y  =  element_line(
        colour = "#c9c0b7",
        linetype = "dotted",
        size = .5),
      axis.ticks = element_line(size = .5, colour = "#c9c0b7"),
      axis.ticks.x = element_blank(),
      axis.line = element_line(
        size = .5,
        colour = "#c9c0b7",
        linetype = "solid"),
      axis.line.y = element_blank(),
      axis.text.y = element_text(colour = "black", 
                                 margin = margin(r = 2), hjust = 1),
      axis.text.x = element_text(colour = "black"),
      # we done here
      complete = TRUE
    )
}

ex_1 <- read.csv("C:/Users/Zaeen de Souza/Documents/GitHub/ggplotworkshop/data/ex_1.csv")
ex_2 <- read.csv("C:/Users/Zaeen de Souza/Documents/GitHub/ggplotworkshop/data/ex_2.csv")
ex_3 <- read.csv("C:/Users/Zaeen de Souza/Documents/GitHub/ggplotworkshop/data/ex_3.csv")
map_data <- read.csv("C:/Users/Zaeen de Souza/Documents/GitHub/ggplotworkshop/data/map_data.csv")


# Basic scatterplot
plot_1 <- ggplot(ex_1, 
                 aes(x = mobility, 
                     y = incgr)) + 
  geom_text(aes(label = state_month)) +
  geom_smooth(method = "lm")
plot_1

# classic theme
plot_1 +
  theme_classic()

# 538
plot_1 +
  theme_fivethirtyeight()

# The Economist
plot_1 + 
  theme_economist()

# Stata!
plot_1 +
  theme_stata()


# Histogram + basic theme
plot_2 <- ggplot(ex_2, 
                 aes(x = ln_wage, 
                     fill = union)) +
  geom_histogram(position = "identity", 
                 bins = 30, 
                 size = 0.05, 
                 alpha = .5,
                 colour = "white") +
  scale_fill_manual(values = c("#007bff", "#E64173"), 
                    labels = c("Non-Union", "Union")) +
  ylab("Count") +
  xlab("Ln(Wage)") +
  labs(fill = "")
plot_2


# Histogram
plot_2 <- ggplot(ex_2, aes(x = ln_wage, 
                           fill = union)) +
  geom_histogram(position = "identity", 
                 bins = 30, 
                 size = 0.05, 
                 alpha = .5,
                 colour = "white") +
  scale_fill_manual(values = c("#007bff", "#E64173"), 
                    labels = c("Non-Union", "Union")) +
  ylab("Count") +
  xlab("Ln(Wage)") +
  labs(fill = "")
plot_2

plot_3 <- ggplot(ex_2, aes(x = ln_wage, 
                 fill = union)) +
  geom_histogram(position = "identity", 
                 bins = 30, 
                 size = 0.1, 
                 alpha = .5,
                 colour = "white") +
  scale_fill_manual(values = c("#E64173", "#007bff"), 
                    labels = c("Non-Union", "Union")) +
  ylab("Count") +
  xlab("Ln(Wage)") +
  labs(fill = "") +
  my_theme(base_size = 15) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(n.breaks = 10) +
  labs(title = "Wage Distributions by Union Status", 
       subtitle = "Something something something something",
       caption = "Data: Internet")

plot_3


# alluvial

library(ggforce)
data_1 <- read.csv("F:/Surbhi (Data Viz)/ex_3.csv")

data_alluvial <- data_1 %>%
  gather_set_data(2:1)

data_alluvial$x <-
  factor(data_alluvial$x,
         levels = c("pre", "post"),
         labels = c(1, 2))

plot_3 <- data_alluvial %>%
  ggplot(aes(x, id = id, 
             split = y, 
             value = percentage))
plot_3

plot_3 <- plot_3 +
  geom_parallel_sets(aes(fill = pre, 
                         alpha=0.5),
                     strength = .4, 
                     show.legend = F)
plot_3

plot_3 <- plot_3 +
  geom_parallel_sets_labels(angle = 0) +
  geom_parallel_sets_axes(axis.width = 0.01)
plot_3

plot_3 <- plot_3 +
  scale_x_discrete(labels = c('2019', '2020'), 
                   position = "top") + theme_void()
plot_3

plot_3 <- plot_3 + 
  theme(axis.text.x = 
          element_text(
            size = 15, 
            face = "bold"),
        plot.margin = 
          unit(c(1,1,1,1), "cm"))


# Dumbell plot

plot_data <- arrange(map_data, 
                     recovery_male) %>%
  mutate(state.fact = factor(ST_NM, 
                             levels = unique(ST_NM)))

female <- "#00838f"
male <- "#f8434f"

plot_4 <- ggplot() +
  geom_dumbbell(
    data = plot_data,
    aes(y = state.fact,
        x = recovery_male,
        xend = recovery_female),
    size = 1,
    color = "grey",
    alpha = 1,
    size_x = 5,
    size_xend = 5,
    colour_x = male,
    colour_xend = female)

plot_4 <- plot_4 + geom_text(
  # use this to filter the y axis; we are only adding a label on top, 
  #  in line with Tamil Nadu - so that it isn't cluttered
  data = filter(plot_data,
                ST_NM == "Tamil Nadu"),
  aes(x = recovery_male,
      y = ST_NM,
      label = "Men"),
  color = male,
  vjust = -.9,
  fontface = "bold") + 
  # Don't forget to use your theme!  
  my_theme()


plot_4 + expand_limits(y= c(1, 24)) + xlab("% of workers who recovered their jobs") + ylab("") + 
  labs(title = "Gender Differences in Job Loss & Recovery",
       subtitle = "Fewer women were able to recover their jobs, after losing them",
       caption = "Data: State of Working India, 2021") +
  geom_text(
    data = filter(plot_data,
                  ST_NM == "Tamil Nadu"),
    aes(x = recovery_female,
        y = ST_NM,
        label = "Women"),
    color = female,
    vjust = -.9,
    fontface = "bold")

map_data <- read.csv("F:/Surbhi (Data Viz)/map_data.csv")


map_data$difference_recovery <- map_data$recovery_male - map_data$recovery_female


plot <- arrange(map_data, 
                recovery_male) %>%
  mutate(state.fact = factor(ST_NM, 
                             levels = unique(ST_NM)))

plot <- subset(plot, ST_NM!="Uttarakhand")

female <- "#00838f"
male <- "#f8434f"

plot_4 <- ggplot() +
  geom_dumbbell(
    data = plot,
    aes(y = state.fact,
        x = recovery_male,
        xend = recovery_female),
    size = 1,
    color = "grey80",
    alpha = 1,
    size_x = 5,
    size_xend = 5,
    colour_x = male,
    colour_xend = female)

plot_4 <- plot_4 +  
  geom_text(
    data = filter(plot,
                  ST_NM == "Tamil Nadu"),
    aes(x = recovery_male,
        y = ST_NM,
        label = "Men"),
    color = male,
    vjust = -.9,
    fontface = "bold") +
  geom_text(
    data = filter(plot,
                  ST_NM == "Tamil Nadu"),
    aes(x = recovery_female,
        y = ST_NM,
        label = "Women"),
    color = female,
    vjust = -.9,
    fontface = "bold") +
  expand_limits(x=c(0, 0.9), y= c(1, 22)) +
  geom_text(
    data = filter(plot,
                  ST_NM!= "Maharashtra" & ST_NM!= "Assam"),
    aes(
      x = recovery_male,
      y = state.fact,
      label = paste0(round(recovery_male*100,0),"%")),
    color = "black",
    size = 4,
    fontface = "bold",
    hjust = -0.4) +
  geom_text(
    data = filter(plot,
                  ST_NM!= "Maharashtra" & ST_NM!= "Assam"),
    aes(
      x = recovery_female,
      y = state.fact,
      label = paste0(round(recovery_female*100,0),"%")),
    color = "black",
    size = 4,
    fontface = "bold",
    hjust = 1.5) +
  ylab("") + 
  xlab("") +
  labs(title = "Gender Differences in Job Loss & Recovery",
       subtitle = "Fewer women were able to recover their jobs, after losing them",
       caption = "Data: State of Working India, 2021") +
  my_theme() + scale_x_continuous(labels = scales::percent)
plot_4


plot_4 <- plot_4 + geom_curve(
  aes(
    x = .7,
    y = 7.4,
    xend = .6,
    yend = 4),
  colour = "#555555",
  curvature = -0.2,
  size = .7,
  # NPC stands for Normalised Parent Coordinates
  arrow = arrow(length = unit(0.03, "npc"))) 

plot_4 <- plot_4 +
  geom_richtext(
    aes(x = .65, y = 7.3,
        # this is markdown syntax! ** word** for bold 
        # *word* for italic        
        # <br> is common to markdown, HTML etc - it adds a linebreak
        label = 
          "***Note:***<br>*Something about the <br>
Karnataka point being really high!*"),
lineheight = 0.8,
colour = "#2b2b2b",
fill = "white",
vjust = 0,
hjust = 0,
label.size = NA,
size = 4) 
