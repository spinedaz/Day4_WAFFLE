library(waffle)
library(tidyverse)
library(ggtext)
library(ggthemes)
library(patchwork)
library(gridExtra)
library(ggpubr)
library(camcorder)
library(showtext)
library(here)

# Read data
data <- read.csv(here("Data/Length of paid leave (calendar days) .csv")) %>% 
  drop_na() 

# Wrangle for the top 10 countries
data2 <- data %>% 
  rename("Number_of_days" = "Length.of.paid.maternity.leave..calendar.days.") %>% 
  arrange(desc(Number_of_days))  %>% 
  filter(Number_of_days>0) %>% 
  slice(1:10) %>% 
  mutate(new = Number_of_days/10)

# Create palette with chroma palette helper
color_palette <- c('#00429d', '#3e67ae', '#618fbf', '#85b7ce', '#b1dfdb', '#ffcab9', '#fd9291', '#e75d6f', '#c52a52', '#93003a'
)

# Specify the order of levels in the "Economy" variable
data2$Economy <- factor(data2$Economy, levels = unique(data2$Economy)[order(data2$new)])

# Create plot
b <- ggplot(data2, aes(fill = Economy, values = new)) +
  geom_waffle(n_rows = 15, size = 0.33, colour = "white") +
  coord_equal() +
  scale_fill_manual(values = color_palette, breaks = unique(data2$Economy)) +
  # scale_fill_carto_d("Sunset", breaks = unique(data1$Economy)) +
  theme_void(base_family = "sans") +
  theme(legend.title = element_blank())

# Add more stuff to the plot

b +
  labs(title = "Lenght of paid leave top 10 countries",
       subtitle = " Data: Gender data portal - The World Bank",
       caption = " **Plot** | @PinusPineda and @guiriflautico" ) +
  theme(
    legend.title = element_blank(),
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(family="ral",size=18,hjust=0.5, vjust= 12,color="gray30",face='bold',margin=margin(1.5,0,0,0,'cm')),
    plot.subtitle = element_text(family="ral",size=10,hjust=0.5,vjust = 21,color="gray50",margin=margin(0.25,0,-1.5,0,'cm')),
    plot.caption = element_markdown(family="ral",size=10,hjust=0.5,vjust=-0.55, color="gray30",margin=margin(-1.25,0,0.5,0,'cm'))
  ) 

