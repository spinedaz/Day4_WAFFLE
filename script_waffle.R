library(waffle)
library(tidyverse)
library(ggtext)
library(ggthemes)
library(patchwork)
library(gridExtra)
library(ggpubr)
library(camcorder)
library(showtext)

data <- read.csv("C:/Users/spine/Downloads/Length of paid leave (calendar days)  [WB Gender Portal]/Length of paid leave (calendar days) .csv") %>% 
  drop_na() 


data1 <- read.csv("C:/Users/spine/Downloads/Length of paid leave (calendar days)  [WB Gender Portal]/Length of paid leave (calendar days) .csv") %>% 
  drop_na() %>% 
  rename("Number_of_days" = "Length.of.paid.maternity.leave..calendar.days.") %>% 
  arrange(Number_of_days)  %>% 
  filter(Number_of_days>0) %>% 
  slice(1:10) %>% 
  mutate(new = Number_of_days/10) %>% 
  arrange(desc(new))

data2 <- read.csv("C:/Users/spine/Downloads/Length of paid leave (calendar days)  [WB Gender Portal]/Length of paid leave (calendar days) .csv") %>% 
  drop_na() %>% 
  rename("Number_of_days" = "Length.of.paid.maternity.leave..calendar.days.") %>% 
  arrange(desc(Number_of_days))  %>% 
  filter(Number_of_days>0) %>% 
  slice(1:10) %>% 
  mutate(new = Number_of_days/10)


# Specify the order of levels in the "Economy" variable
data1$Economy <- factor(data1$Economy, levels = unique(data1$Economy)[order(data1$new)])

# Plot with the specified legend order
a<- ggplot(data1, aes(fill = Economy, values = new)) +
  geom_waffle(n_rows = 15, size = 0.33, colour = "white") +
  coord_equal() +
  scale_fill_manual(values = color_palette, breaks = unique(data1$Economy)) +
  # scale_fill_carto_d("Sunset", breaks = unique(data1$Economy)) +
  theme_void(base_family = "sans") +
  theme(legend.title = element_blank())



# Specify the order of levels in the "Economy" variable
data2$Economy <- factor(data2$Economy, levels = unique(data2$Economy)[order(data1$new)])

b <- ggplot(data2, aes(fill = Economy, values = new)) +
  geom_waffle(n_rows = 15, size = 0.33, colour = "white") +
  coord_equal() +
  scale_fill_manual(values = color_palette, breaks = unique(data2$Economy)) +
  # scale_fill_carto_d("Sunset", breaks = unique(data1$Economy)) +
  theme_void(base_family = "sans") +
  theme(legend.title = element_blank())


color_palette <- c('#00429d', '#3e67ae', '#618fbf', '#85b7ce', '#b1dfdb', '#ffcab9', '#fd9291', '#e75d6f', '#c52a52', '#93003a'
)


b +
  labs(title = "Lenght of paid leave (calendar days)",
       subtitle = " Data: Gender data portal - The World Bank",
       caption = " **Plot** | @PinusPineda and @guiriflautico") +
  theme(
        legend.title = element_blank()
  ) +
  theme(
    plot.background = element_rect(fill="white",color=NA),
    plot.title = element_text(family="ral",size=18,hjust=0.5, vjust= 12,color="gray30",face='bold',margin=margin(1.5,0,0,0,'cm')),
    plot.subtitle = element_text(family="ral",size=10,hjust=0.5,vjust = 21,color="gray50",margin=margin(0.25,0,-1.5,0,'cm')),
    plot.caption = element_markdown(family="ral",size=10,hjust=0.5,vjust=-0.65, color="gray30",margin=margin(-1.25,0,0.5,0,'cm'))
  ) 

