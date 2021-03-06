# practice code 
library(palmerpenguins)
library(tidyverse)
library(DT)


# filter for body masses 
body_mass_df <- penguins %>% 
  filter(body_mass_g %in% 3000:4000)

# scatterplot
ggplot(na.omit(body_mass_df), 
       aes(x = flipper_length_mm, y = bill_length_mm,
           color = species, shape = species)) +
  geom_point() +
  scale_color_manual(values = c("#FEA346", "#B251F1", "#4BA4A4")) +
  labs(x = "Flipper Length (mm)", y = "Bill Length (mm)", 
       color = "Penguin Species", 
       shape = "Penguin Species")



# DT table 

DT::datatable(data = penguins)

# Reactive Histogram

island_df <- penguins %>% 
  filter(island == "Dream")


ggplot(data = na.omit(penguins), aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), alpha = 0.6) +
  labs(x = "Flipper Length (mm)",
       y = "Frequency",
       fill = "Species") +
  scale_fill_manual(values = c("Adelie" = "#FEA346",
                               "Chinstrap" = "#B251F1",
                               "Gentoo" = "#4BA4A4"))

