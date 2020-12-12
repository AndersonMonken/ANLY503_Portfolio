library(tidyverse)
library(readr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

unit_square <- read_csv('unit_square_10k.csv')


ggplot(unit_square) +
  geom_point(aes(x = x, y = y)) +
  coord_equal()

p1 <- ggplot(unit_square)
p2 <- ggplot(unit_square, aes(x = x, y = y))
p3 <- p2 + geom_point()
p3 + coord_equal()


unit_square %>% mutate(in_circle = dist <= 1) %>%
  ggplot() + 
  geom_point(aes(x = x,
                 y = y,
                 color = if_else(!in_circle,
                                 "outside",
                                 quadrant))) +
  coord_equal() +
  scale_colour_brewer(palette = "Set1", name = "Point Location") +
  theme_bw() +
  labs(x = 'X', 
       y = 'Y',
       title = 'Unit Circle')
