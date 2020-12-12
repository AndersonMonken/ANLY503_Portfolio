library(tidyverse)
library(readr)

df_raw <- read_tsv('data/dataset1.tds')

tidy_gene <- df_raw %>% separate(NAME, 
                    into = c("name", "bp", "mf", "systematic_name", "number"),
                    '\\|\\|') 

tidy_gene2 <- tidy_gene %>%
  mutate_at(c("name", "bp", "mf", "systematic_name", "number"), 
            str_trim)

tidy_gene3 <- tidy_gene2 %>% 
                select(-number, -GID, -YORF, -GWEIGHT)

tidy_gene4 <- tidy_gene3 %>%
                pivot_longer(G0.05:U0.3, names_to = 'sample')

tidy_gene5 <- tidy_gene4 %>% separate(sample, into = c('nutrient','rate'), sep = 1, convert = TRUE)

tidy_gene5



ggplot(tidy_gene5, aes(x = value, fill = nutrient)) + geom_histogram() +facet_wrap(~nutrient)

ggplot(tidy_gene5, aes(x = as.factor(rate), y = value, color = nutrient)) + 
  geom_violin(fun.data="mean_sdl") +
  facet_wrap(~nutrient)+ 
  stat_summary(fun.data="mean_sdl", mult=1,geom="crossbar", width=0.2 )
                                                                                                                                                  geom="crossbar", width=0.2 )
