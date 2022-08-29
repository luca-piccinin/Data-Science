install.packages("readxl")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("sf")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("plotrix")
install.packages("Matrix", type = "source")
install.packages("maps")
install.packages("mapdata")
install.packages("cowplot")
install.packages("hrbrthemes")
install.packages("gganimate")
require(forcats)
require(stats)
require(Matrix)
library(readxl)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(sf)
library(tidyverse)
library(magrittr)
library(dplyr)
library(plotrix)
library(maps)
library(cowplot)
library(datasets)
library(hrbrthemes)
library(gganimate)


df <- read.csv2("police_deaths_in_america_v3.csv")
View(df)
  

#CASUSE DELLA MORTE, USANDO GRAFICO A TORTA
df %>%
  count(Cause_of_Death) %>%
  ggplot() +
  theme_bw() +
  geom_bar(aes(x = "", y = n, fill = Cause_of_Death),
           stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  ggtitle(paste0(length(unique(df$Cause_of_Death)), " Cause of Death")) +
  theme(plot.title = element_text(hjust = .5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  guides(fill = guide_legend(reverse = FALSE))

#ANNO CON PIU' MORTI, USANDO GRAFICO LINEARE
df %>%
  count(Year) %>%
  ggplot( aes(x = Year, y = n)) +
  geom_point(color = "black", size = .75) +
  geom_line(color = "grey", size = .5) +
  ylim(0,700) +
  xlim(1790,2022) +
  xlab("Year") +
  ylab("Dead") +
  theme_ipsum() +
  labs(title = paste0(nrow(df), " Dead case in ", (2022 - 1971), " different Year"),
       subtitle = paste0("Added case from: 1971 to 2022",
                         x = "", y = ""))

#MESE DELL'ANNO CON PIU' MORTI, USANDO ISTOGRAMMA
df %>%
  add_count(Month) %>%
  mutate(Month = as.factor(Month),
         Month = factor(df$Month, levels = month.name)) %>%
  count(Month) %>%
  ggplot(aes(x = n, y = Month)) +
  geom_col(aes(fill = Month), color = "black") +
  geom_label(aes(label = n, fill = Month),
             color = "black", alpha = 0.5, nudge_x = 10, size = 4) +
  theme_bw() +
  xlab("Dead") +
  ylab("Month") +
  theme(legend.position = "none") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0, 2500)) +
  labs(title = paste0(nrow(df), " Dead case for every month"))

#GIORNO DELLA SETTIMANA CON PIU' MORTI, USANDO ISTOGRAMMA
df %>%
  add_count(Day) %>%
  mutate(Day = as.factor(Day),
         Day = forcats::fct_reorder(Day, -n)) %>%
  count(Day) %>%
  ggplot(aes(x = n, y = Day)) +
  geom_col(aes(fill = Day), color = "black") +
  geom_label(aes(label = n, fill = Day),
             color = "black", alpha = 0.5, nudge_x = 10, size = 4) +
  theme_bw() +
  xlab("Dead") +
  ylab("Day") +
  theme(legend.position = "none") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0, 4200)) +
  labs(title = paste0(nrow(df), " Dead case for every day of week"))

#STATO CON PIU' MORI, USANDO ISTOGRAMMA
df %>%
  add_count(State) %>%
  mutate(State = as.factor(State),
         State = forcats::fct_reorder(State, -n)) %>%
  mutate(State_other = forcats::fct_lump(f = State, n = 30,
         other_level = paste0(length(unique(df$State)) -30, "
                              other state"))) %>%
  count(State_other) %>%
  ggplot(aes(x = n, y = State_other)) +
  geom_col(aes(fill = State_other), color = "black") +
  geom_label(aes(label = n, fill = State_other),
             color = "black", alpha = 0.5, nudge_x = 10, size = 4) +
  theme_bw() +
  xlab("Dead") +
  ylab("State") +
  theme(legend.position = "none") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0, 3000)) +
  labs(title = paste0(nrow(df), " Dead case in ",
                      length(unique(df$State)), " different State"),
       subtitle = paste0("Added case from: 1971 to 2022",
       x = "", y = ""))

#STATO CON PIU' MORI, USANDO LA MAPPA
usa <- map_data("state")

centroids <- usa %>%
  filter(usa$region %in% df$State) %>%
  group_by(region) %>%
  summarize(xlong = mean(long),
            ylat = mean(lat))

centroids2 <- usa %>%
  filter(usa$subregion %in% df$State) %>%
  group_by(subregion) %>%
  summarize(xlong = mean(long),
            ylat = mean(lat))

names(centroids2)[1] <- "region"

centroids <- bind_rows(centroids, centroids2)

cases <- df %>% count(State)
centroids$cases <- cases$n[match(centroids$region, cases$State)]

centroids %>% left_join(cases, by = c("region" = "State"))

usa %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(color = "black", fill = "lightgrey") +
  theme_bw() +
  geom_point(data = centroids,
             mapping = aes(x = xlong, y = ylat,
                           size = cases, group = region),
             color = "red", alpha = 0.5) +
  coord_cartesian(xlim = c(-125,-65), ylim = c(25,50)) +
  scale_size(range = c(1,10)) +
  labs(size = "Cases", x = "", y = "")

#MORTI PER RUOLO, CON ISTOGRAMMA (più funzionale)
df %>%
  add_count(Rank) %>%
  mutate(Rank = as.factor(Rank),
         Rank = forcats::fct_reorder(Rank, -n)) %>%
  mutate(Rank_other = forcats::fct_lump(f = Rank, n = 10,
                                        other_level = paste0(length(unique(df$Rank)) -10, "
                              other rank"))) %>%
  count(Rank_other) %>%
  ggplot(aes(x = n, y = Rank_other)) +
  geom_col(aes(fill = Rank_other), color = "black") +
  geom_label(aes(label = n, fill = Rank_other),
             color = "black", alpha = 0.5, nudge_x = 10, size = 4) +
  theme_bw() +
  xlab("Dead") +
  ylab("Rank") +
  theme(legend.position = "none") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0, 9000)) +
  labs(title = paste0(nrow(df), " Dead case for ",
                      length(unique(df$Rank)), " different police rank"))

#MORTI PER RUOLO, CON GRAFICO A TORTA
df %>%
  count(Rank) %>%
  ggplot() +
  theme_bw() +
  geom_bar(aes(x = "", y = n, fill = Rank),
           stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  ggtitle(paste0(length(unique(df$Rank)), " Cause of Death for Police Rank")) +
  theme(plot.title = element_text(hjust = .5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  guides(fill = guide_legend(reverse = FALSE))