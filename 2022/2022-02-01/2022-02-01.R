# Tidy Tuesday, Week 5 
# Bettina MJ Kern
# 
# Set up 
library(ggplot2)       # plotting
library(ggimage)       # include images in plot
library(tidytuesdayR)  # ccess to TT aata 
library(tidyverse)     # other useful tidyverse functions
library(ggtext)        # labels
library(ggbump)       # bump chart geom
library(janitor)
#library(extrafont)
library(ggforce)
library(gganimate)     # animate the plot
theme_set(theme_minimal(base_size = 14))

# get data
ranks <- readr::read_csv(file = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv")

# Data Preparation ----

# show summary for rank variables
ranks |> 
  select(ends_with(" Rank")) |>  
  summary() 

# show rows with NA values
ranks[rowSums(is.na(ranks)) > 0, ] # 20 rows have at least one NA

# drop rows with NA
ranks <- drop_na(ranks)

# remove space in rank column names
names(ranks) <- gsub(names(ranks), pattern = " Rank$", replacement = "")

# rename some of the breeds
ranks$Breed[ranks$Breed == "Retrievers (Labrador)"] <- "Labrador Retriever"
ranks$Breed[ranks$Breed == "Retrievers (Golden)"] <- "Golden Retriever"
ranks$Breed[ranks$Breed == "Pointers (German Shorthaired)"] <- "German Shorthaired"
ranks$Breed[ranks$Breed == "American English Coonhounds"] <- "AE Coonhounds"

# bring data to long format
ranks_long <- pivot_longer(ranks, cols = starts_with("20"), names_to = "Year", values_to = "Rank") |> 
  group_by(Breed) |> 
  mutate(first_year = Year == min(Year),
         last_year =  Year == max(Year)) |> 
  ungroup()

# turn breed into a factor variable
ranks_long$Breed <- factor(ranks_long$Breed)

# get top and bottom 10 dogs
hotdogs <- ranks_long[ranks_long$Rank <= 10, ]
underdogs <- ranks_long[ranks_long$Rank >= (max(ranks_long$Rank) - 12), ]
length(unique(underdogs$Breed))


# Data Visualisation ----

## Basic Bump Chart ----
ggplot() +
  geom_bump(data = hotdogs, 
            mapping = aes(x = Year, 
                          y = Rank, 
                          group = Breed, 
                          colour = Breed)) +
  geom_image(data = hotdogs, mapping = aes(x = Year, y = Rank, image = Image)) +
  scale_y_reverse(breaks = seq(from = 1, to = 10)) + 
  labs(title = "Hot Dogs: Top Ranked Dog Breeds from 2013 until 2020") +
  scale_color_manual(values = c("#E5F16D", "#B7EE7B", "#8CE790", "#65DEA5", "#47D2B8", "#40C5C5", "#53B5CB", "#6FA5C9", "#8893BE", "#9B81AC", "#A67095", "#A9617B"))


## Beautify the Bump Chart ----
ggplot() +
  geom_bump(data = hotdogs, 
            mapping = aes(x = Year, 
                          y = Rank, 
                          group = Breed, 
                          colour = Breed)) +
  geom_image(data = hotdogs, mapping = aes(x = Year, y = Rank, image = Image)) +
  scale_y_reverse(breaks = seq(from = 1, to = 10)) + 
  labs(title = "Hot Dogs!",
       subtitle = "Top Ranked Dog Breeds from 2013 until 2020") +
  scale_color_manual(values = c("#E5F16D", "#B7EE7B", "#8CE790", "#65DEA5", "#47D2B8", "#40C5C5", "#53B5CB", "#6FA5C9", "#8893BE", "#9B81AC", "#A67095", "#A9617B")) +
  geom_text(data = hotdogs |> filter(Year == max(Year)), 
            aes(x = Year, y = Rank, label = Breed), 
            hjust = 0, 
            nudge_x = 0.3) + # nudge the text element along the x axis
  coord_cartesian(clip = "off") +
  theme(legend.position = "none", # remove legend
        panel.grid  = element_blank(), # remove all grid lines
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.margin = margin(0.5, 5, 0.5, 0.5, "cm"))


# Data Animation ---- 
## Animate the Hot Dogs ----

# gganimate doesnot work with the ggbump package, thus I had to improvise a little
animated_hotdogs <- ggplot(hotdogs, aes(x = Year, y = Rank, group = Breed)) +
  geom_line(data = hotdogs, 
            mapping = aes(x = Year, 
                          y = Rank, 
                          colour = Breed)) +
  scale_color_manual(values = c("#E5F16D", "#B7EE7B", "#8CE790", "#65DEA5", "#47D2B8", 
                                "#40C5C5", "#53B5CB", "#6FA5C9", "#8893BE", "#9B81AC", "#A67095", "#A9617B", "#47D2B8")) +
  scale_y_reverse(breaks = seq(from = 1, to = max(hotdogs$Rank))) +
  geom_image(data = hotdogs, aes(x = Year, y = Rank, image = Image)) +
  theme(legend.position = "none",
        panel.grid  = element_blank(), # remove all grid lines
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.5, 5, 0.5, 0.5, "cm"),
        axis.text = element_text(size = 14),
        plot.caption = element_text(hjust = 1, vjust = 0)
  ) +
  geom_text(data = hotdogs |> filter(Year == max(Year)), 
            aes(x = Year, y = Rank, label = Breed, size = 14), 
            hjust = 0, 
            nudge_x = 0.3)  +
  labs(title = "Hot Dogs!",
       subtitle = "Top Ranked Dog Breeds from 2013 until 2020",
       caption = "Data: American Kennel Club \n Visualisation: Bettina MJ Kern") +
  coord_cartesian(clip = "off") +
  transition_reveal(as.numeric(Year))

animated_hotdogs_finetuned <- animate(animated_hotdogs, end_pause = 120, duration = 20, fps = 10, width = 800, height = 700)
# #save animated gif
anim_save("2022/2022-02-01/hotdogs_ranking.gif", animated_hotdogs_finetuned)


## Animate the Underdogs ----
animated_underdogs <- ggplot(underdogs, aes(x = Year, y = Rank, group = Breed)) +
  geom_line(data = underdogs, 
            mapping = aes(x = Year, 
                          y = Rank, 
                          colour = Breed)) +
  scale_color_manual(values = c("#E5F16D", "#B7EE7B", "#8CE790", "#65DEA5", "#47D2B8", 
                                "#40C5C5", "#53B5CB", "#6FA5C9", "#8893BE", "#9B81AC", 
                                "#A67095","#E5F16D", "#B7EE7B")) +
  scale_y_reverse(breaks = seq(from = min(underdogs$Rank), to = max(underdogs$Rank))) +
  geom_image(data = underdogs, aes(x = Year, y = Rank, image = Image)) +
  theme(legend.position = "none",
        panel.grid  = element_blank(), # remove all grid lines
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.5, 6, 0.5, 0.5, "cm"),
        axis.text = element_text(size = 14),
        plot.caption = element_text(hjust = 1, vjust = 0)
  ) +
  geom_text(data = underdogs |> filter(Year == max(Year)), 
            aes(x = Year, y = Rank, label = Breed, size = 14), 
            hjust = 0, 
            nudge_x = 0.3)  +
  labs(title = "Underdogs",
       subtitle = "Rather Niche Dog Breeds from 2014 until 2020",
       caption = "Data: American Kennel Club \n Visualisation: Bettina MJ Kern") +
  coord_cartesian(clip = "off") +
  transition_reveal(as.numeric(Year))

animated_underdogs_finetuned <- animate(animated_underdogs, end_pause = 120, duration = 20, fps = 10, width = 800, height = 700)
# save the animated gif
anim_save("/Users/tinchen/Documents/Datenanalyse/Coding/Projects/TidyTuesday/2022/2022-02-01/underdogs_ranking.gif", animated_underdogs_finetuned)
