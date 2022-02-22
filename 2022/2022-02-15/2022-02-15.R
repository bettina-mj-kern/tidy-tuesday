# Tidy Tuesday, week 7 -- DuBois Challenge: Recreate one of DuBois' iconic data visualisations for the 1900 Paris world exhibition.
# Bettina M. J. Kern
# 
# I will recreate the illiteracy visualisation (challenge07)
# 
# Packages
library(tidytuesdayR)
library(tidyverse)
library(showtext)

# Data Import ----
illiteracy <- readr::read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge06/data.csv")

# These data are rather unique -- just a percentage for each decade from 1860 until 1900. I expected the complete data that were used to calculate these percentages. But then again, Du Bois' visualisations are unique in themselves. 
# I have never used ggplot2 without having these individual data points, so this is something new and very interesting. Let's get started!

# Format preparation ----
# Du Bois used aesthetic fonts in his creations, so I will replace the ggplot2 standard Arial accordingly using the showtext package. It's the first time I'm using it, so you will have to put up with generous commenting. 

# usage pattern for showtext:
# 1. Load the font
# 2. Open the graphics device
# 3. Specify that showtext be used to render the text
# 4. Do the plotting
# 5. Close graphics device
# 6. Not in the documentation, but was necessary for me: Define the new font family in the theme(text = element_text(family = "new font family name"))
# 
# If you want to use showtext globally, use the showtext_auto() function once, and the graphics devices will automatically use showtext for rendering text.
# If you need a more fine-grained control over which parts of your code should use showtext, the functions showtext_begin() and showtext_end() let you do just that.

# Loading Fonts
# Fonts are loaded via the dependency sysfonts. This happens via the function
# font_add(), where to arguments need to be defined:
# 1. the font family
# 2. the font path (argument: regular =)

# Important here: Just specifying the font name is  not  enough, since the names differ between operating systems. The central information is the font file which provides the actual character glyphs.
# Font files are usually located in some standard system directories.
# 
# Two relevant functions:
# 1. font_paths() gives you the current font paths to the font  files
# 2. font_files() lists the available font files in the current font paths.


# slide 16: https://speakerdeck.com/ajstarks/recreating-the-dubois-data-portraits?slide=16 
# This font family is available on google fonts. The showtext package has a function for this.
font_add_google(name = "Chakra Petch", family = "Chakra Petch")
showtext_auto()
theme_set(theme_minimal())

# Data Preparation ---- 
# With such a little data set there isn't much to do. 
colnames(illiteracy)[colnames(illiteracy) == "Iliteracy Rate"] <- "Rate"

illiteracy$NegRate <- illiteracy$Rate * -1

# Data Visualisation ----

ggplot(data = illiteracy, mapping = aes(y = Rate, x = NegRate)) +
  geom_bar(stat = "identity", fill = "#191a15", width = 1) +
  geom_segment(aes(x = NegRate, 
                   y = Rate,
                   xend = -102,
                   yend = Rate),
               size = 3, colour = "#191a15") +
  geom_segment(aes(x = NegRate+0.8,
                   y = Rate,
                   xend = -101.8,
                   yend = Rate),  
               size = 2.5, colour = "#d4c4b4") +
  scale_x_continuous(breaks = illiteracy$NegRate, 
                     labels = paste(as.character(illiteracy$Rate), 
                                    "%", 
                                    sep = ""),
                     name = NULL) + # removes the variable name 
  scale_y_continuous(breaks = illiteracy$Rate,
                     labels = illiteracy$Date,
                     name = NULL) +
  ggtitle(label = "ILLITERACY.") +
  labs(caption = "Du Bois Challenge 2022 \n Recreator: Bettina MJ Kern") +
  theme(text = element_text(family = "Chakra Petch"))






