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
plate06 <- readr::read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge06/data.csv")

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



# Let us add a font I found on Google that is a little similar to what Du Bois used
font_add_google(name = "Chakra Petch", family = "Chakra Petch")
showtext_auto()
theme_set(theme_minimal(base_family = "Chakra Petch", base_size = 14))

# Data Preparation ---- 
# With such a little data set there isn't much to do. 
colnames(plate06)[colnames(plate06) == "Iliteracy Rate"] <- "Rate"


# Data Visualisation ----
ggplot(data = plate06, aes(x = Rate, y = Date)) +
  # reverse scale to reflect reduction in illiteracy rate
  scale_x_reverse(breaks = plate06$Rate,
                  labels =   ifelse(plate06$Date == "1900", 
                                    "(50%?)", 
                                    paste(as.character(illiteracy$Rate), "%", sep = "")), 
                  name = NULL) +
  # mirror y axis so year decreases going down from the top
  scale_y_reverse(labels = (ifelse(plate06$Date != "1900", plate06$Date, "(1900?)")), name = NULL, breaks = plate06$Date) + 
  # draw vertical segments
  geom_segment(aes(x = Rate, y = Date, xend = Rate, yend = 1940),  
               lineend = "square", lwd = 3.6) +
  # draw horizontal line segments
  geom_segment(aes(x = 100, y = Date, xend = Rate, yend = Date),
               lineend = "square", lwd = 3.6) +
  # draw second, slightly smaller horizontal segment in different colour
  geom_segment(aes(x = 100 + 0.05, y = Date, xend = Rate - 0.05, yend = Date),
             lineend = "round", lwd = 3, col = "#dccab2") +
  # expand y axis to make plot appear a little slimmer
  expand_limits(y = c(1860, 1930)) +
  # add title and caption
  labs(title = "\nILLITERACY.", caption = "\n#DuBoisChallenge2022 \n Recreation by @bettina_mj_kern") +
  theme(panel.grid = element_blank(), # remove grid lines
        plot.background = element_rect(fill = "#D1C0A8"), # background colours
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # arrange title
        plot.caption = element_text(hjust = 0.5, color = "#777777", family = "Chakra Petch"), # tune caption
        text = element_text(family = "Chakra Petch", size = 14), # text settings
        plot.margin = margin(0, 6, 0.5, 6, "cm")) # stretch the plot slim

ggsave(filename = "2022/2022-02-15/plate6-recreation.png")












