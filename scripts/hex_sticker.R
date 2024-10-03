# Install and load necessary packages
if (!require("hexSticker")) install.packages("hexSticker")
library(hexSticker)

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

if (!require("showtext")) install.packages("showtext")
library(showtext)

# Load Google Fonts
font_add_google("Fira Sans", "firasans")
showtext_auto()

# Create a plot symbolizing indexing and data flow
icon_plot <- ggplot() +
  # Draw a stack of index cards or files
  geom_rect(aes(xmin = -0.4, xmax = 0.4, ymin = -0.2, ymax = 0.2), fill = "#FFE082", color = "#F57F17", size = 1) +
  geom_rect(aes(xmin = -0.3, xmax = 0.5, ymin = -0.1, ymax = 0.3), fill = "#FFD54F", color = "#F57F17", size = 1) +
  geom_rect(aes(xmin = -0.2, xmax = 0.6, ymin = 0, ymax = 0.4), fill = "#FFC107", color = "#F57F17", size = 1) +
  # Add arrows to represent saving and retrieving
  geom_curve(aes(x = -0.7, y = -0.5, xend = -0.1, yend = 0), curvature = 0.3,
             arrow = arrow(length = unit(0.15, "inches")), size = 1.2, color = "#D32F2F") +
  geom_curve(aes(x = 0.1, y = 0, xend = 0.7, yend = -0.5), curvature = -0.3,
             arrow = arrow(length = unit(0.15, "inches")), size = 1.2, color = "#1976D2") +
  theme_void() +
  theme_transparent()

# Generate the hex sticker with enhanced visuals
sticker(
  subplot = icon_plot,          # The plot to display on the sticker
  package = "indexr",           # Package name to display
  p_size = 24,                  # Package name font size
  p_family = "firasans",        # Font family for the package name
  s_x = 1,                      # X-position of the subplot
  s_y = 0.8,                    # Y-position of the subplot
  s_width = 1.5,                # Width of the subplot
  s_height = 1.5,               # Height of the subplot
  h_fill = "#ECEFF1",           # Hexagon fill color (light grey)
  h_color = "#455A64",          # Hexagon border color (blue-grey)
  h_size = 1.5,                 # Hexagon border size
  spotlight = TRUE,             # Add a spotlight effect
  l_x = 1,                      # X-position of the spotlight
  l_y = 0.8,                    # Y-position of the spotlight
  l_width = 3,                  # Width of the spotlight
  l_height = 3,                 # Height of the spotlight
  p_color = "#37474F",          # Package name color (dark blue-grey)
  url = "cran.r-project.org",   # Optional URL at the bottom
  u_color = "#37474F",          # URL text color
  u_size = 5,                   # URL text size
  filename = "/man/figures/indexr_hex_sticker.png"  # Output filename
)
