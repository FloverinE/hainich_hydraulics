library(magick)
library(tidyverse)
fasy01_40 <- magick::image_read("data/fasy01_40.tif")
image <- image_transparent(fasy01_40, 
                "white", 
                fuzz = 30)

image |> as.data.frame() 

# Let's assume `image` is your magick image object

# Convert the image to RGB and extract the color data
color_data <- image_data(image, channels = "RGB")

# A magick `image_data` object is a 3-dimensional array: height x width x color
# Convert this to a data frame for ease of use
color_df <- as.data.frame.table(color_data)

unique(color_df$Freq)

# Aggregate the data by color, counting the number of occurrences of each
color_counts <- aggregate(. ~ Var3, color_df, length)

# Convert the color codes (1 for red, 2 for green, 3 for blue) to names
color_counts$Var3 <- c("Red", "Green", "Blue")[color_counts$Var3]

# Plot a histogram with the color data
ggplot(color_counts, aes(x = Var1, y = Freq, fill = Var3)) + 
  geom_histogram(binwidth = 10, position = "identity", alpha = 0.5)

image_df <- image_quantize(image)


# Convert image to array and reshape
image_array <- as.integer(image_data(image, 'rgb'))


# Get the dimensions of the array
dims <- dim(image_array)

# Reshape the array into a 2D matrix where each row is a pixel and columns are R, G, B
df <- data.frame(
  r = matrix(image_array[,,1]),
  g = matrix(image_array[,,2]),
  b = matrix(image_array[,,3])
)
df_long <- df |> mutate(sum = rowSums(df))

df_long <- df |> 
  filter(sum != 765) |> 
  pivot_longer(cols = c("r", "g", "b"), names_to = "channel", values_to = "value")

ggplot(df_long, aes(x = value, fill = channel)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("red", "green", "blue")) +
  labs(title = "Color Histogram", x = "Color Value", y = "Frequency") +
  theme_minimal()


# TGI ---------------------------------------------------------------------

## wavelength centers
r480 = 480
r550 = 550
r670 = 670

f_tgi = function(row) {
  tgi = ((row[1] - row[2]) * (r480 - r550) - (row[1] - row[3]) * (r480 - r670)) / 2
  return(tgi)
}

leaf1 <- df |> 
  filter(sum != 765) 

f_tgi = function(row) {
  tgi = ((row[2] - row[1]) + (row[2] - row[3])) / 2
  return(tgi)
}

f_tgi(c(0, 255, 0))

plot(1,1, col = rgb(255, 255, 100, maxColorValue = 255), pch = 19, cex = 10)
plot(1,1, col = rgb(255, 255, 190, maxColorValue = 255), pch = 19, cex = 10)
