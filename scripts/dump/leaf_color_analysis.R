# setup -------------------------------------------------------------------

library(magick)
library(tidyverse)

# crop to content and save intermediate -----------------------------------

# fasy01_40 <- magick::image_read("data/flurometer-heat/fasy01_40.tif")
# fasy01_40 %>% image_transparent("white", fuzz = 40) %>% image_trim() %>% image_write("data/flurometer-heat/test.png")

image_path <- "data/flurometer-heat/leaf_scans/cam4/"

(images <- list.files(image_path, pattern = "\\.TIF", full.names = T))

for(i in 1:length(images)){
  images[i] %>% 
    image_read() %>% 
    image_transparent("white", fuzz = 30) %>% 
    image_trim() %>% 
    image_write(paste0("data/flurometer-heat/leaf_scans/cropped/cam4_", images[i] %>% 
                         str_replace("\\_0", "0") %>% 
                         str_replace("\\_(?=\\d{2})", "\\_t") %>% 
                         str_remove("deg") %>% 
                         str_remove("\\d{2}min") %>% 
                         tolower() %>% 
                         str_remove("data/flurometer-heat/leaf_scans/cam4//")))
  print(i)
}

images[i] %>% str_replace("\\_(?=\\d{2})", "\\_t")

# greenness ---------------------------------------------------------------

f.image_to_rgb <- function(image){
  image_array = image %>% 
    image_read() %>% 
    image_data("rgb") %>% 
    as.integer()
  df = data.frame(
    r = matrix(image_array[,,1]),
    g = matrix(image_array[,,2]),
    b = matrix(image_array[,,3])
  )
  df = df %>%  
    mutate(sum = rowSums(df))%>% 
    filter(sum != 765) %>% 
    mutate(green_perc = g / (r + g + b),
           tgi = ((g - r) + (g - b)) / 2) 
  return(df)
}

image_path <- "data/flurometer-heat/leaf_scans/cropped/"

images <- list.files(image_path, pattern = "\\.tif", full.names = T)

df.images <- images %>% str_remove("data/flurometer-heat/leaf_scans/cropped/") %>% 
  as.data.frame() %>% 
  rename(image = ".") %>% 
  nest(image_data = -image) %>% 
  mutate(image_paths = images,
         campaign = image %>% str_extract("(?<=cam)\\d") %>% as.factor(),
         temp = image %>% str_extract("t\\d{2}"),
         sample_id = image %>% str_extract("fasy\\d{2}|frex\\d{2}"),
         species = sample_id %>% str_extract("fasy|frex"),
         temp = temp %>% str_extract("\\d+") %>% as.numeric()
         )

df.images <- df.images %>%
  mutate(image_data = map(image_paths, ~ .x %>%
                            f.image_to_rgb()))

df.images_unnest <- df.images %>% 
  unnest(cols = image_data) 

df.images_unnest %>% 
  group_by(species, sample_id, campaign, temp) %>% 
  summarise(mean_greenness = mean(green_perc),
            median_greenness = median(green_perc),
            mean_tgi = mean(tgi),
            median_tgi = median(tgi)
  ) %>% 
  ggplot() +
  geom_point(aes(x = temp, y = mean_tgi, col = sample_id)) +
  # geom_point(aes(x = temp, y = median_tgi), col = "black") +
  facet_wrap(campaign~species)


# leaf area ---------------------------------------------------------------
f.pixel_count_to_cm2 = function(n_pixels) {
  area_cm2 = n_pixels / dpi / inch2_to_cm2
  return(area_cm2)
}
dpi = 150
inch2_to_cm2 = 6.4516

df.images = df.images %>% 
  mutate(leaf_area = NA)

for(i in 1:length(df.images$image)) {
  pixels = df.images$image_paths[i] %>%
    image_read() %>% 
    image_quantize(colorspace = "gray") %>% 
    image_transparent("white", fuzz = 50) %>% 
    image_trim() %>% 
    image_level(black_point = 50) %>% 
    as.integer()
  n_black_pixels = pixels[pixels != 0] %>% length()
  df.images$leaf_area[i] = f.pixel_count_to_cm2(n_black_pixels)
  print(i)
}

df.images %>% 
  select(species, campaign, sample_id, leaf_area, temp) %>% 
  ggplot() +
  geom_boxplot(aes(x = sample_id, y = leaf_area))


test <- images[i] %>%
  image_read() %>% 
  image_quantize(colorspace = "gray") %>% 
  image_transparent("white", fuzz = 50) %>% 
  image_trim() %>% 
  image_level(black_point = 50) %>% 
  as.integer()
  
test[test == 0] %>% length()
test[test != 0] %>% length()


f.pixel_count_to_cm2 = function(n_pixels) {
  area_cm2 = n_pixels / dpi / inch2_to_cm2
  return(area_cm2)
}

f.pixel_count_to_cm2(test[test != 0] %>% length())

# tutorial ----------------------------------------------------------------

fasy01_40 <- magick::image_read("data/flurometer-heat/cam2/scans/fasy01_40deg_15min.TIF")
image <- image_transparent(fasy01_40, 
                "black", 
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
  filter("sum" != 765) |> 
  pivot_longer(cols = c("r", "g", "b"), names_to = "channel", values_to = "value")

ggplot(df_long |> filter(value != 255), aes(x = value, fill = channel)) +
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



