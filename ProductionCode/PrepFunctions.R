library(imager)
library(magick)
library(keras)
library(tidyr)

#Read in the PDF:
image_pdf <- magick::image_read_pdf(path = "Training/ASCI surveys/pdfs/asci_circle.pdf", density = 100)
#Convert to png by way of cimg:
png_cimg <- magick::image_convert(image_pdf, "png") %>% magick2cimg()

#Crop out words surrounding relevant data area:
image_crop <- imsub(png_cimg, y > 284 & y < 1005, x > 279 & x < 600)
#Create strips with one question per strip and stack strips along z-axis:
strips <- imsplit(image_crop, "y", -36) %>% imappend(axis = "z")
#Pad y dimension to match the y-dimension size of the prepost strips (y = 46):
padded_strips <- strips %>% pad(10, axes = "y")
#Turn the padding from black to white so we don't inadvertently train that padded black area:
padded_strips[, c(1:5, 42:46),,] <- 1

#Visually confirm strips:
for (i in 1:10){
  frame(padded_strips, i) %>% plot()
}

#Visually confirm cropped ASCI surveys:
for (i in 1:10){
  frame(image_crop, i) %>% plot()
}
