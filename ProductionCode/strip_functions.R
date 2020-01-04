library(imager)
library(magick)
library(keras)
library(tidyr)

#If reading in from PDF set image_file = path to location on disk and fromfile = TRUE.
#If this function is being called from another file where the PDF has already been turned into a cimg, then set 
#image_file = object_name, and fromfile = FALSE.

#image_file <- ""
#fromfile <- TRUE
#type <- "4page_combined"

################################################################################################################
#                                                                                                              #
#                                            prep_asci                                                         #
#                                                                                                              #
# This function can read in ASCI surveys, whether they are the stand-alone ASCI PDFs located in a directory    #
# somewhere on disk or they are part of the 4 page combined surveys (in which they are the first page) and     #
# already converted to cimg by the preprocess.R script, and returns tensor objects for the model after slicing #
# the survey into one strip per item.                                                                          #
################################################################################################################
prep_asci <- function(image_file, fromfile = FALSE, type = "scan"){
  #If the ASCI PDF has been scanned in and is stored in a directory somewhere, e.g., a single ASCI survey PDF.
  if (fromfile){
    #Read in the PDF.
    image_pdf <- magick::image_read_pdf(image_file, density = 100)
    #Convert to png by way of cimg.
    png_cimg <- magick::image_convert(image_pdf, "png") %>% magick2cimg()
  } else {
    #Doesn't need the above conversion since it's already been converted into a cimg, e.g., it's a part of the 4 page combined surveys.
    png_cimg <- image_file 
  }
  
  #If the ASCI PDF has been scanned in and is stored in a directory somewhere-- a continuation of the above if statement code block.
  if (type == "scan"){
    #Crop out words surrounding relevant data area:
    image_crop <- imsub(png_cimg, y > 284 & y < 1005, x > 279 & x < 600)
    #Create strips with one question per strip and stack strips along z-axis:
    strips <- imsplit(image_crop, "y", -36) %>% imappend(axis = "z")
    #Pad y dimension to match the y-dimension size of the prepost strips (y = 46):
    padded_strips <- strips %>% pad(10, axes = "y")
    #Turn the padding from black to white so we don't inadvertently train that padded black area:
    padded_strips[, c(1:5, 42:46),,] <- 1
  }
  
  #If this is the ASCI survey part from the 4 page combined survey.
  if (type %in% c("pdf", "4pg_combined_scan")){
    #Crop out words surrounding relevant data area.
    image_crop <- imsub(png_cimg, y > 264 & y < 1025, x > 279 & x < 600)
    #Create strips with one question per strip and stack strips along z-axis.
    strips <- imsplit(image_crop, "y", -38) %>% imappend(axis = "z")
    #Pad y dimension to match the y dimension size of the prepost strips (y = 46).
    padded_strips <- strips %>% pad(8, axes = "y")
    #Turn the padding from black to white so we don't inadvertently train that padded black area.
    padded_strips[, c(1:4, 43:46),,] <- 1
  }
  
  #Convert to tensor format.
  z <- dim(padded_strips)[3]
  y <- dim(padded_strips)[2]
  x <- dim(padded_strips)[1]
  d <- dim(padded_strips)[4]
  tnsr <- array(NA, dim = c(z, y, x, d))
  
  return(list(tnsr = tnsr, strips = padded_strips))
}

########################################################################################################################
#                                                                                                                      #
#                                                   prep_prepost                                                       #
#                                                                                                                      #
# This function reads in the prepost survey whether it is a stand-alone PDF or a part of the 4 paged combined surveys, #
# makes strips out of each item and returns it as tensor object to be used in a model. It can read in prepost survey   #
#  PDFs whether they come from a directory stored on disk or already converted into cimg objects.                      #
########################################################################################################################
prep_prepost <- function(image_file, fromfile = FALSE){
  #
  if(fromfile){
    survey_pdf <- magick::image_read_pdf(image_file, density = 72)
    png_cimg <- magick::image_convert(survey_pdf, "png") %>% magick2cimg()
  } else {
    #
    png_cimg <- image_file
  }
  
  num_pages <- dim(png_cimg)[3]
  
  #Trin excess, including left and right words.
  first_page <- imsub(frame(png_cimg, seq(1, num_pages, 2)), y > 184 & y < 737, x > 200 & x < 451)
  second_page <- imsub(frame(png_cimg, seq(2, num_pages, 2)), y > 61 & y < 430, x > 200 & x < 451)
  
  #Make strips with one go and stack along the z-axis.
  first_page_long <- imsplit(first_page, axis = "y", -46) %>% imappend(axis = "z")
  second_page_long <- imsplit(second_page, axis = "y", -46) %>% imappend(axis = "z")
  all_pages_long <- imappend(list(first_page_long, second_page_long), axis = "z")
  
  #Pad along th x-axis so we can merge along the x-axis.
  padded_strips <- all_pages_long %>% pad(70, axes = "x")
  padded_strips[c(1:35, 286:320),,,] <- 1
  
  #Convert to tensor object.
  z <- dim(padded_strips)[3]
  y <- dim(padded_strips)[2]
  x <- dim(padded_strips)[1]
  d <- dim(padded_strips)[4]
  tnsr <- array(NA, dim = c(z, y, x, d))
  
  return(list(tnsr = tnsr, strips = padded_strips))
}

####################################################################################################################
#                                                                                                                  #
#                                                prep_labels                                                       #
#                                                                                                                  #
# This function takes in a read csv file and a PDF file, matches up the labels to that particular PDF, categorizes #
# the labels corresponding to the items answered in the PDF survey, and returns those labels to preprocess.R.      #
####################################################################################################################
prep_labels <- function(label_data, file_name){
  #Match up the particular data from the .csv file rows to the appropriate PDF file.
  labels <- label_data[label_data$file == file_name,]
  #Gather takes multiple columns and collapses into key = Q, value = truth pairs. Basically, the columns q1-q20 are turned
  #into rows-- each item answer becomes a record with the variable names Q (q1-q20), and truth (that particular answer) as 
  #a dataframe object.
  labels <- tidyr::gather(labels, Q, truth, q1:q20)
  #One-hot encodes each record's answer as a new variable named category.
  labels$category <- keras::to_categorical(labels$truth, num_classes = 10)
  return(labels)
}


  