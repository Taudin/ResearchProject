library(imager)
source("ProductionCode/strip_functions.R")

########################
#                      #
# Only one survey type #
#                      #
########################
#Get strips and tensor objects (large integer arrays) created from PDFs.
asci_combo <- prep_asci(image_file = "Training/ASCI surveys/pdfs/asci_circle.pdf", fromfile = TRUE, type = "scan")$tnsr
prepost_math <- prep_prepost(image_file = "Training/pre-post surveys/pdfs/marked_pre_math.pdf", fromfile = TRUE)$tnsr
chc_prepost <- prep_prepost(image_file = "Training/pre-post surveys/pdfs/marked_pre_chc.pdf", fromfile = TRUE)$tnsr
validation_data1 <- prep_asci(image_file = "Validation/ASCI surveys/pdfs/asci_validation_pdf.pdf", fromfile = TRUE,
                              type = "pdf")$tnsr

###########################
#                         #
# 4 page combined surveys #
#                         #
###########################
#Get the path to the combined survey PDF.
#file <- "Training/combined surveys/pdfs/PHSC_train.pdf"
file <- "Validation/combined surveys/pdfs/4pg_validation.pdf"

#Index the appropriate surveys for separation.
num_pages <- pdftools::pdf_info(file)$pages                                         #Total number of pages in PDF file.
asci_pages <- seq(1, num_pages, by = 4)                                             #A vector of indices corresponding to the ASCI surveys (which are the first page of each group).
prepost_pages <- c(seq(2, num_pages, by = 4), seq(3, num_pages, by = 4)) %>% sort() #A vector of indices corresponding to the pre/post surveys (which are the second and third pages of each group).

#Separate the two surveys

#Make the ASCI survey extraction for testing and validation while reading in from the 4 page combined survey PDF.
asci_pdf <- magick::image_read_pdf(file, density = 100, pages = asci_pages)
asci_cimg <- magick::image_convert(asci_pdf, "png") %>% magick2cimg()               #Convert to cimg.
#phsc_asci <- prep_acsci(image_file = asci_cimg, fromfile = FALSE, type = "4pg_combined_scan")$tnsr
validation_data_asci <- prep_asci(image_file = asci_cimg, fromfile = FALSE, type = "4pg_combined_scan")$tnsr

#Make the pre/post survey extraction for testing and validation while reading in from the 4 page combined survey PDF.
prepost_pdf <- magick::image_read_pdf(file, density = 72, pages = prepost_pages)
prepost_cimg <- magick::image_convert(prepost_pdf, "png") %>% magick2cimg()         #Convert to cimg.
#phsc_prepost <- prep_prepost(image_file = prepost_cimg, fromfile = FALSE)$tnsr
validation_data_prepost <- prep_prepost(image_file = prepost_cimg, fromfile = FALSE)$tnsr

#Combine the training and validation data into one array object able to be used by the model.
training_data <- abind::abind(asci_combo, prepost_math, chc_prepost, along = 1)
validation_data <- abind::abind(validation_data1, validation_data_asci, validation_data_prepost, along = 1)
#Remove objects that won't be necessary for the export.
rm(prepost_pdf, prepost_cimg, asci_pdf, asci_cimg)
gc()

####################
#                  #
# Label extraction #
#                  #
####################
#If combining more than one file, you must extract the labels for each file separately and combine them to ensure
#that they are in the correct order.
training_labels <- read.csv("Training/training labels/asci_circle_training_labels.csv")
training_labels1 <- read.csv("Training/training labels/all_training_data_labels.csv")    #extra training labels object to serve exploratory purpose.
validation_labels <- read.csv("Validation/validation labels/validation_labels.csv")

#Prepare the training labels that correspond to the PDFs that will be used for training.
training_labels_one <- prep_labels(label_data = training_labels, file_name = "asci_circle.pdf")$category
training_labels_two <- prep_labels(label_data = training_labels1, file_name = "marked_pre_math.pdf")$category
training_labels_three <- prep_labels(label_data = training_labels1, file_name = "marked_pre_chc.pdf")$category
#labels_four <- prep_labels(label_data = training_labels1, file_name = "PHSC_train.pdf")
#labels_four_asci <- labels_four$category[labels_four$survey == "asci",]
#labels_four_prepost <- labels_four$category[labels_four$survey == "prepost",]
#Combine all training labels into one matrix object.
train_labels <- abind::abind(training_labels_one, training_labels_two, training_labels_three, along = 1)  #Make sure to include objects in line 70 and 71 in this function call's arguments.

#Prepare the validation labels that correspond to the PDFs that will be used for validation.
validation_labels_one <- prep_labels(label_data = validation_labels, file_name = "asci_validation.pdf")$truth
validation_labels_two <- prep_labels(label_data = validation_labels, file_name = "4pg_validation.pdf")$truth
valid_labels <- abind::abind(validation_labels_one, validation_labels_two, along = 1)

#################################################
#                                               #
# Save as R workspace for use in model building #
#                                               #
#################################################
rm(list = ls()[!ls() %in% c("training_data", "train_labels", "validation_data", "valid_labels")])
gc()
base::save.image("ProductionCode/train_and_validation_data.Rdata")
    