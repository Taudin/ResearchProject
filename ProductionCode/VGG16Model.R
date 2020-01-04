#TensorFlow:
#install.packages("tensorflow")
library("tensorflow")
#install_tensorflow(version = "gpu")

#Keras: 
#install.packages("keras")
library("keras")
#install_keras(tensorflow = "gpu")

load("ProductionCode/train_and_validation_data.Rdata")

#Instantiating the VGG16 convolutional base:
conv_base <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(46, 320, 3)
)

#This is the architecture of the VGG16 convolutional base:
conv_base

#Adding a densely connected classifier on top of the convolutional base:
model <- keras_model_sequential() %>% 
  conv_base %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 10, activation = "softmax")

#Here's what the model looks like now:
model

cat("This is the number of trainable weights before freezing the conv base:",
    length(model$trainable_weights), "\n")

freeze_weights(conv_base)

cat("This is the number of trainable weights after freezing the conv base:",
    length(model$trainable_weights), "\n")

#Training the model with a frozen convolutional base.
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  training_data, train_labels, 
  epochs = 75, batch_size = 25,
  validation_split = 0.2
)

#Unfreezing previously frozen layers.
unfreeze_weights(conv_base, from = "block3_conv1")

#Fine-tuning the model:
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(), 
  metrics = ("accuracy")
)

history <- model %>% fit(
  training_data, train_labels,
  epochs = 75, batch_size = 25,
  validation_split = 0.2
)
