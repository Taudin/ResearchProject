#TensorFlow:
#install.packages("tensorflow")
library("tensorflow")
#install_tensorflow(version = "gpu")

#Keras:
#install.packages("keras")
library("keras")
#install_keras(tensorflow = "gpu")
  
load("ProductionCode/train_and_validation_data.Rdata")

lenet_model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                                      input_shape = c(46, 320, 3)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 10, activation = "softmax")

summary(lenet_model)

#Compilation step.
lenet_model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

#Train the model.
history <- lenet_model %>% fit(
  training_data, train_labels,
  epochs = 17, batch_size = 50,
  validation_split = 0.2
)

plot(history)

#Generate predictions on new data.
class_predict <- lenet_model %>% predict_classes(validation_data)
mean(valid_labels == class_predict)
table(valid_labels, class_predict)
