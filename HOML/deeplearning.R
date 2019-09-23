library(dplyr)
library(keras) 

mnist <- dslabs::read_mnist()
mnist_x <- mnist$train$images
mnist_y <- mnist$train$labels

colnames(mnist_x) <- paste0("V", 1:ncol(mnist_x))
mnist_x <- mnist_x / 255

mnist_y <- to_categorical(mnist_y, 10)

model <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = "relu", input_shape = ncol(mnist_x)) %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 10, activation = "softmax") %>%
  layer_dropout(rate = 0.2)  %>%
  compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )

fit1 <- model %>%
  fit(
    x = mnist_x,
    y = mnist_y,
    epochs = 25,
    batch_size = 128,
    validation_split = 0.2,
    verbose = FALSE
  )

plot(fit1)



