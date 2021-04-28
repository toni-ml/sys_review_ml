# https://github.com/topepo/caret/blob/master/models/files/mlpKerasDecayCost.R

cnn_2mlp <- list(
  label = "CNN",
  library = "keras",
  loop = NULL,
  type = c("Classification"),
  #-------------------------------------------------------------------------------
  # parameter
  #-------------------------------------------------------------------------------
  parameters = data.frame(
    parameter = c(
      "filters_conv_1", "filters_size_conv_1", 
      "units_1",
      "batch_size", "epochs", "patience", 
      "lr", "class_weights", 
      "activation"
    ),
    class = c(rep("numeric", 8), rep("character",1)),
    label = c(
      "#filter for CNN-layer 1", "Size for CNN-layer 1", 
      "#Hidden Units_1",
      "Batch Size", "epochs", "patience", 
      "Learning Rate", "Class weights", 
      "Activation Function"
    )
  ),
  #-------------------------------------------------------------------------------
  # grid
  #-------------------------------------------------------------------------------
  grid = function(x, y, len = NULL, search = "grid") {
    afuncs <- c("sigmoid", "relu", "tanh")
    if (search == "grid") {
      out <- expand.grid(
        filters_conv_1 = 50,
        filters_size_conv_1 = 3,
        units_1 = ((1:len) * 2) - 1,
        lr = 2e-6,
        activation = "relu",
        patience = 10,
        batch_size = 20,
        epochs = 10,
        class_weights = 1
      )
    } else {
      n <- nrow(x)
      out <- data.frame(
        filters_conv_1 = 50,
        filters_size_conv_1 = 3,
        units_1 = sample(2:20, replace = TRUE, size = len),
        lr = runif(len),
        activation = sample(
          afuncs,
          size = len,
          replace = TRUE
        ),
        patience = 10,
        batch_size = floor(n * runif(len, min = .1)),
        epochs = 10,
        class_weights = 1
      )
    }
    out
  },
  #-------------------------------------------------------------------------------
  # fit the model
  #-------------------------------------------------------------------------------

  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    require(dplyr)

    K <- keras::backend()
    K$clear_session()
    if (!is.matrix(x)) x <- as.matrix(x)
    y <- class2ind(y)

    # SEQUENTIAL MODEL
    #-----------------
    model <- keras::keras_model_sequential() %>%
      # EMBEDDING LAYER
      #-----------------
    keras::layer_embedding(
      name = "word_embedding_layer_1",
      input_dim = max_features, # number of vocabulary
      input_length = pad_max_length, # length of max words in train data - like input shape if the first layer is a dense layer
      output_dim = embedding_dims, # vector space of ? dimensions which word will be embedded
      weights = list(embedding_weights),
      trainable = TRUE
    ) %>% # embedding layer can't be updated
      
      # CONVULUTIONAL LAYER
      #-----------------
    keras::layer_conv_1d(
      name = "conv1d_layer_1",
      filters = param$filters_conv_1,
      kernel_size = param$filters_size_conv_1,
      padding = "valid", # means "no padding"
      activation = "relu",
      use_bias = FALSE,
      strides = 1
    ) %>% 
      
      # MAX POOLING LAYER
      #-----------------
    keras::layer_global_max_pooling_1d(name = "max_pooling_layer_1") %>%
      
      # FULLY CONNECTED LAYER
      #-----------------
    keras::layer_dense(
      name = "fully_connected_layer_1",
      units = param$units_1,
      activation = as.character(param$activation)
    ) %>%
      
      # FULLY CONNECTED LAYER
      #-----------------
    keras::layer_dense(
      name = "fully_connected_layer_2",
      # units = 1,
      units = 2,
      # activation = "sigmoid"
      activation = "softmax"
    )
    

    # compile the model
    #-------------------
    
    model %>%
      keras::compile(
        # loss = "mean_squared_error",
        loss = "binary_crossentropy",
        optimizer = keras::optimizer_adam(
          lr = param$lr,
          beta_1 = 0.9, # default
          beta_2 = 0.999, # default
          decay = 0
        ),
        metrics = "accuracy"
      )
    
    summary(model)

    # callbacks
    #--------------------
    
    callbacks <- list(
      keras::callback_early_stopping(
        patience = param$patience,
        monitor = "acc"
        # monitor = "val_acc"
      ),
      keras::callback_reduce_lr_on_plateau(
        monitor = "loss",
        # monitor = "val_loss",
        factor = 0.1,
        verbose = 1,
        patience = floor(param$patience / 2),
        mode = "auto",
        min_delta = 1e-04,
        cooldown = 0,
        min_lr = 0
      )
    )
    
    # fit the model
    #----------------------

    model %>% keras::fit(
      x = x,
      y = y,
      batch_size = param$batch_size,
      epochs = param$epochs,
      callbacks = callbacks,
      class_weight = list("0" = 1, "1" = param$class_weights),
      verbose = 1,
      ...
    )
    if (last) {
      model <- keras::serialize_model(model)
    }
    list(object = model)
  },
  
  
  #-------------------------------------------------------------------------------
  # predict
  #-------------------------------------------------------------------------------

  predict = function(modelFit, newdata, submodels = NULL) {
    if (inherits(modelFit$object, "raw")) {
      modelFit$object <- keras::unserialize_model(modelFit$object)
    }
    if (!is.matrix(newdata)) {
      newdata <- as.matrix(newdata)
    }
    out <- predict(modelFit$object, newdata)
    ## check for model type
    if (ncol(out) == 1) {
      out <- out[, 1]
    } else {
      out <- modelFit$obsLevels[apply(out, 1, which.max)]
    }
    out
  },
  prob = function(modelFit, newdata, submodels = NULL) {
    if (inherits(modelFit$object, "raw")) {
      modelFit$object <- keras::unserialize_model(modelFit$object)
    }
    if (!is.matrix(newdata)) {
      newdata <- as.matrix(newdata)
    }
    out <- predict(modelFit$object, newdata)
    colnames(out) <- modelFit$obsLevels
    as.data.frame(out)
  },
  varImp = NULL,
  tags = c("Neural Network"),
  sort = function(x) x[order(x$units_1), ],
  notes = paste(
    "After `train` completes, the keras model object is serialized",
    "so that it can be used between R session. When predicting, the",
    "code will temporarily unsearalize the object. To make the",
    "predictions more efficient, the user might want to use ",
    "`keras::unsearlize_model(object$finalModel$object)` in the current",
    "R session so that that operation is only done once.",
    "Also, this model cannot be run in parallel due to",
    "the nature of how tensorflow does the computations.",

    "Unlike other packages used by `train`, the `dplyr`",
    "package is fully loaded when this model is used."
  ),
  check = function(pkg) {
    testmod <- try(keras::keras_model_sequential(),
      silent = TRUE
    )
    if (inherits(testmod, "try-error")) {
      stop("Could not start a sequential model. ",
        "`tensorflow` might not be installed. ",
        "See `?install_tensorflow`.",
        call. = FALSE
      )
    }
    TRUE
  }
)
