# https://github.com/topepo/caret/blob/master/models/files/mlpKerasDecayCost.R

mlp_bow <- list(
  label = "Multilayer Perceptron Network with Dropout",
  library = "keras",
  loop = NULL,
  type = c("Classification"),
  #-------------------------------------------------------------------------------
  # parameter
  #-------------------------------------------------------------------------------
  parameters = data.frame(
    parameter = c(
      "units_1", "units_2", "units_3",
      "dropout_1", "dropout_2",
      "batch_size", "epochs", "patience",
      "lr", "decay",
      "activation"
    ),
    class = c(rep("numeric", 10), "character"),
    label = c(
      "#Hidden Units_1", "#Hidden Units_2", "#Hidden Units_3",
      "Dropout Rate_1", "Dropout Rate_2",
      "Batch Size", "epochs", "patience", "Learning Rate",
      "Learning Rate Decay",
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
        units_1 = ((1:len) * 2) - 1,
        units_2 = ((1:len) * 2) - 1,
        units_3 = ((1:len) * 2) - 1,
        epochs = 10,
        patience = 10,
        dropout_1 = seq(0, .7, length = len),
        dropout_2 = seq(0, .7, length = len),
        batch_size = 32,
        lr = 2e-6,
        decay = 0,
        activation = "relu"
      )
    } else {
      n <- nrow(x)
      out <- data.frame(
        units_1 = sample(2:20, replace = TRUE, size = len),
        units_2 = sample(2:20, replace = TRUE, size = len),
        units_3 = sample(2:20, replace = TRUE, size = len),
        dropout_1 = runif(len, max = .7),
        dropout_2 = runif(len, max = .7),
        epochs = 10,
        patience = 10,
        batch_size = floor(n * runif(len, min = .1)),
        lr = runif(len),
        decay = 10^runif(len, min = -5, 0),
        activation = sample(
          afuncs,
          size = len,
          replace = TRUE
        )
      )
    }
    out
  },
  #-------------------------------------------------------------------------------
  # fit
  #-------------------------------------------------------------------------------

  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    require(dplyr)
    K <- keras::backend()
    K$clear_session()
    if (!is.matrix(x)) x <- as.matrix(x)
    y <- class2ind(y)

    model <- keras::keras_model_sequential() %>%
      # FULLY CONNECTED LAYER
      #-----------------
      keras::layer_dense(
        name = "fully_connected_layer_1",
        units = param$units_1,
        activation = as.character(param$activation),
        input_shape = ncol(x)
      ) %>%
      # DROP-OUT LAYER
      #-----------------
      keras::layer_dropout(
        name = "drop_out_layer_1",
        rate = param$dropout_1,
        seed = 42
      ) %>%
      # FULLY CONNECTED LAYER
      #-----------------
      keras::layer_dense(
        name = "fully_connected_layer_2",
        units = param$units_2,
        activation = as.character(param$activation)
      ) %>%
      # DROP-OUT LAYER
      #-----------------
      keras::layer_dropout(
        name = "drop_out_layer_2",
        rate = param$dropout_2,
        seed = 42
      ) %>%
      # FULLY CONNECTED LAYER
      #-----------------
      keras::layer_dense(
        name = "fully_connected_layer_3",
        units = param$units_3,
        activation = as.character(param$activation)
      ) %>%
      # FULLY CONNECTED LAYER
      #-----------------
      keras::layer_dense(
        # units = 1
        units = 2,
        # activation = "sigmoid"
        activation = "softmax"
      )


    #-------------------------------------------------------------------------------
    # compile the model
    #-------------------------------------------------------------------------------
    
    model %>%
      keras::compile(
        # loss = "mean_squared_error",
        loss = "binary_crossentropy",
        optimizer = keras::optimizer_adam(
          lr = param$lr,
          beta_1 = 0.9, # default
          beta_2 = 0.999, # default
          decay = param$decay
        ),
        metrics = "accuracy"
      )

    summary(model)

    #-------------------------------------------------------------------------------
    # callbacks
    #-------------------------------------------------------------------------------


    callbacks <- list(
      keras::callback_early_stopping(
        patience = param$patience,
        monitor = "accuracy"
        # monitor = "val_acc"
      ),
      keras::callback_reduce_lr_on_plateau(
        # monitor = "val_loss",
        monitor = "loss",
        factor = 0.1,
        verbose = 1,
        patience = floor(param$patience / 2),
        mode = "auto",
        min_delta = 1e-04,
        cooldown = 0,
        min_lr = 0
      )
    )

    #-------------------------------------------------------------------------------
    # fit the model
    #-------------------------------------------------------------------------------

    model %>% keras::fit(
      x = x,
      y = y,
      batch_size = param$batch_size,
      epochs = param$epochs,
      callbacks = callbacks,
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
  sort = function(x) x[order(x$units_1, -x$dropout_1), ],
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
