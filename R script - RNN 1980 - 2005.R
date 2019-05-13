# PACKAGES  --------------------------------------------------------------------

# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
library(cowplot)
library(ggplot2)

# Preprocessing
library(recipes)

# Sampling / Accuracy
library(rsample)
library(yardstick) 

# Modeling
library(keras)
library(tfruns)

library(dplyr)


# DATA -------------------------------------------------------------------------

dataco2 <- read.table("C:/Users/amber/Downloads/co2_mm_gl.txt", quote="\"")
dataco2$date <- paste(dataco2$V1, dataco2$V2, sep = "/")
dataco2[,2:3] <- NULL
dataco2$V5 <- NULL
names(dataco2) <- c('year','CO2', 'index')
dataco2$index <- as.Date(paste(dataco2$index, "1", sep = "/", format = "%Y/%m/$d"))

data <- dataco2[dataco2$year < 2005,]

data <- data %>%
  tk_tbl() %>%
  mutate(index = as_date(index)) %>%
  as_tbl_time(index = index)



# PLOT DATASETS ----------------------------------------------------------------
library(viridis)

p1 <- data %>%
  ggplot() +
  geom_point(aes(index, CO2, color = CO2), alpha = 0.9, size = 2) +
  scale_color_viridis(discrete = FALSE, option = "D") +
  scale_x_date(name = "Year") +
  labs(
    title = "From 1980 to 2004 (Full Data Set)"
  )
p1


p2 <- data[data$year > 2001, ] %>%
  ggplot() +
  geom_point(aes(index, CO2, color = CO2), alpha = 0.9, size = 3) +
  geom_line(aes(index, CO2, color = CO2), alpha = 0.5, size = 2) +
  scale_color_viridis(discrete = FALSE, option = "D") +
  scale_x_date(name = "Year") +
  labs(
    title = "From 2002 to 2005 (Monthly Changes)"
  )


# BACKTESTING ------------------------------------------------------------------

rolling_origin_resamples <- rolling_origin(
  data,
  initial    = 12 * 15,
  assess     = 12 * 4,
  cumulative = FALSE,
  skip = 12
)

rolling_origin_resamples 


# VISUALIZE SPLITS -------------------------------------------------------------
#plotting function for a single split
plot_split <- function(split, expand_y_axis = TRUE, alpha = 1, size = 1, base_size = 14) {
  
  #manipulate data
  train_tbl <- rsample::training(split) %>% 
    tibble::add_column(key = "training")
  
  test_tbl <- rsample::testing(split) %>% 
    tibble::add_column(key = "testing")
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>% 
    tibbletime::as_tbl_time(index = index) %>% 
    mutate(key = forcats::fct_relevel(key, "training", "testing"))
  
  #collect attributes
  train_time_summary <- train_tbl %>% 
    timetk::tk_index() %>% 
    timetk::tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>% 
    timetk::tk_index() %>% 
    timetk::tk_get_timeseries_summary()
  
  #visualize
  g <- data_manipulated %>% 
    ggplot(aes(x = index, y = CO2, color = key)) + 
    geom_line(size = size, alpha = alpha) + 
    tidyquant::theme_tq(base_size = base_size) + 
    tidyquant::scale_color_tq() + 
    labs(
      title = glue::glue("Split: {split$id}"),
      subtitle = glue::glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) + 
    theme(legend.position = "none") 
  
  if(expand_y_axis) {
    time_summary <- data %>% 
      timetk::tk_index() %>% 
      timetk::tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(time_summary$start,
                              time_summary$end))
  }
  
  return(g)
}


rolling_origin_resamples$splits[[6]] %>%
  plot_split(expand_y_axis = TRUE) +
  theme(legend.position = "bottom")


# Plotting function that scales to all splits 
plot_sampling_plan <- function(sampling_tbl, expand_y_axis = TRUE, 
                               ncol = 3, alpha = 1, size = 1, base_size = 14, 
                               title = "Sampling Plan") {
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map(splits, plot_split, 
                          expand_y_axis = expand_y_axis,
                          alpha = alpha, base_size = base_size))
  
  # Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  p_title <- ggdraw() + 
    draw_label(title, size = 14, fontface = "bold", 
               colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, 
                 rel_heights = c(0.05, 1, 0.05))
  
  g
}


rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = T, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Samples")



# LSTM MODEL -------------------------------------------------------------------

example_split    <- rolling_origin_resamples$splits[[6]]
example_split_id <- rolling_origin_resamples$id[[6]]

df_trn <- analysis(example_split)[1:140, , drop = FALSE]
df_val <- analysis(example_split)[141:180, , drop = FALSE]
df_tst <- assessment(example_split)

df <- bind_rows(
  df_trn %>% add_column(key = "training"),
  df_val %>% add_column(key = "validation"),
  df_tst %>% add_column(key = "testing")
) %>%
  as_tbl_time(index = index)

df

# DATA PREPROCESSING -----------------------------------------------------------
rec_obj <- recipe(CO2 ~ ., df) %>%
  step_sqrt(CO2) %>%
  step_center(CO2) %>%
  step_scale(CO2)%>%
  prep()

df_processed_tbl <- bake(rec_obj, df)

df_processed_tbl

center_history <- rec_obj$steps[[2]]$means
scale_history  <- rec_obj$steps[[3]]$sds

c("center" = center_history, "scale" = scale_history)

# RESHAPE DATASET --------------------------------------------------------------

n_timesteps <- 12
n_predictions <- n_timesteps
batch_size <- 5

# functions used
build_matrix <- function(tseries, overall_timesteps) {
  t(sapply(1:(length(tseries) - overall_timesteps + 1), function(x) 
    tseries[x:(x + overall_timesteps - 1)]))
}

reshape_X_3d <- function(X) {
  dim(X) <- c(dim(X)[1], dim(X)[2], 1)
  X
}

# extract values from data frame
train_vals <- df_processed_tbl %>%
  filter(key == "training") %>%
  select(CO2) %>%
  pull()
valid_vals <- df_processed_tbl %>%
  filter(key == "validation") %>%
  select(CO2) %>%
  pull()
test_vals <- df_processed_tbl %>%
  filter(key == "testing") %>%
  select(CO2) %>%
  pull()


# build the windowed matrices
train_matrix <-
  build_matrix(train_vals, n_timesteps + n_predictions)
valid_matrix <-
  build_matrix(valid_vals, n_timesteps + n_predictions)
test_matrix <- build_matrix(test_vals, n_timesteps + n_predictions)

# separate matrices into training and testing parts
# also, discard last batch if there are fewer than batch_size samples
# (a purely technical requirement)
X_train <- train_matrix[, 1:n_timesteps]
y_train <- train_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_train <- X_train[1:(nrow(X_train) %/% batch_size * batch_size), ]
y_train <- y_train[1:(nrow(y_train) %/% batch_size * batch_size), ]

X_valid <- valid_matrix[, 1:n_timesteps]
y_valid <- valid_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_valid <- X_valid[1:(nrow(X_valid) %/% batch_size * batch_size), ]
y_valid <- y_valid[1:(nrow(y_valid) %/% batch_size * batch_size), ]

X_test <- test_matrix[, 1:n_timesteps]
y_test <- test_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_test <- X_test[1:(nrow(X_test) %/% batch_size * batch_size), ]
y_test <- y_test[1:(nrow(y_test) %/% batch_size * batch_size), ]

# add on the required third axis
X_train <- reshape_X_3d(X_train)
X_valid <- reshape_X_3d(X_valid)
X_test <- reshape_X_3d(X_test)

y_train <- reshape_X_3d(y_train)
y_valid <- reshape_X_3d(y_valid)
y_test <- reshape_X_3d(y_test)


# BUILD LSTM MODEL -------------------------------------------------------------
FLAGS <- flags(
  # Should we use several layers of LSTM?
  # Again, just included for completeness, it did not yield any superior 
  # performance on this task.
  # This will actually stack exactly one additional layer of LSTM units.
  flag_boolean("stack_layers", TRUE),
  # number of samples fed to the model in one go
  flag_integer("batch_size", 5),
  # size of the hidden state, equals size of predictions
  flag_integer("n_timesteps", 12),
  # how many epochs to train for
  flag_integer("n_epochs", 100),
  # fraction of the units to drop for the linear transformation of the inputs
  flag_numeric("dropout", 0),
  # fraction of the units to drop for the linear transformation of the 
  # recurrent state
  flag_numeric("recurrent_dropout", 0.1),
  # loss function. Found to work better for this specific case than mean
  # squared error
  flag_string("loss", "logcosh"),
  # optimizer
  flag_string("optimizer_type", "SGD"),
  # size of the LSTM layer
  flag_integer("n_units", 256),
  # learning rate
  flag_numeric("lr", 0.005),
  # parameter to the early stopping callback
  flag_integer("patience", 10)
)

# the number of predictions we'll make equals the length of the hidden state
n_predictions <- FLAGS$n_timesteps
# how many features = predictors we have
n_features <- 1
# just in case we wanted to try different optimizers, we could add here
optimizer <- FLAGS$optimizer_type
# callbacks to be passed to the fit() function
# We just use one here: we may stop before n_epochs if the loss on the
# validation set does not decrease (by a configurable amount, over a 
# configurable time)
callbacks <- list(
  callback_early_stopping(patience = FLAGS$patience)
)

# FINAL MODEL ------------------------------------------------------------------
model <- keras_model_sequential()

# add layers
# we have just two, the LSTM and the time_distributed 
model %>%
  layer_lstm(
    units = FLAGS$n_units, 
    # the first layer in a model needs to know the shape of the input data
    batch_input_shape  = c(FLAGS$batch_size, FLAGS$n_timesteps, n_features),
    dropout = FLAGS$dropout,
    recurrent_dropout = FLAGS$recurrent_dropout,
    # by default, an LSTM just returns the final state
    return_sequences = TRUE
  ) %>% time_distributed(layer_dense(units = 1))

model %>%
  compile(
    loss = FLAGS$loss,
    optimizer = optimizer,
    # in addition to the loss, Keras will inform us about current 
    # MSE while training
    metrics = list("mean_squared_error")
  )

history <- model %>% fit(
  x          = X_train,
  y          = y_train,
  validation_data = list(X_valid, y_valid),
  batch_size = FLAGS$batch_size,
  epochs     = FLAGS$n_epochs,
  callbacks = callbacks
)

plot(history, metrics = "loss")

# CHECK PERFORMANCE ------------------------------------------------------------

pred_train <- model %>%
  predict(X_train, batch_size = FLAGS$batch_size) %>%
  .[, , 1]

# Retransform values to original scale
pred_train <- (pred_train * scale_history + center_history) ^2
compare_train <- df %>% filter(key == "training")

# build a dataframe that has both actual and predicted values
for (i in 1:nrow(pred_train)) {
  varname <- paste0("pred_train", i)
  compare_train <-
    mutate(compare_train,!!varname := c(
      rep(NA, FLAGS$n_timesteps + i - 1),
      pred_train[i,],
      rep(NA, nrow(compare_train) - FLAGS$n_timesteps * 2 - i + 1)
    ))
}

compare_train %>%
  select(-index,-CO2,-key) %>%
  map_dbl(~rmse_vec(truth = compare_train$CO2,
                    estimate = .x)) %>%
  mean()


ggplot(compare_train, aes(x = index, y = CO2)) + geom_line(size = 1) +
  geom_line(aes(y = pred_train10) ,size = 1, color = "blue") +
  geom_line(aes(y = pred_train20) ,size = 1, color = "red") +
  geom_line(aes(y = pred_train30) ,size = 1, color = "green") +
  geom_line(aes(y = pred_train40) ,size = 1, color = "violet") +
  geom_line(aes(y = pred_train50), size = 1, color = "cyan") +
  geom_line(aes(y = pred_train60), size = 1, color = "red") +
  geom_line(aes(y = pred_train70), size = 1, color = "green") +
  geom_line(aes(y = pred_train80) ,size = 1, color = "blue") +
  geom_line(aes(y = pred_train90) ,size = 1, color = "red") +
  geom_line(aes(y = pred_train100) ,size = 1, color = "cyan") +
  geom_line(aes(y = pred_train110) ,size = 1, color = "blue")
ggtitle("Predictions on the training set")

# MODEL ON TEST DATA -----------------------------------------------------------
pred_test <- model %>%
  predict(X_test, batch_size = FLAGS$batch_size) %>%
  .[, , 1]

# Retransform values to original scale
pred_test <- (pred_test * scale_history + center_history) ^2
pred_test[1:10, 1:5] %>% print()
compare_test <- df %>% filter(key == "testing")

# build a dataframe that has both actual and predicted values
for (i in 1:nrow(pred_test)) {
  varname <- paste0("pred_test", i)
  compare_test <-
    mutate(compare_test,!!varname := c(
      rep(NA, FLAGS$n_timesteps + i - 1),
      pred_test[i,],
      rep(NA, nrow(compare_test) - FLAGS$n_timesteps * 2 - i + 1)
    ))
}


compare_test %>%
  select(-index,-CO2,-key) %>%
  map_dbl(~rmse_vec(truth = compare_test$CO2,
                    estimate = .x)) %>%
  mean()

ggplot(compare_test, aes(x = index, y = CO2)) + geom_line(size = 1) +
  geom_line(aes(y = pred_test5) ,size = 1, color = "blue") +
  geom_line(aes(y = pred_test10) ,size = 1, color = "red") +
  geom_line(aes(y = pred_test15) ,size = 1, color = "green") +
  geom_line(aes(y = pred_test20) ,size = 1, color = "violet") +
  geom_line(aes(y = pred_test25), size = 1, color = "cyan") 
ggtitle("Predictions on the training set")

# APPLY TO ALL SPLITS ----------------------------------------------------------

obtain_predictions <- function(split) {  
  df_trn <- analysis(split)[1:140, , drop = FALSE]
  df_val <- analysis(split)[141:180, , drop = FALSE]
  df_tst <- assessment(split)
  
  df <- bind_rows(
    df_trn %>% add_column(key = "training"),
    df_val %>% add_column(key = "validation"),
    df_tst %>% add_column(key = "testing")
  ) %>%
    as_tbl_time(index = index)
  
  rec_obj <- recipe(CO2 ~ ., df) %>%
    step_sqrt(CO2) %>%
    step_center(CO2) %>%
    step_scale(CO2)%>%
    prep()
  
  df_processed_tbl <- bake(rec_obj, df)
  
  center_history <- rec_obj$steps[[2]]$means
  scale_history  <- rec_obj$steps[[3]]$sds
  
  FLAGS <- flags(
    flag_boolean("stack_layers", TRUE),
    flag_integer("batch_size", 5),
    flag_integer("n_timesteps", 12),
    flag_integer("n_epochs", 100),
    flag_numeric("dropout", 0.0),
    flag_numeric("recurrent_dropout", 0.1),
    flag_string("loss", "logcosh"),
    flag_string("optimizer_type", "SGD"),
    flag_integer("n_units", 256),
    flag_numeric("lr", 0.005),
    flag_integer("patience", 10)
  )
  
  n_predictions <- FLAGS$n_timesteps
  n_features <- 1
  
  optimizer <- FLAGS$optimizer_type
  
  callbacks <- list(
    callback_early_stopping(patience = FLAGS$patience)
  )
  
  
  train_vals <- df_processed_tbl %>%
    filter(key == "training") %>%
    select(CO2) %>%
    pull()
  
  valid_vals <- df_processed_tbl %>%
    filter(key == "validation") %>%
    select(CO2) %>%
    pull()
  
  test_vals <- df_processed_tbl %>%
    filter(key == "testing") %>%
    select(CO2) %>%
    pull()
  
  
  train_matrix <-
    build_matrix(train_vals, FLAGS$n_timesteps + n_predictions)
  valid_matrix <-
    build_matrix(valid_vals, FLAGS$n_timesteps + n_predictions)
  test_matrix <- 
    build_matrix(test_vals, FLAGS$n_timesteps + n_predictions)
  
  X_train <- train_matrix[, 1:FLAGS$n_timesteps]
  y_train <- train_matrix[, (FLAGS$n_timesteps + 1):(FLAGS$n_timesteps * 2)]
  X_train <- X_train[1:(nrow(X_train) %/% FLAGS$batch_size * FLAGS$batch_size), ]
  y_train <- y_train[1:(nrow(y_train) %/% FLAGS$batch_size * FLAGS$batch_size), ]
  
  X_valid <- valid_matrix[, 1:FLAGS$n_timesteps]
  y_valid <- valid_matrix[, (FLAGS$n_timesteps + 1):(FLAGS$n_timesteps * 2)]
  X_valid <- X_valid[1:(nrow(X_valid) %/% FLAGS$batch_size * FLAGS$batch_size), ]
  y_valid <- y_valid[1:(nrow(y_valid) %/% FLAGS$batch_size * FLAGS$batch_size), ]
  
  X_test <- test_matrix[, 1:FLAGS$n_timesteps]
  y_test <- test_matrix[, (FLAGS$n_timesteps + 1):(FLAGS$n_timesteps * 2)]
  X_test <- X_test[1:(nrow(X_test) %/% FLAGS$batch_size * FLAGS$batch_size), ]
  y_test <- y_test[1:(nrow(y_test) %/% FLAGS$batch_size * FLAGS$batch_size), ]
  
  X_train <- reshape_X_3d(X_train)
  X_valid <- reshape_X_3d(X_valid)
  X_test <- reshape_X_3d(X_test)
  
  y_train <- reshape_X_3d(y_train)
  y_valid <- reshape_X_3d(y_valid)
  y_test <- reshape_X_3d(y_test)
  
  
  model <- keras_model_sequential()
  
  model %>%
    layer_lstm(
      units = FLAGS$n_units, 
      batch_input_shape  = c(FLAGS$batch_size, FLAGS$n_timesteps, n_features),
      dropout = FLAGS$dropout,
      recurrent_dropout = FLAGS$recurrent_dropout,
      return_sequences = TRUE
    ) %>% time_distributed(layer_dense(units = 1))
  
  model %>%
    compile(
      loss = FLAGS$loss,
      optimizer = optimizer,
      metrics = list("mean_squared_error")
    )
  
  model %>% fit(
    x          = X_train,
    y          = y_train,
    validation_data = list(X_valid, y_valid),
    batch_size = FLAGS$batch_size,
    epochs     = FLAGS$n_epochs,
    callbacks = callbacks
  )
  
  pred_train <- model %>%
    predict(X_train, batch_size = FLAGS$batch_size) %>%
    .[, , 1]
  
  pred_train <- (pred_train * scale_history + center_history) ^2
  compare_train <- df %>% filter(key == "training")
  
  for (i in 1:nrow(pred_train)) {
    varname <- paste0("pred_train", i)
    compare_train <-
      mutate(compare_train,!!varname := c(
        rep(NA, FLAGS$n_timesteps + i - 1),
        pred_train[i,],
        rep(NA, nrow(compare_train) - FLAGS$n_timesteps * 2 - i + 1)
      ))
  }
  
  pred_test <- model %>%
    predict(X_test, batch_size = FLAGS$batch_size) %>%
    .[, , 1]
  
  pred_test <- (pred_test * scale_history + center_history) ^2
  compare_test <- df %>% filter(key == "testing")
  
  for (i in 1:nrow(pred_test)) {
    varname <- paste0("pred_test", i)
    compare_test <-
      mutate(compare_test,!!varname := c(
        rep(NA, FLAGS$n_timesteps + i - 1),
        pred_test[i,],
        rep(NA, nrow(compare_test) - FLAGS$n_timesteps * 2 - i + 1)
      ))
  }
  
  list(train = compare_train, test = compare_test)
}

all_split_preds <- rolling_origin_resamples %>%
  mutate(predict = map(splits, obtain_predictions))


calc_rmse <- function(df) {
  df %>%
    select(-index,-CO2,-key, -year) %>%
    map_dbl(~rmse_vec(truth = df$CO2,
                      estimate = .x)) %>%
    mean()
}

# RESULTS ----------------------------------------------------------------------

all_split_preds <- all_split_preds %>% unnest(predict)
all_split_preds_train <- all_split_preds[seq(1, 11, by = 2), ]
all_split_preds_test <- all_split_preds[seq(2, 12, by = 2), ]

all_split_rmses_train <- all_split_preds_train %>%
  mutate(rmse = map_dbl(predict, calc_rmse)) %>%
  select(id, rmse)

all_split_rmses_test <- all_split_preds_test %>%
  mutate(rmse = map_dbl(predict, calc_rmse)) %>%
  select(id, rmse)

all_split_rmses_train
all_split_rmses_test


plot_train <- function(slice, name) {
  ggplot(slice, aes(x = index, y = CO2)) + geom_line(size = 1) +
  geom_line(aes(y = pred_train10) ,size = 1, color = "blue") +
  geom_line(aes(y = pred_train20) ,size = 1, color = "red") +
  geom_line(aes(y = pred_train30) ,size = 1, color = "green") +
  geom_line(aes(y = pred_train40) ,size = 1, color = "violet") +
  geom_line(aes(y = pred_train50), size = 1, color = "cyan") +
  geom_line(aes(y = pred_train60), size = 1, color = "red") +
  geom_line(aes(y = pred_train70), size = 1, color = "green") +
  geom_line(aes(y = pred_train80) ,size = 1, color = "blue") +
  geom_line(aes(y = pred_train90) ,size = 1, color = "red") +
  geom_line(aes(y = pred_train100) ,size = 1, color = "cyan") +
  geom_line(aes(y = pred_train110) ,size = 1, color = "blue") +
    ggtitle(name)
}


train_plots <- map2(all_split_preds_train$predict, all_split_preds_train$id,  plot_train)

p_body_train  <- plot_grid(plotlist = train_plots)
p_title_train <- ggdraw() + 
  draw_label("Backtested Predictions: Training Sets", size = 18, fontface = "bold")

plot_grid(p_title_train, p_body_train, ncol = 1, rel_heights = c(0.05, 1, 0.05))



# TEST SET ---------------------------------------------------------------------
plot_test <- function(slice, name) {
  ggplot(slice, aes(x = index, y = CO2)) + geom_line(size = 1) +
    geom_line(aes(y = pred_test5) ,size = 1, color = "blue") +
    geom_line(aes(y = pred_test10) ,size = 1, color = "red") +
    geom_line(aes(y = pred_test15) ,size = 1, color = "green") +
    geom_line(aes(y = pred_test20) ,size = 1, color = "violet") +
    geom_line(aes(y = pred_test25), size = 1, color = "cyan") +
    ggtitle(name)
}

test_plots <- map2(all_split_preds_test$predict, all_split_preds_test$id, plot_test)

p_body_test  <- plot_grid(plotlist = test_plots, ncol = 3)
p_title_test <- ggdraw() + 
  draw_label("Backtested Predictions: Test Sets", size = 18, fontface = "bold")

testplot <- plot_grid(p_title_test, p_body_test, ncol = 1, rel_heights = c(0.05, 1, 0.05))

testplot

