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


df <- data
rec_obj <- recipe(CO2 ~ ., df) %>%
  step_sqrt(CO2) %>%
  step_center(CO2) %>%
  step_scale(CO2)%>%
  prep()

df_processed_tbl <- bake(rec_obj, df)


center_history <- rec_obj$steps[[2]]$means
scale_history  <- rec_obj$steps[[3]]$sds

c("center" = center_history, "scale" = scale_history)

lag_setting <- 144
n_timesteps <- 1
n_predictions <- 12
train_length <- 300
batch_size <- 1


lag_train_tbl <- df_processed_tbl %>%
  mutate(value_lag = lag(CO2, n = lag_setting)) %>%
  filter(!is.na(value_lag)) %>%
  tail(train_length)

x_train_vec <- lag_train_tbl$value_lag
x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))

y_train_vec <- lag_train_tbl$CO2
y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))

x_test_vec <- y_train_vec %>% tail(lag_setting)
x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))



# BUILD LSTM MODEL -------------------------------------------------------------
FLAGS <- flags(
  # Should we use several layers of LSTM?
  # Again, just included for completeness, it did not yield any superior 
  # performance on this task.
  # This will actually stack exactly one additional layer of LSTM units.
  flag_boolean("stack_layers", TRUE),
  # number of samples fed to the model in one go
  flag_integer("batch_size", 1),
  # size of the hidden state, equals size of predictions
  flag_integer("n_timesteps", 1),
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

for (i in 1:100) {
  model %>% fit(x          = x_train_arr, 
                y          = y_train_arr, 
                batch_size = batch_size,
                epochs     = 1, 
                verbose    = 1, 
                shuffle    = FALSE)
  
  model %>% reset_states()
  cat("Epoch: ", i)
  
}

# Make Predictions
pred_out <- model %>% 
  predict(x_test_arr, batch_size = batch_size) %>%
  .[,1,1]

# Make future index using tk_make_future_timeseries()
idx <- data %>%
  tk_index() %>%
  tk_make_future_timeseries(n_future = lag_setting)


# Retransform values
pred_tbl <- tibble(
  index   = idx,
  CO2   = ((pred_out * scale_history + center_history)^2)
)


# Combine actual data with predictions
tbl_1 <- df %>%
  add_column(key = "actual")

tbl_3 <- pred_tbl %>%
  add_column(key = "predicted")

# Create time_bind_rows() to solve dplyr issue
time_bind_rows <- function(data_1, data_2, index) {
  index_expr <- enquo(index)
  bind_rows(data_1, data_2) %>%
    as_tbl_time(index = !! index_expr)
  }

ret <- list(tbl_1, tbl_3) %>%
  reduce(time_bind_rows, index = index) %>%
  arrange(key, index) %>%
  mutate(key = as_factor(key))

future_CO2 <- ret


plot_prediction <- function(data, id, alpha = 1, size = 2, base_size = 14) {
  
  rmse_val <- calc_rmse(data)
  
  g <- data %>%
    ggplot(aes(index, CO2, color = key)) +
    geom_point(alpha = 0.5, size = size) +
    scale_colour_manual(values=c("red", "blue")) +
    theme(legend.title = element_blank())
  
  return(g)
}



future_CO2 %>%
  filter_time("1980" ~ "end") %>%
  plot_prediction(id = NULL, alpha = 0.4, size = 1.5) +
  theme(legend.position = "right") +
  ggtitle("Prediction of CO2 emissions for 2005 - 2016")


# COMPARE PREDICTED VS ACTUAL --------------------------------------------------

predicted <- as.data.frame(tbl_3)
future_data <- dataco2[dataco2$year>2004 & dataco2$year<2017,]

combined_tables <- merge(future_data, predicted, by = 'index')
combined_tables <- rename(combined_tables, actual = CO2.x, predicted = CO2.y)
combined_tables$key <- NULL


  ggplot(combined_tables, aes(index)) +
  geom_point(aes(y = predicted, colour = "predicted"), alpha = 0.5) + 
  geom_point(aes(y = actual, colour = "actual"), alpha = 0.5) +
  scale_colour_manual(values=c("red", "blue")) +
  theme(legend.title = element_blank()) +
  ylab("CO2") +
  ggtitle("Prediction of CO2 emissions for 2005 - 2016")


rmse(combined_tables, actual, predicted)
