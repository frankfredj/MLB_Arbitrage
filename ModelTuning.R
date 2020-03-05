path <- "D:/MLB/"


n <- 8
lambda <- 0.75
split <- 10

library(caret)
library(caretEnsemble)
library(doParallel)
library(ggplot2)
library(mlrMBO)

path_temp <- paste("D:/MLB_R_models/retained_features_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
variables_per_model <- readRDS(path_temp)

lapply(variables_per_model, length)

data <- read.csv(paste(path, "averages_", as.character(lambda), "_", as.character(n),  ".csv", sep = ""))


X <- match(variables_per_model$NN, colnames(data))
X <- data[,X]

X <- as.matrix(X)

rmv <- unique(which(is.infinite(X), arr.ind = TRUE)[,1])

if(length(rmv) > 0){

	X <- X[-rmv,]
	data <- data[-rmv, ]

}



###############################################################
###############################################################
##################   Tune xgboost linear  #####################
###############################################################
###############################################################

y <- rep("A_win", nrow(data))
y[which(data$Win == 0)] <- "B_loss"
y <- as.factor(y)

X <- match(variables_per_model$xgbLinear, colnames(data))
X <- data[,X]

X <- as.matrix(X)

rmv <- unique(which(is.infinite(X), arr.ind = TRUE)[,1])

if(length(rmv) > 0){

	X <- X[-rmv,]
	y <- y[-rmv]

}

rmv <- nearZeroVar(X)
if(length(rmv) > 0){

	a <- rmv[which(rmv <= ncol(X) / 2)]
	b <- rmv[which(rmv > ncol(X) / 2)]

	a <- c(a, a + ncol(X)/2)
	b <- c(b, b - ncol(X)/2)

	rmv <- unique(c(a,b))

	X <- X[,-rmv]

}

X <- as.data.frame(X)
X$Win <- y


folds <- list(Folds1 = as.integer(c(ceiling(0.2*nrow(X)):nrow(X))))


train_control_2 <-  trainControl(method = "cv", number = 1, verboseIter = TRUE, allowParallel = FALSE, 
								savePredictions = "final", summaryFunction = twoClassSummary, classProbs = TRUE,
								index = folds)


to_maximise <- function(x){

	tune_grid <- expand.grid(nrounds = x$nrounds, 
								lambda = x$lambda,
								alpha = x$alpha,
								eta = x$eta)


	temp <- caret::train(
		
		Win~., data=X,
		trControl=train_control_2,
		metric="ROC",
		method="xgbLinear", 
		tuneGrid = tune_grid, 
		preProc=c("center", "scale")

		)


	return(max(temp$results$ROC))

}


ps = makeParamSet(
  makeNumericParam("lambda", lower = 0, upper = 1),
  makeNumericParam("alpha", lower = 0, upper = 1),
  makeIntegerParam("nrounds", lower = 1, upper = 250),
  makeNumericParam("eta", lower = 0.01, upper = 3)
)



f <- makeSingleObjectiveFunction(name = "xgbLinear_AUROC",
								fn = to_maximise,
								par.set = ps,
								minimize = FALSE,
								has.simple.signature = FALSE)




ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 250)
ctrl = setMBOControlInfill(ctrl, crit = crit.ei)

tuning <- mbo(f, control = ctrl, show.info = TRUE)


xgbLinear_model <-  caret::train(
		
			Win~., data=X,
			trControl=train_control_2,
			metric="ROC",
			method="xgbLinear", 
			tuneGrid = as.data.frame(t(unlist(tuning$x))), 
			preProc=c("center", "scale")

		)


path_temp <- paste("D:/MLB_R_models/xgbLinear_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
saveRDS(xgbLinear_model, path_temp)


path_temp <- paste("D:/MLB_R_models/XY_xgbLinear_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
saveRDS(X, path_temp)




###############################################################
###############################################################
##################   Tune glmnet   ############################
###############################################################
###############################################################

y <- rep("A_win", nrow(data))
y[which(data$Win == 0)] <- "B_loss"
y <- as.factor(y)

X <- match(variables_per_model$glm, colnames(data))
X <- data[,X]

X <- as.matrix(X)

rmv <- unique(which(is.infinite(X), arr.ind = TRUE)[,1])

if(length(rmv) > 0){

	X <- X[-rmv,]
	y <- y[-rmv]

}

rmv <- nearZeroVar(X)
if(length(rmv) > 0){

	a <- rmv[which(rmv <= ncol(X) / 2)]
	b <- rmv[which(rmv > ncol(X) / 2)]

	a <- c(a, a + ncol(X)/2)
	b <- c(b, b - ncol(X)/2)

	rmv <- unique(c(a,b))

	X <- X[,-rmv]

}

X <- as.data.frame(X)
X$Win <- y


folds <- list(Folds1 = as.integer(c(ceiling(0.2*nrow(X)):nrow(X))))

train_control_2 <-  trainControl(method = "cv", number = 1, verboseIter = TRUE, allowParallel = FALSE, 
								savePredictions = "final", summaryFunction = twoClassSummary, classProbs = TRUE,
								index = folds)



to_maximise <- function(x){

	tune_grid <- expand.grid(alpha = x$alpha, 
								lambda = x$lambda)


	temp <- caret::train(
		
		Win~., data=X,
		trControl=train_control_2,
		metric="ROC",
		method="glmnet", 
		tuneGrid = tune_grid, 
		preProc=c("center", "scale")

		)


	return(max(temp$results$ROC))

}


ps = makeParamSet(
  makeNumericParam("alpha", lower = 0, upper = 1),
  makeNumericParam("lambda", lower = 0, upper = 10)
)



f <- makeSingleObjectiveFunction(name = "glmnet_AUROC",
								fn = to_maximise,
								par.set = ps,
								minimize = FALSE,
								has.simple.signature = FALSE)




ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 250)
ctrl = setMBOControlInfill(ctrl, crit = crit.ei)

tuning <- mbo(f, control = ctrl, show.info = TRUE)




glmnet_model <- caret::train(
	
	Win~., data=X,
	trControl=train_control_2,
	metric="ROC",
	method="glmnet", 
	tuneGrid = as.data.frame(t(unlist(tuning$x))), 
	preProc=c("center", "scale")

	)



path_temp <- paste("D:/MLB_R_models/glmnet_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
saveRDS(glmnet_model, path_temp)


path_temp <- paste("D:/MLB_R_models/XY_glmnet_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
saveRDS(X, path_temp)


###############################################################
###############################################################
##################   Tune xgboost tree  #######################
###############################################################
###############################################################

y <- rep("A_win", nrow(data))
y[which(data$Win == 0)] <- "B_loss"
y <- as.factor(y)

X <- match(variables_per_model$xgbTree, colnames(data))
X <- data[,X]

X <- as.matrix(X)

rmv <- unique(which(is.infinite(X), arr.ind = TRUE)[,1])

if(length(rmv) > 0){

	X <- X[-rmv,]
	y <- y[-rmv]

}

rmv <- nearZeroVar(X)
if(length(rmv) > 0){

	a <- rmv[which(rmv <= ncol(X) / 2)]
	b <- rmv[which(rmv > ncol(X) / 2)]

	a <- c(a, a + ncol(X)/2)
	b <- c(b, b - ncol(X)/2)

	rmv <- unique(c(a,b))

	X <- X[,-rmv]

}

X <- as.data.frame(X)
X$Win <- y


folds <- list(Folds1 = as.integer(c(ceiling(0.2*nrow(X)):nrow(X))))



train_control_2 <-  trainControl(method = "cv", number = 1, verboseIter = TRUE, allowParallel = FALSE, 
								savePredictions = "final", summaryFunction = twoClassSummary, classProbs = TRUE,
								index = folds)


to_maximise <- function(x){

	tune_grid <- expand.grid(eta = x$eta, 
								max_depth = x$max_depth,
								gamma = x$gamma,
								colsample_bytree = x$colsample_bytree,
								min_child_weight = x$min_child_weight,
								subsample = x$subsample,
								nrounds = x$nrounds)


	temp <- caret::train(
		
		Win~., data=X,
		trControl=train_control_2,
		metric="ROC",
		method="xgbTree", 
		tuneGrid = tune_grid, 
		preProc=c("center", "scale")

		)


	return(max(temp$results$ROC))

}


ps = makeParamSet(
  makeNumericParam("eta", lower = 0, upper = 1),
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  makeNumericParam("gamma", lower = 0, upper = ncol(X)),
  makeNumericParam("colsample_bytree", lower = 0.2, upper = 1),
  makeIntegerParam("min_child_weight", lower = 1, upper = ncol(X)),
  makeNumericParam("subsample", lower = 0.2, upper = 1),
  makeIntegerParam("nrounds", lower = 1, upper = 3*ncol(X))
)



f <- makeSingleObjectiveFunction(name = "xgbTree_AUROC",
								fn = to_maximise,
								par.set = ps,
								minimize = FALSE,
								has.simple.signature = FALSE)




ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 250)
ctrl = setMBOControlInfill(ctrl, crit = crit.ei)

tuning <- mbo(f, control = ctrl, show.info = TRUE)


xgbTree_model <-  caret::train(
		
			Win~., data=X,
			trControl=train_control_2,
			metric="ROC",
			method="xgbTree", 
			tuneGrid = as.data.frame(t(unlist(tuning$x))), 
			preProc=c("center", "scale")

		)


path_temp <- paste("D:/MLB_R_models/xgbTree_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
saveRDS(xgbTree_model, path_temp)


path_temp <- paste("D:/MLB_R_models/XY_xgbTree_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
saveRDS(X, path_temp)



###############################################################
###############################################################
##################   Tune ranger   ############################
###############################################################
###############################################################

y <- rep("A_win", nrow(data))
y[which(data$Win == 0)] <- "B_loss"
y <- as.factor(y)

X <- match(variables_per_model$ranger, colnames(data))
X <- data[,X]

X <- as.matrix(X)

rmv <- unique(which(is.infinite(X), arr.ind = TRUE)[,1])

if(length(rmv) > 0){

	X <- X[-rmv,]
	y <- y[-rmv]

}

rmv <- nearZeroVar(X)
if(length(rmv) > 0){

	a <- rmv[which(rmv <= ncol(X) / 2)]
	b <- rmv[which(rmv > ncol(X) / 2)]

	a <- c(a, a + ncol(X)/2)
	b <- c(b, b - ncol(X)/2)

	rmv <- unique(c(a,b))

	X <- X[,-rmv]

}

X <- as.data.frame(X)
X$Win <- y


folds <- list(Folds1 = as.integer(c(ceiling(0.2*nrow(X)):nrow(X))))


train_control <-  trainControl(method = "cv", number = 1, verboseIter = TRUE, allowParallel = TRUE, 
								savePredictions = "final", summaryFunction = twoClassSummary, classProbs = TRUE,
								index = folds, search = "random")

cl <- makeCluster(detectCores() - 2) 
clusterEvalQ(cl, library(foreach))
registerDoParallel(cl)

ranger_model <-  caret::train(
		
			Win~., data=X,
			trControl=train_control,
			metric="ROC",
			method="ranger", 
			num.trees = 2*ncol(X),
			tuneLength = 100, 
			preProc=c("center", "scale")

		)

stopCluster(cl)

path_temp <- paste("D:/MLB_R_models/ranger_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
saveRDS(ranger_model, path_temp)


path_temp <- paste("D:/MLB_R_models/XY_ranger_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
saveRDS(X, path_temp)




###############################################################
###############################################################
##################   Tune NNet   ##############################
###############################################################
###############################################################


y <- data$Win

X <- match(variables_per_model$NN, colnames(data))
X <- data[,X]

X <- as.matrix(X)

rmv <- unique(which(is.infinite(X), arr.ind = TRUE)[,1])

if(length(rmv) > 0){

	X <- X[-rmv,]
	y <- y[-rmv]

}

rmv <- nearZeroVar(X)
if(length(rmv) > 0){

	a <- rmv[which(rmv <= ncol(X) / 2)]
	b <- rmv[which(rmv > ncol(X) / 2)]

	a <- c(a, a + ncol(X)/2)
	b <- c(b, b - ncol(X)/2)

	rmv <- unique(c(a,b))

	X <- X[,-rmv]

}



library("tensorflow")
library("keras")

tf_gpu_configured()

train_index <- as.integer(c(ceiling(0.2*nrow(X)):nrow(X)))

test <- X[-train_index,]
train <- X[train_index,]


test <- test %>% scale()
train <- train %>% scale()


y_test <- to_categorical(y[-train_index])
y_train <- to_categorical(y[train_index])

n_hidden <- as.integer(2 * ncol(train) / 3)

model <- keras_model_sequential() 

model %>% 
  layer_dense(units = n_hidden, activation = "relu", input_shape = ncol(train)) %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = as.integer(0.5*n_hidden), activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 2, activation = "sigmoid")


history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

model %>% fit(
  train, y_train, 
  epochs = 100, 
  batch_size = 5,
  validation_data = list(test, y_test)
)


model_name <- paste("Keras_NN_", as.character(lambda), "_", as.character(n),  ".csv", sep = "")

save_model_hdf5(model, model_name)

path_temp <- paste("D:/MLB_R_models/XY_NNet_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
saveRDS(X, path_temp)

predictions <- model %>% predict_proba(test)
predictions <- c(predictions[,1])

library(pROC)
roc_obj <- roc(c(y[-train_index]), predictions)
auc(roc_obj)




