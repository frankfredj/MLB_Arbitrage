path <- "D:/MLB/"


n <- 8
lambda <- 0.75
split <- 10

library(caret)
library(caretEnsemble)
library(doParallel)
library(ggplot2)
library(mlrMBO)
library(pROC)

library("tensorflow")
library("keras")

tf_gpu_configured()


path_temp <- paste("D:/MLB_R_models/retained_features_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
variables_per_model <- readRDS(path_temp)

models <- list()

path_temp <- paste("D:/MLB_R_models/xgbLinear_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
models[[3]] <- readRDS(path_temp)
variables_per_model[[3]] <- colnames(readRDS(paste("D:/MLB_R_models/XY_xgbLinear_", as.character(lambda), "_", as.character(n), ".rds", sep = "")))
variables_per_model[[3]] <- variables_per_model[[3]][-length(variables_per_model[[3]])]

path_temp <- paste("D:/MLB_R_models/xgbTree_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
models[[4]]<- readRDS(path_temp)
variables_per_model[[4]] <- colnames(readRDS(paste("D:/MLB_R_models/XY_xgbTree_", as.character(lambda), "_", as.character(n), ".rds", sep = "")))
variables_per_model[[4]] <- variables_per_model[[4]][-length(variables_per_model[[4]])]

path_temp <- paste("D:/MLB_R_models/glmnet_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
models[[1]] <- readRDS(path_temp)
variables_per_model[[1]] <- colnames(readRDS(paste("D:/MLB_R_models/XY_glmnet_", as.character(lambda), "_", as.character(n), ".rds", sep = "")))
variables_per_model[[1]] <- variables_per_model[[1]][-length(variables_per_model[[1]])]

path_temp <- paste("D:/MLB_R_models/ranger_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
models[[2]] <- readRDS(path_temp)
variables_per_model[[2]] <- colnames(readRDS(paste("D:/MLB_R_models/XY_ranger_", as.character(lambda), "_", as.character(n), ".rds", sep = "")))
variables_per_model[[2]] <- variables_per_model[[2]][-length(variables_per_model[[2]])]

path_temp <- paste("Keras_NN_", as.character(lambda), "_", as.character(n),  ".csv", sep = "")
models[[5]] <- load_model_hdf5(path_temp)
variables_per_model[[5]] <- colnames(readRDS(paste("D:/MLB_R_models/XY_NNet_", as.character(lambda), "_", as.character(n), ".rds", sep = "")))

names(models) <- names(variables_per_model)




oof_preds <- matrix(nrow = nrow(models[[1]]$pred), ncol = 5)
colnames(oof_preds) <- names(models)

for(i in 1:4){

	index <- models[[i]]$pred$rowIndex
	oof_preds[index,i] <- models[[i]]$pred$A_win

}



#Get out of fold predictions from the Keras neural network

data <- read.csv(paste(path, "averages_", as.character(lambda), "_", as.character(n),  ".csv", sep = ""))

X <- match(variables_per_model$NN, colnames(data))
X <- data[,X]

X <- as.matrix(X)

rmv <- unique(which(is.infinite(X), arr.ind = TRUE)[,1])

if(length(rmv) > 0){

	X <- X[-rmv,]
	data <- data[-rmv, ]

}

train_index <- as.integer(c(ceiling(0.2*nrow(X)):nrow(X)))


mu <- apply(X[train_index, ], 2, mean)
sigma <- apply(X[train_index, ], 2, sd)

X <- as.matrix(X[-train_index, ])
for(j in 1:ncol(X)){

	X[,j] <- (X[,j] - mu[j]) / sigma[j]

}


predictions <- models[[5]] %>% predict_proba(X)
oof_preds[,5] <- predictions[,2]

y_test <- data$Win[-train_index]  


AUROC_frame <- matrix(nrow = 5, ncol = 1)
for(i in 1:length(AUROC_frame)){

	AUROC_frame[i] <- auc(roc(y_test, c(oof_preds[,i])))

}

rownames(AUROC_frame) <- names(models)
colnames(AUROC_frame) <- "AUROC"

print(AUROC_frame)


index_temp <- c(1:2430)

dummy_f <- function(){

	u <- rep(0, 5)
	for(i in 1:5){

		u[i] <- runif(n = 1, min = 0, 
						max = runif(n = 1, min = 1, max = 3))

	}

	u <- u / sum(u)
	p <- (oof_preds %*% u)[-index_temp]

	auroc <- auc(roc(y_test[-index_temp], c(p)))

	return(c(u, auroc))

}


cl <- makeCluster(detectCores() - 4)
clusterEvalQ(cl, library(foreach))
registerDoParallel(cl)

	aurocs <- foreach(i = 1:100, .combine = "rbind", .packages = "pROC") %dopar% dummy_f()

stopCluster(cl)


aurocs <- as.data.frame(aurocs)
colnames(aurocs) <- c(paste("w", c(1:5), sep = ""), "y")
rownames(aurocs) <- NULL



#Find the weights which give the maximum ROC
to_maximise <- function(x){

	w <- c(x$w1, x$w2, x$w3, x$w4, x$w5)
	w <- w / sum(w)
	p <- (oof_preds %*% w)[-index_temp]

	return(auc(roc(y_test[-index_temp], c(p))))

}


ps = makeParamSet(
  makeNumericParam("w1", lower = 0, upper = 1),
  makeNumericParam("w2", lower = 0, upper = 1),
  makeNumericParam("w3", lower = 0, upper = 1),
  makeNumericParam("w4", lower = 0, upper = 1),
  makeNumericParam("w5", lower = 0, upper = 1)
)


f <- makeSingleObjectiveFunction(name = "Linear_comp_AUROC",
								fn = to_maximise,
								par.set = ps,
								minimize = FALSE,
								has.simple.signature = FALSE)


ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 250)
ctrl = setMBOControlInfill(ctrl, crit = crit.ei)

tuning <- mbo(f, design = aurocs, control = ctrl, show.info = TRUE)





weights <- unlist(tuning$x) 
weights <- weights / sum(weights)

weights_frame <- matrix(nrow = 5, ncol = 1, weights)
colnames(weights_frame) <- "Weights"
rownames(weights_frame) <- names(models)

print("Optimal weights:", quote = FALSE)
print(weights_frame)

print("", quote = FALSE)
print(paste("Optimal AUROC:", as.character(tuning$y)), quote = FALSE)




#Build the final model object 

final_model <- list()
final_model$models <- models
final_model$variables_per_model <- variables_per_model
final_model$scaling_params <- list(mu = mu, sigma = sigma)
final_model$ensemble_weights <- weights


fit_data <- function(data){

	mu <- final_model$scaling_params$mu
	sigma <- final_model$scaling_params$sigma

	y_factor <- rep("A_win", nrow(data))
	y_factor[which(data$Win == 0)] <- "B_loss"
	y_factor <- as.factor(y_factor)

	y_numeric <- data$Win

	X <- list()
	for(i in 1:length(final_model$variables_per_model)){

		X[[i]] <- data[,match(final_model$variables_per_model[[i]], colnames(data))]

	}


	X[[length(X)]] <- as.matrix(X[[length(X)]])
	for(j in 1:ncol(X[[length(X)]])){

		X[[length(X)]][,j] <- (X[[length(X)]][,j] - mu[j]) / sigma[j]

	}

	for(i in 1:4){

		X[[i]]$Win <- y_factor

	}


	preds <- matrix(nrow = nrow(data), ncol = 5)
	colnames(preds) <- names(final_model$models)

	for(j in 1:4){

		preds[,j] <- predict(final_model$models[[j]], X[[j]], type = "prob")$A_win

	}

	preds[,5] <- (final_model$models[[5]] %>% predict_proba(X[[5]]))[,2]

	preds <- preds %*% final_model$ensemble_weights

	colnames(preds) <- "A_win"

	return(preds)

}

final_model$fit <- fit_data



#Construct a backtesting frame for model validation and include it with the model

backtest_frame <- read.csv("D:/MLB/scores_BT.csv")
oof_data <- data[-train_index, ][index_temp, ]
oof_preds <- c(oof_preds[index_temp, ] %*% weights)


index_bt <- match(oof_data$ID, backtest_frame$ID)
a <- which(!is.na(index_bt))
b <- index_bt[a]

oof_data <- oof_data[a,]
oof_preds <- oof_preds[a]
backtest_frame <- backtest_frame[b,]

backtest_frame$P1_p <- oof_preds
backtest_frame$P2_p <- 1 - backtest_frame$P1_p
backtest_frame$R1_p <- 1 / backtest_frame$P1_p
backtest_frame$R2_p <- 1 / backtest_frame$P2_p

backtest_frame$Win <- 1
backtest_frame$Win[which(backtest_frame$Score_1 < backtest_frame$Score_2)] <- 0
write.csv(backtest_frame, paste(path, "test_set_preds", as.character(lambda), "_", as.character(n),  ".csv", sep = ""), row.names= FALSE)

final_model$backtest_frame <- backtest_frame


#Include basic model statistics

final_model$models_AUROC <- AUROC_frame

backtest_frame$win_factor <- "win"
backtest_frame$win_factor[which(backtest_frame$Score_1 < backtest_frame$Score_2)] <- "loss"
backtest_frame$win_factor <- as.factor(backtest_frame$win_factor)

p <- backtest_frame$win_factor
p[] <- "win"
p[which(backtest_frame$P1_p < 0.5)] <- "loss"


final_model$ensemble_oof_confusion_matrix <- confusionMatrix(p, backtest_frame$win_factor, positive = "win")
print(final_model$ensemble_oof_confusion_matrix)

final_model$ensemble_oof_AUROC <- auc(roc(c(backtest_frame$Win), c(backtest_frame$P1_p)))

path_temp <- paste("D:/MLB_R_models/final_ensemble_model_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
saveRDS(final_model, path_temp)
