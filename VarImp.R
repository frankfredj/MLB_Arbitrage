path <- "D:/MLB/"


n <- 8
lambda <- 0.75
split <- 10

library(caret)
library(caretEnsemble)
library(doParallel)


importances <- list()
tunes <- list()


for(u in 1:split){

	fold <- u

	data <- read.csv(paste(path, "averages_", as.character(lambda), "_", as.character(n),  ".csv", sep = ""))
	data <- data[which(data$Fold == fold),]

	y <- rep("A_win", nrow(data))
	y[which(data$Win == 0)] <- "B_loss"
	y <- as.factor(y)
	data$Win <- y

	x_start <- which(colnames(data) == "Rested_days_2") + 1
	m <- ncol(data) - x_start + 1

	x <-data[,c(x_start:ncol(data))]

	x_1 <- x[,c(1:(m/2))]
	x_2 <- x[,c((m/2 + 1):ncol(x))]

	x <- cbind(data[c("Streak_1", "LStreak_1", "In_a_row_1", "THA_1", "Rested_days_1")], x_1)
	x_1 <- NULL

	x <- cbind(x, data[c("Streak_2", "LStreak_2", "In_a_row_2", "THA_2", "Rested_days_2")])
	x <- cbind(x, x_2)
	x_2 <- NULL

	original_vars = colnames(x)


	#Remove features with near-zero variances
	x <- as.matrix(x)

	rmv <- unique(which(is.infinite(x), arr.ind = TRUE)[,1])

	if(length(rmv) > 0){

		x <- x[-rmv,]
		y <- y[-rmv]

	}


	rmv <- nearZeroVar(x)

	if(length(rmv) > 0){

		a <- rmv[which(rmv <= ncol(x)/2)]
		b <- rmv[which(rmv > ncol(x)/2)]

		a <- c(a, a + ncol(x)/2)
		b <- c(b, b - ncol(x)/2)

		c <- sort(unique(c(a,b)))

		x <- x[,-rmv]

	}

	#



	x <- as.data.frame(x)
	x$Win <- y


	folds <- createFolds(x$Win, 5, returnTrain = TRUE)

	train_control <-  trainControl(method = "cv", verboseIter = TRUE, allowParallel = TRUE, 
								savePredictions = "final", summaryFunction = twoClassSummary, classProbs = TRUE,
									search = "random", index = folds)


	ranger_grid <- expand.grid(mtry = c(200), splitrule = c("extratrees", "gini"), min.node.size = c(5,7,10,12))


	cl <- makeCluster(detectCores() - 2) 
	clusterEvalQ(cl, library(foreach))
	registerDoParallel(cl)

	models <- caretList(
	  Win~., data = x,
	  trControl=train_control,
	  metric="ROC",
	  tuneList=list(
	    glm=caretModelSpec(method = "glmnet", tuneLength = 50, preProc = c("center", "scale")),
	    ranger=caretModelSpec(method = "ranger", tuneGrid = ranger_grid, preProc = c("center", "scale"), importance = "permutation"),
	    xgbLinear=caretModelSpec(method = "xgbLinear", tuneLength = 30, preProc = c("center", "scale")),
	    xgbTree=caretModelSpec(method = "xgbTree", tuneLength = 30, preProc = c("center", "scale"))
	  )
	)

	stopCluster(cl)

	print_frame <- matrix(nrow = length(models), ncol = 1)
	colnames(print_frame) <- "ROC"

	for(i in 1:length(print_frame)){

		temp <- models[[i]]$results
		a <- order(temp$ROC, decreasing = TRUE)[1]
		print_frame[i] <- temp$ROC[a]

	}

	rownames(print_frame) <- names(models)

	print(paste("Iteration #", as.character(u), " ..."), quote = FALSE)
	print("Overfitted models metrics:", quote = FALSE)

	print(print_frame)


	temp <- varImp(models[[1]], scale = FALSE)$importance
	for(i in 2:length(models)){

		a <- varImp(models[[i]], scale = FALSE)$importance
		b <- match(rownames(temp), rownames(a))

		temp <- cbind(temp, a[b,])

	}

	colnames(temp) <- names(models)
	importances[[u]] <- temp


	temp <- list()
	for(i in 1:length(models)){

		temp[[i]] <- models[[i]]$results

	}

	names(temp) <- names(models)
	tunes[[u]] <- temp


}




path_temp <- paste("D:/MLB_R_models/importances_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
saveRDS(importances, path_temp)

path_temp <- paste("D:/MLB_R_models/tuning_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
saveRDS(tunes, path_temp)









