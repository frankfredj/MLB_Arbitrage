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
library(reshape2)



path_temp <- paste("D:/MLB_R_models/final_ensemble_model_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
final_model <- readRDS(path_temp)

backtest <- final_model$backtest_frame

backtest$P1 <- backtest$P_1
backtest$P_1 <- NULL

backtest$P2 <- backtest$P_2
backtest$P_2 <- NULL

backtest$R1 <- backtest$R_1
backtest$R_1 <- NULL

backtest$R2 <- backtest$R_2
backtest$R_2 <- NULL



ROC <- function(backtest){

	#ROC

	out <- list()

	wins <- backtest$Win
	cutoffs <- seq(from = 0, to = 1, length.out = nrow(backtest)*2)

	sens <- matrix(nrow = length(cutoffs), ncol = 2)
	colnames(sens) <- c("Model_Sensitivity", "Book_Sensitivity")
	rownames(sens) <- cutoffs

	spec <- matrix(nrow = length(cutoffs), ncol = 2)
	colnames(spec) <- c("Model_FP", "Book_FP")
	rownames(spec) <- cutoffs	

	positive <- which(wins == 1)
	negative <- which(wins == 0)

	n_p <- length(positive)
	n_n <- length(negative)

	for(k in 1:length(cutoffs)){

		t <- cutoffs[k]

		pred_win <- which(backtest$P1_p >= t)
		sens[k,1] <- length(intersect(pred_win, positive)) / n_p
		spec[k,1] <- length(intersect(pred_win, negative)) / n_n

		pred_win <- which(backtest$P1 >= t)
		sens[k,2] <- length(intersect(pred_win, positive)) / n_p
		spec[k,2] <- length(intersect(pred_win, negative)) / n_n

	}

	roc_frame_model <- as.data.frame(cbind(spec[,1], sens[,1]))
	colnames(roc_frame_model) <- c("FP", "TP")
	roc_frame_model$name <- "Model"

	roc_frame_book <- as.data.frame(cbind(spec[,2], sens[,2]))
	colnames(roc_frame_book) <- c("FP", "TP")
	roc_frame_book$name <- "Book"

	roc_frame <- rbind(roc_frame_model, roc_frame_book)

	nm <- paste("ROC_", as.character(lambda), "_", as.character(n), ".png", sep = "")
	out$ROC_plot <- ggplot(data = roc_frame, aes(x = FP, y = TP, color = name)) + 
					geom_point() + geom_abline(intercept = 0, slope = 1) + 
					ggtitle("ROC curves") + xlim(0,1) + ylim(0,1) +
					ggsave(nm)

	print(out$ROC_plot)


	ROC <- matrix(nrow = 2, ncol =2)
	rownames(ROC) <- c("Area", "SD")
	colnames(ROC) <- c("Model", "Book")

	library(pracma)

	temp <- order(roc_frame_model$FP)
	x <- as.numeric(roc_frame_model$FP[temp])
	y <- as.numeric(roc_frame_model$TP[temp])

	A <- trapz(x = x, y = y)
	E_x <- trapz(x = x, y = x*y) / A
	E_x_sq <- trapz(x = x, y = (x^2)*y) / A

	ROC[1,1] <- A
	ROC[2,1] <- E_x_sq - E_x^2


	temp <- order(roc_frame_book$FP)
	x <- as.numeric(roc_frame_book$FP[temp])
	y <- as.numeric(roc_frame_book$TP[temp])

	A <- trapz(x = x, y = y)
	E_x <- trapz(x = x, y = x*y) / A
	E_x_sq <- trapz(x = x, y = (x^2)*y) / A

	ROC[1,2] <- A
	ROC[2,2] <- E_x_sq - E_x^2	


	out$ROC <- ROC

	print(ROC)

	return(out)

}



test_ROC <- ROC(backtest)

offered_return <- backtest


print("*********************", quote = FALSE)
print("******* MODEL *******", quote = FALSE)
print("*********************", quote = FALSE)
reals <- rep("win", nrow(backtest))
reals[which(backtest$Win == 0)] <- "loss"
reals <- as.factor(reals)

preds <- rep("win", nrow(backtest))
preds[which(backtest$P1_p < 0.5)] <- "loss"
preds <- as.factor(preds)

print(confusionMatrix(reals, preds, positive = "win"))

print("*********************", quote = FALSE)
print("******* BOOK ********", quote = FALSE)
print("*********************", quote = FALSE)
reals <- rep("win", nrow(backtest))
reals[which(backtest$Win == 0)] <- "loss"
reals <- as.factor(reals)

preds <- rep("win", nrow(backtest))
preds[which(backtest$P1 < 0.5)] <- "loss"
preds <- as.factor(preds)

print(confusionMatrix(reals, preds, positive = "win"))






#Betting

gains <- function(min_gap, lower_p){

	bet_home <- which(backtest$R1 > backtest$R1_p)
	bet_away <- which(backtest$R2 > backtest$R2_p)

	#Apply min.gap between Rn and Rn_p condition

	keep <- which(backtest$R1[bet_home] - backtest$R1_p[bet_home] >= min_gap)
	bet_home <- bet_home[keep] 

	keep <- which(backtest$R2[bet_away] - backtest$R2_p[bet_away] >= min_gap)
	bet_away <- bet_away[keep] 

	#Apply lower_p <= p 

	if(length(bet_home) > 0){

		keep <- which(backtest$P1_p[bet_home] >= lower_p)
		bet_home <- bet_home[keep] 

		home_gains <- backtest$R1[bet_home] * backtest$Win[bet_home] - 1

	} else {

		home_gains <- c(0)

	}

	if(length(bet_away) > 0){

		keep <- which(backtest$P2_p[bet_away] >= lower_p)
		bet_away <- bet_away[keep] 	

		away_gains <- backtest$R2[bet_away] * (1 - backtest$Win[bet_away]) - 1

	} else {

		away_gains <- c(0)

	}


	gains <- c(home_gains, away_gains)

	out <- matrix(nrow = 3, ncol = 1)
	out[1] <- mean(gains)
	out[2] <- sd(gains)
	out[3] <- length(gains)

	return(out)

}





design_grids <- list(mu = matrix(nrow = 500, ncol = 500, 0),
						sigma = matrix(nrow = 500, ncol = 500, 0),
						n = matrix(nrow = 500, ncol = 500, 0))


min_gaps <- seq(from = 0, to = 1, length.out = 500)
lower_p <- seq(from = 0, to = 1, length.out = 500)


for(i in c(1:500)){

	for(j in c(1:500)){

		temp <- gains(min_gaps[i], lower_p[j])

		design_grids[[1]][i,j] <- temp[1]
		design_grids[[2]][i,j] <- temp[2]
		design_grids[[3]][i,j] <- temp[3]

	}

}



design_grids[[4]] <- design_grids[[1]] / design_grids[[2]]

#No sharpe ratio for n <= 200
rmv <- which(design_grids[[3]] <= 200)
if(length(rmv) > 0){design_grids[[4]][rmv] <- NA}



for(i in 1:length(design_grids)){

	fix <- which(design_grids[[i]] == 0)

	if(length(fix) > 0){

		design_grids[[i]][fix] <- NA

	}

}



library(plotly)

contour_plot_mu <- plot_ly(z = design_grids[[1]], x = min_gaps, y = lower_p, type = "contour" ) %>% 
					layout(title = "Mean return per dollar",
							xaxis = list(zeroline = TRUE,
											range = c(min(min_gaps), max(min_gaps)),
											title = "Minimum return gap to bet"
									),
							yaxis = list(zeroline = TRUE,
											range = c(min(lower_p), max(lower_p)),
											title = "Minimum estimated odds of winning to bet"
									)
							)
					
print(contour_plot_mu)



contour_plot_sigma <- plot_ly(z = design_grids[[2]], x = min_gaps, y = lower_p, type = "contour" ) %>% 
					layout(title = "Return per dollar standard deviation",
							xaxis = list(zeroline = TRUE,
											range = c(min(min_gaps), max(min_gaps)),
											title = "Minimum return gap to bet"
									),
							yaxis = list(zeroline = TRUE,
											range = c(min(lower_p), max(lower_p)),
											title = "Minimum estimated odds of winning to bet"
									)
							)
					
print(contour_plot_sigma)




contour_plot_n <- plot_ly(z = design_grids[[3]], x = min_gaps, y = lower_p, type = "contour" ) %>% 
					layout(title = "Number of bets made",
							xaxis = list(zeroline = TRUE,
											range = c(min(min_gaps), max(min_gaps)),
											title = "Minimum return gap to bet"
									),
							yaxis = list(zeroline = TRUE,
											range = c(min(lower_p), max(lower_p)),
											title = "Minimum estimated odds of winning to bet"
									)
							)
					
print(contour_plot_n)



contour_plot_sharpe <- plot_ly(z = design_grids[[4]], x = min_gaps, y = lower_p, type = "contour" ) %>% 
					layout(title = "Sharpe ratios",
							xaxis = list(zeroline = TRUE,
											range = c(min(min_gaps), max(min_gaps)),
											title = "Minimum return gap to bet"
									),
							yaxis = list(zeroline = TRUE,
											range = c(min(lower_p), max(lower_p)),
											title = "Minimum estimated odds of winning to bet"
									)
							)
					
print(contour_plot_sharpe)






#Profit function
#Must be callable on a single row

#Arbitrage, betting 1$
arbitrage_simple <- function(x){

	if(x$R1 > x$R1_p){return(x$R1 * x$Win - 1)}

	else if(x$R2 > x$R2_p){return(x$R2 * (1 - x$Win) - 1)}

	else(return(0))

}

#Random strategy, betting 1$
random_bet <- function(x){

	u <- runif(min = 0, max = 1, n = 1)

	if(u > 0.5){return(x$R1 * x$Win - 1)}

	else(return(x$R2 * (1 - x$Win) - 1))

}

#Betting only on wins
win_bet <- function(x){

	if(x$P1_p > x$P2_p){return(x$R1 * x$Win - 1)} 

	else{return(x$R2 * (1 - x$Win) - 1)}

}



#Function to get the returns vector
return_vector <- function(f){
	bet_returns <- rep(0, nrow(offered_return))

	for(i in 1:length(bet_returns)){bet_returns[i] <- f(offered_return[i,])}

	return(bet_returns)
}


#Arbitrage with treshold


optim_f <- function(t){

	dummy_f <- function(x){

		if(x$R1 > x$R1_p & x$R1 - x$R1_p > t){return(x$R1 * x$Win - 1)}

		else if(x$R2 > x$R2_p & x$R2 - x$R2_p > t){return(x$R2 * (1 - x$Win) - 1)}

		else(return(0))

	}

	x <- return_vector(dummy_f)

	return(-sum(x))

}

optim_t <- optimise(optim_f, interval = c(0,2))$minimum


t_bet <- function(x){

	if(x$R1 > x$R1_p & x$R1 - x$R1_p > optim_t){return(x$R1 * x$Win - 1)}

	else if(x$R2 > x$R2_p & x$R2 - x$R2_p > optim_t){return(x$R2 * (1 - x$Win) - 1)}

	else(return(0))

}



#Function to plot the gains
plot_gains <- function(x){

	x <- as.data.frame(x[which(x > 0)])
	colnames(x) <- c("Gains")

	return(ggplot(x, aes(x = Gains)) + geom_density(color="darkblue", fill="lightblue", alpha = 0.25) + geom_vline(xintercept = mean(x$Gains), linetype="dotted", 
                color = "blue", size=1.5))


}


#Function to get strat stats
strat_stats <- function(x){

	n_1 <- length(x)
	x <- x[which(x != 0)]
	n_2 <- length(x)

	out <- matrix(nrow = 4, ncol = 1)
	rownames(out) <- c("Bets_placed_p", "returns_on_dollar", "SD", "CV")
	colnames(out) <- "Stat"

	out[1] <- n_2 / n_1
	out[2] <- sum(x) / n_2
	out[3] <- sd(x)
	out[4] <- out[3] / out[2]

	return(out)

}


#Function to get strat_stats of list of strats

strat_stats_all <- function(returns, f){

	out <- strat_stats(returns[,1])

	for(i in c(2:ncol(returns))){out <- cbind(out, strat_stats(returns[,i]))}

	colnames(out) <- names(f)

	return(out)

}



#Function to compare strategies
#Code à optimiser
compare_strat <- function(f){

	n <- nrow(offered_return)
	returns <- matrix(nrow = n, ncol = length(f))
	colnames(returns) <- names(f)

	for(i in c(1:length(f))){

		returns[,i] <- return_vector(f[[i]])

	}

	r <- returns
	for(i in c(1:ncol(returns))){returns[,i] <- cumsum(returns[,i])}

	frame = as.data.frame(returns)
	frame$n_games <- c(1:nrow(frame))

	frame <- melt(frame, id.var = "n_games")
	colnames(frame)[2] <- "Strategy_used"
	colnames(frame)[3] <- "Profits"

	nm <- paste("Strategy_plot_", as.character(lambda), "_", as.character(n), ".png", sep = "")

	out <- list()
	out$returns <- returns
	out$plot <- ggplot(frame, aes(x = n_games, y = Profits, col = Strategy_used)) + geom_line() + geom_hline(yintercept=0, linetype="dashed", color = "red") + ggsave(nm)

	nm <- paste("Strategy_plot_", as.character(lambda), "_", as.character(n), sep = "")

	print(out$plot)
	print("Mean returns:", quote = FALSE)
	print(apply(returns, 2, mean))
	print("", quote = FALSE)
	print("Sd", quote = FALSE)
	print(apply(returns, 2, sd))
	print("", quote = FALSE)

	cv <- matrix(nrow = 1, ncol = length(f))
	colnames(cv) <- names(f)
	cv[] <- apply(returns, 2, sd) / apply(returns, 2, mean) 

	print("CV", quote = FALSE)
	print(cv)

	out$strat_stats <- strat_stats_all(r, f)
	out$delta <- r
	print("", quote = FALSE)
	print("", quote = FALSE)
	print("", quote = FALSE)

	print(out$strat_stats)

	return(out)

}




strats <- list()
strats$arbitrage <- arbitrage_simple
strats$p_win <- win_bet
strats$t_bet <- t_bet
strats$random <- random_bet

logistic_test <- compare_strat(strats)


