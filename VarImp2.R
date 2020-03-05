path <- "D:/MLB/"


n <- 8
lambda <- 0.75
split <- 10

library(caret)
library(caretEnsemble)
library(doParallel)
library(ggplot2)

library(VGAM)


path_temp <- paste("D:/MLB_R_models/importances_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
importances <- readRDS(path_temp)

features <- rownames(importances[[1]])
for(i in 2:length(importances)){

	features <- unique(c(features), rownames(importances[[i]]))

}





#Build with Bernoulli variable conditioned on whether or not a feature was deemed relevant at fold 1, 2, ... , split

grids <- list()
for(i in 1:ncol(importances[[1]])){

	grids[[i]] <- matrix(nrow = length(features), ncol = length(importances), 0)
	rownames(grids[[i]]) <- features
	colnames(grids[[i]]) <- paste("Fold", c(1:ncol(grids[[i]])), sep = "")

}

names(grids) <- colnames(importances[[1]])

for(i in 1:length(grids)){

	for(j in 1:length(importances)){

		if(i == 1){

			a <- which(importances[[j]][,i] > 0)

		} else if(i == 2){

			a <- which(importances[[j]][,i] > 0)
			b <- importances[[j]][a,i]

			a <- a[which(b > mean(b))]

		} else {

			a <- which(importances[[j]][,i] > mean(importances[[j]][,i]))

		}

		a <- match(rownames(importances[[j]])[a], rownames(grids[[i]]))
		grids[[i]][a,j] <- 1

	}

}



#Order the grids properly, home then away

k <- 1
home <- c()
away <- c()

for(i in 1:length(features)){

	a <- features[i]
	if(substr(a, nchar(a)-1, nchar(a)) == "_2"){

		home <- c(home, substr(a, 1, nchar(a) - 2))
		away <- c(away, a)

	}

}

a <- match(home, rownames(grids[[1]]))
a <- which(!is.na(a))

home <- match(home[a], features)
away <- match(away[a], features)

for(i in 1:length(grids)){

	grids[[i]] <- grids[[i]][c(home, away),]
}



#Feature selection

hit_graph <- function(k, grids){

	a <- c(1:(nrow(grids[[k]])/2))

	temp <- cbind(grids[[k]][a,], grids[[k]][-a,])


	keep <- which(apply(temp, 1, sum) != 0)
	temp <- temp[keep,]

	hits <- apply(temp, 1, sum)

	plot_frame <- as.data.frame(unique(hits))
	colnames(plot_frame) <- "Hits"
	plot_frame$Hits <- sort(plot_frame$Hits)

	plot_frame$N <- 0
	for(i in 1:length(plot_frame$Hits)){

		plot_frame$N[i] <- length(which(hits == plot_frame$Hits[i]))

	}

	print(ggplot(plot_frame) + 
	geom_bar(aes(x = Hits, y = N), stat = "identity") + 
	ggtitle(paste("Importance hits for model:", names(grids)[k])))

}


for(i in 1:length(grids)){hit_graph(i, grids)}





f_select <- function(k, grids, alpha){

	a <- c(1:(nrow(grids[[k]])/2))

	temp <- cbind(grids[[k]][a,], grids[[k]][-a,])

	keep <- which(apply(temp, 1, sum) != 0)
	temp <- temp[keep,]

	hits <- apply(temp, 1, sum) - 1

	N <- ncol(temp) - 1
	rate <- 1 + N*length(hits) - sum(hits)
	shape <- 1 + sum(hits)

	p_vals <- pbetabinom(hits, N, (shape - hits) / (rate + shape - hits))

	keep <- which(p_vals >= 1 - alpha)

	keep <- rownames(temp)[keep]
	keep <- c(keep, paste(keep, "_2", sep = ""))

	return(keep)

}


variables_per_model <- list()

variables_per_model[[1]] <- f_select(1, grids, 0.1)
variables_per_model[[2]] <- f_select(2, grids, 0.25)
variables_per_model[[3]] <- f_select(3, grids, 0.05)
variables_per_model[[4]] <- f_select(4, grids, 0.05)
variables_per_model[[5]] <- unique(unlist(variables_per_model))

names(variables_per_model) <- c(names(grids), "NN")

path_temp <- paste("D:/MLB_R_models/retained_features_", as.character(lambda), "_", as.character(n), ".rds", sep = "")
saveRDS(variables_per_model, path_temp)


