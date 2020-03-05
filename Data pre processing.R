path <- "D:/MLB/"


n <- 8
lambda <- 0.75
split <- 10

w <- lambda^c(1:n)
w <- w / sum(w)




debugg <- function(x, ID){

	return(x[which(x$ID == ID), c("ID", "Date", "Team", "Name")])

}

debugg_2 <- function(x, ID){

	return(x[which(x$ID == ID), c("ID", "Date", "Team_1", "Team_2")])

}

print("Loading data...", quote = FALSE)




bat <- read.csv(paste(path, "bat_clean", ".csv", sep = ""))
pitch <- read.csv(paste(path, "pitch_clean", ".csv", sep = ""))

bat <- bat[which(!is.na(bat$Date)), ]
pitch <- pitch[which(!is.na(pitch$Date)), ]

temp <- colnames(bat)
for(i in 1:length(temp)){
	if(substr(temp[i],1,1) == "X" & nchar(temp[i]) > 2){
		colnames(bat)[i] = substr(temp[i],2,nchar(temp[i]))
	}
}

temp <- colnames(pitch)
for(i in 1:length(temp)){
	if(substr(temp[i],1,1) == "X" & nchar(temp[i]) > 2){
		colnames(pitch)[i] = substr(temp[i],2,nchar(temp[i]))
	}
}



bat$Date <- as.numeric(as.POSIXct(bat$Date, format = "%Y-%m-%d")) 
index <- order(bat$Date, decreasing = TRUE)
bat <- bat[index,]

pitch$Date <- as.numeric(as.POSIXct(pitch$Date, format = "%Y-%m-%d")) 
index <- order(pitch$Date, decreasing = TRUE)
pitch <- pitch[index,]


scores <- read.csv(paste(path, "scores_reg", ".csv", sep = ""))

date_string <- scores$Date

scores$Date <- as.numeric(as.POSIXct(scores$Date, format = "%Y-%m-%d")) 
index <- order(scores$Date, decreasing = TRUE)
scores <- scores[index,]
date_string <- date_string[index]


k <- which(colnames(bat) == "Batting")
colnames(bat)[k] <- "Name"

k <- which(colnames(pitch) == "Pitching")
colnames(pitch)[k] <- "Name"


#Correction

n_v <- 2
cnames <- c("Streak", "LStreak", "In_a_row", "THA", "Rested_days")

frames <- list()
frames_2 <- list()

for(i in 1:n_v){

	frames[[i]] <- matrix(nrow = nrow(scores), ncol = length(cnames), 0)
	frames_2[[i]] <- matrix(nrow = nrow(scores), ncol = length(cnames), 0)

	colnames(frames[[i]]) <- paste(cnames, as.character(i), sep = "_")
	colnames(frames_2[[i]]) <- paste(cnames, as.character(i), sep = "_")

}



teams <- as.character(unique(scores$Team_1))


for(k in 1:length(teams)){


	index_h <- which(scores$Team_1 == teams[k]) 
	index_a <- which(scores$Team_2 == teams[k]) 

	m_1 <- scores[c("Team_2", "Score_1", "Score_2", "Date")][index_h, ]
	m_2 <- scores[c("Team_1", "Score_2", "Score_1", "Date")][index_a, ]

	m_1$Home <- 1
	m_2$Home <- 0

	colnames(m_1) <- c("Opp", "Score_t", "Score_opp", "Date", "Home")
	colnames(m_2) <- colnames(m_1)

	m <- rbind(m_1, m_2)[order(c(index_h, index_a)),]

	hm_index <- c(index_h, index_a)[order(c(index_h, index_a))]
	h1 <- match(index_h, hm_index)
	hm_index <- hm_index[match(index_h, hm_index)]

	aw_index <- c(index_h, index_a)[order(c(index_h, index_a))]
	a1 <- match(index_a, aw_index)
	aw_index <- aw_index[match(index_a, aw_index)]

	m$Win <- 1
	m$Win[which(m$Score_t < m$Score_opp)] <- 0

	m$Streak <- 0

	s <- 1
	for(i in 1:nrow(m)){

		j <- nrow(m) - i + 1

		if(s > 0){

			if(m$Win[j] == 1){

				s <- s + 1

			} else {

				s <- -1

			}

		} else {

			if(m$Win[j] == 0){

				s <- s - 1

			} else {

				s <- 1

			}

		}

		m$Streak[j] <- s		

	}

	m$LStreak <- 0

	neg <- which(m$Streak < 0)
	m$LStreak[neg] <- -m$Streak[neg]
	m$Streak[neg] <- 0


	m$In_a_row <- 0

	s <- 1
	for(i in (nrow(m)-1):1){

		if(m$Opp[i] == m$Opp[i + 1]){

			s <- s + 1

		} else {

			s <- 1

		}

		m$In_a_row[i] <- s

	}

	m$Rested_days <- c(abs(diff(m$Date)), 0) / (24 * 60 * 60)


	m$THA <- 0

	s <- 1
	for(i in nrow(m):1){

		if(s > 0){

			if(m$Home[i] == 1){

				s <- s + 1

			} else {

				s <- -1

			}

		}

			 else {

			if(m$Home[i] == 0){

				s <- s - 1

			} else {

				s <- 1

			}

		}

		m$THA[i] <- s

	}

	m$THA <- abs(m$THA)

	m <- as.matrix(m[c("Streak", "LStreak", "In_a_row", "THA", "Rested_days")])

	frames_2[[1]][hm_index, ] <- m[h1,]	
	frames_2[[2]][aw_index, ] <- m[a1,]	

	k <- nrow(m)

	m[c(1:(k-1)) ,1] <- m[c(2:k) ,1]
	m[c(1:(k-1)) ,2] <- m[c(2:k) ,2]

	frames[[1]][hm_index, ] <- m[h1,]	
	frames[[2]][aw_index, ] <- m[a1,]	


}

scores$Win <- 1
scores$Win[which(scores$Score_1 < scores$Score_2)] <- 0

frames <- cbind(frames[[1]], frames[[2]])
frames_2 <- cbind(frames_2[[1]], frames_2[[2]])

now <- scores

scores <- cbind(scores, frames)

write.csv(cbind(now, frames_2), paste(path, "current_stats.csv", sep = ""), row.names = FALSE)
write.csv(scores, paste(path, "scores_with_reg_vars.csv", sep = ""), row.names = FALSE)



#On estime les effets des sous-positions qui ne sont pas disponibles dans les starting lineups


sub_pos <- c('PH','PR', 'DH')
pos = c("1B", "2B", "SS", "3B", "C", "RF", "CF", "LF")

random_vars <- apply(bat[pos], 1, sum)
random_vars <- which(random_vars == 0)

bat_random <- bat[random_vars,]
bat <- bat[-random_vars,]


random_effect_frame <- function(mat, sub_pos, i){

	index = which(mat[sub_pos[i]] == 1)
	mat <- mat[index,]

	mat$Name <- paste(mat$Team, sub_pos[i], sep = "_")

	return(mat)
}

temp <- random_effect_frame(bat_random, sub_pos, 1)
for(i in 2:length(sub_pos)){

	temp <- rbind(temp, random_effect_frame(bat_random, sub_pos, i))

}

bat_random <- temp




#Clean duplicates

clean_dup <- function(bat){

	numeric_index_bat <- which(sapply(bat, class) != "factor" & sapply(bat, class) != "character")
	numeric_index_bat <- numeric_index_bat[which(colnames(bat)[numeric_index_bat] != "ID" & colnames(bat)[numeric_index_bat] != "Date")]

	out <- as.matrix(bat[,numeric_index_bat])

	players <- as.character(unique(bat$Name))

	rmv <- c()

	print("Averaging and dropping duplicates...", quote = FALSE)
	tp <- txtProgressBar(min = 0, max = length(players), style = 3)

	drop_all <- c()

	for(i in 1:length(players)){

		index <- which(bat$Name == players[i])

		x <- bat$Date[index]
		d <- diff(x)
		d <- c(d,1)

		problem <- which(d == 0)

		if(length(problem) == 0){

			setTxtProgressBar(tp, i)
			next

		}

		d[which(d != 0)] <- 1


		drop <- c()

		while(TRUE){

			frm = problem[1]
			to = which(d[c(frm:length(d))] != 0)[1] + frm - 1
			
			w <- rep(1 / (to - frm + 1), to - frm + 1)
			out[index[frm],] <- w %*% out[index[c(frm:to)],]

			bat[index[c(frm:to)],]

			drop <- c(c(drop), c((frm+1):to))

			problem <- problem[which(problem > to)]
			if(length(problem) == 0){break}

		}

		drop_all <- c(drop_all, drop)

		setTxtProgressBar(tp, i)

	}

	bat[,numeric_index_bat] <- out
	bat <- bat[-drop_all,]


	print(paste("Dropped ", as.character(length(drop_all)), " duplicates.", sep = ""), quote = FALSE)

	return(bat)

}


bat <- clean_dup(bat)
pitch <- clean_dup(pitch)
bat_random <- clean_dup(bat_random)

temp <- NULL


#Find missing matches

mia <- match(scores$ID, bat$ID)
mia <- which(is.na(mia))

mia2 <- match(scores$ID, pitch$ID)
mia2 <- which(is.na(mia2))

mia <- sort(unique(c(mia, mia2)))

if(length(mia) > 0){

	temp <- scores[mia,]
	temp$Date <- date_string[mia]

	write.csv(temp, paste(path, "to_scrape.csv", sep = ""), row.names = FALSE)

	scores <- scores[-mia,]

}





#Calculate the autocorrelations

print("Calculating autocorelations...", quote = FALSE)

numeric_index_bat <- which(sapply(bat, class) != "factor" & sapply(bat, class) != "character")
numeric_index_pitch <- which(sapply(pitch, class) != "factor" & sapply(pitch, class) != "character")

rmv <- which(colnames(bat)[numeric_index_bat] == "Date")
numeric_index_bat <- numeric_index_bat[-rmv]

rmv <- which(colnames(pitch)[numeric_index_pitch] == "Date")
numeric_index_pitch <- numeric_index_pitch[-rmv]

rmv <- which(colnames(bat)[numeric_index_bat] == "ID")
numeric_index_bat <- numeric_index_bat[-rmv]

rmv <- which(colnames(pitch)[numeric_index_pitch] == "ID")
numeric_index_pitch <- numeric_index_pitch[-rmv]


mat_bat <- as.matrix(bat[,numeric_index_bat])
mat_pitch <- as.matrix(pitch[,numeric_index_pitch])
mat_bat_random <- as.matrix(bat_random[,numeric_index_bat])





auto_cov_moments <- function(bat, mat_bat, n){

	names <- unique(as.character(bat$Name))

	atc_bat_1 <- matrix(nrow = n, ncol = ncol(mat_bat), 0)

	atc_bat_2 <- matrix(nrow = n, ncol = ncol(mat_bat), 0)

	n_bat <- matrix(nrow = 1, ncol = ncol(mat_bat), 1)

	pb <- txtProgressBar(min = 0, max = length(names), style = 3)

	for(j in 1:length(names)){

		name <- names[j]
		index <- which(bat$Name == name)

		if(length(index) < n){

			setTxtProgressBar(pb,j)
			next

		}

		temp <- scale(mat_bat[index,])
		keep <- which(!is.na(temp[1,]))

		temp <- temp[,keep]

		f <- function(x){return(acf(x, type = "covariance", plot = FALSE)$acf)}

		ac <- apply(temp, 2, f)

		if(nrow(ac) < n){

			setTxtProgressBar(pb,j)
			next

		} 

		ac <- ac[c(1:n),]

		rmv <- which(is.na(ac), arr.ind = TRUE)
		if(length(rmv) > 0){

			ac <- ac[,-rmv]
			keep <- keep[-rmv]

		}

		atc_bat_1[,keep] <- ((n_bat[keep] - 1) / n_bat[keep]) * atc_bat_1[,keep] + (1 / n_bat[keep]) * ac
		atc_bat_2[,keep] <- ((n_bat[keep] - 1) / n_bat[keep]) * atc_bat_2[,keep] + (1 / n_bat[keep]) * ac^2

		n_bat[keep] <- n_bat[keep] + 1

		setTxtProgressBar(pb,j)

		
	}

	atc_bat_2 <- atc_bat_2 - atc_bat_1^2

	#for(i in 1:ncol(atc_bat_1)){

		#atc_bat_1[,i] <- atc_bat_1[,i] / atc_bat_1[1,i]
		#atc_bat_2[,i] <- atc_bat_2[,i] / atc_bat_1[1,i]^2

	#}

	fix <- which(is.na(atc_bat_1))
	if(length(fix) > 0){atc_bat_1[fix] <- 0}

	fix <- which(is.na(atc_bat_2))
	if(length(fix) > 0){atc_bat_2[fix] <- 0}

	colnames(atc_bat_1) <- colnames(mat_bat)
	colnames(atc_bat_2) <- colnames(mat_bat)


	return(list(mu = atc_bat_1, sigma = atc_bat_2 - atc_bat_1^2))

}	



auto_cov_mats <- function(autocovs_bat){

	out <- list()

	m <- nrow(autocovs_bat[[1]])

	for(k in 1:ncol(autocovs_bat[[1]])){

		temp <- matrix(nrow = m, ncol = m)
		colnames(temp) <- paste("acov", c(1:m), sep = "")
		rownames(temp) <- colnames(temp)

		for(i in 1:m){

			for(j in 1:m){

				temp[i,j] <- autocovs_bat[[1]][abs(i - j) + 1, k]

			}

		}

		out[[k]] <- temp

	}

	names(out) <- colnames(autocovs_bat[[1]])

	return(out)

}



library(ggplot2)
library(reshape2)

auto_cor_w <- function(autocovs_mat_bat,w){

	m <- ncol(autocovs_mat_bat[[1]])

	out <- matrix(nrow = m, ncol = length(autocovs_mat_bat))

	for(j in 1:ncol(out)){

		for(i in 1:m){

			p_1 <- c(1:m)
			p_2 <- c(i:(m+i-1))

			x <- matrix(nrow = m, ncol = m, 0)

			for(u in 1:nrow(x)){

				for(v in 1:ncol(x)){

					d = abs(p_1[u] - p_2[v]) + 1

					if(d <= m){

						x[u,v] <- autocovs_mat_bat[[j]][d,1]

					}

				
				}

			}

			out[i,j] <- t(w) %*% x %*% w

		}

		out[,j] <- out[,j] / out[1,j]

	}

	colnames(out) <- names(autocovs_mat_bat)
	rownames(out) <- paste("lag" , c(0:(m-1)), sep = "")

	fix <- which(is.na(out) | abs(out) > 1)
	if(length(fix) > 0){out[fix] <- 0}

	print(ggplot(melt(out), aes(Var2,Var1, fill=value)) + geom_raster() +
		scale_fill_gradient2(low = "red", mid = "blue", high = "red") + 
			theme(axis.text.x = element_text(angle = 90, hjust = 1))) +
				xlab("Variable") + ylab("Auto-Correlations")

	return(out)


}



 	
autocovs_bat <- auto_cov_moments(bat, mat_bat, n)
autocovs_pitch <- auto_cov_moments(pitch, mat_pitch, n)

autocovs_mat_bat <- auto_cov_mats(autocovs_bat)
autocovs_mat_pitch <- auto_cov_mats(autocovs_pitch)


autocors_w_bat <- auto_cor_w(autocovs_mat_bat , w)
autocors_w_pitch <- auto_cor_w(autocovs_mat_pitch, w)


#Compute rolling averages



get_avg <- function(bat, mat_bat, w){

	out <- matrix(nrow = nrow(mat_bat), ncol = ncol(mat_bat))
	colnames(out) <- colnames(mat_bat)
	players <- unique(as.character(bat$Name))

	k <- length(w)

	most_recent <- matrix(nrow = length(players), ncol = ncol(mat_bat))

	tp <- txtProgressBar(min = 0, max = length(players), style = 3)

	for(i in 1:length(players)){

		player <- players[i]

		index <- which(bat$Name == player)

		if(length(index) < 2){next}

		temp <- mat_bat[index,]

		w_temp <- w[c(1:min(length(w), length(index)))]
		w_temp <- w_temp / sum(w_temp)

		to_mult <- temp[c(1:min(length(w), length(index))), ]

		most_recent[i,] <- t(w_temp) %*% to_mult

		for(j in 2:(length(index))){

			to <- min(length(index), j + k - 1)

			to_mult <- temp[c(j:to),]
			w_temp <- w[c(1:(to - j + 1))]
			w_temp <- w_temp / sum(w_temp)

			out[index[j-1],] <- t(w_temp) %*% to_mult

		}

		setTxtProgressBar(tp, i)

	}

	meds <- apply(out, 2, median, na.rm = TRUE)

	m <- length(unique(which(is.na(out), arr.ind = TRUE)[,1])) / nrow(out)
	m <- round(m, 7) * 100

	print(paste("Imputing the remaining ", as.character(m), "% with medians.", sep = ""), quote = FALSE)

	for(j in 1:ncol(out)){

		fix <- which(is.na(out[,j]))
		out[fix,j] <- meds[j]

	}


	colnames(most_recent) <- colnames(mat_bat)
	most_recent <- as.data.frame(most_recent)
	most_recent$Name <- as.factor(players)

	print("Done.", quote = FALSE)
	return(list(avg = out, most_recent = most_recent))

}



avg_bat <- get_avg(bat, mat_bat, w)
write.csv(avg_bat$most_recent, paste(path, "bat_most_recent", ".csv", sep = ""), row.names = FALSE)
avg_bat <- avg_bat$avg

avg_bat_random <- get_avg(bat_random, mat_bat_random, w)
write.csv(avg_bat_random$most_recent, paste(path, "bat_random_most_recent", ".csv", sep = ""), row.names = FALSE)
avg_bat_random <- avg_bat_random$avg


avg_pitch <- get_avg(pitch, mat_pitch, w)
write.csv(avg_pitch$most_recent, paste(path, "pitch_most_recent", ".csv", sep = ""), row.names = FALSE)
avg_pitch <- avg_pitch$avg

bat_random$Name <- as.factor(bat_random$Name)







reconstr <- function(k){

	out_list <- list()

	temp1 <- avg_bat_random[which(bat_random[sub_pos[k]] == 1), ]
	temp2 <- bat_random[which(bat_random[sub_pos[k]] == 1), ]

	teams <- unique(temp2$Team)

	print("Building random effect frame...")
	tb <- txtProgressBar(min = 0, max = length(teams), style = 3)

	for(i in 1:length(teams)){

		index <- which(bat$Team == teams[i])
		IDs <- unique(bat$ID[index])

		temp_avg <- as.data.frame(matrix(nrow = length(IDs), ncol = ncol(avg_bat_random))) 
		colnames(temp_avg) <- colnames(avg_bat_random)

		temp_data <- as.data.frame(matrix(nrow = length(IDs), ncol = ncol(bat_random)))
		colnames(temp_data) <- colnames(bat_random)
		temp_data$ID <- IDs

		have_index <- which(temp2$Team == teams[i])
		a <- temp1[have_index,]
		b <- temp2[have_index,]


		paste_index <- match(temp_data$ID, b$ID)
		i_1 <- which(!is.na(paste_index))
		i_2 <- paste_index[which(!is.na(paste_index))]

		temp_avg[i_1,] <- a[i_2,]
		temp_data[i_1,] <- b[i_2,]

		temp_data$Team[i_1] <- as.character(b$Team[i_2])
		temp_data$Team <- as.factor(temp_data$Team)

		temp_data$Name[i_1] <- as.character(b$Name[i_2])
		temp_data$Name <- as.factor(temp_data$Name)


		last <- which(!is.na(temp_avg), arr.ind = TRUE)[,1]
		last <- max(last)

		temp_avg[nrow(temp_avg),] <- temp_avg[last,]
		temp_data[nrow(temp_data),] <- temp_data[last,]


		for(j in (nrow(temp_data)-1):1){

			if(is.na(temp_avg[j,1])){

				temp_avg[j,] <- temp_avg[j+1,]
				temp_data[j,] <- temp_data[j+1,]

			}

		}	
		

		temp_data$ID <- IDs	

		date_index <- match(temp_data$ID, bat$ID)
		temp_data$Date <- bat$Date[date_index]


	out_list[[i]] <- list(avg = temp_avg, data = temp_data)
	setTxtProgressBar(tb, i)

	}

	out_avg <- out_list[[1]][[1]]
	out_data <- out_list[[1]][[2]]

	for(i in 2:length(out_list)){

		out_avg <- rbind(out_avg, out_list[[i]][[1]])
		out_data <- rbind(out_data, out_list[[i]][[2]])


	}

	colnames(out_avg) <- colnames(avg_bat_random)
	colnames(out_data) <- colnames(bat_random)

	index <- order(out_data$Date, decreasing = TRUE)

	out_avg <- out_avg[index,]
	out_data <- out_data[index,]

	return(list(avg = out_avg, data = out_data))

}


temp <- list()
for(i in 1:length(sub_pos)){temp[[i]] <- reconstr(i)}

avg_bat_random <- temp[[1]][[1]]
bat_random <- temp[[1]][[2]]

for(i in 2:length(sub_pos)){

	avg_bat_random <- rbind(avg_bat_random, temp[[i]][[1]])
	bat_random <- rbind(bat_random, temp[[i]][[2]])

}

bat <- rbind(bat, bat_random)
avg_bat <- rbind(avg_bat, avg_bat_random)


temp <- NULL
avg_bat_random <- NULL
bat_random <- NULL

#Aggregate matrices 

positions = c(c("1B", "2B", "SS", "3B", "C", "RF", "CF", "LF"), sub_pos)

agg_bat <- function(i, positions){

	ID <- scores$ID[i]
	index_bat <- which(bat$ID == ID)
	index_col <- match(positions, colnames(avg_bat))

	teams <- as.character(bat$Team[index_bat])

	index <- index_bat[which(teams == scores$Team_1[i])]

	temp <- avg_bat[index,-index_col]
	pos <- bat[positions][index,]

	s <- apply(pos, 2, sum)

	for(j in 1:ncol(pos)){

		if(s[j] != 0){pos[,j] <- pos[,j] / s[j]}

	}

	if(sum(pos[,c(1:8)]) < 6){return(c(0))}

	actual_players <- which(apply(pos[,c(1:8)], 1, sum)  > 0)

	fix <- which(apply(pos[,c(1:8)], 2, sum)  == 0)
	if(length(fix) > 0){pos[actual_players,fix] <- 1/length(actual_players)}	


	pos_mu <- matrix(nrow = nrow(pos), ncol = 1, 0)
	pos_mu[actual_players] <- 1 / length(actual_players)
	colnames(pos_mu) <- "mu"
	pos <- cbind(pos, pos_mu)

	pos <- as.matrix(pos)
	temp <- as.matrix(temp)

	per_position <- t(pos) %*% temp
	m <- nrow(per_position)

	out <- t(as.matrix(per_position[1,]))
	colnames(out) <- paste(rownames(per_position)[1], colnames(per_position), sep = "_")

	for(j in 2:m){

	temp <- t(as.matrix(per_position[j,]))
	colnames(temp) <- paste(rownames(per_position)[j], colnames(per_position), sep = "_")

	out <- cbind(out,temp)

	}


	index <- index_bat[which(teams == scores$Team_2[i])]

	temp <- avg_bat[index,-index_col]
	pos <- bat[positions][index,]


	s <- apply(pos, 2, sum)

	for(j in 1:ncol(pos)){

		if(s[j] != 0){pos[,j] <- pos[,j] / s[j]}

	}

	if(sum(pos[,c(1:8)]) < 6){return(c(0))}

	actual_players <- which(apply(pos[,c(1:8)], 1, sum)  > 0)

	fix <- which(apply(pos[,c(1:8)], 2, sum)  == 0)
	if(length(fix) > 0){pos[actual_players,fix] <- 1/length(actual_players)}	


	pos_mu <- matrix(nrow = nrow(pos), ncol = 1, 0)
	pos_mu[actual_players] <- 1 / length(actual_players)
	colnames(pos_mu) <- "mu"
	pos <- cbind(pos, pos_mu)

	pos <- as.matrix(pos)
	temp <- as.matrix(temp)

	per_position <- t(pos) %*% temp
	m <- nrow(per_position)


	for(j in 1:m){

	temp <- t(as.matrix(per_position[j,]))
	colnames(temp) <- paste(rownames(per_position)[j], colnames(per_position), sep = "_")
	colnames(temp) <- paste(colnames(temp), "_2", sep = "")

	out <- cbind(out,temp)

	}

	rownames(out) <- ID


	return(out)

}



agg_pitch <- function(i){

	ID <- scores$ID[i]
	index_pitch <- which(pitch$ID == ID)
	index_col <- which(colnames(avg_pitch) == "Pit")

	teams <- as.character(pitch$Team[index_pitch])

	index <- index_pitch[which(teams == scores$Team_1[i])]


	if(length(index ) == 1){

		per_position <- avg_pitch[index,-index_col]

	} else {

	temp <- avg_pitch[index,]
	pos <- as.matrix(temp[,index_col])
	pos <- pos / sum(pos)

	temp <- avg_pitch[index,-index_col]

	temp <- as.matrix(temp)

	per_position <- t(pos) %*% temp

	}

	out <- per_position


	index <- index_pitch[which(teams == scores$Team_2[i])]

	if(length(index ) == 1){

		per_position <- t(as.matrix(avg_pitch[index,-index_col]))

	} else {

	temp <- avg_pitch[index,]
	pos <- as.matrix(temp[,index_col])
	pos <- pos / sum(pos)

	temp <- avg_pitch[index,-index_col]

	temp <- as.matrix(temp)

	per_position <- t(pos) %*% temp

	}

	m <- nrow(per_position)

	colnames(per_position) <- paste(colnames(per_position), "_2", sep = "")

	out <- cbind(out, per_position)

	rownames(out) <- ID

	return(out)

}



agg_mat <- function(i, positions){

	return(cbind(agg_bat(i, positions), agg_pitch(i)))

}



#Parallel loop

print("Computing final data frame...", quote = FALSE)

library(doParallel)

ncores <- detectCores() - 2

cl <- makeCluster(ncores) 
registerDoParallel(cl)

	rows <- foreach(i = 1:nrow(scores)) %dopar% {
	    res <- tryCatch({
	        agg_mat(i, positions)
	    }, error=function(e) c(0))
	}

stopCluster(cl)


ncols <- unlist(lapply(rows, length))

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

ncol <- getmode(ncols)

keep <- which(ncols == ncol)
scores <- scores[keep,]
rows <- rows[keep]


position_matrix <- matrix(nrow = nrow(scores), ncol = ncol)
colnames(position_matrix) <- colnames(rows[[which(ncols == ncol)[1]]])

tb <- txtProgressBar(min = 0, max = nrow(scores), style = 3)

for(i in 1:nrow(scores)){

		position_matrix[i, ] <- rows[[i]]

	setTxtProgressBar(tb, i)

}



meds <- apply(position_matrix, 2, median, na.rm = TRUE)

for(j in 1:ncol(position_matrix)){

	fix <- which(is.na(position_matrix[,j]))

	if(length(fix) > 0){

		position_matrix[fix,j] <- meds[j]

	}

}


away <- matrix(nrow = ceiling(ncol(position_matrix) / 2), ncol = 1)
vars <- rep("", length(away))

k <- 1

for(i in 1:ncol(position_matrix)){

	a <- colnames(position_matrix)[i]
	if(substr(a, nchar(a)-1, nchar(a)) == "_2"){

		away[k] <- i
		vars[k] <- substr(a, 1, nchar(a) - 2)
		k <- k + 1

	}

}

vars <- vars[which(!is.na(away))]
away <- away[which(!is.na(away))]

home <- match(vars, colnames(position_matrix))
away <- away[which(!is.na(home))]
home <- home[which(!is.na(home))]

position_matrix <- position_matrix[,c(home,away)]



scores$Win <- 0
scores$Win[which(scores$Score_1 > scores$Score_2)] <- 1


final_frame <- cbind.data.frame(scores, as.data.frame(position_matrix))



final_frame$Fold <- 0
teams <- unique(scores$Team_1)


for(i in 1:length(teams)){

	team <- teams[i]

	home_index <- which(final_frame$Team_1 == team)
	away_index <- which(final_frame$Team_2 == team)

	total <- sort(c(home_index,away_index))
	gap <- c(1:length(total))

	index <- match(home_index, total)

	gap <- gap[index]

	fold_index <- gap %% split + 1

	final_frame$Fold[home_index] <- fold_index

}

print("Saving...", quote = FALSE)

write.csv(final_frame, paste(path, "averages_", as.character(lambda), "_", as.character(n),  ".csv", sep = ""), row.names = FALSE)

print("Done.", quote = FALSE)


avg_bat <- as.data.frame(avg_bat)
avg_bat$ID <- bat$ID
avg_bat$Name <- bat$Name
avg_bat$Date <- bat$Date
avg_bat$Team <- bat$Team 

avg_pitch <- as.data.frame(avg_pitch)
avg_pitch$ID <- pitch$ID
avg_pitch$Name <- pitch$Name
avg_pitch$Date <- pitch$Date
avg_pitch$Team <- pitch$Team

write.csv(avg_bat, paste(path, "bat_averages_", as.character(lambda), "_", as.character(n),  ".csv", sep = ""), row.names = FALSE)
write.csv(avg_pitch, paste(path, "pitch_averages_", as.character(lambda), "_", as.character(n),  ".csv", sep = ""), row.names = FALSE)

write.csv(bat, paste(path, "bat_final_", as.character(lambda), "_", as.character(n),  ".csv", sep = ""), row.names = FALSE)
write.csv(pitch, paste(path, "pitch_final_", as.character(lambda), "_", as.character(n),  ".csv", sep = ""), row.names = FALSE)









