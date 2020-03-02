# Overview

## Goals
* Build a classifier for MLB games outcomes
* Use said classifier to identify favourable moneylines

## Data
The data is made up of players, teams and moneylines samples from 2007 to 2019. More details below:
* Batting, pitching and scores: 31 900 matches scrapped at https://www.baseball-reference.com/
* Moneylines: 26 375 lines scrapped at https://www.sportsbookreview.com/
* Predicted lineups (for deployment) : https://www.rotowire.com/baseball/daily-lineups.php


# Data pre-processing 

## Regression matrix
We first compute a decaying average of players' statistics, then fit them into a matrix according to their respective positions (1B, 2B, 3B, SS, C, RF, CF, LF, DH, PR, PH). The last three positions are not avaible on the predicted lineups website, and are thus averaged on a per-match basis before being given the name "Position" + "Team". 

The weights used for the decaying average are 0.75<sup>n</sup> , n = 0, 1, ... , 7.

## Autocorrelations 
Averaging player statistics produce strong autocorrelations. Given the weights used, the expected autocorrelation at lags 0, ... , 7 for batting and pitching statistics can be seen via the heatmaps below. (Note that the persisting autocorrelations in the batting graph are due to the position binary variables, as players usually play the same position.)

![](https://i.imgur.com/NsZtR1G.png)
![](https://i.imgur.com/KWBUEtY.png)

# Feature selection

## Estimating feature importance  
The data will be sorted by date then split into 10 folds. Each fold will contain one serie of home game number {j, j + 10, ... , j + 10n} per team. This will segment the data in a way such that no decaying average have aggregated points in common. Each fold will be processed in the same way:
* Run glmnet, xgboost (linear and tree learners) and ranger. (Cross-validation, n = 5)
* Keep track of variables importance for each model.
* Store the results in a grid at each iteration

The conditions for possible relevancy per model are given below.

## Glmnet
Any variable with a non-zero coefficient is considered as possibly relevant. 

## Ranger
The *importance* setting of the ranger package was set to "permutation". We discard variables with a negative score (i.e.: permuting them resulted in an increase in accuracy). The mean of the non-negative scores is computed, and the remaining variables above said mean are considered as possibly relevant.
 
 ## Xgboost
 Any variable with a relative importance above its mean m<sup>-1</sup> is considered as possibly relevant. 
 
 ## Weeding-out retained features
If a variable is genuinely important, then we expect it to be consistently selected by the procedures described above. The odds of being retained can be estimated from a grid made up of Bernoulli random variables with **n** = number of features (624) and **m** = number of folds (10). 

Since features are pair-wise (i.e.: *x_home*, *x_away*), we fuse pairs together to produce a **n/2 x 2m** grid instead. Rows that sum up to zero are the discarded. Histograms for the number of times either member of the pair (*x_home*, *x_away*) was selected are displayed below.

![](https://i.imgur.com/HXvrQSy.png)
![](https://i.imgur.com/bRkkAY4.png)
![](https://i.imgur.com/zuA1rJ3.png)
![](https://i.imgur.com/lCI7jsN.png)


 
 













