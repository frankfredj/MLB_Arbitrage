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

Since features are pair-wise (i.e.: *x_home*, *x_away*), we fuse pairs together to produce a **n/2 x 2m** grid instead. Rows that sum up to zero are then discarded. Histograms for the number of times either member of the pair (*x_home*, *x_away*) was selected are displayed below.

![](https://i.imgur.com/HXvrQSy.png)
![](https://i.imgur.com/bRkkAY4.png)
![](https://i.imgur.com/zuA1rJ3.png)
![](https://i.imgur.com/lCI7jsN.png)

We will treat the quantity **number of hits - 1** as a Binomial random variable with parameters **N = 2m - 1** and **P having a Gamma prior with parameters (α = β = 1)**, which will result in a Beta Binomial posterior predictive. Said Beta Binomial distribution can be used to approximate P(X >= x<sub>j</sub> | x<sub>1</sub> ... x<sub>j-1</sub>, x<sub>j+1</sub>, ... x<sub>n</sub>), which we will take as x<sub>j</sub>'s p-value given the sample it originated from. 

The alphas used for each model when selecting variables with respect to their posterior predictive's p-values are:

* glmnet: α = 0.1
* ranger: α = 0.25
* xgboost (linear and tree): α = 0.05

The union of the 4 sets of retained features will be used to train a neural network as our 5th model. The number of retained predictors for each model is:

* glmnet: m = 48
* ranger: m = 22
* xgboost linear: m = 72 
* xgboost tree: m = 76
* neural network: m = 116

# Model fitting

## Hyperparameters tuning

The xgboost and glmnet models are tuned with the package mlrMBO, whereas a random grid is used for ranger. In all cases, the chosen objective function to be maximised is the AUROC. 

All hyperparameters, including the neural network, were tuned using the most recent 6063 matches as out-of-fold validation data. Usual cross-validation cannot be used here, as there is unreasonable autocorrelations between successive matches by the same team. Moreoever, we need to validate our model using testing data that temporally supercedes the training set.

The achieved AUROC per models with optimal tuning were:

![](https://i.imgur.com/MXS8fAJ.png)



## Meta-Model

Once the tuning is done, mlrMBO is used once again to build a weighted average ensemble model using all but the most recent 2430 matches from the out-of-fold validation set. (I.e.: find the set of non-negative weights which maximizes the AUROC). The following weights were produced, with an associated out-of-fold AUROC of 0.8208:

![](https://i.imgur.com/yqQKwIO.png)


# Backtesting

The ensemble model's out-of-fold predictions were column-merged with their associated scores and moneylines in order to simulate one full MLB season betting run, as well as to compare the model's performance versus the odds suggested by the moneylines. Of the 2430 matches, 2309 had avaible moneylines to scrape and hence this will be the sample size of the final testing set.

## Model comparison

A quick summary of both models' respective performances is given below. (Note that the moneyline odds are refered to as *Book* in the following section.)

![](https://i.imgur.com/Hcc3ZsH.png)

![](https://i.imgur.com/DWAJzhp.png)

(Right: Model, left: book)

![](https://i.imgur.com/4YN8KSw.png)

## Betting

Consider our estimator p̂(x) alongside the offered returns R<sub>h</sub>(x) and R<sub>a</sub>(x). Our estimate for fair returns are the reciprocal of p̂(x) and 1 - p̂(x), which can in turn be used to identify favourable moneylines. Bets can then be placed based on the spread between the offered and estimated fair return rates, and on the estimated odds of winning said bet. Different contour plots were produced in order to visualise the mean, standard deviation and Sharpe ratio of the returns under constrained betting strategies, as well as the proportion of matches satisfying the constrains. Constrains eliminating more than 90% of the matches were discarded from our domain.

![](https://i.imgur.com/4lb7Mks.png)
![](https://i.imgur.com/8t8frVt.png)
![](https://i.imgur.com/1hlxw1O.png)
![](https://i.imgur.com/pdxJ3j0.png)

More careful strategies produce higher Sharpe ratio, at the expense of having less betting opportunities. A graph of the optimal statistics with respect to the miminum proportion of bets placed is given below.

![](https://i.imgur.com/mrmU9Fp.png)




 













