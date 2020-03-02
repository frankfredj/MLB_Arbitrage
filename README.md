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

# Variable selection
The data will be sorted by date then split into 8 folds. Each fold will contain one serie of home game number {j, j + 8, ... , j + 8n} per team. This will segment the data in a way such that no decaying average have aggregated points in common. Each fold will be processed in the same way:
* Run our ensemble of models.
* Keep track of variable importance for each model.
* Store the results in a grid






