# Simulate FIFA 2018 results using Monte Carlo


## Preparation
1. Clone this repository `git clone https://github.com/hbuchta/fifa2018`
2. Download dataset with international football results: https://www.kaggle.com/martj42/international-football-results-from-1872-to-2017/data
3. place this file in the data subdirectory of project
4. Modify script parameters in script R/fifa2018 (see below)


## Script parameters

| Parameter | sample value | description |
| ---
| `opt.date` | `as.Date("2018-06-20")` |  For evaluation the strength of each team, existing results are used up to the following date. Forecast of future results is computed started from the day after this date
| `opt.rounds` | `100000` | number of monte carlo rounds
| `opt.years` | `30` | number of years backwards to estimate the strength of each team. Older matches are not taken into account
| `opt.fulldecay` | `0.01` | for estimating the strength of each team, games are weighted depending so that recent games are weighted more than past games. This factor gives the weight for games at the age of `opt.fulldecay`



## Some results
This script in `R/fifa2018_results.R` is used to plot some results. The following chart shows the situation up until June 20, 2018. The percentage value shows the probability for the team to win the world cup. As you can see, England profits from the win against Tunisia and also how the chances for Germany are reduced after loosing against Mexico.

![intermediate results](images/result_20180621.png)
