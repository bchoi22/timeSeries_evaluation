# timeSeries_evaluation

# Finding most similar sections of time series (using Euclidean distance and DTW)
Using sliding window techniques to find most similar sections between two time series for a given window size.

Method 1: Using the Genetic algorithm (ga) to evaluate the similarity between 2 time series; 1 file (ga_EucDist(realValued).R) is for a given window size and other ga file (ga(realValued_with_window_param).R) is using the window size as a hyper parameter 

Method 2: Brute force method to find the most similar sections between 2 time series for a given window size (greedy_search_TS.R).  


# Evaluating a portfolio of stocks (time series matrix) for levels of performance volatility
Using the covariance to evaluate stock volatility and using weight vectors to measure the level of affect on the overall portfolio (portfolioPerformance_v1.R).   

