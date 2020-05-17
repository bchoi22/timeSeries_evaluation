# timeSeries_evaluation

# Finding most similar sections of time series (using Euclidean distance and DTW)
Using sliding window techniques to find most similar sections between two time series for a given window size.

Method 1: Using the ****Genetic algorithm (ga)** to evaluate the similarity between 2 time series; 1 file (ga_EucDist(realValued).R) is for a given window size and other ga file (ga(realValued_with_window_param).R) is using the window size as a hyper parameter 

**The ga algorithm uses concepts called cross-over probability and mutation to help redefine the solution space associated with your variables.  Basically, the ga() function redefines the domain of values for its variables by choosing the values with the highest fitness, encoding them into bits, and then creating similar values by applying cross-over probability – or the level of similarity to the values that were just used to gain the highest fitness in the first place.  After applying cross-over probability, the values then undergo a “mutation” – or the random selection of some bits such that to further differentiate these values from the original.  This is so that the algorithms search doesn’t get tunneled into a particular solution set, but designed to allow it to have a broader search area.  So the ga algorithm, once the values goes through these steps, gets added back into the search space or into the population. 

Method 2: Brute force method to find the most similar sections between 2 time series for a given window size (greedy_search_TS.R).  


# Evaluating a portfolio of stocks (time series matrix) for levels of performance volatility
Using the covariance to evaluate stock volatility and using weight vectors to measure the level of affect on the overall portfolio (portfolioPerformance_v1.R).   

