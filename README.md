# Fantasy-Basketball
 Our  goal  was to use statistical learning to outperform the other teams in our league. We  focused on using  predictive models that would be beneficial in playing fantasy basketball . We assumed the  2017 season  was about to begin and each team  needs  to select their players through a draft where each team will select 10 players in sequence.  We wanted to predict based on 2015 and 2016 data how well they will play in the 2017 season. We were  tasked  with building predictive models to help us make better decisions in our draft. 
 
 
Modeling Strategy 

We created new variables using the games_training data set . We began by first determining which week in the season each game was played for the 2015 and 2016 seasons. We then aggregated totals for every week to get a sum of PTS , ASTS, STL, BLK, TRB, FT% and 3PTS  for every week by player. We then summarized the available data into weekly averages for the two seasons.  We also computed the weekly standard deviations of  the seven categories to include in the modeling. The belief being the standard deviation could be a proxy to determine which players are the most consistent on a week to week basis. It also helps to explain players that may be more injury prone (and thus miss entire weeks at a time). We then also did the same tabulation for minutes played. 

After this we standardised all the variables in the model to ensure efficient comparability across all the variables . Next we used all the 2015 data points for all the variables created to predict the 2016 measures of  PTS , ASTS, STL, BLK, TRB, FT and 3PTS for each player . 

Initially we used random forest to model each of the 7  measures using all 324 players. Following the mock draft we expanded our and used more modelling approaches to see which model gives the better prediction MSE.



Since we are really only interested in how the “top” players perform, we subsetted the data to include the top 163 players. This determination was made by summing the standardized total for all seven categories and dropping those players whose sum was less than zero. The variables were all standardized, so a player with a sum less than zero is more than likely to be below the median player in most categories. 


We then built  models for each of the variables using 8 different  modelling approaches: Bagging, Random Forests, Boosting,K Nearest Neighbors , Simple Multi Linear Regression, Principal Components Regression and Partial Least Squares Regression.  We tuned each of these models for all the player statistics  to get the “best” version of that modeling type (the full results for all the models is in the Appendix). 


Next, we calculated a LOOCV for each model and calculated the averaged MSE for the various models. We also stored the predictions for each player from all eight modeling strategies. 


Inspection of the individual MSEs showed that some models were better at predicting a particular player than others, so we didn’t want to just discard the seven of the models and simply choose the best one. Instead we created a prediction weighting based on the MSE. First, we dropped the boosting model since it was clearly the worst for all the statistics. For the points, assists and FT%, the single tree was quite poor too so they were dropped for those three statistics respectively. We then created a weighting based on 1/MSE (so larger MSEs had a lesser weight) and the results of the weighted predictions are seen below:



Selection Strategy 

The first thing we did was create a ranking for each player by summing the scaled predictions for each statistic to get a total contribution. For the first two selections we make, we will choose the lowest ranked player remaining. 


Once we get to our third selection, each team will have at least two players so we can get a sense of the other team’s strengths. Now we will compare our team to the other three teams to see how many categories we will win based on our predictions (we averaged each category because at every point in the draft the number of players on each team will not be even). The algorithm then cycles through every remaining player to see which player will maximize the number of categories we will win. We will continue to use this process for the remaining eight picks.


Collaborators on this project were  
Nishant Sreekrishna, Palma Daawin and  Yongjun Liu


