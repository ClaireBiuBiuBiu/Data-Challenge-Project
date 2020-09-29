# Data-Challenge-Project
This is the data science solution for the University of Maryland Data Challenge Competition

Generalized Linear Models applied (Logistic regression, linear regression, Poisson regression)

-----------------------------------------------------------------------------------------------------

Maryland Small Business Development Center (SBDC) provides free individual business consulting and group training to small businesses, both existing and pre-venture ones. The dataset lists businesses who received individual consulting services during the last 10 years (2009-2018) and includes: (1) economic impact outcomes achieved by the clients as a result of consulting; (2) consulting and training activity; (3) characteristics of the businesses, and (4) socio-demographics of the owners. 
The analysis could help uncover factors that determine positive economic impact of business consulting and may include, for example, the following areas:

●	Which (if any) socio-demographic, geographical, and industry (NAICS code) characteristics of pre-venture businesses do predict successful business start? (logistic) In other words, are clients with certain demographics more successful in starting business them others? (sql, grouping, model coefficient) Are there industries with higher success rate? (Tableau, and coefficient in model) Rate industries by the percentage of clients who started business. (rank + grouping, quantile)

●	What are the determinants of securing capital investments by the clients? Which of the available factors (if any) affect the amount of investments? (logistic (whether or not, hedel model), linear regression, variable creating, interaction, glm, normal+ log link)

●	Are there factors that predict increase in revenue and increase in the number of employees for existing businesses? If yes, what are the most important factors?


For question 3, we divided the dataset into existing businesses and non-existing businesses (pre-venture ones) according to the variable Business Status. And our analysis is based on those existing ones. The answer is absolutely yes, there exists some important factors that could predict the increase of both of them. The longer the total counselling time, the better for the increasing of new jobs and company revenue. And another thing needs to be noticed is that, the woman-owned industry behaves well in adding the revenue and number of employees for existing companies. Also, larger numbers of initial employees and initial revenues will be helpful for the capital accumulation and trainee growth. Lastly, industry Code53 performs excellent in this prediction and should be paid more attention to.
