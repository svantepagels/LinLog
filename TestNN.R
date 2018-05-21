# Test neural network
# https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/

# Load data
set.seed(500)
library(MASS)
data <- Boston

# Check if missing data: No mmissing data
apply(data,2,function(x) sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

# Preparing to fit the neural network
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

plot(nn)

WELCOME!
  
  Here you will find daily news and tutorials about R, contributed by over 750 bloggers. 
There are many ways to follow us - 
  By e-mail:
  
  On Facebook: 
  If you are an R blogger yourself you are invited to add your own R content feed to this site (Non-English R bloggers should add themselves- here)
RSS JOBS FOR R-USERS

Author Books on R for Packt Publishing
Software Engineer in Test for RStudio
Phd opportunity @ Barcelona / Moorepark
Research Software Engineer @ Bailrigg, England
Senior Data Scientist @ Minneapolis, Minnesota, U.S.

POPULAR SEARCHES

googlevis
heatmap
twitter
latex
sales forecasting
web Scraping
eof
hadoop
random forest
sql
3 d clusters
anova
blotter
boxplot
decision tree
discriminant
financial
ggplot background grid colour
how to import image file to r
maps
pivot table
purrr
rattle
Trading
bar chart
barplot
Binary
climate
cohort
contingency table data frame
RECENT POSTS

datazar
Deep Learning Dude pt 1
Global choropleth maps of military expenditure
RTutor: The effect of the TseTse fly on African Development
Deferred & Remote Function Execution in R
R Weekly Bulletin Vol – X
(Linear Algebra) Do not scale your matrix
Hacking the principles of #openscience #workshops
Teach kids about R with Minecraft
Weather forecast with regression models – part 1
The code (and other stuff…)
Bringing Together People and Projects at Unconf17
A Shiny App for Exploring Commodities Prices and Economic Indicators, via Quandl
Python and R top 2017 KDnuggets rankings
Canada Labour Market: Future Perspectives
OTHER SITES

Jobs for R-users
SAS blogs
Fitting a neural network in R; neuralnet package
September 23, 2015
By Michy Alice

inShare
(This article was first published on DataScience+, and kindly contributed to R-bloggers)
1.6k
SHARES
Share
Tweet
Neural networks have always been one of the most fascinating machine learning model in my opinion, not only because of the fancy backpropagation algorithm, but also because of their complexity (think of deep learning with many hidden layers) and structure inspired by the brain.
Neural networks have not always been popular, partly because they were, and still are in some cases, computationally expensive and partly because they did not seem to yield better results when compared with simpler methods such as support vector machines (SVMs). Nevertheless Neural Newtorks have, once again, raised attention and become popular.

In this post we are going to fit a simple neural network using the neuralnet package and fit a linear model as a comparison.

The dataset
We are going to use the Boston dataset in the MASS package.
The Boston dataset is a collection of data about housing values in the suburbs of Boston. Our goal is to predict the median value of owner-occupied homes (medv) using all the other continuous variables available.

set.seed(500)
library(MASS)
data <- Boston
First we need to check that no datapoint is missing, otherwise we need to fix the dataset.

apply(data,2,function(x) sum(is.na(x)))

crim      zn   indus    chas     nox      rm     age     dis     rad     tax ptratio 
0       0       0       0       0       0       0       0       0       0       0 
black   lstat    medv 
0       0       0 
There is no missing data, good. We proceed by randomly splitting the data into a train and a test set, then we fit a linear regression model and test it on the test set. Note that I am using the gml() function instead of the lm() this will become useful later when cross validating the linear model.

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
The sample(x,size) function simply outputs a vector of the specified size of randomly selected samples from the vector x. By default the sampling is without replacement: index is essentially a random vector of indeces.
Since we are dealing with a regression problem, we are going to use the mean squared error (MSE) as a measure of how much our predictions are far away from the real data.

Preparing to fit the neural network
Before fitting a neural network, some preparation need to be done. Neural networks are not that easy to train and tune.

As a first step, we are going to address data preprocessing.
It is good practice to normalize your data before training a neural network. I cannot emphasize enough how important this step is: depending on your dataset, avoiding normalization may lead to useless results or to a very difficult training process (most of the times the algorithm will not converge before the number of maximum iterations allowed). You can choose different methods to scale the data (z-normalization, min-max scale, etc…). I chose to use the min-max method and scale the data in the interval [0,1]. Usually scaling in the intervals [0,1] or [-1,1] tends to give better results.
We therefore scale and split the data before moving on:
  
  maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]