# Topic
A discovery of what affects happiness

# Data
From [kaggle](https://www.kaggle.com/unsdsn/world-happiness/home)

# Methods
Regression <br/> Model Selection <br/> ANOVA <br/> PCA

# Workflow Notes
+ First we want to know if region has effect on happiness score, then we used boxplots on 2017 dataset. There is an obvious difference for different groups. 
+ Then we want to test if year is an important factors, we made another parallel boxplots (need to be done). We found year is not important, 
+ but what if year and region have an interaction? Then we used interaction plots to see if there is one. Then we found there is not. 
+ To make sure that, we used two-way anova to test interaction, and dropped interaction to test year and region. In conclusion, region is important but year is not.
+ We move to fix continuos factors. A pair plot shows that 1. there is corrrlation between factors, and 2. generosity does not seem to have relationship with score. 3. region has effect only on health, family and economy.
+ We used AIC and BIC to test different models. Backward model selection tells us to keep all six variables (With AIC). BIC shows that generosity is not important. 
+ Since there are correlations between continuous factors. ANOVA table return wierd result dut to collinearity. (Need to read book and fix problem) 
+ We tried to use PCA to reduce the correlation between variables. It is hard to interpret the result. We found that PCA does not drop many variables for our data. We need to keep four out six principal components to get 95% variance.
+ The above PCA did not work very well, we think it is because the different scales of our data. Thus, we standardized data and did the pca again. We tried to keep 4 PCs in the model, but the PC3 and PC4 are not statistically significant. 
+ We added region factor to the model which contains two PCs. ANOVA table shows that Region has significance on the model. And both AIC and BIC dropped comparing to the previous models. Next, we need some plots to support our findings. And Tukey test to find which region has significant influnce on happiness score.  
+ Tukey test and `facet_grid` plot shows that we can group ten regions to 3 groups.
+ We split data to test data and training data
