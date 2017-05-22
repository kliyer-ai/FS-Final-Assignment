###FINAL ASSIGNMENT


#Some general tips:

#remember you need plots of your effects. If you have many predictors, there can be too many graphs for use of the allEffects() function. In this case, it is better to plot the effects individually. 
#For main effects, you can do this by using plot(Effect("name of effect", name_of_model_object))
#For interactions, you can use plot(Effect(c("name of effect 1","name of effect 2"), name_of_model_object))
#If your interaction is between 2 numerical predictors, it's better to use the contour/elevation plots we went over a couple of times.

#When checking for multicollinearity, note that vif values for interactions and single predictors that participate in interactions can be quite high. This is nothing to worry about. Single predictors that do not participate in interactions, however, should have vif values below 10.

#If you violate an assumption, use a different method if we've learned about one. However, if not, just note the assumption violation when you do the reporting and what this means for the interpretation of the results.

#In addition to the standard reporting of the results, please give a brief discussion of the findings. For example, if there is a significant interaction, talk about why this might be. What does the interaction actually mean logically? It's ok to speculate a bit here, since this is a statistics course and not a course about any one particular field.

#You'll have to load the necessary libraries! I do not provide those lines of code here!

#Also, when using drop1(), you must use the Chisq test instead of the F test when you have created a model using random intercepts.

#It may be necessary to remove rows that contain missing data!


###################
#####Swimming Data
###################

#datafile: swimming.csv

#In this study, the times it took for different participants to swim a lap in a swimming pool were measured. Swimmers varied in terms of which end of the pool they started from (End variable), whether they wore goggles, whether they wore a shirt, and whether they used flippers. Perform an analysis and consider up to 2-way interactions between predictors.
swim = read.delim(file = "swimming.csv", header = T)
ggplot(swim, aes(Shirt, Time)) + geom_boxplot(notch = T) + stat_summary(fun.y = mean, geom = "point", color = "red")

library(ggplot2)
library(effects)
library(car)

swim_model1 = lm(swim$Time ~ swim$Shirt + swim$Goggles + swim$Flippers + swim$End); summary(swim_model1)
#if there is no interaction, our adjusted r2 is 0.3

swim_model2 = lm(Time ~ Shirt + Goggles + Flippers + End + Goggles:Flippers, data = swim)
summary(swim_model2)
#we chose that interaction to see if the combination of flippers and goggles make you even faster
#when we add the interaction our variance explained goes up by 10 percent -> better fit
plot(allEffects(swim_model2))
#if we look at the interaction, we see that when someone wears no flippers but does wear goggles, that person has a faster time than wearing no goggles. But when someone wears flippers and also wears goggels, this person is actually slower than a person who does wear flippers but no goggels.

#check if we can drop predictors
drop1(swim_model2, test="F")
#interaction is significant and we cannot drop predictors that are involved in an interaction. shirt predictors is significant as well. End, however, is not significant -> will be dropped
swim_model3 = lm(swim$Time ~ swim$Shirt + swim$Goggles + swim$Flippers + swim$Goggles:swim$Flippers)
summary(swim_model3)
drop1(swim_model3, test = "F")
#it doesnt make sense to drop another predictor so we are not gonna do that.

plot(swim_model3)

#CHECK ASSUMPTIONS?
#Reporting: our final model is Time ~ Shirt + Goggels + Flippers + Goggels:Flippers. The main effects were all significant (p<0.05) and the interaction was significant as well. Specifically, when someone wears no flippers but does wear goggles, that person has a faster time than wearing no goggles. But when someone wears flippers and also wears goggels, this person is actually slower than a person who does wear flippers but no goggels. The model itself is significant (F(65)=14.11, p<0.05) with a high explained variance (mult. R2=0.4648, adj. R2=0.4319). Checking of model assumptions revealed no problems.

##########################
##########Reaction Times
##########################

#datafile: reaction_times.csv

#In this study, participants had to make a decision on whether a sequence of letters presented to them on a computer screen was a word or not. How long it took for them to make this decision was recorded (their reaction time). This is called a lexical decision task. In this kind of task, many different factors can influence reaction times, including how frequent the word is, how familiar the work is, and how "imageable" it is--that is, how easy it is to imagine a picture of the word's meaning. Investigate whether, for this data set, this is true. Consider up to the 3-way interaction. Note that reaction times are averages from across participants, so although this was originally a within-subject experiment, participant-level variance is not shared across data points.
reaction = read.delim(file = "reaction_times.csv", header = T, row.names = 1)
reaction$FAMILIARITY = relevel(reaction$FAMILIARITY, "lo"); levels(reaction$FAMILIARITY)
reaction$IMAGEABILITY = relevel(reaction$IMAGEABILITY, "lo")


reaction_model1 = lm(RT ~ FREQUENCY + FAMILIARITY + IMAGEABILITY ,data = reaction)
summary(reaction_model1)
#Adjusted R-squared:  0.1503
summary(lm(RT ~ FREQUENCY + FAMILIARITY + IMAGEABILITY + FREQUENCY:FAMILIARITY,data = reaction))
#Adjusted R-squared:  0.1475 and interaction not significant
summary(lm(RT ~ FREQUENCY + FAMILIARITY + IMAGEABILITY + FREQUENCY:IMAGEABILITY,data = reaction))
#Adjusted R-squared:  0.1324 and interaction not significant
summary(lm(RT ~ FREQUENCY + FAMILIARITY + IMAGEABILITY + FAMILIARITY:IMAGEABILITY,data = reaction))
#Adjusted R-squared:  0.1313 and not significant
summary(lm(RT ~ FREQUENCY + FAMILIARITY + IMAGEABILITY + FAMILIARITY:IMAGEABILITY:FREQUENCY,data = reaction))
#Adjusted R-squared:  0.1086 and the threeway interaction doesnt give us anything, probably because there is no interaction.

drop1(reaction_model1, test = "F")
#dropping imageability makes sense because the predictor is not significant and AIC drops by almost 2
reaction_model2 = lm(RT ~ FREQUENCY + FAMILIARITY,data = reaction); summary(reaction_model2)
drop1(reaction_model2, test = "F")
#all predictors are significant now

vif(reaction_model2)
#all values are really close to 1 so no violation of assumption
durbinWatsonTest(reaction_model2)
#p value not significant so no auto correlation
plot(reaction_model2)
#no violations of assumptions here

#REPORT

#########################
#########Housing Data
#########################

#Datafile: housing_data.csv

#In this study, researchers collected data on crime rates for different city blocks in and around Boston in the U.S. You are interested in determining which variables predict crime rate. You are particularly interested in how economic status and ethnic diversity interact with each other as well as how each one interacts with the other predictors. Perform a proper analysis. The column information is below. (Note: because towns are repeated, we are actually violating the independence assumption. Normally, we would want to assign random intercepts to each town, but because we do not have a "town" variable, this is difficult...so we'll just ignore this here :-)


#1. CRIM      per capita crime rate by block
#2. INDUS     proportion of non-retail business acres per town
#3. CHAS      whether property is next to the Charles River or not
#4. NOX       nitric oxides concentration (parts per 10 million)
#5. RM        average number of rooms per dwelling
#6. AGE       proportion of owner-occupied units built prior to 1940
#7. DIS       weighted distances to five Boston employment centres
#8. RAD       index of accessibility to radial highways
#9. PTRATIO   pupil-teacher ratio by town
#10. B        measure of ethnic diversity
#11. LSTAT    % lower economic status of the population
#12. MEDV     Median value of owner-occupied homes in $1000's




###############################
#########Idiom Experiment
###############################

#Datafile: idiom_lexical_decision.csv

#In this experiment, participants were shown Dutch idioms such as "de koe bij de hoorns vatten" (grab the cow by the horns), which hase a figurative meaning of "to take control of a situation." After each idiom, they were shown either a word or a nonword and had to decide whether it was a word or not. The words were related either to the figurative meaning ("Fig" Condition) or to the literal meaning of the last word ("hoorns", i.e. the "Lit" Condition). The experimenter measured how much time, in milliseconds, it took for the participants to make the decision (this is called a lexical decision task), and then she took the log of these reaction times. If reading the idiom activated its figurative meaning in the mind of the subject, then they will be quicker to decide that a figuratively related word ("Fig") is indeed a word. If reading the idioms activated its literal meaning, then the subject will be quicker to make a decision on a literally related words. 

#Other factors might affect reaction times as well. These include: 
#the frequency of the word, 
#the literal plausibility (LP) of the idiom (how easy is it to interpret literally, e.g., "staan voor aap" = "stand for ape" is not literally plausible!)
#the transparency of the idiom (how related are the figurative and literal meanings, e.g., "tegen de lamp lopen" = "walk into the lamp" is not very related to figurative meaning "get caught")
#How well the subject knows the idiom (Knowledge)

#Your job is to conduct an analysis of this data and report your results. Consider all 2-way interactions. 

#Note: there are multiple data points per subject and per idiom, so both of the variables cause a violation of the independence assumption of regression. You will thus need to give both of these variables random intercepts. (you can just add a second random intercept term to your model equation like you add a first one)

#Note: You only need to check the assumptions of homoscedasticity and normally distributed residuals. Plotting the model will only give you the first diagnostic plot, which you can use to check homoscedasticity. To check for normal residuals, you can first get the residuals by passing the model object to the residuals() function, and then plotting the output.







#########################
#########Attributions
#########################

#Datafile: clause_order_data.csv

#In this experiment, participants read stories about different people and then indicate how favorable they feel about these people (the normV variable). The last sentence of a story is something like "Jay climbed through a window and stole the jewels, police said." This sentence can either be positive or negative (in the Exp column, "Pos" or "Neg"). Furthermore, the story itself can overall be a mixture of positive and negative actions (storyType=positive) or it can agree with the tone of the final sentence (storyType=accordant); that is, a positive story can go with a positive final sentence and a negative story can go with a negative final sentence. Finally, the "attribution phrase" (i.e., "police said") in the sentence can come either at the beginning of the sentence or the end: "Jay climbed through a window and stole the jewels, police said" versus "Police said that Jay climbed through the window and stole the jewels." Attribution phrases are known to weaken statements when placed first. Therefore, if a sentence is negative, then having "police said" at the beginning will make the statement less severe, and we expect the participants to rate Jay more positively. Similarly, if a sentence is positive, having "police said" at the end will weaken the positive statement less, and we expect the participants to rate Jay more positively. Both of these situations represent the bias=positiveBias condition. On the other hand, placing "police said" at the end of a negative sentence weakens the sentence less, and we expect people to rate Jay more negatively. Similarly, placing "police said" at the beginning of a positive sentence weakens the sentence more, and we expect people to rate Jay more negatively. Both of these situations represent the bias=negativeBias condition.

#Analyze the data to try to determine which of the variables discussed (and their interactions) predict participants' favorability ratings of the people in the stories. In addition, we are also interested in age and gender, although you don't need to include these as interactions with other variables.

#Note: the data in columns "situation" and "person" were used to determine whether the participant was paying attention. If they were, there should be a value of "burglary" or "PSA" in the "situation" column, and "WJ" in the person column. All other data is not valid and should be excluded.


