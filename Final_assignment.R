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

library(ggplot2)
library(effects)
library(car)

###################
#####Swimming Data
###################

#datafile: swimming.csv

#In this study, the times it took for different participants to swim a lap in a swimming pool were measured. Swimmers varied in terms of which end of the pool they started from (End variable), whether they wore goggles, whether they wore a shirt, and whether they used flippers. Perform an analysis and consider up to 2-way interactions between predictors.
swim = read.delim(file = "swimming.csv", header = T)

swim_model1 = lm(Time ~ (Shirt + Goggles + Flippers + End)^2, data=swim); summary(swim_model1)
#Multiple R-squared:  0.5044,	Adjusted R-squared:  0.4205 
#overall model significant but alot of insignificant predictors
plot(allEffects(swim_model1))
#goggles and flippers seems interesting

#start dropping
drop1(swim_model1, test = "F")
#drop interaction between shirt and end because insignificant and AIC value drops the most

sm2 = lm(Time ~ Shirt + Goggles + Flippers + End + Goggles:Flippers + Shirt:Goggles + Shirt:Flippers + Goggles:End + Flippers:End, data = swim); summary(sm2)
#Multiple R-squared:  0.5013,	Adjusted R-squared:  0.4265 
drop1(sm2, test = "F")
# drop interactions between shirt and goggles

sm3 = lm(Time ~ Shirt + Goggles + Flippers + End + Goggles:Flippers + Shirt:Flippers + Goggles:End + Flippers:End, data = swim); summary(sm3)
#Multiple R-squared:  0.4993,	Adjusted R-squared:  0.4337 
drop1(sm3, test = "F")
#drop interaction goggles and end

sm4 = lm(Time ~ Shirt + Goggles + Flippers + End + Goggles:Flippers + Shirt:Flippers + Flippers:End, data = swim); summary(sm4)
#Multiple R-squared:  0.4955,	Adjusted R-squared:  0.4385 
drop1(sm4, test = "F")
#drop interaction flippers and end

sm5=lm(Time ~ Shirt + Goggles + Flippers + End + Goggles:Flippers + Shirt:Flippers, data = swim);summary(sm5)
#Multiple R-squared:  0.4912,	Adjusted R-squared:  0.4428 
drop1(sm5, test = "F")
#drop end

sm6 = lm(Time ~ Shirt + Goggles + Flippers + Goggles:Flippers + Shirt:Flippers, data = swim);summary(sm6)
#Multiple R-squared:  0.4891,	Adjusted R-squared:  0.4491 
drop1(sm6, test = "F")
# the interaction between shirt and flippers is not significant, p= 0.086 > 0.05. However, dropping the interaction would actually increase the AIC value and lead to a lower amount of variance explained.
plot(allEffects(sm6))
#interaction between shirt and flippers changes the slope quite a bit and the y intercept is lower.


#Model without interaction between shirt and flippers
sm7 = lm(Time ~ Shirt + Goggles + Flippers + Goggles:Flippers, data = swim); summary(sm7)
#Multiple R-squared:  0.4648,	Adjusted R-squared:  0.4319 
# both values of R^2 are lower so it might be useful to inlcude the interactions.

#Assumptions
par(mfrow = c(2,2))
plot(sm6)
#there seems to be homogenity of variance between predictors and the error of residuals is normally distributed as well --> no violations


#CHECK ASSUMPTIONS?
#Reporting: our final model is Time ~ Shirt + Goggels + Flippers + Goggels:Flippers + Shirt:Flippers. The main effects were all significant (p<0.05) and the interaction between Goggles and Flippers was significant as well. However the interaction between Shirt and Flippers was not (p= 0.086). If we would drop this interaction, our model would have less variance explained so we decided to keep it in the model. Specifically, when someone wears no flippers but does wear goggles, that person has a faster time than wearing no goggles. But when someone wears flippers and also wears goggels, this person is actually slower than a person who does wear flippers but no goggels. We see that when someone wears a shirt but no flippers this person is slower than someone not wearing a shirt. When someone wears flippers and a shirt, this person is slower than someone just wearing flippers. The model itself is significant (F(65)=14.11, p<0.05) with a high explained variance (mult. R2=0.4891, adj. R2=0.4491). Checking of model assumptions revealed no problems.

#Interactions: When we first saw the predictors we expected the interaction between Goggles and Flippers to give us the best results, but in the end someone wearing both actually made them slower. This seems weird. Since a shirt makes you slower and flippers make you faster, it wasn't a surprise when we saw that combining them made you slower than just wearing flippers.

##########################
##########Reaction Times
##########################

#datafile: reaction_times.csv

#In this study, participants had to make a decision on whether a sequence of letters presented to them on a computer screen was a word or not. How long it took for them to make this decision was recorded (their reaction time). This is called a lexical decision task. In this kind of task, many different factors can influence reaction times, including how frequent the word is, how familiar the work is, and how "imageable" it is--that is, how easy it is to imagine a picture of the word's meaning. Investigate whether, for this data set, this is true. Consider up to the 3-way interaction. Note that reaction times are averages from across participants, so although this was originally a within-subject experiment, participant-level variance is not shared across data points.
reaction = read.delim(file = "reaction_times.csv", header = T, row.names = 1)
reaction$FAMILIARITY = relevel(reaction$FAMILIARITY, "lo"); levels(reaction$FAMILIARITY)
reaction$IMAGEABILITY = relevel(reaction$IMAGEABILITY, "lo")

#remove rows with missing data
reaction = subset(reaction, FREQUENCY != 0)

rm1=lm(RT~ (FREQUENCY + IMAGEABILITY + FAMILIARITY)^3, data=reaction); summary(rm1)
#Multiple R-squared:  0.2433,	Adjusted R-squared:  0.0875 
plot(allEffects(rm1))
#3 way interaction does not seems significant

#model is not significant
drop1(rm1, test='F')

#we drop the interaction betrween frequency, imageability and familiarity
rm2=lm(RT~ (FREQUENCY + IMAGEABILITY + FAMILIARITY)^2, data=reaction); summary(rm2)
#Multiple R-squared:  0.2418,	Adjusted R-squared:  0.1119 
plot(allEffects(rm2))
#none of the interaction seems significant. there are large confidence intervalls

#model not significant
drop1(rm2, test='F')

#dropping the interaction between frequency and imageability
rm3=lm(RT~ FREQUENCY + IMAGEABILITY + FAMILIARITY + FREQUENCY:FAMILIARITY + IMAGEABILITY:FAMILIARITY, data=reaction); summary(rm3)
#Multiple R-squared:  0.2416,	Adjusted R-squared:  0.1363 
#model still not significant
drop1(rm3, test = 'F')

#we are dropping the interaction between imageability and familiarity
rm4=lm(RT~ FREQUENCY + IMAGEABILITY + FAMILIARITY + FREQUENCY:FAMILIARITY, data=reaction); summary(rm4)
#Multiple R-squared:  0.2395,	Adjusted R-squared:  0.1572 
#Model is significant
drop1(rm4, test = 'F')

#dropping imageability
rm5=lm(RT~ FREQUENCY + FAMILIARITY + FREQUENCY:FAMILIARITY, data=reaction); summary(rm5)
#Multiple R-squared:  0.2393,	Adjusted R-squared:  0.1793 
drop1(rm5, test = 'F')

#dropping the interaction between frequency and familiarity
rm6=lm(RT~ FREQUENCY + FAMILIARITY, data=reaction); summary(rm6)
#Multiple R-squared:  0.2273,	Adjusted R-squared:  0.1877
drop1(rm6, test = 'F')

#dropping Familiarity
rm7=lm(RT~ FREQUENCY, data=reaction); summary(rm7)
#Multiple R-squared:  0.172,	Adjusted R-squared:  0.1513 
drop1(rm7, test = 'F')
#FINAL MODEL
plot(allEffects(rm7))


#ASSUMPTIONS  
#final model is just single regression so no need to check for multi collinearity
durbinWatsonTest(rm7)
#p value not significant so no auto correlation
plot(rm7)
#there might be some homoscedasticity but nothing severe
#no violations of assumptions here

#REPORT
#The final Model is RT ~ FREQUENCY. The main effects were significant: p< 0.01. There was apostive relationship between frequency and reactiontime. The model was highly significant(F(1, 40)=8.307, p = 0.0063) and achieved a low level of variance explained (Multiple R-squared:  0.172,	Adjusted R-squared:  0.1513).

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
#!10. B        measure of ethnic diversity
#!11. LSTAT    % lower economic status of the population
#12. MEDV     Median value of owner-occupied homes in $1000's

crime = read.delim(file = "housing_data.csv", header = T)
cm1 = lm(CRIM ~ B*(INDUS + CHAS + NOX + RM + AGE + DIS + RAD + PTRATIO + MEDV) + LSTAT*(INDUS + CHAS + NOX + RM + AGE + DIS + RAD + PTRATIO + MEDV) + B:LSTAT, data = crime);summary(cm1)



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

idiom = read.delim(file = "idiom_lexical_decision.csv", header = T)
apply(idiom, 2, function(x) any(is.na(x)))
#so no missintg data

library(lme4)
library(lmerTest)
library(MuMIn)

im1 = lmer(log_RTs ~ (Condition + Freqs + Knowledge + LP + Transparency)^2 + (1|Subject) + (1|Idiom), data = idiom); summary(im1)
r.squaredGLMM(im1)
# marginal R^2: 0.02637807
drop1(im1, test = "Chisq")
#drop interaction between freqs and lp

im2 = lmer(log_RTs ~ Condition + Freqs + Knowledge + LP + Transparency + Condition:Freqs + Condition:Knowledge + Condition:LP + Condition:Transparency + Freqs:Knowledge + Freqs:Transparency + Knowledge:LP + Knowledge:Transparency + LP:Transparency + (1|Subject) + (1|Idiom), data = idiom); summary(im2)
r.squaredGLMM(im2)
#0.02619832
drop1(im2, test = "Chisq")

#dropping the interaction between Freqs and Knowledge
im3 = lmer(log_RTs ~ Condition + Freqs + Knowledge + LP + Transparency + Condition:Freqs + Condition:Knowledge + Condition:LP + Condition:Transparency + Freqs:Transparency + Knowledge:LP + Knowledge:Transparency + LP:Transparency + (1|Subject) + (1|Idiom), data = idiom); summary(im3)
r.squaredGLMM(im3)
#0.02618121
drop1(im3, test = "Chisq")

#dropping interaction between knowledge and transparency
im4 = lmer(log_RTs ~ Condition + Freqs + Knowledge + LP + Transparency + Condition:Freqs + Condition:Knowledge + Condition:LP + Condition:Transparency + Freqs:Transparency + Knowledge:LP + LP:Transparency + (1|Subject) + (1|Idiom), data = idiom); summary(im4)
r.squaredGLMM(im4)
#0.02615937
drop1(im4, test = 'Chisq')

#dropping interaction between Condition and Freqs
im5= lmer(log_RTs ~ Condition + Freqs + Knowledge + LP + Transparency + Condition:Knowledge + Condition:LP + Condition:Transparency + Freqs:Transparency + Knowledge:LP + LP:Transparency + (1|Subject) + (1|Idiom), data = idiom); summary(im5)
r.squaredGLMM(im5)
#0.02559873
drop1(im5, test = 'Chisq')

#dropping the interaction between Condition and Transparency
im6=lmer(log_RTs ~ Condition + Freqs + Knowledge + LP + Transparency + Condition:Knowledge + Condition:LP  + Freqs:Transparency + Knowledge:LP + LP:Transparency + (1|Subject) + (1|Idiom), data = idiom); summary(im6)
r.squaredGLMM(im6)
#0.02530543
drop1(im6, test = 'Chisq')

#dropping interaction between freqs and transparency
im7 = lmer(log_RTs ~ Condition + Freqs + Knowledge + LP + Transparency + Condition:Knowledge + Condition:LP + Knowledge:LP + LP:Transparency + (1|Subject) + (1|Idiom), data = idiom); summary(im7)
r.squaredGLMM(im7)
#0.02558375
drop1(im7, test = 'Chisq')

#dropping interaction betweenknowledge and LP
im8=lmer(log_RTs ~ Condition + Freqs + Knowledge + LP + Transparency + Condition:Knowledge + Condition:LP + LP:Transparency + (1|Subject) + (1|Idiom), data = idiom); summary(im8)
r.squaredGLMM(im8)
#0.02445571
drop1(im8, test = 'Chisq')

#dropping interaction between LP and Transparency
im9=lmer(log_RTs ~ Condition + Freqs + Knowledge + LP + Transparency + Condition:Knowledge + Condition:LP + (1|Subject) + (1|Idiom), data = idiom); summary(im9)
r.squaredGLMM(im9)
#0.02375036
drop1(im9, test = 'Chisq')

#dropping transparency
im10=lmer(log_RTs ~ Condition + Freqs + Knowledge + LP + Condition:Knowledge + Condition:LP + (1|Subject) + (1|Idiom), data = idiom); summary(im10)
r.squaredGLMM(im10)
#0.02442197
drop1(im10, test = 'Chisq')

#dropping interaction condition and lp
im11=lmer(log_RTs ~ Condition + Freqs + Knowledge + LP + Condition:Knowledge + (1|Subject) + (1|Idiom), data = idiom); summary(im11)
r.squaredGLMM(im11)
#0.0247221
drop1(im11, test = 'Chisq')

#drop lp
im12=lmer(log_RTs ~ Condition + Freqs + Knowledge + Condition:Knowledge + (1|Subject) + (1|Idiom), data = idiom); summary(im12)
r.squaredGLMM(im12)
#0.02397234
drop1(im12, test = 'Chisq')
#FINAL MODEL
plot(allEffects(im12))

#ASSUMPTIONS
plot(im12)
plot(residuals(im2))
#no violation of assumptions

#REPORT
#our final model is log_RTs ~ Condition + Freqs + Knowledge + Condition:Knowledge + (1|Subject) + (1|Idiom). Freqs and Knowledge are significant (p<0.05), Condition isn't (p>0.05). Also the interaction between Condition and Knowledge is not significant but if we would drop this, there would be less variance explained so we decided to keep it in the model. 

#SPECULATIONS
#we would not expect to drop LP since the meaning of the idiom seems to influence the reaction time to decide whether it is Lit or Fig if you think about it. The Condition is of course important since that is what the experiment is about, the frequency also seems important because of learning the meaning of the idiom, if you know the idiom (knowledge) it is expected that you recognise its condition faster so this makes sense. The interaction between Condition=Lit and Knowledge seems reasonable, when you know the idiom and you see the idiom with a literal meaning you are probably faster.


#########################
#########Attributions
#########################

#Datafile: clause_order_data.csv

#In this experiment, participants read stories about different people and then indicate how favorable they feel about these people (the normV variable). The last sentence of a story is something like "Jay climbed through a window and stole the jewels, police said." This sentence can either be positive or negative (in the Exp column, "Pos" or "Neg"). Furthermore, the story itself can overall be a mixture of positive and negative actions (storyType=positive) or it can agree with the tone of the final sentence (storyType=accordant); that is, a positive story can go with a positive final sentence and a negative story can go with a negative final sentence. Finally, the "attribution phrase" (i.e., "police said") in the sentence can come either at the beginning of the sentence or the end: "Jay climbed through a window and stole the jewels, police said" versus "Police said that Jay climbed through the window and stole the jewels." Attribution phrases are known to weaken statements when placed first. Therefore, if a sentence is negative, then having "police said" at the beginning will make the statement less severe, and we expect the participants to rate Jay more positively. Similarly, if a sentence is positive, having "police said" at the end will weaken the positive statement less, and we expect the participants to rate Jay more positively. Both of these situations represent the bias=positiveBias condition. On the other hand, placing "police said" at the end of a negative sentence weakens the sentence less, and we expect people to rate Jay more negatively. Similarly, placing "police said" at the beginning of a positive sentence weakens the sentence more, and we expect people to rate Jay more negatively. Both of these situations represent the bias=negativeBias condition.

#Analyze the data to try to determine which of the variables discussed (and their interactions) predict participants' favorability ratings of the people in the stories. In addition, we are also interested in age and gender, although you don't need to include these as interactions with other variables.

#Note: the data in columns "situation" and "person" were used to determine whether the participant was paying attention. If they were, there should be a value of "burglary" or "PSA" in the "situation" column, and "WJ" in the person column. All other data is not valid and should be excluded.
att = read.delim(file = "clause_order_data.csv", header = T)

