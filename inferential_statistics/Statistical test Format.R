# All statistical Analsyis
library("psych")
library(lsr)
library(lmtest)
library(car)

library(dplyr)
library(MASS)     
library("ggplot2")
library("readxl")
library("Rcmdr")
library("foreign")
library(tidyr)

#Read and Wrangle Data File-------------------------------------------------------------------------------------

Data<-gather(data = dataset,factor_key = T,key ="col_name for factors", value= "column name for values",
             "columns numbers over which this needs to be applied")

#Initial Exploration-----------------------------------------------------------------------------------------
1. summary()
2. describe() # Gives all normality descriptive stats (needs  psych)
3. shapiro.test() # Test for normality- gives qqplot
#Find Outliers #Refer General Package----------------------------------------------------------------------------
Create_outlier_column fun(Data,Col,Group)
#Data= Data File
#Col: Column in which outlier need to be found
#Group: Column in which you have variable acc. to which groups sub-sets needs to be formed

#Box Plot----------------------------------------------------------------------------------------------------------
#General Code
Box_label<-labs(x="colname_Group", y=colname_Variable, title="BOX PLOT")
Theme<-theme(axis.title = element_text(size=12),title =element_text(size=15),axis.text = element_text(size=11))
geom_text<-geom_text(aes("colname_Group",label = ifelse(as.character(Data$"colname_outlier"),
                                                        yes = round(Data$"colname_Variable",2),
                                                        no = NA),na.rm=TRUE, hjust = -0.3))

ggplot(data =Data,aes("colname_Group","colname_Variable"))+
  geom_boxplot(size=2)+
  Box_label_1+
  Theme+geom_text


#Kinds of Statistical Tests-----------------------------------------------------------------

##Chi- Square tests-----------------------------------------------------------------------------
The "goodness-of-fit test" is a way of determining whether a set of categorical data came from a claimed discrete distribution or not. Counts of occurences
-----1. Only one categorical variable.names ->goodnessOfFitTest
-----2. Two categorical variables  - > test of independence (or association)

#Chi-Squre distrinution = take a bunch of things that have a standard normal distribution, # square them, then add them up, then the resulting quantity has a chi-square distribution.
lsr::goodnessOfFitTest() #Inputs a frequency table, probabilities can be specified separately, else assumed equal. e.g. 200 people select a suite, is the choice independent? 
associationTest() #Chi-square test of categorical association
#Effect size is measured by cramersV test: 
cramersV()

###Assumptions of chi-square tests;
#1. Expected frequencies are sufficiently large (>5): 
#If not satifies, use fishcer
fisher.test()
#2. Data are independent of one another. If not satisfied, use mcnemar.test
mcnemar.test()

#Code
df = survey
goodnessOfFitTest(x = df$Exer)
associationTest(formula = ~Exer+Smoke ,data = df)

fisher.test(x = df$Exer,y=df$Smoke)

##T-tests---------------------------------------------------------------------------------------
###Assumptions: Normality, Independence of observations(theoretically)

## Single  Sample testing

oneSampleTTest("data", "mean to compare with")

## Two-Sample test---------------
###t-stat= Diff of sample means/Std. Error   (Sampling Distribution of Diff of Means)

#We have to calculate pooled estimate of variance to figure out the std. error for the differnce 

leveneTest(y = variable~group,data = Data) #test for diff. of variance for more than 2 group
var.test( x,y )  #test for diff. of variance for 1 group

#depending on results from variance test, we choose different kinds of independnt t-tests

#Independent Test-------------no relationship between the pair-----------
###Student's t-test:  no variance differences between the two samples being compared.
independentSamplesTTest(formula ="value~group","Data",var.equal = T )

###Student's t-test:   variance differences EXISTS between the two samples being compared.
independentSamplesTTest(formula ="value~group","Data",var.equal = F ) #Welch Test

#dependent Test------------- relationship between the pair-----------
# We can either use the one sample test on the diff or use 
pairedSamplesTTest()
Wilcoxon Signed Rank test # Non-Parametric 
#Test for Normality
shapiro.test(x)

#IF NORMALITY NOT SATISFIED- Wilcoxon Signed Rank test
wilcox.test( formula = scores ~ group, data = awesome, mu="value if mean in caase of one-sample)", paired = "TRUE if paired"))

#Effect Size------------------
cohensD( formula = grade ~ tutor,data = harpo, mu="value if mean in caase of one-sample)", method = c("equal", "unequal","paired", "Blank for one sample")) #Effect size

#Run=========================
 
shapiro.test(x = df$Wr.Hnd)  #Normal results
oneSampleTTest(x = df$Wr.Hnd,mu = 18) #Presuming 18 is the population mean

#between genders height
leveneTest(y = df$Height,group = df$Sex) # same variance
shapiro.test(df$Height) # normal Height
independentSamplesTTest(Height~Sex,data = df)


## One- way ANOVA--------------------------------------------------------------------------------------------------

#One-way Anova- one kinds of groups and one variable. 
#Assumptions: Normality, Independence of observations, homogeinity of variance
#Theory
variance=Sum of sq. of differnces(SS) divided by N. In ANOVA we only use SS
SS(Total)=SS(between groups)+SS(error)
SS(within groups)= SS(error)

F(Stats)=MS(b)/MS(e)=MS(b)/MS(w)=(SS(b)/DOF(b))/(SS(w)/DOF(w)); need to be greater than 1
#Actual tests
model<-aov()
summary(model)# to get statistics and p-value
etaSquared( x = model ) #for effect size

# pair-wise t-tests for all groups,corrrection for multiple comparison
posthocPairwiseT(model,p.adjust.method = "bonferroni" or "Holm") 

#Test for homogeniety
leveneTest( my.anova )
oneway.test(model) # welch-test in case of non-homogenous data

#Test for Normality
shapiro.test(model)
kruskal.test(formula, data)# Kruskal-Wallis in case of non-normal data- non-parametric

#Repearted measures ANNOVA
SS(Total)=SS(between groups)+SS(within groups)
SS(within_group)=SS(subjects)+SS(error)
F(Stats)=MS(b)/MS(error)=(SS(b)/DOF(b))/(SS(e)/DOF(e))

##Linear Regression---------------------------------------------------------
Assumptions:
#1. Normality: It's actually okay if the predictors X and the outcome Y are non-normal, so long as the residuals o are normal
#2. Linearity: A pretty fundamental assumption of the linear regression model is that relationship between X and Y actually be linear
#3. Homogeniety of Variance: Strictly speaking, the regression model assumes that each residual oi is generated from a normal 
#   distribution with mean 0, and (more importantly for the current purposes) with a standard deviation ?? that is the same for every single residual
Non-Techincal Assumptions: 
#4. Uncorrelated predictors:Predictors that are too strongly correlated with each other (referred to as "collinearity") can cause problems when evaluating the model
#5. Residuals are independent of each other: This is really just a "catch all" assumption, to the effect that "there's nothing else funny going on in the residuals".
#6: No "bad" outliers:   
  
  
#t-stats for  each coeffcient give us the p-value for the possibility that the coefficient is accidental and not equal to 0.
#The test for the significance of a correlation is identical to the t test that we run on a coefficient in a regression model.
#Beta-values of coeffcient
#the standardised coefficients are the coefficients that you would have obtained if you'd converted all the variables to z-scores before running the regression
# ?? value (standardized coeffcient) of 1 means that an increase in the predictor of 1 standard deviation will produce
# a corresponding 1 standard deviation increase in the outcome variable. Therefore, if variable A has a
# larger ?? value than variable B, it is deemed to have a stronger relationship with the outcome.


df$female= ifelse(df$Sex=="Female",yes = 1,no = 0)
df$male= ifelse(df$Sex=="Male",yes = 1,0)

model<-lm(data,subset = -x) #if you want to excluse x'th entry
summary(model)
correlate(x = parenthood,test=TRUE) #correaltes all values, unreliableas there are too many significance tests being conducted
confint(object = model,level = .99)
standardCoefs(model)

#Regression diagnostics...install.packages("car") #excellent tools for regression diagnostics
res<-residuals( object = model ) #residuals
res_std<-rstandard(model = model) #standardized residuals
rstudent( model = model ) # Studentised residual, the estimate of the residual standard deviation that you would have obtained 
#if you just deleted the ith observation from the data set.
hatvalues( model = model )
#Three kinds o f anomlous Data
# 1. Outlier: an outlier is an observation that has a very large Studentised residual,
# 2. high leverage data: this happens when the observation is very different from all the other observations. 
# This doesn't necessarily have to correspond to a large residual: if the observation happens to be unusual on all variables in 
# precisely the same way, it can actually lie very close to the regression line. The leverage of an observation is operationalised 
# in terms of its hat value(hi).  hi is a measure of the extent to which the i-th observation is "in control" of where the regression 
# line ends up going. You can extract the hat values using the following command
# 3. Influence:A high influence observation is an outlier that has high leverage. We operationalise influence in terms of a measure known as Cook's distance>1. 

plot(model) #plots various diagnostic plots 
plot(x = model, which = 4) # Cook's distance

###Checking linearity of relationships------------------------------------------
plot(x = fitted.values(model),y="observed_values")#check for roughly linear relation between them
residualPlots( model = model)

#Curvatures in residula-varaible relationships can be made better using transformations
#if what you're trying to do is convert a data to normal, or as normal as possible, there's the powerTransform() function in the car
#package that can estimate the best value of ??.
powerTransform(model)
t<-boxCox(object = model)

###Check for homogeiniety of variance-------------------------------------
plot(x = model, which = 3) 
ncvTest( model ) #non-constant variance test

#if non-homogenous data use coeftest() to do your regression analsyis.
# if homogeneity of variance is violated, is that the standard error estimates associated with
# the regression coefficients are no longer entirely reliable, and so your t tests for the coefficients aren't
# quite right either. A simple fix to the problem is to make use of a "heteroscedasticity corrected covariance
# matrix" when estimating the standard errors
coeftest(x = model) #lmtest package

###Checking for collinearity between dependent variables; called variance inflation factors----------------------------------------------------
vif( mod = model )# less the correlation, the closer the value of sq root of VIFs is to 1.  

#Selecting between models
#1. Okaham Razor: AIC value- increases both as variance increases and as nuber of predictibles inclrease
step(model) #gets AIC value for the model
step(model,direction = "backward") # goes backwards itowards simpler model to se AIC values change

#2. F-test ; hierarchical regression
M0= Null model with basic predictables
M1= Model with extra predictables of interest
anova(M0,M1) #Aov gives us models, anova comapres the modesl 

##Factorial ANOVA/2-way ANOVA------------------------------------------------------------------------------
#Used when we want to explain a variable based on two differnt groups of values.
#main effect= effect of each group on the dependent variable
aov() #will tell us which factors have significant effect
etaSquared() #power measurement
TukeyHSD() #To find out pair-wise differences to identify which group has an effect

colnames(parenthood)
model<-lm(dan.grump~dan.sleep+baby.sleep,data = parenthood)
summary(model)
correlate(parenthood, test=TRUE)
