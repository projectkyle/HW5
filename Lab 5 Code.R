#lab 5
#code for regression with Asians with a college degree between the ages 25 and 65

attach(acs2017_ny)
use_varb <- (AGE >= 25) & (AGE <= 65) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35) & (RACE == 'Asian') & ((educ_college == 1) | (educ_advdeg == 1))
dat_use <- subset(acs2017_ny,use_varb) # 
detach()


require(stargazer)

lm1 <- lm(INCWAGE ~ AGE + I(AGE^2) + I(AGE^3) + Chinese + educ_college)
summary(lm1)
stargazer(lm1, type = "text", title = "Regression 1")

plot(lm1)

require(AER)

# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)
# ^^ that looks like crap since Wages are soooooooo skew!  So try to find some sensible ylim = c(0, ??)
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
# discus what you see in this plot

# change this line to fit your regression
to_be_predicted2 <- data.frame(AGE = 25:65, AfAm = 0, Asian = 1, Amindian = 0, race_oth = 0, Hispanic = 0, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 1)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)




detach()


#output

Call:
lm(formula = INCWAGE ~ AGE + I(AGE^2) + I(AGE^3) + I(AGE^4) + 
    AfAm + educ_college)

Residuals:
    Min      1Q  Median      3Q     Max 
-101127  -38018  -16187   12111  614121 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)   1.428e+05  2.013e+05   0.709   0.4780    
AGE          -2.188e+04  2.128e+04  -1.028   0.3039    
I(AGE^2)      1.192e+03  8.265e+02   1.442   0.1492    
I(AGE^3)     -2.336e+01  1.398e+01  -1.671   0.0947 .  
I(AGE^4)      1.549e-01  8.701e-02   1.781   0.0750 .  
AfAm         -1.961e+04  1.178e+03 -16.647   <2e-16 ***
educ_college  2.272e+04  8.507e+02  26.709   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 81650 on 46964 degrees of freedom
Multiple R-squared:  0.03818,	Adjusted R-squared:  0.03806 
F-statistic: 310.7 on 6 and 46964 DF,  p-value: < 2.2e-16

> stargazer(lm1, type = "text", title = "Regression 1")

Regression 1
===============================================
                        Dependent variable:    
                    ---------------------------
                              INCWAGE          
-----------------------------------------------
AGE                         -21,880.600        
                           (21,283.390)        
                                               
I(AGE2)                      1,192.042         
                             (826.463)         
                                               
I(AGE3)                      -23.363*          
                             (13.979)          
                                               
I(AGE4)                       0.155*           
                              (0.087)          
                                               
AfAm                      -19,608.520***       
                            (1,177.875)        
                                               
educ_college               22,720.740***       
                             (850.685)         
                                               
Constant                    142,783.100        
                           (201,258.300)       
                                               
-----------------------------------------------
Observations                  46,971           
R2                             0.038           
Adjusted R2                    0.038           
Residual Std. Error   81,646.030 (df = 46964)  
F Statistic         310.712*** (df = 6; 46964) 
===============================================
Note:               *p<0.1; **p<0.05; ***p<0.01



