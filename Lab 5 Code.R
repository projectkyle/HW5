#lab 5

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

