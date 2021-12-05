library(ggplot2)
library(datasets)
library(joineR)
library(MASS)
library(rstanarm)
library(bayesplot)
library(mice)
attach(Nepal_missing)


model=lm(weight ~ visit+bf_bin+age+bf_bin*visit,data=Nepal_missing)
summary(model)
confint(model,level = 0.9)


##bayesian model Fitting

beta_prior=c(-0.1095,-0.5154,0.1438,0.1625)
beta_prior_var=c(0.02550,0.09503,0.005691,0.02245)
prior=normal(location=beta_prior,scale=beta_prior_var
             ,autoscale = FALSE)

model_sim=stan_glm(weight ~ visit+bf_bin+age+bf_bin*visit,
                   data=Nepal_missing,prior=prior)
summary(model_sim)
round(model_sim$coefficients,3)

pp_check(model_sim)
posterior_vs_prior(model_sim,group_by_parameter = TRUE)
posterior_interval(model_sim)

prior_summary(model_sim)



#Convergence Plot

b_visit=array()
b_age=array()
b_bf_bin=array()
b_visit_bf=array()

N=1000
y=array()
num_sim=10000
for(j in 1:num_sim)
{
  prior_visit=array()
  prior_bf_bin=array()
  prior_age=array()
  prior_visit_bf=array()
  for (i in 1:N)
  {
    prior_visit[i]=rnorm(1,-0.1095,0.02550)
    prior_bf_bin[i]=rnorm(1,-0.5154,0.09503)
    prior_age[i]=rnorm(1,0.1438,0.005691)
    prior_visit_bf[i]=rnorm(1,0.1625,0.02245)
    prior_error_sd=0.1764
    y[i]=rnorm(1,6.0606+(prior_visit[i]*visit[i])+
                 (prior_bf_bin[i]*bf_bin[i])+(prior_age[i]*age[i])
               +prior_visit_bf[i]*visit[i]*bf_bin[i],prior_error_sd)
    
  }
  
  b_visit[j]=lm(y~ visit+bf_bin+visit+age+bf_bin*visit,
                data=Nepal_missing)$coefficients[2]
  b_bf_bin[j]=lm(y ~ visit+bf_bin+visit+age+bf_bin*visit,
                 data=Nepal_missing)$coefficients[3]
  b_age[j]= lm(y ~ visit+bf_bin+visit+age+bf_bin*visit,
               data=Nepal_missing)$coefficients[4]
  b_visit_bf[j]=lm(y ~ visit+bf_bin+visit+age+bf_bin*visit,
                   data=Nepal_missing)$coefficients[5]
}


avg_b_visit=array()
avg_b_bf_bin=array()
avg_b_age=array()
avg_b_visit_bf=array()

for(i in 1 : num_sim)
{
  avg_b_visit[i]=mean(b_visit[1:i])
  avg_b_bf_bin[i]=mean(b_bf_bin[1:i])
  avg_b_age[i]=mean(b_age[1:i])
  avg_b_visit_bf[i]=mean(b_visit_bf[1:i])
}

par(mfrow=c(2,2))
plot(avg_b_visit,type="l",main="Convergence plot of coefficient of Visit")  
plot(avg_b_bf_bin,type="l",main="Convergence plot of coefficient of Breast feed") 
plot(avg_b_age,type="l",main="Convergence plot of coefficient of Age") 
plot(avg_b_visit_bf,type="l",main="Convergence plot of coefficient of 
     interaction effect of visit and breast feeding") 
old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)


#residual Analysis
residuals=model_sim$residuals
qqnorm(residuals)
fitted=model_sim$fitted.values
plot(fitted,residuals,main="Residual vs Fitted plot",col="Blue")
abline(b=0,a=0,col="Red")


#missing data analysis

missing=is.na(Nepal_missing$weight)
md.pattern(Nepal_missing,rotate.names = TRUE)

miss=as.integer(missing)
summary(glm(miss~visit+bf_bin+visit+age,data = Nepal_missing
            ,family = "binomial",maxit=100))

predicted=predict(model_sim,D[-1,-1],interval=prediction,na.action = na.pass)
predict_missing=predicted*miss;predict_missing

#missing completely at Random


