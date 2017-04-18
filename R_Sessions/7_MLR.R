#April13
#MLR models

data1=read.csv("correlation.csv")
attach(data1)

#general model fitting and diagnostics
plot(data1)
fit=lm(flow_mm~temp_min_c+temp_max_c, data=data1)
summary(fit)
fitted(fit)
coefficients(fit)
confint(fit)
anova(fit)

#generic plotting
plot(fit)

plot(year, flow_mm, type="l")
lines(year, fitted(fit), col=2, lwd=2)

#calculate confidence intervals
pred=predict(fit, data1, interval='confidence')
lines(year, pred[,2], col='blue', lty=3, lwd=2)
lines(year, pred[,3], col='blue', lty=3, lwd=2)

pred=predict(fit, data1, interval='prediction')
lines(year, pred[,2], col='pink', lty=3, lwd=2)
lines(year, pred[,3], col='pink', lty=3, lwd=2)

#plot your residuals
plot(year, residuals(fit))
plot(fitted(fit), residuals(fit))
plot(flow_mm, residuals(fit))

#Compare 2 models
fit_simple=lm(flow_mm~temp_min_c, data=data1)
anova(fit_simple, fit)


plot(temp_min_c, temp_max_c)
