#Transforming to right format (scaling, removing NA's, making country factor)
df_N2O <- subset(final_data, select = c("country", "year", "N2O", "member"))%>%
  transform(year=year-1989) %>%
  na.omit()
df$country <- factor(df$country)

#Base model (random intercept model)
m.base.N2O <- lmer(N2O ~ year + (1 | country), data=df_N2O, REML=F)

#Adding fixed effect of being member
m.0.N2O <- lmer(N2O ~ year + member + (1 | country), data=df_N2O, REML=F)

#Adding the FE on slope (Effects of being member on the rate of growth of CO2 emissions):
m.1.N2O <- lmer(N2O ~ year * member + (1 | country), data=df_N2O, REML=F)
m.1.N2O

#Statistically evaluate model: model comparison of goodness of fit
anova(m.base.N2O, m.0.N2O, m.1.N2O)

##No significant differences between the models

#Add linear effect on time to capture individual differences in growth rate (year on country)
m.t1.N2O <- lmer(N2O ~ year * member + (1 + year | country), data=df_N2O, REML=F)

anova(m.t1.N2O, m.t1.N2O)
coef(summary(m.t1.N2O))

coefs_N2O <- data.frame(coef(summary(m.t1.N2O))) 
coefs_N2O$p <- format.pval(2*(1-pnorm(abs(coefs_N2O$t.value))), digits=2, eps=0.0001)
coefs_N2O

##This significantly improved model fit


#Create orthogonal polynomials, determine which order gives best model fit
#Create polynominals
t <- poly((unique(df_N2O$year)), 8)

#Create orthogonal polynomial time terms
df_N2O[,paste("ot", 1:8, sep="")] <- t[df_N2O$year, 1:8]

## SECOND ORDER
m.full2.N2O <- lmer(N2O ~ (ot1+ot2)*member + 
                      (ot1 + ot2 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_N2O, REML=F)

## THIRD ORDER
m.full3.N2O <- lmer(N2O ~ (ot1+ot2+ot3)*member + 
                      (ot1+ot2+ot3 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_N2O, REML=F)

## FOURTH ORDER
m.full4.N2O <- lmer(N2O ~ (ot1+ot2+ot3+ot4)*member + 
                      (ot1+ot2+ot3+ot4 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_N2O, REML=F)

##FIFTH ORDER
m.full5.N2O <- lmer(N2O ~ (ot1+ot2+ot3+ot4+ot5)*member + 
                      (ot1+ot2+ot3+ot4+ot5 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_N2O, REML=F)

##SIXTH ORDER
m.full6.N2O <- lmer(N2O ~ (ot1+ot2+ot3+ot4+ot5+ot6)*member + 
                      (ot1+ot2+ot3+ot4+ot5+ot6 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_N2O, REML=F)


##SEVENTH ORDER
m.full7.N2O <- lmer(N2O ~ (ot1+ot2+ot3+ot4+ot5+ot6+ot7)*member + 
                      (ot1+ot2+ot3+ot4+ot5+ot6+ot7 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_N2O, REML=F)

##EIGTH ORDER
m.full8.N2O <- lmer(N2O ~ (ot1+ot2+ot3+ot4+ot5+ot6+ot7+ot8)*member + 
                      (ot1+ot2+ot3+ot4+ot5+ot6+ot7+ot8 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_N2O, REML=F)


#compare model fit
anova(m.t1.N2O, m.full2.N2O, m.full3.N2O, m.full4.N2O)
#Assume normal distribution to report p-values
coefs <- data.frame(coef(summary(m.t1.N2O))) 
coefs$p <- format.pval(2*(1-pnorm(abs(coefs$t.value))), digits=2, eps=0.0001)
coefs




#-------------------------------------------------------------------------------
# Plot GCA model

df_N2O$year <- df_N2O$year + 1989

plot_N2O <- ggplot(df_N2O, aes(year, N2O, color=member)) +
  stat_summary(fun.data=mean_se, geom="pointrange", alpha =0.5, size = 0.8) + 
  labs(y="Aggregate N2O emissions", x="Year", color = "") +
  scale_color_manual(labels = c("Non-Members", "Members"), values = c("blue", "red")) +
  stat_summary(aes(y=fitted(m.full2.N2O)), fun.y=mean, geom="line", size = 0.8)
