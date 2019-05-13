#Transforming to right format (scaling, removing NA's, making country factor)
df_GHG <- subset(final_data, select = c("country", "year", "GHG", "member"))%>%
  transform(year=year-1989) %>%
  na.omit()
df$country <- factor(df$country)

#Base model (random intercept model)
m.base.GHG <- lmer(GHG ~ year + (1 | country), data=df_GHG, REML=F)

#Adding fixed effect of being member
m.0.GHG <- lmer(GHG ~ year + member + (1 | country), data=df_GHG, REML=F)

#Adding the FE on slope (Effects of being member on the rate of growth of CO2 emissions):
m.1.GHG <- lmer(GHG ~ year * member + (1 | country), data=df_GHG, REML=F)
m.1.GHG

#Statistically evaluate model: model comparison of goodness of fit
anova(m.base.GHG, m.0.GHG, m.1.GHG)

##No significant differences between the models

#Add linear effect on time to capture individual differences in growth rate (year on country)
m.t1.GHG <- lmer(GHG ~ year * member + (1 + year | country), data=df_GHG, REML=F)

anova(m.t1.GHG, m.t1.GHG)
coef(summary(m.t1.GHG))

coefs_GHG <- data.frame(coef(summary(m.t1.GHG))) 
coefs_GHG$p <- format.pval(2*(1-pnorm(abs(coefs_GHG$t.value))), digits=2, eps=0.0001)
coefs_GHG

##This significantly improved model fit


#Create orthogonal polynomials, determine which order gives best model fit
#Create polynominals
t <- poly((unique(df_GHG$year)), 8)

#Create orthogonal polynomial time terms
df_GHG[,paste("ot", 1:8, sep="")] <- t[df_GHG$year, 1:8]

## SECOND ORDER
m.full2.GHG <- lmer(GHG ~ (ot1+ot2)*member + 
                  (ot1 + ot2 | country) + 
                  (ot1+ot2 | country:member), 
                control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                data=df_GHG, REML=F)

## THIRD ORDER
m.full3.GHG <- lmer(GHG ~ (ot1+ot2+ot3)*member + 
                  (ot1+ot2+ot3 | country) + 
                  (ot1+ot2 | country:member), 
                control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                data=df_GHG, REML=F)

## FOURTH ORDER
m.full4.GHG <- lmer(GHG ~ (ot1+ot2+ot3+ot4)*member + 
                  (ot1+ot2+ot3+ot4 | country) + 
                  (ot1+ot2 | country:member), 
                control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                data=df_GHG, REML=F)

##FIFTH ORDER
m.full5.GHG <- lmer(GHG ~ (ot1+ot2+ot3+ot4+ot5)*member + 
                  (ot1+ot2+ot3+ot4+ot5 | country) + 
                  (ot1+ot2 | country:member), 
                control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                data=df_GHG, REML=F)

##SIXTH ORDER
m.full6.GHG <- lmer(GHG ~ (ot1+ot2+ot3+ot4+ot5+ot6)*member + 
                  (ot1+ot2+ot3+ot4+ot5+ot6 | country) + 
                  (ot1+ot2 | country:member), 
                control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                data=df_GHG, REML=F)


##SEVENTH ORDER
m.full7.GHG <- lmer(GHG ~ (ot1+ot2+ot3+ot4+ot5+ot6+ot7)*member + 
                  (ot1+ot2+ot3+ot4+ot5+ot6+ot7 | country) + 
                  (ot1+ot2 | country:member), 
                control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                data=df_GHG, REML=F)

##EIGTH ORDER
m.full8.GHG <- lmer(GHG ~ (ot1+ot2+ot3+ot4+ot5+ot6+ot7+ot8)*member + 
                  (ot1+ot2+ot3+ot4+ot5+ot6+ot7+ot8 | country) + 
                  (ot1+ot2 | country:member), 
                control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                data=df_GHG, REML=F)


#compare model fit
anova(m.t1.GHG, m.full2.GHG, m.full3.GHG, m.full4.GHG, m.full5.GHG, m.full6.GHG, m.full7.GHG, m.full8.GHG)
#Assume normal distribution to report p-values
coefs <- data.frame(coef(summary(m.t1.GHG))) 
coefs$p <- format.pval(2*(1-pnorm(abs(coefs$t.value))), digits=2, eps=0.0001)
coefs




#-------------------------------------------------------------------------------
# Plot GCA model

df_GHG$year <- df_GHG$year + 1989

plot_GHG <- ggplot(df_GHG, aes(year, GHG, color=member)) +
  stat_summary(fun.data=mean_se, geom="pointrange", alpha =0.5, size = 0.8) + 
  labs(y="Aggregate GHG emissions", x="Year", color = "") +
  scale_color_manual(labels = c("Non-Members", "Members"), values = c("blue", "red")) +
  stat_summary(aes(y=fitted(m.full2.GHG)), fun.y=mean, geom="line", size = 0.8)


library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(plot_CO2), ggplotGrob(plot_GHG), size = "last"))


