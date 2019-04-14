
## Interaction ####

int <- gss %>% 
  filter(reltrad == 1 | reltrad == 2 | reltrad == 4) %>% 
  mutate(reltrad = as.factor(reltrad))

reg1 <- glm.nb(numpets ~ attend*reltrad + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal, data = int)

gg2 <- interact_plot(reg1, pred= attend, modx = reltrad, int.width = .76, interval = TRUE, modx.labels = c("Evangelical", "Mainline", "Catholic"), color.class = "Qual2") 

gg2 +
  labs(x = "Church Attendance", y = "Prediciting Number of Pets", title = "The Interaction Between Church Attendance and Number of Pets") +
  theme_gg("Montserrat") +
  theme(legend.position = c(.75, .85)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8), labels = c("Never", "Less than\nOnce a Year", "Once\na Year", "Several Times\na Year", "Once\na Month", "2-3x\nMonth", "Nearly\nEvery Week", "Every\nWeek", "More than\nonce a Week")) +
  ggsave("D://pets/images/fig4.png", type = "cairo-png", width = 9, height = 7)



stargazer(reg1, type = "text", title = "Figure 4 Regression Model", dep.var.labels = c("Predicting Number of Pets"),
          covariate.labels = c("Church Attendance", "Mainline", "Catholic", "Male", "Income", "Republican ID", "Number of Kids", "Age", "Education", "Urban", "Literalism", "Attend*Mainline", "Attend*Catholic"),
          star.cutoffs = c(0.05), out = "D://pets/images/fig4.htm")
