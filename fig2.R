
gss <- gss %>% 
  mutate(male = car::recode(sex, "1=1; else =0")) %>% 
  mutate(male = as.factor(male)) %>% 
  mutate(pid7 = car::recode(partyid, "7:99 = NA")) %>% 
  mutate(pid72 = pid7/6) %>% 
  mutate(urban = car::recode(size, "750:9999 = 1; else =0")) %>% 
  mutate(urban = as.factor(urban)) %>% 
  mutate(childs2 = childs/8) %>% 
  mutate(educ2 = educ/20) %>% 
  mutate(income2 = income/12) %>% 
  mutate(age2 = age/89) %>% 
  mutate(att2 = attend/8) %>% 
  mutate(literal = car::recode(bible, "1=1; else =0")) %>% 
  mutate(humans = car::recode(bible, "3=1; else =0")) %>% 
  mutate(white = car::recode(race, "1=1; else=0")) %>% 
  mutate(reltrad2 = as.factor(reltrad)) 

reg1 <- glm.nb(numpets ~ attend + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + humans + reltrad2 , data = gss)

gg2 <- effect_plot(reg1, pred= attend, int.width = .76, interval = TRUE, data = gss) 

gg2 +
  labs(x = "Church Attendance", y = "Predicting Number of Pets", title = "The Interaction Between Church Attendance and Number of Pets", caption = "Data: GSS 2018") +
  theme_gg("Montserrat") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8), labels = c("Never", "Less than\nOnce a Year", "Once\na Year", "Several Times\na Year", "Once\na Month", "2-3x\nMonth", "Nearly\nEvery Week", "Every\nWeek", "More than\nonce a Week")) +
  ggsave("D://pets/images/fig2_new.png", type = "cairo-png", width = 9, height = 7)

library(stargazer)
stargazer(reg1, type = "text", title = "Figure 2 Regression Model", dep.var.labels = c("Predicting Number of Pets"),
          covariate.labels = c("Church Attendance", "White", "Male", "Income", "Republican ID", "Number of Kids", "Age", "Education", "Urban", "Literalism", "Written by Humans", "Mainline", "Black Prot.", "Catholic", "Jewish", "Other Faith", "No Religion"), column.labels = c(""),
          star.cutoffs = c(0.05), out = "D://pets/images/fig2_new.htm")


