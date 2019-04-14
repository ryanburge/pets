
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
  mutate(literal = car::recode(bible, "1=1; else =0"))

evan <- gss %>% 
  filter(evangelical ==1)

ml <- gss %>% 
  filter(mainline ==1)

cath <- gss %>% 
  filter(catholic ==1)

reg1 <- glm.nb(numpets ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal, data = evan)
reg2 <- glm.nb(numpets ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal, data = ml)
reg3 <- glm.nb(numpets ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal, data = cath)

coef_names <- c("Church Attendance" = "att2", "Education" = "educ2", "Number of Kids" = "childs2", "Male" = "male1", "Republican ID" = "pid72", "Age" = "age2", "Urban" = "urban1", "Income" =  "income2", "Literalism" = "literal", "Constant" = "(Intercept)")

coef_names <- coef_names[1:9]

plot <- plot_summs(reg1, reg2, reg3, coefs = coef_names, point.shape = FALSE, model.names = c("Evangelical", "Mainline", "Catholic"), color.class = "Qual2") 

plot +
  labs(x ="Coefficient Estimate", y = "", subtitle = "", title = "Predicting Number of Pets") +
  theme_gg("Montserrat") +
  theme(legend.position = c(.75,.55)) +
  ggsave("D://pets/images/fig2.png", width = 10, height = 6, type = "cairo-png")


reg <- gss %>% 
  filter(reltrad == 1 | reltrad == 2 | reltrad == 4) %>% 
  split(.$reltrad) %>% 
  purrr::map(~ glm.nb(numpets ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal, data = .x)) 


stargazer(reg, type = "text", title = "Figure 2 Regression Model", dep.var.labels = c("Predicting Number of Pets"),
          covariate.labels = c("Church Attendance", "Male", "Income", "Republican ID", "Number of Kids", "Age", "Education", "Urban", "Literalism"), column.labels = c("Evangelical", "Mainline", "Catholic"),
          star.cutoffs = c(0.05), out = "D://pets/images/fig2.htm")
