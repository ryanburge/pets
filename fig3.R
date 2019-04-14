
regg <- gss %>% 
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
  mutate(reltrad2 = as.factor(reltrad))

gss <- gss %>% 
  mutate(cat2 = car::recode(cat, "1=1; 0=0; else = NA")) %>% 
  mutate(dog2 = car::recode(dog, "1=1; 0=0; else = NA")) %>% 
  mutate(horse2 = car::recode(horse, "1=1; 0=0; else = NA")) %>% 
  mutate(fish2 = car::recode(fish, "1=1; 0=0; else = NA")) %>% 
  mutate(pig2 = car::recode(pig, "1=1; 0=0; else= NA"))

reg1 <- glm(cat2 ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + factor(reltrad), family = "binomial", data = regg)
reg2 <- glm(dog2 ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + factor(reltrad), family = "binomial", data = regg)
reg3 <- glm(horse2 ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + factor(reltrad), family = "binomial", data = regg)
reg4 <- glm(fish2 ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + factor(reltrad), family = "binomial", data = regg)
reg5 <- glm(pig2 ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + factor(reltrad), family = "binomial", data = regg)


coef_names <- c("Church Attendance" = "att2", 
                "Education" = "educ2", 
                "Number of Kids" = "childs2", 
                "Male" = "male1", 
                "Republican ID" = "pid72", 
                "Age" = "age2", 
                "Urban" = "urban1", 
                "Income" =  "income2", 
                "Literalism" = "literal", 
                "Mainline" = "factor(reltrad)2", 
                "Black Prot." = "factor(reltrad)3",
                "Catholic" = "factor(reltrad)4",
                "Jewish" = "factor(reltrad)5",
                "Other Faith" = "factor(reltrad)6", 
                "No Religion" = "factor(reltrad)7",
                "Constant" = "(Intercept)")

coef_names <- coef_names[1:15]

plot <- plot_summs(reg1, reg2, reg3, reg4, reg5, coefs = coef_names, point.shape = FALSE, model.names = c("Cat", "Dog", "Horse", "Fish", "Pig"), color.class = "Qual2") 

plot +
  labs(x ="Coefficient Estimate", y = "", subtitle = "", title = "Predicting Having Each Pet") +
  theme_gg("Montserrat") +
  scale_x_continuous(limits = c(-5,5)) +
  theme(legend.position = c(.85,.55)) +
  ggsave("D://pets/images/fig3.png", width = 10, height = 6, type = "cairo-png")



stargazer(reg1, reg2, reg3, reg4, reg5, type = "text", title = "Figure 3 Regression Model", dep.var.labels = c("Cat", "Dog", "Horse", "Fish", "Pig"),
          covariate.labels = c("Church Attendance", "Male", "Income", "Republican ID", "Number of Kids", "Age", "Education", "Urban", "Literalism", "Mainline", "Black Prot.", "Catholic", "Jewish", "Other Faith", "No Religion"), 
          star.cutoffs = c(0.05), out = "D://pets/images/fig3.htm")
