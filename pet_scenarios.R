gss <- gss %>% 
  mutate(cat2 = car::recode(cat, "1=1; 0=0; else = NA")) %>% 
  mutate(dog2 = car::recode(dog, "1=1; 0=0; else = NA"))

gss <- gss %>% 
  filter(cat != "NA") %>% 
  filter(dog != "NA")


gss <- gss %>%
  mutate(catonly = case_when(cat == 1 & dog == 0 ~ 1, TRUE ~ 0)) %>% 
  mutate(dogonly = case_when(cat == 0 & dog == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(both    = case_when(cat == 1 & dog == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(neither = case_when(cat == 0 & dog == 0 ~ 1, TRUE ~ 0))


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
  mutate(humans = car::recode(bible, "3=1; else =0")) %>% 
  mutate(reltrad2 = as.factor(reltrad)) %>% 
  mutate(white = car::recode(race, "1=1; else =0"))


reg1 <- glm(catonly ~ att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + humans + factor(reltrad), family = "binomial", data = regg)
reg2 <- glm(dogonly ~ att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + humans + factor(reltrad), family = "binomial", data = regg)
reg3 <- glm(both    ~ att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + humans + factor(reltrad), family = "binomial", data = regg)
reg4 <- glm(neither ~ att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + humans + factor(reltrad), family = "binomial", data = regg)




coef_names <- c("Church Attendance" = "att2", 
                "White" = "white",
                "Education" = "educ2", 
                "Number of Kids" = "childs2", 
                "Male" = "male1", 
                "Republican ID" = "pid72", 
                "Age" = "age2", 
                "Urban" = "urban1", 
                "Income" =  "income2", 
                "Literalism" = "literal", 
                "Written by Humans" = "humans",
                "Mainline" = "factor(reltrad)2", 
                "Black Prot." = "factor(reltrad)3",
                "Catholic" = "factor(reltrad)4",
                "Jewish" = "factor(reltrad)5",
                "Other Faith" = "factor(reltrad)6", 
                "No Religion" = "factor(reltrad)7",
                "Constant" = "(Intercept)")

coef_names <- coef_names[1:17]


plot <- plot_summs(reg1, reg2, reg3, reg4, coefs = coef_names, point.shape = FALSE, model.names = c("Cat Only", "Dog Only", "Both", "Neither"), color.class = "Qual2") 


plot +
  labs(x ="Coefficient Estimate", y = "", subtitle = "", title = "Predicting Each Pet Scenario", caption = "Data: GSS 2018") +
  theme_gg("Montserrat") +
  scale_x_continuous(limits = c(-7.5,7.5)) +
  theme(legend.position = c(.85,.55)) +
  ggsave("D://pets/images/scenarios.png", width = 10, height = 9, type = "cairo-png")




stargazer(reg1, reg2, reg3, reg4, type = "text", title = "Scenarios Regression Model", dep.var.labels = c("Cat Only", "Dog Only", "Both", "Neither"),
          covariate.labels = c("Church Attendance", "White", "Male", "Income", "Republican ID", "Number of Kids", "Age", "Education", "Urban", "Literalism", "Written by Humans", "Mainline", "Black Prot.", "Catholic", "Jewish", "Other Faith", "No Religion"), 
          star.cutoffs = c(0.05), out = "D://pets/images/scenarios.htm")

