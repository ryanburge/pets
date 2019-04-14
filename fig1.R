
## Figure 1 ####

library(jtools)
library(MASS)


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

reg1 <- glm.nb(numpets ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + factor(reltrad), data = regg)

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

plot <- plot_summs(reg1, coefs = coef_names, point.shape = FALSE) 

plot +
  labs(x ="Coefficient Estimate", y = "", subtitle = "", title = "Predicting Number of Pets", caption = "Data: GSS 2018") +
  theme_gg("Montserrat") +
  ggsave("D://pets/images/fig1.png", width = 10, height = 6, type = "cairo-png")


stargazer(reg1, type = "text", title = "Figure 1 Regression Model", dep.var.labels = c("Predicting Number of Pets"),
          covariate.labels = c("Church Attendance", "Male", "Income", "Republican ID", "Number of Kids", "Age", "Education", "Urban", "Literalism", "Mainline", "Black Prot.", "Catholic", "Jewish", "Other Faith", "No Religion"),
          star.cutoffs = c(0.05), out = "D://pets/images/fig1.htm")