
reg1 <- glm(cat2 ~ dog2 + att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + humans + factor(reltrad), family = "binomial", data = regg)
reg2 <- glm(dog2 ~ cat2 + att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + humans + factor(reltrad), family = "binomial", data = regg)




coef_names <- c("Owns a Dog" = "dog2",
                "Owns a Cat" = "cat2",
                "Church Attendance" = "att2", 
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

# coef_names <- coef_names[1:17]

plot <- plot_summs(reg1, reg2, coefs = coef_names, point.shape = FALSE, model.names = c("Cat", "Dog"), color.class = "Qual2") 

plot +
  labs(x ="Coefficient Estimate", y = "", subtitle = "", title = "Predicting Having Each Pet", caption = "Data: GSS 2018") +
  theme_gg("Montserrat") +
  scale_x_continuous(limits = c(-7.5,7.5)) +
  theme(legend.position = c(.85,.55)) +
  ggsave("D://pets/images/dummies.png", width = 10, height = 9, type = "cairo-png")



stargazer(reg1, reg2, type = "text", title = "Regression Model", dep.var.labels = c("Cat", "Dog"),
          covariate.labels = c("Owns a Dog", "Owns a Cat", "Church Attendance", "White", "Male", "Income", "Republican ID", "Number of Kids", "Age", "Education", "Urban", "Literalism", "Written by Humans", "Mainline", "Black Prot.", "Catholic", "Jewish", "Other Faith", "No Religion"), 
          star.cutoffs = c(0.05), out = "D://pets/images/dummies.htm")