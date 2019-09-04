
## Old Stuff ####

evan <- gss %>% 
  filter(evangelical ==1)

ml <- gss %>% 
  filter(mainline ==1)

cath <- gss %>% 
  filter(catholic ==1)

reg1 <- glm.nb(numpets ~ att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + humans, data = evan)
reg2 <- glm.nb(numpets ~ att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + humans, data = ml)
reg3 <- glm.nb(numpets ~ att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + humans, data = cath)

coef_names <- c("Church Attendance" = "att2", 
                "White" = "white", 
                "Education" = "educ2", 
                "Number of Kids" = "childs2", 
                "Male" = "male1", 
                "Republican ID" = "pid72", 
                "Age" = "age2", "Urban" = "urban1", 
                "Income" =  "income2", 
                "Literalism" = "literal", 
                "Written by Humans" = "humans",
                "Constant" = "(Intercept)")

coef_names <- coef_names[1:11]

plot <- plot_summs(reg1, reg2, reg3, coefs = coef_names, point.shape = FALSE, model.names = c("Evangelical", "Mainline", "Catholic"), color.class = "Qual2") 

plot +
  labs(x ="Coefficient Estimate", y = "", subtitle = "", title = "Predicting Number of Pets", caption = "Data: GSS 2018") +
  theme_gg("Montserrat") +
  theme(legend.position = c(.75,.55)) +
  ggsave("D://pets/images/fig3_new.png", width = 10, height = 6, type = "cairo-png")


reg <- gss %>% 
  filter(reltrad == 1 | reltrad == 2 | reltrad == 4) %>% 
  split(.$reltrad) %>% 
  purrr::map(~ glm.nb(numpets ~ att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + humans, data = .x)) 


stargazer(reg1, reg2, reg3, type = "text", title = "Figure 2 Regression Model", dep.var.labels = c("Predicting Number of Pets"),
          covariate.labels = c("Church Attendance", "White", "Male", "Income", "Republican ID", "Number of Kids", "Age", "Education", "Urban", "Literalism", "Written by Humans"), column.labels = c("Evangelical", "Mainline", "Catholic"),
          star.cutoffs = c(0.05), out = "D://pets/images/fig3_new.htm")