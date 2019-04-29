## Loading Data and Packages ####

library(fst)
library(socsci)
library(car)
library(jtools)
library(ggsci)
source("D://theme.R")

gss <- read.fst("C://gss18.fst") %>% 
  filter(year == 2018)

## Creating Dichotomous DVs ####

gss <- gss %>% 
  mutate(cat2 = car::recode(cat, "1=1; 0=0; else = NA")) %>% 
  mutate(dog2 = car::recode(dog, "1=1; 0=0; else = NA")) %>% 
  mutate(horse2 = car::recode(horse, "1=1; 0=0; else = NA")) %>% 
  mutate(fish2 = car::recode(fish, "1=1; 0=0; else = NA")) %>% 
  mutate(pig2 = car::recode(pig, "1=1; 0=0; else= NA"))


cfun <- function(df, var, var2) {
  
  var <- enquo(var)
  
  df %>% 
    mean_ci(!! var) %>% 
    mutate(animal = var2)
  
}
  
yyy1 <- gss %>% cfun(cat2, "Cat")
yyy2 <- gss %>% cfun(dog2, "Dog")
yyy3 <- gss %>% cfun(horse2, "Horse")
yyy4 <- gss %>% cfun(fish2, "Fish")
yyy5 <- gss %>% cfun(pig2, "Pig")

graph <- bind_df("yyy") %>% 
  mutate(reltrad = frcode(reltrad ==1 ~ "Evangelical", 
                          reltrad == 2 ~ "Mainline",
                          reltrad == 3 ~ "Black Prot.",
                          reltrad == 4 ~ "Catholic",
                          reltrad == 5 ~ "Jewish",
                          reltrad == 6 ~ "Other Faith",
                          reltrad == 7 ~ "No Faith"))

graph %>% 
  mutate(mean = round(mean, 3)) %>% 
  filter(reltrad != "NA") %>% 
  ggplot(., aes(x = reltrad, y = mean, fill = reltrad)) +
  geom_col(color = "black") +
  facet_wrap(~ animal) +
  theme_gg("Montserrat") +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  scale_fill_npg() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9))  +
  geom_text(aes(y = .25, label = paste0(mean*100, '%')), position = position_stack(vjust = 0.5), size = 2, family = "font") +
  theme(axis.text.x = element_text(family = "font", size =12, angle = 45, hjust = 1)) +
  labs(x = "Religious Tradition", y = "Percent with Each Animal", title = "Religious Traditions and Pet Ownership", caption = "Data: GSS 2018") +
  ggsave("D://pets/images/yes_pet_reltrad.png", type = "cairo-png", width = 9)


### Number of Pets Per Tradition ####

gg <- gss %>% 
  group_by(reltrad) %>% 
  mean_ci(numpets)


gg <- gg %>% 
  ungroup(reltrad) %>% 
  mutate(reltrad = frcode(reltrad ==1 ~ "Evangelical", 
                          reltrad == 2 ~ "Mainline",
                          reltrad == 3 ~ "Black Prot.",
                          reltrad == 4 ~ "Catholic",
                          reltrad == 5 ~ "Jewish",
                          reltrad == 6 ~ "Other Faith",
                          reltrad == 7 ~ "No Faith"))


gg %>% 
  mutate(mean = round(mean, 2)) %>% 
  filter(reltrad != "NA") %>% 
  ggplot(., aes(x = reltrad, y = mean, fill = reltrad)) +
  geom_col(color = "black") +
  theme_gg("Montserrat") +
  theme(legend.position = "none") +
  scale_fill_npg() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9))  +
  geom_text(aes(y = .32, label = paste0(mean, '\nPets')), position = position_stack(vjust = 0.5), size = 4, family = "font") +
  labs(x = "Religious Tradition", y = "Mean Number of Household Pets", title = "Religious Traditions and Number of Pets", caption = "Data: GSS 2018") +
  ggsave("D://pets/images/number_pet_reltrad.png", type = "cairo-png", width = 9)

## Number of Pets by RELTRAD and Attendance ####

gg2 <- gss %>% 
  group_by(reltrad, attend) %>% 
  mean_ci(numpets) %>% 
  ungroup(reltrad, attend)

gg2 <- gg2 %>% 
  filter(reltrad != "NA") %>% 
  filter(attend != "NA") %>% 
  mutate(reltrad = frcode(reltrad ==1 ~ "Evangelical", 
                          reltrad == 2 ~ "Mainline",
                          reltrad == 3 ~ "Black Prot.",
                          reltrad == 4 ~ "Catholic",
                          reltrad == 5 ~ "Jewish",
                          reltrad == 6 ~ "Other Faith",
                          reltrad == 7 ~ "No Faith")) %>% 
  mutate(attend = frcode(attend == 0 ~ "Never",
                         attend == 1 ~ "Less than\nOnce a Year", 
                         attend == 2 ~ "Once\na Year",
                         attend == 3 ~ "Several Times\na Year",
                         attend == 4 ~ "Once\na Month",
                         attend == 5 ~ "2-3x\nMonth",
                         attend == 6 ~ "Nearly\nEvery Week", 
                         attend == 7 ~ "Every\nWeek", 
                         attend == 8 ~ "More than\nonce a week")) 
  
gg2 %>% 
  ggplot(., aes(x = attend, y = mean, color = reltrad, group = reltrad)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, color = reltrad, fill = reltrad), alpha = .4, show.legend = FALSE) +
  facet_wrap(~ reltrad) +
  theme_gg("Montserrat") +
  scale_fill_npg() +
  scale_color_npg() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8), labels = c("Never", "", "", "", "Once\na Month", "", "", "Every\nWeek", "")) +
  labs(x = "", y = "Mean Number of Pets", title = "Religious Attendance and Number of Pets", caption = "Data: GSS 2018") +
  ggsave("D://pets/images/pet_number_reltrad_att.png", type = "cairo-png", width = 9)

## Making the Regression Plot - Number of Pets ####
library(jtools)
library(MASS)


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
  ggsave("D://pets/images/regression.png", width = 10, height = 6, type = "cairo-png")

## Printing the Regression Table ##

library(stargazer)

reg <- gss %>% 
  filter(reltrad == 1 | reltrad == 2 | reltrad == 4) %>% 
  split(.$reltrad) %>% 
  purrr::map(~ glm.nb(numpets ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban, data = .x)) 


stargazer(reg, type = "text", title = "Regression Model", dep.var.labels = c("Predicting Number of Pets"),
          covariate.labels = c("Church Attendance", "Male", "Income", "Republican ID", "Number of Kids", "Age", "Education", "Urban"), column.labels = c("Evangelical", "Mainline", "Catholic"),
          star.cutoffs = c(0.05), out = "D://pets/images/regression1.htm")



## Regression - Have a Pet ####

gss <- gss %>% 
  mutate(havepet = car::recode(numpets, "0=0; 1:20=1; else = NA"))


evan <- gss %>% 
  filter(evangelical ==1)

ml <- gss %>% 
  filter(mainline ==1)

cath <- gss %>% 
  filter(catholic ==1)

reg1 <- glm(havepet ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal, family = "binomial", data = evan)
reg2 <- glm(havepet ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal, family = "binomial", data = ml)
reg3 <- glm(havepet ~ att2 + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal, family = "binomial", data = cath)


coef_names <- c("Church Attendance" = "att2", "Education" = "educ2", "Number of Kids" = "childs2", "Male" = "male1", "Republican ID" = "pid72", "Age" = "age2", "Urban" = "urban1", "Income" =  "income2", "Literalism" = "literal", "Constant" = "(Intercept)")

coef_names <- coef_names[1:9]

plot <- plot_summs(reg1, reg2, reg3, coefs = coef_names, point.shape = FALSE, model.names = c("Evangelical", "Mainline", "Catholic"), color.class = "Qual2") 

plot +
  labs(x ="Coefficient Estimate", y = "", subtitle = "", title = "Predicting Having a Pet") +
  theme_gg("Montserrat") +
  theme(legend.position = c(.75,.55)) +
  ggsave("D://pets/images/regression_havepet.png", width = 10, height = 6, type = "cairo-png")


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
  ggsave("D://pets/images/interaction.png", type = "cairo-png", width = 9)


attend == 0 ~ "Never",
attend == 1 ~ "Less than\nOnce a Year", 
attend == 2 ~ "Once\na Year",
attend == 3 ~ "Several Times\na Year",
attend == 4 ~ "Once\na Month",
attend == 5 ~ "2-3x\nMonth",
attend == 6 ~ "Nearly\nEvery Week", 
attend == 7 ~ "Every\nWeek", 
attend == 8 ~ "More than\nonce a week")