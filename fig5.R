
gss <- gss %>% 
  mutate(cat2 = car::recode(cat, "1=1; 0=0; else = NA")) %>% 
  mutate(dog2 = car::recode(dog, "1=1; 0=0; else = NA")) %>% 
  mutate(fish2 = car::recode(fish, "1=1; 0=0; else = NA")) %>% 
  mutate(bird2 = car::recode(bird, "1=1; 0=0; else = NA")) %>% 
  mutate(mamm2 = car::recode(smammal, "1=1; 0=0; else = NA"))


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
  mutate(reltrad2 = as.factor(reltrad)) %>% 
  mutate(white = car::recode(race, "1=1; else =0")) 

reg1 <- glm(cat2 ~ att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + factor(reltrad), family = "binomial", data = regg)
reg2 <- glm(dog2 ~ att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + factor(reltrad), family = "binomial", data = regg)
reg3 <- glm(fish2 ~ att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + factor(reltrad), family = "binomial", data = regg)
reg4 <- glm(bird2 ~ att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + factor(reltrad), family = "binomial", data = regg)
reg5 <- glm(mamm2 ~ att2 + white + male + income2 + pid72 + childs2 + age2 + educ2 + urban + literal + factor(reltrad), family = "binomial", data = regg)

reg1t <- tidy(reg1) %>% mutate(dv = "Cat")
reg2t <- tidy(reg2) %>% mutate(dv = "Dog")
reg3t <- tidy(reg3) %>% mutate(dv = "Fish")
reg4t <- tidy(reg4) %>% mutate(dv = "Bird")
reg5t <- tidy(reg5) %>% mutate(dv = "Small Mammal")

graph <- bind_rows(reg1t, reg2t, reg3t, reg4t, reg5t)

graph <- graph %>% 
  filter(term == "factor(reltrad)2" | term == "factor(reltrad)3" |  term == "factor(reltrad)4" |term == "factor(reltrad)5" |term == "factor(reltrad)6" | term == "factor(reltrad)7")

graph <- graph %>% 
 mutate(term = car::recode(term, "'factor(reltrad)2' = 'Mainline';
                                  'factor(reltrad)3' = 'Black\nProt.';
                                  'factor(reltrad)4' = 'Catholic';
                                  'factor(reltrad)5' = 'Jewish';
                                  'factor(reltrad)6' = 'Other Faith';
                                  'factor(reltrad)7' = 'No Religion'"))

graph <- graph %>% 
  mutate(sig = case_when(p.value <= .05 ~ " Yes ",
                         TRUE ~ " No "))


graph$dv <- factor(graph$dv, levels = c("Dog", "Cat", "Small Mammal", "Bird", "Fish"))

library(showtext)

theme_gg <- function(fff, base_size = 14, base_family = "font") 
{
  
  font_add_google(fff, "font")
  showtext_auto()
  
  theme_minimal() +
    theme(legend.position = "none") +
    # theme(legend.title = element_blank()) +
    theme(text=element_text(size=14, family="font"))
  
  
}

showtext_opts(dpi = 300)

graph %>% 
  ggplot(., aes(x = term, y = estimate, fill = sig)) +
  geom_col(color = "black") +
  facet_wrap(~ dv) +
  theme_gg("Montserrat") +
  theme(legend.position = c(.85, .05)) +
  # scale_fill_npg() +
  # geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9))  +
  # geom_text(aes(y = .25, label = paste0(mean*100, '%')), position = position_stack(vjust = 0.5), size = 2, family = "font") +
  theme(axis.text.x = element_text(family = "font", size =12, angle = 45, hjust = 1)) +
  labs(x = "Religious Tradition", y = "Logit Estimate for Owning Each Animal", title = "Religious Traditions and Pet Ownership", caption = "Data: GSS 2018") +
  guides(fill = guide_legend(reverse=T)) +
  scale_fill_manual(values=c("azure3", "black"), name="Statistical Significance") +
  ggsave("D://pets/images/yes_pet_reltrad.png", type = "cairo-png", width = 9)