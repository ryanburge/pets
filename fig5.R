
gss <- gss %>% 
  mutate(cat2 = car::recode(cat, "1=1; 0=0; else = NA")) %>% 
  mutate(dog2 = car::recode(dog, "1=1; 0=0; else = NA")) %>% 
  mutate(fish2 = car::recode(fish, "1=1; 0=0; else = NA")) %>% 
  mutate(bird2 = car::recode(bird, "1=1; 0=0; else = NA")) %>% 
  mutate(mamm2 = car::recode(smammal, "1=1; 0=0; else = NA"))

cfun <- function(df, var, var2) {
  
  var <- enquo(var)
  
  df %>% 
    group_by(reltrad) %>% 
    mean_ci(!! var) %>% 
    mutate(animal = var2)
  
}

yyy1 <- gss %>% cfun(cat2, "Cat")
yyy2 <- gss %>% cfun(dog2, "Dog")
yyy3 <- gss %>% cfun(fish2, "Fish")
yyy4 <- gss %>% cfun(bird2, "Bird")
yyy5 <- gss %>% cfun(mamm2, "Small Mammal")

graph <- bind_df("yyy") %>% 
  mutate(reltrad = frcode(reltrad ==1 ~ "Evangelical", 
                          reltrad == 2 ~ "Mainline",
                          reltrad == 3 ~ "Black Prot.",
                          reltrad == 4 ~ "Catholic",
                          reltrad == 5 ~ "Jewish",
                          reltrad == 6 ~ "Other Faith",
                          reltrad == 7 ~ "No Faith"))
graph$animal <- factor(graph$animal, levels = c("Dog", "Cat", "Small Mammal", "Bird", "Fish"))

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