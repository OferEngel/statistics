# 10% of people have less than 10 friends, 
# 20% have less than 25 friends, 
# while 50% (the median) have over 100 friends. 
# Meanwhile, because the distribution is highly skewed, 
# the average friend count is 190.

x <- seq(1,5000, 1)
y <- dnorm(log(x), mean=log(190), sd=log(190/50))
ggplot() + geom_point(aes(x,y)) + 
  scale_x_log10(breaks=c(1,5,10,50,100, 190,500,1000, 5000))

myf <- function(x) 
  return(dnorm(log(x), mean=log(100), sd=log(190/40)))

b <- c(1,5,10,25, 50,100, 190,500,1000, 5000)
ggplot() + geom_point(aes(x=seq(1,5000, b=10), myf(seq(1,5000, b=10))))+ 
  geom_function(fun=myf) + 
  scale_x_log10(limits=c(1,5000), breaks=b) + 
  labs(x="Friends", y="Probability")





penguins <- penguins %>% na.omit(sex)

ggplot(penguins, 
       aes(x=bill_depth_mm, 
           y=body_mass_g, 
           color=sex)) + 
  geom_point() + 
  theme(legend.position ="bottom")




dat_raw <- haven::read_sav(here::here( "data", 
                                       "sadc_2017_national.sav"))
foreign::read.spss(here::here( "data", 
                               "sadc_2017_national.sav"))



library(tidyverse)
nobel <- read.csv("https://bit.ly/3Ikf3i0")


nobel %>%
  filter(!is.na(country),gender != "org",is.na(died_date)) %>% 
  mutate(country_us = if_else(country == "USA", "USA", "Other"),
         born_country_us = if_else(born_country == "USA", "USA", "Other")) %>% 
  ggplot(aes(fill=born_country_us, x=country_us)) + 
  geom_bar() + 
  scale_fill_manual(name="country of birth", 
                    values=c("#E69F00", "#56B4E9")) +
  facet_wrap(~category, nrow=1) + labs(x="") + coord_flip() + 
  theme(legend.position = "bottom")
  

# 37 out of 128


x <- numeric()
for(i in 1:10000)
  x <- c(x,sum(sample(c(rep(1,37), rep(0, 128-37)), 128, replace = TRUE)))

ggplot() + 
  geom_histogram(aes(x=x, y=..density..), binwidth = 1) +
  geom_vline(xintercept = c(27, mean(x), 47)) + 
  scale_x_continuous()

sort(x)[.025*10000]
sort(x)[.975*10000]



