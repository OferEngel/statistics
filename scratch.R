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

paintings <- 
  read_csv("https://bit.ly/36IfLaO", 
           na = c("n/a", "", "NA"))

paintings <- paintings %>% 
  drop_na(Width_in, Height_in) %>% 
  mutate(layout=ifelse(Height_in > Width_in, 
                       "portrait", 
                       "landscape"))



ggplot(paintings, 
       aes(x = Width_in, 
           y = Height_in, 
           color=layout, 
           shape=layout)) + 
  scale_x_log10() + 
  scale_y_log10() +
  geom_point() + 
  labs(
    title = "Height vs. Width of paintings"  ,
    subtitle = "Paris auctions, 1764 - 1780" ,
    x = "Width (Inches)" ,
    y = "Height (Inches)") + 
  geom_abline(slope=1, intercept=0, color="purple") + 
  scale_color_manual(values=c("red", "green"))
  

mtcars
bootstraps(mtcars, times = 2)



data.frame(random.varaible=1:20)


library(tidymodels)
library(tidyverse)
data.frame(random.var=1:100) %>%
  ggplot() + geom_histogram(aes(random.var), binwidth = .5)



boot <- data.frame(random.var=1:100) %>%
  # specify the variable of interest
  specify(response = random.var) %>% 
  # generate 10000 bootstrap samples
  generate(reps = 10000, type = "bootstrap") %>% 
  # calculate the mean of each bootstrap sample
  calculate(stat = "mean") 

quantile(boot$stat, probs=c(0.025,0.975))
quantile(boot$stat, probs=.99)


# A student is collecting some data, and she came back 
# with a new sample whose mean was 57. 
# What is the p-value? 
# 
# p-value: assuming that the students' sample was taken from the 
# same population, what is the probability that we would get
# we would get the mean 57 or more extreme? 
mean(boot$stat>____)



ggplot() + geom_histogram(aes(x=stat), binwidth = .5) + 
  geom_vline(xintercept = c(mean(1:100)))


# Calculate the mean of each bootstrap
boot <- data.frame(random.var=____) %>%
  # specify the variable of interest
  specify(response = ____) %>% 
  # generate 10000 bootstrap samples
  generate(reps = ____, type = "bootstrap") %>% 
  # calculate the mean of each bootstrap sample
  calculate(stat = "mean")

# Show the distribution of those means...
ggplot()....



# Imagine we took a different sample

# Create five bootstraps
boot <- data.frame(random.var=1:100) %>%
  # specify the variable of interest
  specify(response = random.var) %>% 
  # generate 5 bootstrap samples
  generate(reps = 5, type = "bootstrap")

# create a density plot of the five bootstraps
ggplot(boot) + 
  geom_density(aes(x=random.var, 
                   fill=factor(replicate)), 
               alpha=.4, color=NA)


# Create five bootstraps
boot <- data.frame(random.var=____) %>%
  # specify the variable of interest
  specify(response = ____) %>% 
  # generate 5 bootstrap samples
  generate(reps = ____, type = "bootstrap")

# create a density plot of the five bootstraps
ggplot(boot) + 
  geom_density(aes(x=____, 
                   fill=____), 
               alpha=.4, color=NA)



