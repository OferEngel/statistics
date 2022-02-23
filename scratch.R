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




