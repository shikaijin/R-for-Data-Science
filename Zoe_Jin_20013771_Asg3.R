library(tidyverse)
library(nycflights13)

# Q1(a)
## The ggplot(data = mpg) gives a blank plots with gray background.

# Q1(b)
ggplot(mpg, aes(x = drv, y = class)) +
  geom_point()
## It is not useful because scatter plots are often used to visualize the relationship 
## between two continuous variables while the two variables in this graph are categorical
## and the graph shows no pattern.



# Q2 (a)
## The points are not blue since color = "blue" is inside aes(), which gives an 
## aesthetic mapping `color` -> "blue", and color becomes an additional discrete variable.

# Q2 (b)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")



# Q3
origin_avg_delay <-
  flights %>% 
  group_by(origin) %>% 
  summarize(arrival = mean(arr_delay, na.rm = TRUE), departure = mean(flights$dep_delay, na.rm=TRUE)) %>%
  ungroup() %>% 
  gather(Type, avg_delay, arrival:departure)
  
ggplot(origin_avg_delay, aes(x = origin, y = avg_delay, fill = Type)) +
  geom_col(position = "dodge") +
  labs(
    y = "Average Delay", 
    title = "Average Arrival Delay and Departure Delay by Origins"
  )



# Q4(a)
(avg_price <- summarize(group_by(diamonds, cut), avg_price = mean(price, na.rm = TRUE))$avg_price)
## [1] 4358.758 3928.864 3981.760 4584.258 3457.542
## The results are not very reasonable since the average price of fair cut is higher
## than that of good cut, very good cut and ideal cut.

# Q4(b)
ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  facet_grid(cut ~ ., scales = "free_y")
## We do not expect the observation that a fair diamond tends to be more expensive than an 
## ideal diamond since the scales vary across columns and therefore the scales of count are different.

# Q4(c)
diamonds %>% 
  filter(between(carat, 0.9, 1)) %>% 
  ggplot(aes(x = price)) +
  geom_histogram() +
  facet_grid(cut ~ ., scales = "free_y")



# Q5
diamonds_q5 <- filter(diamonds, cut == "Ideal", clarity == "VS2")
ggplot(diamonds_q5, aes(x = carat, y = price, color = color)) +
  geom_point(size = 0.9)
## When diamonds are of the same weight, the better the color, the higher the price.
## When diamonds are of the same color, the heavier the diamond, the more expensive it is.


# Q6
## The se argument to geom_smooth() determines to display confidence interval around smooth or not.
## The se = True inside the geom_smooth() function adds confidence interval on the smooth.
## The se = False inside the geom_smooth() function removes confidence interval on the smooth.



# Q7
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()



# Q8
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv), se = FALSE)



# Q9
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(data = filter(mpg, drv == "r"), se = FALSE)



# Q10(a)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Q10(b)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(mapping = aes(x = displ, y = hwy), se = FALSE)



