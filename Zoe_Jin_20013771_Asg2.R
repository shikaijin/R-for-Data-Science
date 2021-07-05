library(tidyverse)

# Q1
filter(mtcars, gear == 4)



# Q2(a)
filter(mtcars, cyl == 6 | gear == 4)

# Q2(b)
nrow(filter(mtcars, cyl == 6, gear == 4))
## There are 4 cars in mtcars have both 6 cylinders and 4 forward gears.


# Q3
## To find the heaviest car, we sort in descending order of weight
arrange(mtcars, desc(wt))
## The heaviest car is Lincoln Continental

## To find the lightest car, we sort in ascending order of weight
arrange(mtcars, wt)
## The lightest car is Lotus Europa



library(nycflights13)
# Q4
filter(flights, month == 1 | month == 11, arr_delay <= 10)



# Q5
nrow(filter(flights, month == 5))
## There were 28796 flights that departed in May.


# Q6(a)
flights %>% 
  filter(month == 2) %>% 
  ggplot(aes(x = dep_delay)) +
  geom_histogram(binwidth = 10)

# Q6(b)
flights %>% 
  filter(month == 2, dep_delay < 100) %>% 
  ggplot(aes(x = dep_delay)) +
  geom_histogram(binwidth = 5)



# Q7(a)
delay1 <- filter(flights, month == 1, day == 1, dep_delay < 180)
ggplot(delay1, aes(x = dep_delay, y = arr_delay)) +
  geom_point() 

# Q7(b)
delay2 <- filter(flights, month == 1, day == 1, dep_delay < 10)
ggplot(delay2, aes(x = dep_delay, y = arr_delay)) +
  geom_point() 

# Q7(C)
# Graph in (a) shows a more apparent trend between the two delay times 
# since Graph in (a) shows a stronger positive association than Graph in (b).

# Q7(d)
# The correlation for the points in (a) is:
cor(delay1$dep_delay, delay1$arr_delay, use="complete.obs")
## [1] 0.8513342

# The correlation for the points in (b) is:
cor(delay2$dep_delay, delay2$arr_delay, use="complete.obs")
## [1] 0.3496184
## the results make sense since the correlation for the points in (a) is greater than that in (b).


# Q8
plot_delay <- function(which_month, which_day, lower_range, upper_range){
  flights %>%
    filter(month == which_month, day == which_day, between(dep_delay, lower_range, upper_range)) %>% 
    ggplot(aes(x = dep_delay, y = arr_delay)) +
    geom_point()
}



# Q9
ggplot(flights, aes(x = month)) + 
  geom_bar()
  


# Q10
avg_dep_delay <- 
  flights %>% 
  group_by(month, day) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(Time = 1:365)

ggplot(avg_dep_delay, aes(x = Time, y = avg_dep_delay)) +
  geom_line() 