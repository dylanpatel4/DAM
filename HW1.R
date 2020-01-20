library(tidyverse)

#EXERCISE 1a
ggplot(data=mpg,
       mapping=aes(x=displ, y=hwy)) +
  geom_point()

# Yes, it capture the intuitive relationship I expected.
# Bigger engine leads to higher consumption of gas

ggplot(data=mpg,
       mapping=aes(x=class, y=drv)) +
  geom_point()
# This scatterplot has no obvious meaning, because âdrvâ and âclassâ are discrete variables and can only take a few values, so it is not suitable to use scatterplot to mine information. You can consider using barplot


#EXERCISE 1b
ggplot(data=mpg,
       mapping=aes(x=displ, y=hwy)) +
  geom_point(aes(color=class))

# Conclusion: There are obvious differences between displ and hwy for different classes of cars:
# The most fuel-efficient models are subcompact and compact, and the most fuel-efficient models are suv and pickup
# 2seater's car has the largest displ on average, but the fuel consumption is not the highest (hwy is in the middle position), probably because this car is smaller and lighter


#EXERCISE 2
bank <- read_csv("bank.csv")

ggplot(data=bank,
       mapping=aes(x=job, fill=y)) +
  geom_bar(position="fill") +
  labs(title="ratio of subscribtion for consumers of different jobs",
       y="ratio") +
  coord_flip()

# The figure above shows the distribution of the population of different jobs on the value of y
# It can be seen that the three categories of people "retired" and "student" and "unknown" are the most likely to accept the product,
# Of which "retired" is the highest, "student" is the second highest, and "unknown" is the third highest
# The reason for this may be: Retired people generally have more spare money, and students may be unsteady and easily sold.
# Acceptance rates for the remaining groups are significantly lower than these three groups
# The lowest acceptance is "blue-collar", probably due to less free money

#VISUALIZATION 2
loanLabel = labeller(loan=c("no"="loan: no","yes"="loan: yes"))
ggplot(data=bank,
       mapping=aes(x=housing, fill=y)) +
  geom_bar(position="fill", width=0.5) +
  facet_grid(.~loan, labeller=loanLabel) +
  labs(title="ratio of subscribtion for different situation in housing and loan",
       y="ratio")

# The chart above shows the acceptance rate of bank products for people with and without their own homes, and those with and without loans.
# As can be seen from the figure:
# People with existing loans have a significantly lower acceptance rate than people without loans. This may be because the people with the loans have less spare money in their accounts.
# For people without loans, the acceptance rate of people who already have a house is lower than that of people who do not have a house, probably because they have less spare money after buying a house
# You can also see:
# Among those without loans, the effect of having a house on the acceptance rate is more obvious
# Among the people who carry loans, whether there is a house has no significant effect on the acceptance rate, and the ratio is almost the same
