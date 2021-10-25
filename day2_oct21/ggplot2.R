library(ggplot2)
library(dplyr)
data(mpg)
names(mpg)
head(mpg)

mpg <- mpg %>% rename(man = manufacturer)
names(mpg)


# Geoms

## histogram

ggplot(data = mpg, mapping = aes(x = hwy)) +
  geom_histogram(binwidth = 5)

mpg %>% filter(man == "audi") %>%
  ggplot(aes(x=hwy)) + geom_histogram(binwidth = 1)

mpg %>% filter(man == "audi") %>%
  ggplot(aes(x=log(hwy))) + geom_histogram(binwidth = .01)

## barplot

ggplot(mpg) + geom_bar(mapping = aes(x = man))

ggplot(mpg) + geom_bar(mapping = aes(x = man)) +
  coord_flip()

mpg %>%
  mutate(low_mpg = ifelse(cty < quantile(cty, .2), "yes", "no")) %>%
  ggplot() + geom_bar(aes(x=low_mpg), fill = "blue")


mpg2 <- mpg %>%
  mutate(low_mpg = ifelse(cty < quantile(cty, .2), "yes", "no"))


# geom: points
ggplot(mpg, aes(x=cty, y=hwy)) + geom_point()

ggplot(mpg, aes(x=cty, y=hwy)) + geom_point(aes(color = cyl))

ggplot(mpg, aes(x=cty, y=hwy)) + geom_point(aes(color = "red"))

ggplot(mpg, aes(x=cty, y=hwy)) + 
  geom_point(aes(color = cyl, shape = trans)) +
  labs(color = "Cylinder", shape = "Transmission")


ggplot(mpg, aes(x=cty, y=hwy)) + 
  geom_point(aes(size = desc(year)))

ggplot(mpg, aes(x=cty, y=hwy)) + 
  geom_point(aes(size = desc(year)), alpha = 1/10)


ggplot(mpg, aes(x=year, y=hwy)) + 
  geom_line(aes(group = model), linetype = "dotdash")

ggplot(mpg, aes(x=year, y=hwy)) + 
  geom_line(aes(linetype = model))

mpg %>% group_by(model) %>% mutate(mu_hwy = mean(hwy)) %>%
  ggplot(aes(x = year, y = mu_hwy)) +
    geom_line(aes(linetype = model))


ggplot(mpg) +
  geom_boxplot(aes(x = as.factor(cyl), y = cty)) +
  coord_flip()


data.frame(v1 = rnorm(1000)) %>%
  mutate(v2 = log(v1) + rnorm(1000)) %>%
  ggplot(aes(x = v1, y = v2)) + 
  geom_bin2d()


## Regression results

g1 <- ggplot(mpg, aes(cty)) +
  geom_histogram(aes(y = ..density..), color = "blue", fill = "white")

g1

g2 <- g1 + geom_density(alpha = .2, fill = "orchid") +
  geom_vline(xintercept = mean(mpg$cty))
g2


ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point() + geom_smooth(se = FALSE) +
  labs(title = "Adding a loess smoother", x = "displacement?")

ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point() + geom_smooth(span = .3) +
  labs(title = "Adding a wiggly loess smoother", x = "displacement?")


ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point() + geom_smooth(method = lm) +
  labs(title = "Adding OLS", x = "displacement?")


ggplot(mpg, aes(x = displ, y = cty, color = as.factor(cyl))) +
  geom_point() + geom_smooth(method = lm) +
  labs(title = "Adding OLS", x = "displacement?")


ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point() + geom_smooth(method = lm) +
  facet_wrap(~cyl) + 
  labs(title = "Adding OLS", x = "displacement?")


mod1 <- lm(cty ~ displ + as.factor(cyl), data = mpg)
summary(mod1)

fit1 <- predict(mod1, interval = "confidence", level = 0.90)
fit1
fit1.full <- cbind(mpg, fit1)


colnames(predict(mod1, interval = "confidence", level = 0.90))

ggplot(fit1.full, aes(x = displ, y = cty, color = as.factor(cyl))) +
         geom_point() + geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = as.factor(cyl)), alpha = .2)






















