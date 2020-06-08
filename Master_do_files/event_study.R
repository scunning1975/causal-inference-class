library(tidyverse)
library(lfe)
library(ggthemes)
library(fastDummies)
theme_set(theme_clean() + theme(plot.background = element_blank()))

set.seed(20200403)
## Generate data - 250 firms are treated every period, with the treatment effect still = 0.3 on average
make_data <- function(...) {
  
  # Fixed Effects ------------------------------------------------
  # unit fixed effects
  unit <- tibble(
    unit = 1:1000, 
    unit_fe = rnorm(1000, 0, 1),
    # generate state
    state = sample(1:40, 1000, replace = TRUE),
    # generate treatment effect
    mu = rnorm(1000, 0.3, 0.2))
  
  # year fixed effects 
  year <- tibble(
    year = 1980:2010,
    year_fe = rnorm(31, 0, 1))
  
  # Trend Break -------------------------------------------------------------
  # Put the states into treatment groups
  treat_taus <- tibble(
    # sample the states randomly
    state = sample(1:40, 40, replace = FALSE),
    # place the randomly sampled states into five treatment groups G_g
    cohort_year = sort(rep(c(1986, 1992, 1998, 2004), 10)))
  
  # make main dataset
  # full interaction of unit X year 
  expand_grid(unit = 1:1000, year = 1980:2010) %>% 
    left_join(., unit) %>% 
    left_join(., year) %>% 
    left_join(., treat_taus) %>% 
    # make error term and get treatment indicators and treatment effects
    mutate(error = rnorm(31000, 0, 0.5),
           treat = ifelse(year >= cohort_year, 1, 0),
           tau = ifelse(treat == 1, mu, 0)) %>% 
    # calculate cumulative treatment effects
    group_by(unit) %>% 
    mutate(tau_cum = cumsum(tau)) %>% 
    ungroup() %>% 
    # calculate the dep variable
    mutate(dep_var = unit_fe + year_fe + tau_cum + error)

}

# make data
data <- make_data()

# plot
plot <- data %>% 
  ggplot(aes(x = year, y = dep_var, group = unit)) + 
  geom_line(alpha = 1/8, color = "grey") + 
  geom_line(data = data %>% 
              group_by(cohort_year, year) %>% 
              summarize(dep_var = mean(dep_var)),
            aes(x = year, y = dep_var, group = factor(cohort_year),
                color = factor(cohort_year)),
            size = 2) + 
  labs(x = "", y = "Value") + 
  geom_vline(xintercept = 1986, color = '#E41A1C', size = 2) + 
  geom_vline(xintercept = 1992, color = '#377EB8', size = 2) + 
  geom_vline(xintercept = 1998, color = '#4DAF4A', size = 2) + 
  geom_vline(xintercept = 2004, color = '#984EA3', size = 2) + 
  scale_color_brewer(palette = 'Set1') + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))


plot
