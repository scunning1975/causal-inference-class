# load packages
library(tidyverse)
library(lfe)
library(ggthemes)
library(kableExtra)

# set the theme
theme_set(theme_clean() + theme(plot.background = element_blank()))
options(knitr.kable.NA = '')


# simulate data -----------------------------------------------------------

# set seed 
set.seed(74792766)

# Generate data - 250 firms are placed in each group. Groups 3 and 4 are 
# treated in 1998, Groups 1 and 2 are untreated

make_data <- function(...) {
  
  # Fixed Effects ------------------------------------------------
  # unit fixed effects
  unit <- tibble(
    unit = 1:1000, 
    unit_fe = rnorm(1000, 0, 3),
    # generate state
    state = sample(rep(1:40, 25), 1000, replace = FALSE),
    # generate treatment groups
    group = case_when(
      state %in% 1:10 ~ 1,
      state %in% 11:20 ~ 2,
      state %in% 21:30 ~ 3,
      state %in% 31:40 ~ 4
    ),
    # avg yearly treatment effects by group
    hat_gamma = case_when(
      group == 3 ~ 10,
      group == 4 ~ 6,
      TRUE ~ 0
    )) %>%
    # generate unit specific yearly treatment effects 
    rowwise() %>% 
    mutate(gamma = ifelse(group %in% 3:4, rnorm(1, hat_gamma, 5), 0))
  
  # year fixed effects 
  year <- tibble(
    year = 1981:2010,
    year_fe = rnorm(30, 0, 3))
  
  # Trend Break -------------------------------------------------------------
  # full interaction of unit X year 
  crossing(unit, year) %>% 
    # make error term and get treatment indicators and treatment effects
    mutate(error = rnorm(30000, 0, 3),
           treat = ifelse(group %in% 3:4 & year >= 1998, 1, 0),
           tau = ifelse(treat == 1, gamma, 0)) %>%
    # calculate cumulative treatment effects
    group_by(unit) %>% 
    mutate(tau_cum = cumsum(tau)) %>% 
    ungroup() %>% 
    # calculate the dep variable
    mutate(dep_var = unit_fe + year_fe + tau_cum + error)
  
}

# make data
data <- make_data()

# make the plot and export
plot1 <- data %>% 
  ggplot(aes(x = year, y = dep_var, group = unit)) +
  # unit specific lines
  geom_line(alpha = 1/8, color = "grey") + 
  # group specific averages
  geom_line(
    data = data %>% 
      group_by(group, year) %>% 
      summarize(dep_var = mean(dep_var)),
    aes(x = year, y = dep_var, group = factor(group),
        color = factor(group)),
    size = 2) + 
  labs(x = "", y = "Value", color = "Group") + 
  geom_vline(xintercept = 1998, size = 2) + 
  scale_color_brewer(palette = 'Set1') + 
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))

# save plot
ggsave(here::here("paper/graphics", "sim_plot_1.png"), plot1, dpi = 500,
       width = 11.6, height = 7.12)

# estimate the TWFE regression
sim1 <- felm(dep_var ~ treat | unit + year | 0 | 0, data = data)

# calculate the true treatment effect 
true_tau <- mean(c(
  # estimate for group 3
  sum((1998:2010 - 1998 + 1) * 10)/ length(1998:2010),
  # estimate for gorup 4
  sum((1998:2010 - 1998 + 1) * 6)/ length(1998:2010)
))

# make regression table and export 
sim1_table <- broom::tidy(sim1, conf.int = TRUE) %>% 
  # make confidence interval
  # reformat and export
  select(estimate, std.error) %>% 
  t() %>% as_tibble() %>% set_names(c("estimate")) %>% 
  # add in true values
  bind_cols(tibble(true = c(true_tau, as.numeric(NA)))) %>% 
  # export as kable
  kable(format = "latex", digits = 2, booktabs = T, align = 'c', escape = F,
        col.names = c("$\\widehat{\\delta}$", "$\\delta$"), 
        caption = "Estimated ATT using TWFE with dynamic treatment effects and constant timing",
        label = "sim1") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"),
                latex_options = "hold_position") %>% 
  add_header_above(c("TWFE Estimate" = 1, "True Value" = 1))

write_lines(sim1_table, here::here("paper", "sim1_table.tex"))


# Make treatment effect size table ----------------------------------------
te_size_table <- tribble(
  ~`Treatment Group`, ~`Treatment Year`, ~`Average Effect Size`,
  "$G_1$", 1986, 10,
  "$G_2$", 1992, 8,
  "$G_3$", 1998, 6,
  "$G_4$", 2004, 4
) %>% 
  kable(format = "latex", booktabs = T, align = 'c', escape = F,
        caption = "Average Effect Size By Treatment Group",
        label = "ef_size") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"),
                latex_options = "hold_position")

write_lines(te_size_table, here::here("paper", "ef_size_table.tex"))


# Second Simulation - Constant Treatment Effect But Staggering ------------
# Generate data - 250 firms are placed in each group. Groups 3 and 4 are 
# treated in 1998, Groups 1 and 2 are untreated

make_data <- function(...) {
  
  # Fixed Effects ------------------------------------------------
  # unit fixed effects
  unit <- tibble(
    unit = 1:1000, 
    unit_fe = rnorm(1000, 0, 3),
    # generate state
    state = sample(rep(1:40, 25), 1000, replace = FALSE),
    # generate treatment groups
    group = case_when(
      state %in% 1:10 ~ 1,
      state %in% 11:20 ~ 2,
      state %in% 21:30 ~ 3,
      state %in% 31:40 ~ 4
    ),
    # avg yearly treatment effects by group
    hat_gamma = case_when(
      group == 1 ~ 10,
      group == 2 ~ 8,
      group == 3 ~ 6,
      group == 4 ~ 4),
    # get treatment year by group
    treat_year = case_when(
      group == 1 ~ 1986,
      group == 2 ~ 1992,
      group == 3 ~ 1998, 
      group == 4 ~ 2004
    )) %>%
    # generate unit specific yearly treatment effects 
    rowwise() %>% 
    mutate(gamma = rnorm(1, hat_gamma, 3), 0)
  
  # year fixed effects 
  year <- tibble(
    year = 1981:2010,
    year_fe = rnorm(30, 0, 3))
  
  # Trend Break -------------------------------------------------------------
  # full interaction of unit X year 
  crossing(unit, year) %>% 
    # make error term and get treatment indicators and treatment effects
    mutate(error = rnorm(30000, 0, 3),
           treat = ifelse(year >= treat_year, 1, 0),
           tau = ifelse(treat == 1, gamma, 0)) %>%
    # calculate the dep variable
    mutate(dep_var = unit_fe + year_fe + tau + error)
}

# make data
data <- make_data()

# make the plot and export
plot2 <- data %>% 
  ggplot(aes(x = year, y = dep_var, group = unit)) +
  # unit specific lines
  geom_line(alpha = 1/8, color = "grey") + 
  # group specific averages
  geom_line(
    data = data %>% 
      group_by(group, year) %>% 
      summarize(dep_var = mean(dep_var)),
    aes(x = year, y = dep_var, group = factor(group),
        color = factor(group)),
    size = 2) + 
  labs(x = "", y = "Value", color = "Group") + 
  geom_vline(xintercept = 1986, color = '#E41A1C', size = 2) + 
  geom_vline(xintercept = 1992, color = '#377EB8', size = 2) + 
  geom_vline(xintercept = 1998, color = '#4DAF4A', size = 2) + 
  geom_vline(xintercept = 2004, color = '#984EA3', size = 2) + 
  scale_color_brewer(palette = 'Set1') + 
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))

# save plot
ggsave(here::here("paper/graphics", "sim_plot_2.png"), plot2, dpi = 500,
       width = 11.6, height = 7.12)

# estimate the TWFE regression
sim2 <- felm(dep_var ~ treat | unit + year | 0 | state, data = data)

# calculate the true treatment effect 
unweighted_true_tau <- mean(c(10, 8, 6, 4))
weighted_true_tau <- weighted.mean(
  x = c(10, 8, 6, 4),
  w = c(
    var(c(rep(0, length(1981:1985)), rep(1, length(1986:2010)))),
    var(c(rep(0, length(1981:1991)), rep(1, length(1992:2010)))),
    var(c(rep(0, length(1981:1997)), rep(1, length(1998:2010)))),
    var(c(rep(0, length(1981:2003)), rep(1, length(2004:2010))))))

# make regression table and export 
sim2_table <- broom::tidy(sim2, conf.int = TRUE) %>% 
  # make confidence interval
  # reformat and export
  select(estimate, std.error) %>% 
  t() %>% as_tibble() %>% set_names(c("estimate")) %>% 
  # add in true values
  bind_cols(tibble(true_uw = c(unweighted_true_tau, as.numeric(NA)),
                   true_w = c(weighted_true_tau, as.numeric(NA)))) %>% 
  # export as kable
  kable(format = "latex", digits = 2, booktabs = T, align = 'c', escape = F,
        col.names = c("$\\widehat{\\delta}$", "$\\delta$", "$\\delta$"), 
        caption = "Estimated ATT using TWFE with constant treatment effects and staggered timing",
        label = "sim2") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"),
                latex_options = "hold_position") %>% 
  add_header_above(c("TWFE Estimate" = 1, "True Value (Unweighted)" = 1,
                     "True Value (Weighted)"))

write_lines(sim2_table, here::here("paper", "sim2_table.tex"))


# Simulation 3 - Dynamics + Treatment Effect Heterogeneity and Staggering --------

# function to make data
make_data <- function(...) {
  
  # Fixed Effects ------------------------------------------------
  # unit fixed effects
  unit <- tibble(
    unit = 1:1000, 
    unit_fe = rnorm(1000, 0, 3),
    # generate state
    state = sample(rep(1:40, 25), 1000, replace = FALSE),
    # generate treatment groups
    group = case_when(
      state %in% 1:10 ~ 1,
      state %in% 11:20 ~ 2,
      state %in% 21:30 ~ 3,
      state %in% 31:40 ~ 4
    ),
    # avg yearly treatment effects by group
    hat_gamma = case_when(
      group == 1 ~ 10,
      group == 2 ~ 8,
      group == 3 ~ 6,
      group == 4 ~ 4),
    # get treatment year by group
    treat_year = case_when(
      group == 1 ~ 1986,
      group == 2 ~ 1992,
      group == 3 ~ 1998, 
      group == 4 ~ 2004
    )) %>%
    # generate unit specific yearly treatment effects 
    rowwise() %>% 
    mutate(gamma = rnorm(1, hat_gamma, 3), 0)
  
  # year fixed effects 
  year <- tibble(
    year = 1981:2010,
    year_fe = rnorm(30, 0, 3))
  
  # Trend Break -------------------------------------------------------------
  # full interaction of unit X year 
  crossing(unit, year) %>% 
    # make error term and get treatment indicators and treatment effects
    mutate(error = rnorm(30000, 0, 3),
           treat = ifelse(year >= treat_year, 1, 0),
           tau = ifelse(treat == 1, gamma, 0)) %>%
    # calculate cumulative treatment effects
    group_by(unit) %>% 
    mutate(tau_cum = cumsum(tau)) %>% 
    ungroup() %>% 
    # calculate the dep variable
    mutate(dep_var = unit_fe + year_fe + tau_cum + error)
  
}

# make data
data <- make_data()

# make the plot and export
plot3 <- data %>% 
  ggplot(aes(x = year, y = dep_var, group = unit)) +
  # unit specific lines
  geom_line(alpha = 1/8, color = "grey") + 
  # group specific averages
  geom_line(
    data = data %>% 
      group_by(group, year) %>% 
      summarize(dep_var = mean(dep_var)),
    aes(x = year, y = dep_var, group = factor(group),
        color = factor(group)),
    size = 2) + 
  labs(x = "", y = "Value", color = "Group") + 
  geom_vline(xintercept = 1986, color = '#E41A1C', size = 2) + 
  geom_vline(xintercept = 1992, color = '#377EB8', size = 2) + 
  geom_vline(xintercept = 1998, color = '#4DAF4A', size = 2) + 
  geom_vline(xintercept = 2004, color = '#984EA3', size = 2) + 
  scale_color_brewer(palette = 'Set1') + 
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))

# save plot
ggsave(here::here("paper/graphics", "sim_plot_3.png"), plot3, dpi = 500,
       width = 11.6, height = 7.12)

# estimate the TWFE regression
sim3 <- felm(dep_var ~ treat | unit + year | 0 | state, data = data)

# calculate the true treatment effect 
unweighted_true_tau <- mean(c(
  # estimate for group 1
  sum((1986:2010 - 1986 + 1) * 10)/ length(1986:2010),
  # estimate for group 2
  sum((1992:2010 - 1992 + 1) * 10)/ length(1992:2010),
  # estimate for group 3
  sum((1998:2010 - 1998 + 1) * 10)/ length(1998:2010),
  # estimate for group 4
  sum((2004:2010 - 2004 + 1) * 10)/ length(2004:2010)))

weighted_true_tau <- weighted.mean(
  x = c(
    sum((1986:2010 - 1986 + 1) * 10)/ length(1986:2010),
    # estimate for group 2
    sum((1992:2010 - 1992 + 1) * 10)/ length(1992:2010),
    # estimate for group 3
    sum((1998:2010 - 1998 + 1) * 10)/ length(1998:2010),
    # estimate for group 4
    sum((2004:2010 - 2004 + 1) * 10)/ length(2004:2010)),
  w = c(
    var(c(rep(0, length(1981:1985)), rep(1, length(1986:2010)))),
    var(c(rep(0, length(1981:1991)), rep(1, length(1992:2010)))),
    var(c(rep(0, length(1981:1997)), rep(1, length(1998:2010)))),
    var(c(rep(0, length(1981:2003)), rep(1, length(2004:2010))))))

# make regression table and export 
sim3_table <- broom::tidy(sim3, conf.int = TRUE) %>% 
  # make confidence interval
  # reformat and export
  select(estimate, std.error) %>% 
  t() %>% as_tibble() %>% set_names(c("estimate")) %>% 
  # add in true values
  bind_cols(tibble(true_uw = c(unweighted_true_tau, as.numeric(NA)),
                   true_w = c(weighted_true_tau, as.numeric(NA)))) %>% 
  # export as kable
  kable(format = "latex", digits = 2, booktabs = T, align = 'c', escape = F,
        col.names = c("$\\widehat{\\delta}$", "$\\delta$", "$\\delta$"), 
        caption = "Estimated ATT using TWFE with dynamic treatment effects and staggered timing",
        label = "sim3") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"),
                latex_options = "hold_position") %>% 
  add_header_above(c("TWFE Estimate" = 1, "True Value (Unweighted)" = 1,
                     "True Value (Weighted)"))

write_lines(sim3_table, here::here("paper", "sim3_table.tex"))

