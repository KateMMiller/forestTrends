library(tidyverse)
library(forestTrends)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

treeBA <- read.csv("D:/NETN/R_Dev/NPSForVeg_Data/results/tree_BA_trend_results.csv")

plot_trend_response(treeBA, xlab = "Cycle", ylab = "BA", group = "Unit_Code") +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))+
  geom_hline(yintercept = rep(30, 3*length(unique(treeBA$Unit_Code))))

head(treeBA)

datapath <- "D:/NETN/R_Dev/NPSForVeg_Data/"
dens_df <- read.csv(paste0(datapath, "EFWG_tree_sapling_seedling_BA_dens.csv"))

dens_park <- dens_df %>% mutate(park = Unit_Code) %>% group_by(Unit_Code) %>% nest()
#added park b/c Unit_Code gets dropped from data


# SAHI AND WOTR AREN"T PLOTTING BUT SHOULD.

treeBA_by_park <- dens_park %>%
  mutate(model = map(data, ~case_boot_lmer(., x = "cycle", y = "Tree_BA_Total", ID = "Plot_Name",
                                           random_type = 'intercept', group = "park",
                                           num_reps = 10, chatty = TRUE)))

treeBA_results <- treeBA_by_park %>% select(Unit_Code, model) %>% unnest(model) %>%
  select(-num_boots)

head(treeBA_results)

plot_trend_response(treeBA_results, xlab = "Cycle", ylab = "BA", group = "Unit_Code") +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))



fake_df <- data.frame(Plot_Name = rep(paste0(rep("PARK.", 9), 1:9), each = 3),
                      park = "PARK",
                      cycle = rep(1:3, times = 9),
                      resp = runif(27, 0, 20))
fake_df

case_boot_sample(fake_df, x = "cycle", y = "resp", ID = "Plot_Name", random_type = "intercept")
resp_df <- case_boot_lmer(fake_df, x = "cycle", y = "resp", ID = "Plot_Name", random_type = "intercept", num_reps = 4)

?case_boot_lmer

fake_sm <- data.frame(Plot_Name = rep(paste0(rep("PARK.", 5), 1:5), each = 3),
                      park = "PARK",
                      cycle = rep(1:3, times = 5),
                      resp = runif(15, 0, 20))


trend_fun(fake_sm, x = 'cycle', y = 'resp', ID = "Plot_Name", random_type = "intercept")
case_boot_sample(fake_sm, x = "cycle", y = "resp", ID = "Plot_Name", random_type = "intercept")

case_boot_lmer(fake_sm, x = "cycle", y = "resp", ID = "Plot_Name", random_type = "intercept", num_reps = 10)
case_boot_lmer(fake_df, x = "cycle", y = "resp", ID = "Plot_Name", random_type = "intercept", num_reps = 10)

resp_df <- case_boot_lmer(fake_df, x = "cycle", y = "resp", ID = "Plot_Name",
                          random_type = "intercept", num_reps = 10)

resp_df <- case_boot_lmer(fake_df, x = "cycle", y = "resp", ID = "Plot_Name", #group = "park",
                          random_type = "intercept", num_reps = 10)


df <- resp_df

plot_trend_response(resp_df, xlab = 'cycle', ylab = 'resp')
plot_trend_response(resp_df, xlab = 'cycle', ylab = 'resp')

# 2 parks but still simple
fake_2pk <- data.frame(Plot_Name = c(rep(paste0(rep("APRK-", 12), sprintf("%02d", 1:12)), each = 3),
                                     rep(paste0(rep("BPRK-", 12), sprintf("%02d", 13:24)), each = 3)),
                       group = c(rep("APRK", 36), rep("BPRK", 36)),
                       cycle = rep(1:3, times = 24),
                       resp = runif(72, 0, 30))
fake_2pk
# data frame that uses year instead of cycle and has a 20% increase in BPRK
year_df_2pk <- data.frame(Plot_Name = c(rep(paste0(rep("APRK-", 12), sprintf("%02d", 1:12)), each = 4),
                                        rep(paste0(rep("APRK-", 12), sprintf("%02d", 13:24)), each = 3),
                                        rep(paste0(rep("BPRK-", 24), sprintf("%02d", 25:48)), each = 3)),
                          group = c(rep("APRK", 84), rep("BPRK", 72)),
                          year = c(rep(c(2006, 2010, 2014, 2018), times = 12),
                                   rep(c(2008, 2012, 2016), times = 12),
                                   rep(c(2007, 2011, 2015), times = 12),
                                   rep(c(2009, 2013, 2017), times = 12)),
                          resp = runif(156, 0, 10))
table(year_df_2pk$group)
year_df_2pk$err <- rnorm(nrow(year_df_2pk), 0, 2)
year_df_2pk$resp_trend <- ifelse(year_df_2pk$group %in% "APRK",
                                 year_df_2pk$resp + (year_df_2pk$year-2006) * 0.5 + year_df_2pk$err,
                                 year_df_2pk$resp)

year_df <- data.frame(Plot_Name = c(rep(paste0(rep("APRK-", 12), sprintf("%02d", 1:12)), each = 4),
                                    rep(paste0(rep("APRK-", 12), sprintf("%02d", 13:24)), each = 3)),
                      group = c(rep("APRK", 84)),
                      year = c(rep(c(2006, 2010, 2014, 2019), times = 12), # used 2019 instead of 2018
                               rep(c(2008, 2012, 2016), times = 12)),
                      resp = runif(84, 0, 10))

year_df$resp_trend <- year_df$resp + (year_df$year-2006) * 0.5 + rnorm(84, 0, 1)

trend_fun(year_df, x = "year", y = "resp", ID = "Plot_Name",  random_type = 'intercept')
trend_fun(year_df, x = "year", y = "resp_trend", ID = "Plot_Name", random_type = 'intercept')

case_boot_sample(year_df, x = "year", y = "resp", ID = "Plot_Name", random_type = 'intercept')
case_boot_sample(year_df, x = "year", y = "resp_trend", ID = "Plot_Name", random_type = 'intercept')

case_boot_lmer(year_df, x = "year", y = "resp", ID = "Plot_Name", random_type = "intercept", num_reps = 10)
case_boot_lmer(year_df, x = "year", y = "resp_trend", ID = "Plot_Name", random_type = "intercept", num_reps = 10)

trend_fun(year_df, x = "year", y = "resp", ID = "Plot_Name", random_type = 'slope')
trend_fun(year_df, x = "year", y = "resp_trend", ID = "Plot_Name", random_type = 'slope')

case_boot_sample(year_df, x = "year", y = "resp", ID = "Plot_Name", random_type = 'slope')
case_boot_sample(year_df, x = "year", y = "resp_trend", ID = "Plot_Name", random_type = 'slope')

case_boot_lmer(year_df, x = "year", y = "resp", ID = "Plot_Name", random_type = "slope", group = 'group', num_reps = 10)
case_boot_lmer(year_df, x = "year", y = "resp_trend", ID = "Plot_Name", random_type = "intercept", group = 'group', num_reps = 10)

df <- case_boot_lmer(year_df, x = "year", y = "resp_trend", ID = "Plot_Name", random_type = "intercept", group = 'group', num_reps = 10)

plot_trend_response(df, xlab = "year", ylab = "resp")

sing_df <- data.frame(Plot_Name = rep(paste0(rep("PARK.", 5), 1:5), each = 3),
                      cycle = rep(1:3, times = 5),
                      resp = 1 + runif(15, 0, 0.1))

fail_df <- data.frame(Plot_Name = rep(paste0(rep("PARK.", 9), 1:9), each = 3),
                      cycle = rep(1:3, times = 9),
                      resp = 0)

trend_fun(fake_df, y = "resp", random_type = 'intercept')
trend_fun(fake_df, y = "resp", random_type = 'slope')
trend_fun(sing_df, y = "resp", random_type = 'intercept')
trend_fun(sing_df, y = "resp", random_type = 'slope')
trend_fun(fail_df, y = "resp", random_type = 'intercept')
trend_fun(fail_df, y = "resp", random_type = 'slope')

case_boot_sample(fake_df, y = "resp", random_type = 'intercept')
case_boot_sample(fake_df, y = "resp", random_type = 'slope')
case_boot_sample(sing_df, y = "resp", random_type = 'intercept')
case_boot_sample(sing_df, y = "resp", random_type = 'slope')
case_boot_sample(fail_df, y = "resp", random_type = 'intercept')
case_boot_sample(fail_df, y = "resp", random_type = 'slope')

case_boot_lmer(fake_df, y = "resp", random_type = 'intercept', num_reps = 1000)
case_boot_lmer(fake_df, y = "resp", random_type = 'slope', num_reps = 100)
case_boot_lmer(sing_df, y = "resp", random_type = 'intercept', num_reps = 100)
case_boot_lmer(sing_df, y = "resp", random_type = 'slope', num_reps = 100)
case_boot_lmer(fail_df, y = "resp", random_type = 'intercept', num_reps = 10)
case_boot_lmer(fail_df, y = "resp", random_type = 'slope', num_reps = 10)

boot1 <- case_boot_lmer(fake_df, y = "resp", random_type = 'intercept', num_reps = 10)

plot_trend_response(boot1, "test")

