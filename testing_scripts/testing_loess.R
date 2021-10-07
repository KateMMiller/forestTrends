library(tidyverse)
library(forestTrends)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

datapath <- "D:/NETN/R_Dev/NPSForVeg_Data/"
dens_df <- read.csv(paste0(datapath, "EFWG_tree_sapling_seedling_BA_dens.csv"))
head(dens_df)

vafo <- dens_df %>% filter(Unit_Code == "VAFO")

#----- Run case_boot_loess on nested dataset -----
netn <- dens_df %>% arrange(Network, Plot_Name, Year) %>% filter(Network == "NETN") %>%
  mutate(grp = Unit_Code) %>% group_by(Unit_Code) %>% nest()

names(dens_df)

span_netn <- 8/diff(range(dens_df %>% filter(Network == "NETN") %>% select(Year)))
span_netn #0.7273

boot_netn <- netn %>% mutate(
  model = map(data, ~case_boot_loess(., x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name",
                                    span = 0.7273, group = "grp",
                                    num_reps = 1000, chatty = TRUE)))

netn_results <- boot_netn %>% select(Unit_Code, model) %>% unnest(model)
head(netn_results)

plot_trend_response(netn_results, xlab = "Year", ylab = "Native Canopy Seedling Density per m2",
                    model_type = 'loess', ribbon = TRUE, group = 'Unit_Code',
                    sign_color = c("white", "dimgrey", "forestgreen", "indianred"), facet_scales = "fixed") +
  scale_x_continuous(breaks = c(2008, 2012, 2016, 2020), labels = c("2008", "2012", "2016", "2020"))

boot_lmer <- netn %>% mutate(
  model = map(data, ~case_boot_lmer(., x = "cycle", y = "Seed_Dens_NatCan", ID = "Plot_Name",
                                     group = "grp", random_type = 'intercept',
                                     num_reps = 100, chatty = TRUE)))

lmer_results <- boot_lmer %>% select(Unit_Code, model) %>% unnest(model)

plot_trend_response(lmer_results, xlab = "Year", ylab = "Native Canopy Seedling Density per m2",
                    model_type = 'lmer', ribbon = TRUE, group = 'Unit_Code',
                    sign_color = c("white", "dimgrey", "forestgreen", "indianred")) +
  scale_x_continuous(breaks = c(2008, 2012, 2016, 2020), labels = c("2008", "2012", "2016", "2020"))


#---------------------------------------------------
# --- all parks ----
nest <- dens_df %>% arrange(Network, Plot_Name, Year) %>%
  mutate(grp = Unit_Code) %>% group_by(Unit_Code) %>% nest()

boot_all <- nest %>% mutate(
  model = map(data, ~case_boot_loess(., x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name",
                                     span = 0.7273, group = "grp", #degree = 2,
                                     num_reps = 100, chatty = TRUE)))

boot_results <- boot_all %>% select(Unit_Code, model) %>% unnest(model) #%>% na.omit()

plot_trend_response(boot_results, xlab = "Year", ylab = "Native Canopy Seedling Density per m2",
                    model_type = 'loess', ribbon = TRUE, group = 'Unit_Code', facet_scales = "free_y",
                    sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C")) +
  scale_x_continuous(breaks = c(2008, 2012, 2016, 2020), labels = c("2008", "2012", "2016", "2020"))


boot_netn

span_auto <- trend_loess(vafo, x = "cycle", y = "Seed_Dens_NatCan", ID = "Plot_Name")
span_auto

span <- 8/diff(range(vafo$Year))
span

span_c <- trend_loess(vafo, x = "cycle", y = "Seed_Dens_NatCan", ID = "Plot_Name", span = 0.3636)

trend_loess(vafo, x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name", span = 0.3636)

case_boot_sample(vafo, x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name", model_type = 'loess', span = 0.3636)

case_boot_loess(vafo, x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name",
                span = 0.3636, num_reps = 100, chatty = T)

lmer_check <- case_boot_lmer(vafo, x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name",
               num_reps = 10)

plot_trend_response(lmer_check, xlab = "year", ylab = "test", model_type = "lmer", ribbon = T)

test <- case_boot_loess(vafo, x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name",
                span = 0.7273, degree = 1,
                num_reps = 1000, chatty = T)

plot_trend_response(test, xlab = 'test', ylab = 'anv', model_type = 'loess', ribbon = F,
                    sign_color = c('grey', 'black', 'forestgreen', 'indianred'))


df <- test

ggplot(test, aes(x = Year, y = estimate))+
  geom_point(size = 2, shape = 21, fill = "blue")+
  geom_line(data = test, aes(x = Year, y = estimate), col = "blue", size = 1) +
  forestNETN::theme_FHM()+
  geom_ribbon(data = test, aes(ymin = lower95, ymax = upper95), alpha = 0.1) #+


testc <- case_boot_loess(vafo, x = "cycle", y = "Seed_Dens_NatCan", ID = "Plot_Name",
                        #span = 0.95,
                        num_reps = 1000, chatty = T)

testc

plot_trend_response(test, xlab = 'cycle', ylab = 'test', model_type = 'loess',
                    ribbon = TRUE, sign_color = c("green", "orange", "blue"))


ggplot(testc, aes(x = cycle, y = estimate))+
  geom_point(size = 2, shape = 21, fill = "blue")+
  geom_line(data = testc, aes(x = cycle, y = estimate), col = "blue", size = 1) +
  forestNETN::theme_FHM()+
  geom_ribbon(data = testc, aes(ymin = lower95, ymax = upper95), alpha = 0.1) #+
  # geom_point(data = vafo, aes(x = cycle, y = Seed_Dens_NatCan), size = 2, shape = 21, fill = "#CACACA") +
  # geom_line(data = vafo, aes(x = cycle, y = Seed_Dens_NatCan, group = Plot_Name), alpha = 0.1)


df <- dens_df %>% filter(Unit_Code == "MORR")

trend_loess(df, x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name", span = 4)

case_boot_sample(df, x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name", model_type = 'loess', span = 4)

case_boot_loess(df, x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name",
                span = 4, num_reps = 100, chatty = T)

# case_boot_lmer(df, x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name",
#                num_reps = 10)

test <- case_boot_loess(df, x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name",
                        span = 4, num_reps = 100, chatty = T)
test
df
View(df)
comb <- left_join(df, test, by = "Year")

ggplot(test, aes(x = Year, y = estimate))+
  geom_point(size = 2, shape = 21, fill = "blue")+
  geom_line(data = test, aes(x = Year, y = estimate), col = "blue", size = 1) +
  forestNETN::theme_FHM()+
  geom_ribbon(data = test, aes(ymin = lower95, ymax = upper95), alpha = 0.1)+
  geom_point(data = df, aes(x = Year, y = Seed_Dens_NatCan), size = 2, shape = 21, fill = "#CACACA") +
  geom_line(data = df, aes(x = Year, y = Seed_Dens_NatCan, group = Plot_Name), alpha = 0.1)



testc <- case_boot_loess(df, x = "cycle", y = "Seed_Dens_NatCan", ID = "Plot_Name", span = 1,
                         num_reps = 100, chatty = T)
testc

trend_loess(df, x = "cycle", y = "Seed_Dens_NatCan", ID = "Plot_Name")

ggplot(testc, aes(x = cycle, y = estimate))+
  geom_point(size = 2, shape = 21, fill = "blue")+
  geom_line(data = testc, aes(x = cycle, y = estimate), col = "blue", size = 1) +
  forestNETN::theme_FHM()+
  geom_ribbon(data = testc, aes(ymin = lower95, ymax = upper95), alpha = 0.1)+
  geom_point(data = df, aes(x = cycle, y = Seed_Dens_NatCan), size = 2, shape = 21, fill = "#CACACA") +
  geom_line(data = df, aes(x = cycle, y = Seed_Dens_NatCan, group = Plot_Name), alpha = 0.1)


test

ggplot(test, aes(x = Year, y = estimate))+
  geom_line(data = test, aes(x = Year, y = estimate), col = "blue", size = 1) +
  forestNETN::theme_FHM()+
  geom_ribbon(data = test, aes(ymin = lower95, ymax = upper95), alpha = 0.1)+
  geom_point(data = vafo, aes(x = Year, y = Seed_Dens_NatCan), size = 2, shape = 21, fill = "#CACACA") +
  geom_line(data = vafo, aes(x = Year, y = Seed_Dens_NatCan, group = Plot_Name), alpha = 0.1)

testC <- case_boot_loess(vafo, x = "cycle", y = "Seed_Dens_NatCan", ID = "Plot_Name",
                         num_reps = 100, chatty = T) %>% arrange(cycle)

testC

ggplot(testC, aes(x = cycle, y = estimate))+
  geom_line(data = testC, aes(x = cycle, y = estimate), col = "blue", size = 1) +
  forestNETN::theme_FHM()+
  geom_ribbon(data = testC, aes(ymin = lower95, ymax = upper95), alpha = 0.1)+
  geom_point(data = vafo, aes(x = cycle, y = Seed_Dens_NatCan), size = 2, shape = 21, fill = "#CACACA") +
  geom_line(data = vafo, aes(x = cycle, y = Seed_Dens_NatCan, group = Plot_Name), alpha = 0.1)


names(vafo)
span_4

plot(vafo$Seed_Dens_NatCan ~ vafo$Year)
lines(span_4$Year, span_4$estimate, col = 'blue')

test <- case_boot_sample(vafo, x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name",
                         model_type = "loess", span = 4, sample = TRUE, sample_num = 1)
test

test <- case_boot_sample(vafo, x = "Year", y = "Seed_Dens_NatCan", ID = "Plot_Name",
                         model_type = "lmer", span = 4, sample = TRUE, sample_num = 1)



sp200 <- loess(Seed_Dens_NatCan ~ cycle, data = vafo, span = 2)
sp100 <- loess(Seed_Dens_NatCan ~ cycle, data = vafo, span = 1)
sp90 <- loess(Seed_Dens_NatCan ~ cycle, data = vafo, span = 0.9)
sp75 <- loess(Seed_Dens_NatCan ~ cycle, data = vafo, span = 0.75)
sp50 <- loess(Seed_Dens_NatCan ~ cycle, data = vafo, span = 0.5)
sp10 <- loess(Seed_Dens_NatCan ~ cycle, data = vafo, span = 0.1)

plot(Seed_Dens_NatCan ~ cycle, data = vafo)
j <- order(vafo$cycle)
lines(vafo$cycle[j], sp200$fitted[j], col = 'blue')
lines(vafo$cycle[j], sp100$fitted[j], col = 'black')
lines(vafo$cycle[j], sp90$fitted[j], col = 'purple')
lines(vafo$cycle[j], sp75$fitted[j], col = 'green')
lines(vafo$cycle[j], sp50$fitted[j], col = 'red')
lines(vafo$cycle[j], sp10$fitted[j], col = 'blue')

sp200y <- loess(Seed_Dens_NatCan ~ Year, data = vafo, span = 2)
sp100y <- loess(Seed_Dens_NatCan ~ Year, data = vafo, span = 1)
sp90y <- loess(Seed_Dens_NatCan ~ Year, data = vafo, span = 0.9)
sp75y <- loess(Seed_Dens_NatCan ~ Year, data = vafo, span = 0.75)
sp50y <- loess(Seed_Dens_NatCan ~ Year, data = vafo, span = 0.5)
sp10y <- loess(Seed_Dens_NatCan ~ Year, data = vafo, span = 0.1)

plot(Seed_Dens_NatCan ~ Year, data = vafo)
jy <- order(vafo$Year)
lines(vafo$Year[jy], sp200y$fitted[jy], col = 'blue')
lines(vafo$Year[jy], sp100y$fitted[jy], col = 'black')
lines(vafo$Year[jy], sp90y$fitted[jy], col = 'purple')
lines(vafo$Year[jy], sp75y$fitted[jy], col = 'green')
lines(vafo$Year[jy], sp50y$fitted[jy], col = 'red')
lines(vafo$Year[jy], sp10y$fitted[jy], col = 'blue')

plot(Seed_Dens_NatCan ~ Year, data = vafo)
lines(vafo$Year[jy], sp200y$fitted[jy], col = 'blue')
lines(vafo$Year[jy], sp100y$fitted[jy], col = 'black')
lines(vafo$Year[jy], sp90y$fitted[jy], col = 'purple')
lines(vafo$Year[jy], sp75y$fitted[jy], col = 'green')


sp4y <- loess(Seed_Dens_NatCan ~ Year, data = vafo, span = 4)
plot(Seed_Dens_NatCan ~ Year, data = vafo)
lines(vafo$Year[jy], sp4y$fitted[jy], col = 'red')

sp4 <- loess(Seed_Dens_NatCan ~ cycle, data = vafo, span = 4)
plot(Seed_Dens_NatCan ~ cycle, data = vafo)
lines(vafo$cycle[j], sp4$fitted[j], col = 'red')


sap4y <- loess(Sap_Dens_NatCan ~ Year, data = vafo, span = 4)
plot(Sap_Dens_NatCan ~ Year, data = vafo)
lines(vafo$Year[j], sap4y$fitted[j], col = 'red')

sap4 <- loess(Sap_Dens_NatCan ~ cycle, data = vafo, span = 4)
plot(Sap_Dens_NatCan ~ cycle, data = vafo)
lines(vafo$cycle[j], sap4$fitted[j], col = 'red')

plsm4y <- loess(tree_polesm_Dens_NatCan ~ Year, data = vafo, span = 4)
plot(tree_polesm_Dens_NatCan ~ Year, data = vafo)
lines(vafo$Year[j], plsm4y$fitted[j], col = 'red')

plsm4 <- loess(tree_polesm_Dens_NatCan ~ cycle, data = vafo, span = 4)
plot(tree_polesm_Dens_NatCan ~ cycle, data = vafo)
lines(vafo$cycle[j], plsm4$fitted[j], col = 'red')

library(fANCOVA)

test <- (loess.as(vafo$Seed_Dens_NatCan, vafo$cycle, degree = 1,
                 criterion = c("aicc", "gcv"), user.span = NULL, plot = T))$pars$span
test
test$pars$span
test <- loess(Seed_Dens_NatCan ~ cycle, data = vafo, span = 0.6467)
plot(Seed_Dens_NatCan ~ cycle, data = vafo)
j <- order(vafo$cycle)
lines(vafo$cycle[j], test$fitted[j], col = 'red')



test <- loess.as(vafo$Seed_Dens_NatCan, vafo$Year, degree = 1, criterion = c("gcv"), user.span = NULL, plot = F)
span <- test$pars$span #0.6205

test_auto <- loess(Seed_Dens_NatCan ~ Year, data = vafo, span = span)
plot(Seed_Dens_NatCan ~ Year, data = vafo)
j <- order(vafo$Year)
lines(vafo$Year[j], test_auto$fitted[j], col = 'red')

test <- loess(Seed_Dens_NatCan ~ Year, data = vafo, span = 4)
plot(Seed_Dens_NatCan ~ Year, data = vafo)
j <- order(vafo$Year)
lines(vafo$Year[j], test$fitted[j], col = 'red')
test_pred <- data.frame(y = predict(test, newdata = sort(unique(vafo$Year))), year = sort(unique(vafo$Year)))
test_pred
lines(test_pred$year, test_pred$y, col = 'blue')
