setwd("..")
library(kSamples)
# Load the data
master <- read.csv('Data/master.csv')
master$type <- as.factor(master$type)

top100mins <- c(
  'jamesle01',
  'duncati01',
  'bryanko01',
  'pippesc01',
  'onealsh01',
  'malonka01',
  'parketo01',
  'johnsma02',
  'jordami01',
  'johnsde01',
  'duranke01',
  'birdla01',
  'fishede01',
  'horryro01',
  'wadedw01',
  'stockjo01',
  'horfoal01',
  'piercpa01',
  'parisro01',
  'grantho01',
  'kiddja01',
  'ginobma01',
  'allenra02',
  'hardeja01',
  'nowitdi01',
  'wallara01',
  'thompkl01',
  'olajuha01',
  'mchalke01',
  'drexlcl01',
  'curryst01',
  'paytoga01',
  'greendr01',
  'paulch01',
  'byronsc01',
  'billuch01',
  'millere01',
  'worthja01',
  'garneke01',
  'iguodan01',
  'ewingpa01',
  'oaklech01',
  'hamilri01',
  'aingeda01',
  'princta01',
  'leonaka01',
  'barklch01',
  'cheekma01',
  'gasolpa01',
  'rodmade01',
  'perkisa01',
  'hornaje01',
  'rondora01',
  'wallabe01',
  'butleji01',
  'westbru01',
  'lowryky01',
  'georgpa01',
  'tatumja01',
  'hillge01',
  'nashst01',
  'greenda01',
  'johnsjo01',
  'majerda01',
  'robinda01',
  'thomais01',
  'bowenbr01',
  'brownja02',
  'greenac01',
  'ibakase01',
  'dumarjo01',
  'malonmo01',
  'terryja01',
  'howardw01',
  'portete01',
  'smithjr01',
  'finlemi01',
  'robincl01',
  'johnske02',
  'jefferi01',
  'mariosh01',
  'wilkeja01',
  'mckeyde01',
  'jacksma01',
  'laimbbi01',
  'divacvl01',
  'davisda01',
  'odomla01',
  'irvinky01',
  'martike01',
  'smartma01',
  'millspa01',
  'cassesa01',
  'cartwbi01',
  'bibbymi01',
  'gasolma01',
  'kerseje01',
  'arizatr01',
  'holidjr01',
  'korveky01'
)


top100 <- master[master$player_id %in% top100mins, ]


#function to perform tests on all the players in the top 100 grouped by id
perform_tests_on_top <- function(top100, master) {
  results <- data.frame(player_id = character(),
                        t_p_value = numeric(),
                        ks_p_value = numeric(),
                        ad_p_value = numeric())
  
  for (player in unique(top100$player_id)) {
    player_data <- top100[top100$player_id == player, ]
    if (length(unique(player_data$type)) < 2 || any(table(player_data$type) < 2)) {
      next  # skip this player
    }
    # Perform the tests
    t_test_result <- t.test(vorp ~ type, data = player_data)
    ks_test_result <- ks.test(vorp ~ type, data = player_data)
    ad_test_result <- ad.test(vorp ~ type, data = player_data)
    
    # Store the results
    results <- rbind(results, data.frame(
      player_id = player,
      t_p_value = t_test_result$p.value,
      t = t_test_result$statistic,
      ks_p_value = ks_test_result$p.value,
      D = ks_test_result$statistic,
      ad_p_value = ad_test_result$ad[6],
      AD = ad_test_result$ad[2]
    ))
  }
  
  return(results)
}

res <- perform_tests_on_top(top100, master)

res$tadj <- p.adjust(res$t_p_value, method='BH')
res$kadj <- p.adjust(res$ks_p_value, method='BH')
res$aadj <- p.adjust(res$ad_p_value, method='BH')

write.csv(res, 'top100minsResults.csv', row.names=FALSE)


library(tidyverse)


ggplot(data = master, aes(x = vorp, color = type))+
  geom_density()+
  scale_color_manual(
    values = c("post" = "red", "reg" = "blue"),
    labels = c("Postseason", "Regular Season")
  )+
  xlab("APVORP")+
  ylab("Density")+
  labs(
    title = "Average Prorated VORP by Season Segment",
    subtitle = "All Players",
    color = "Segment"
    )

library(kSamples)

t.test(vorp ~ type, data = master)
ks.test(vorp ~ type, data = master)
ad.test(vorp ~ type, data = master)





summarystats <- master|>
  group_by(year)|>
  summarise(mean(vorp), sd(vorp), median(vorp))

View(summarystats)

summarystats$mean <- summarystats$`mean(vorp)`

ggplot(summarystats, aes(x = year, y = `mean(vorp)`)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "red", size = 1) + # Line plot
  labs(
    title = "League Average APVORP by Year",
    x = "Year",
    y = "APVORP"
  )+
  ylim(0, 2.5)



library(ggplot2)

# Define several distributions with the same mean
mean_value <- 0

distributions <- data.frame(
  x = rep(seq(-3, 3, length.out = 200), 4),
  density = c(
    dnorm(seq(-3, 3, length.out = 200), mean = mean_value, sd = 0.5),      # Narrow normal
    dnorm(seq(-3, 3, length.out = 200), mean = mean_value, sd = 1),        # Standard normal
    dt(seq(-3, 3, length.out = 200), df = 3),                              # T-distribution
    0.5 * dnorm(seq(-3, 3, length.out = 200), mean = -1, sd = 0.5) +       # Bimodal
      0.5 * dnorm(seq(-3, 3, length.out = 200), mean = 1, sd = 0.5)
  ),
  group = rep(c("Narrow Normal", "Standard Normal", "T-Distribution", "Bimodal"), each = 200)
)

# Plot the distributions
ggplot(distributions, aes(x = x, y = density, color = group)) +
  geom_line(size = 1) +
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "black", size = 0.8) +
  labs(
    title = "Distributions with the Same Mean but Different Shapes",
    x = "x",
    y = "Density",
    color = "Distribution"
  ) +
  theme_minimal()
