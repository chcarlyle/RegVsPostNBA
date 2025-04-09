library(kSamples)

setwd('C:/Users/caleb/OneDrive/Desktop/undergrad_research/old_data')

adv <- read.csv('postseason_advanced.csv')
adv$net <- adv$off - adv$def

setwd('C:/Users/caleb/OneDrive/Desktop/undergrad_research/Page Data/Advanced')

jordan <- read.table('jordami01advanced.txt', header = TRUE)

jordan_post <- adv[adv$player=='jordami01',]

jordan <- jordan[c('ORtg', 'DRtg', 'GmSc')]
jordan_post <- jordan_post[c('off', 'def', 'gs')]

jordan$type <- 'reg'
jordan_post$type <- 'post' 

names(jordan_post) <- names(jordan)




jordan <- rbind(jordan, jordan_post)

jordan$net <- jordan$ORtg - jordan$DRtg


t.test(jordan$GmSc~jordan$type)
t.test(jordan$net~jordan$type)

ks.test(jordan$GmSc~jordan$type)
ks.test(jordan$net~jordan$type)

ad.test(jordan$GmSc~jordan$type)
ad.test(jordan$net~jordan$type)


jordanreg <- jordan[jordan$type == 'reg',]
jordanpost <- jordan[jordan$type == 'post',]

plot(density(na.omit(jordanreg$GmSc)), col = 'blue', lwd = 2, ylim = c(0,0.06))
lines(density(na.omit(jordanpost$GmSc)), col = 'red', lwd = 2)








lebron <- read.table('jamesle01advanced.txt', header = TRUE)


lebron_post <- adv[adv$player=='jamesle01',]

lebron <- lebron[c('ORtg', 'DRtg', 'GmSc')]
lebron_post <- lebron_post[c('off', 'def', 'gs')]

lebron$type <- 'reg'
lebron_post$type <- 'post' 

names(lebron_post) <- names(lebron)

lebron <- rbind(lebron, lebron_post)

lebron$net <- lebron$ORtg - lebron$DRtg


t.test(lebron$GmSc~lebron$type)
t.test(lebron$net~lebron$type)

ks.test(lebron$GmSc~lebron$type)
ks.test(lebron$net~lebron$type)

ad.test(lebron$GmSc~lebron$type)
ad.test(lebron$net~lebron$type)

lebronreg <- lebron[lebron$type == 'reg',]
lebronpost <- lebron[lebron$type == 'post',]
plot(density(na.omit(lebronreg$GmSc)), col = 'blue', lwd = 2)
lines(density(na.omit(lebronpost$GmSc)), col = 'red', lwd = 2)











rodman <- read.table('rodmade01advanced.txt', header = TRUE)


rodman_post <- adv[adv$player=='rodmade01',]

rodman <- rodman[c('ORtg', 'DRtg', 'GmSc')]
rodman_post <- rodman_post[c('off', 'def', 'gs')]

rodman$type <- 'reg'
rodman_post$type <- 'post' 

names(rodman_post) <- names(rodman)

rodman <- rbind(rodman, rodman_post)

rodman$net <- rodman$ORtg - rodman$DRtg


t.test(rodman$GmSc~rodman$type)
t.test(rodman$net~rodman$type)

ks.test(rodman$GmSc~rodman$type)
ks.test(rodman$net~rodman$type)

ad.test(rodman$GmSc~rodman$type)
ad.test(rodman$net~rodman$type)








horry <- read.table('horryro01advanced.txt', header = TRUE)


horry_post <- adv[adv$player=='horryro01',]

horry <- horry[c('ORtg', 'DRtg', 'GmSc')]
horry_post <- horry_post[c('off', 'def', 'gs')]

horry$type <- 'reg'
horry_post$type <- 'post' 

names(horry_post) <- names(horry)

horry <- rbind(horry, horry_post)

horry$net <- horry$ORtg - horry$DRtg


t.test(horry$GmSc~horry$type)
t.test(horry$net~horry$type)

ks.test(horry$GmSc~horry$type)
ks.test(horry$net~horry$type)

ad.test(horry$GmSc~horry$type)
ad.test(horry$net~horry$type)























nexttests <- function(id){
  # Read data and split into regular and postseason data
  data <- read.table(paste0(id, 'advanced.txt'), header = TRUE)
  post_data <- adv[adv$player == id, c('off', 'def', 'gs')]
  
  reg_data <- data[c('ORtg', 'DRtg', 'GmSc')]
  names(post_data) <- names(reg_data)  # Match column names
  
  # Add type labels
  reg_data$type <- 'reg'
  post_data$type <- 'post'
  
  # Combine datasets
  combined_data <- rbind(reg_data, post_data)
  
  # Calculate net rating
  combined_data$net <- combined_data$ORtg - combined_data$DRtg
  
  # Conduct t-tests
  t_test_GmSc <- t.test(GmSc ~ type, data = combined_data)
  t_test_net <- t.test(net ~ type, data = combined_data)
  
  # Conduct KS tests
  ks_test_GmSc <- ks.test(combined_data$GmSc[combined_data$type == 'reg'], combined_data$GmSc[combined_data$type == 'post'])
  ks_test_net <- ks.test(combined_data$net[combined_data$type == 'reg'], combined_data$net[combined_data$type == 'post'])
  
  # Conduct AD tests (assuming package "nortest" is loaded)
  ad_test_GmSc <- ad.test(combined_data$GmSc[combined_data$type == 'reg'], combined_data$GmSc[combined_data$type == 'post'])
  ad_test_net <- ad.test(combined_data$net[combined_data$type == 'reg'], combined_data$net[combined_data$type == 'post'])
  
  # Plot density for Game Score (GmSc)
  reg_GmSc <- na.omit(combined_data$GmSc[combined_data$type == 'reg'])
  post_GmSc <- na.omit(combined_data$GmSc[combined_data$type == 'post'])
  
  plot(density(reg_GmSc), col = 'blue', lwd = 2, main = "Density of GmSc by Season Type")
  lines(density(post_GmSc), col = 'red', lwd = 2)
  
  
  # Return test results
  list(
    t_test_GmSc = t_test_GmSc,
    t_test_net = t_test_net,
    ks_test_GmSc = ks_test_GmSc,
    ks_test_net = ks_test_net,
    ad_test_GmSc = ad_test_GmSc,
    ad_test_net = ad_test_net
  )
}



test <- nexttests('rodmade01')
nexttests('rondora01')
nexttests('greendr01')
nexttests('jordami01')
nexttests('thomais01')
nexttests('horryro01')
nexttests('leonaka01')
nexttests('jamesle01')
nexttests('olajuha01')
nexttests('embiijo01')
nexttests('curryst01')

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
  #'ginobma01',
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
  #'byronsc01',
  'billuch01',
  'millere01',
  'worthja01',
  'garneke01',
  'iguodan01',
  'ewingpa01',
  'oaklech01',
  #'hamilri01',
  'aingeda01',
  'princta01',
  'leonaka01',
  'barklch01',
  'cheekma01',
  #'gasolpa01',
  'rodmade01',
  'perkisa01',
  'hornaje01',
  'rondora01',
  #'wallabe01',
  'butleji01',
  'westbru01',
  'lowryky01',
  'georgpa01',
  'tatumja01',
  'hillge01',
  'nashst01',
  'greenda01',
  #'johnsjo01',
  'majerda01',
  'robinda01',
  'thomais01',
  #'bowenbr01',
  'brownja02',
  'greenac01',
  'ibakase01',
  'dumarjo01',
  #'malonmo01',
  #'terryja01',
  'howardw01',
  'portete01',
  'smithjr01',
  'finlemi01',
  'robincl01',
  'johnske02',
  #'jefferi01',
  #'mariosh01',
  #'wilkeja01',
  'mckeyde01',
  'jacksma01',
  'laimbbi01',
  'divacvl01',
  'davisda01',
  #'odomla01',
  'irvinky01',
  #'martike01',
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







# Use a small subset for testing
test_names <- c('rodmade01', 'jordami01', 'greendr01', 'horryro01', 'leonaka01')

# Initialize an empty data frame
results_df <- data.frame()

# Loop through the subset of names
for (name in top100mins) {
  cat("Processing:", name, "\n") # Debugging statement
  
  test_results <- nexttests(name)
  
  
  # Skip if the function fails
  #if (is.null(test_results)) next
  
  # Extract relevant statistics and append
  test_results_df <- data.frame(
    input = name,
    t_test_GmSc_p = test_results$t_test_GmSc$p.value,
    t_test_net_p = test_results$t_test_net$p.value,
    ks_test_GmSc_p = test_results$ks_test_GmSc$p.value,
    ks_test_net_p = test_results$ks_test_net$p.value,
    ad_test_GmSc_p = test_results$ad_test_GmSc$ad[6],
    ad_test_net_p = test_results$ad_test_net$ad[6],
    stringsAsFactors = FALSE
  )
  
  #Append to results_df
  results_df <- rbind(results_df, test_results_df)
}

# Print the final results
print(results_df)

write.csv(results_df, 'otheradvancedtop100.csv')


jordan <- read.table('jordami01advanced.txt', header = TRUE)
post_jordan <- adv[adv$player == 'jordami01', c('off', 'def', 'gs')]

reg_jordan <- jordan[c('ORtg', 'DRtg', 'GmSc')]
names(post_jordan) <- names(reg_jordan)  # Match column names

# Add type labels
reg_jordan$type <- 'reg'
post_jordan$type <- 'post'

# Combine datasets
combined_jordan <- rbind(reg_jordan, post_jordan)

# Calculate net rating
combined_jordan$net <- combined_jordan$ORtg - combined_jordan$DRtg

ggplot(data = combined_jordan, aes(x = GmSc, color = type))+
  geom_density()+
  scale_color_manual(
    values = c("post" = "red", "reg" = "blue"),
    labels = c("Postseason", "Regular Season")
  )+
  xlab("Game Score")+
  ylab("Density")+
  labs(
    title = "Average Prorated VORP by Season Segment",
    subtitle = "All Players",
    color = "Segment"
  )
library(dplyr)
players <- c('jordami01', 'jamesle01', 'horryro01', 'greendr01', 'thomais01', 'rodmade01', 'rondora01', 'leonaka01')
poi <- adv|>
  filter(player %in% players)

poi <- poi|>
  select(player, off, def, gs)


jordan <- read.table('jordami01advanced.txt', header = TRUE)
jordan$player <- 'jordami01'
lebron <- read.table('jamesle01advanced.txt', header = TRUE)
lebron$player <- 'jamesle01'
horry <- read.table('horryro01advanced.txt', header = TRUE)
horry$player <- 'horryro01'
draymond <- read.table('greendr01advanced.txt', header = TRUE)
draymond$player <- 'greendr01'
isiah <- read.table('thomais01advanced.txt', header = TRUE)
isiah$player <- 'thomais01'
rodman <- read.table('rodmade01advanced.txt', header = TRUE)
rodman$player <- 'rodmade01'
rondo <- read.table('rondora01advanced.txt', header = TRUE)
rondo$player <- 'rondora01'
kawhi <- read.table('leonaka01advanced.txt', header = TRUE)
kawhi$player <- 'leonaka01'

combined <- bind_rows(jordan, lebron, horry, draymond, isiah, rodman, rondo, kawhi)
combined <- combined|>
  select(player, ORtg, DRtg, GmSc)

combined$type <- 'reg'
poi$type <- 'post'

names(combined)
names(poi) <- c('player', 'ORtg', 'DRtg', 'GmSc', 'type')

overall <- bind_rows(combined, poi)

overall |> 
  filter(player == 'jordami01') |> 
  ggplot(aes(x = GmSc, color = type)) + 
  geom_density() + 
  geom_vline(
    aes(xintercept = mean(GmSc, na.rm = TRUE), color = type), 
    linetype = "dashed"
  ) + 
  labs(
    title = "Density Plot of Game Score with Mean Lines by Type",
    x = "Game Score (GmSc)",
    y = "Density"
  ) +
  theme_minimal()
