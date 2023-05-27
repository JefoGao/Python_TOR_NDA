# INSTALL AND LOAD PACKAGES ####################################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# packages (including pacman) with pacman
pacman::p_load(pacman, magrittr, tidyverse, patchwork, ggridges, ggpubr,
               factoextra, devtools, data.table, datasets, e1071, plot3D)

# LOAD AND PREPARE DATA ########################################################

# Save the original dataset
df_0 <- read.csv("communities.csv")

# Save dataset to tibble named "df"
df <- read.csv("communities.csv") %>%
  as_tibble() %>%
  select(TotalOccupancyRate:NewHomeRate) %>%
  print()

# Remove rows with NA values
df <- na.omit(df)

# Convert Location, State from <chr> to <fct>
df$Location %<>% as_factor()
df$State %<>% as_factor()

# Compute the proportions of house with different bedrooms
df %<>%
  mutate(
    Prop1Bed = HouseNo1Bed / TotalHouseNo,
    Prop2Bed = HouseNo2Bed / TotalHouseNo,
    Prop3Bed = HouseNo3Bed / TotalHouseNo
  )

plot(sort(df$TotalOccupancyRate))
hist(df$TotalOccupancyRate, breaks = 150)

# Consider removing TotalOccupancyRate = 100% and 0 %
#df %<>%
#  filter(TotalOccupancyRate != 1 & TotalOccupancyRate != 0)

plot(sort(df$TotalOccupancyRate))
hist(df$TotalOccupancyRate, breaks = 150)
# Follows a truncated normal distribution

# EDA ##########################################################################
# Simple EDA
plot(df_0)
selected_columns <- c("TotalOccupancyRate", "MedianPrice", "TotalHouseNo", 
                      "HouseNo2Bed", "Location", "NewHomeRate")
df_01 <- df_0[, selected_columns]
plot(df_01)

# remove non-numeric columns
df_02 <- copy(df_0)
df_02 %<>%
  select(-CommunityID, -Location, -State)
df_02 <- na.omit(df_02)

# Compute the covariance matrix of pairwise elements in 'df'
cov_matrix <- cor(df_02)

# Print the covariance matrix
print(cov_matrix)

# TOC VS BEDROOM #######################################################

# Scatter plot for Prop1Bed
ggplot(df, aes(x = Prop1Bed, y = TotalOccupancyRate)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  labs(x = "Prop1Bed", y = "TotalOccupancyRate") +
  theme_minimal()
model1 <- lm(TotalOccupancyRate ~ Prop1Bed, data = df)

# Scatter plot for Prop2Bed
model2 <- lm(TotalOccupancyRate ~ Prop2Bed + I(Prop2Bed^2), data = df)
x_intercept2 <- -coef(model2)[2] / (2 * coef(model2)[3])
ggplot(df, aes(x = Prop2Bed, y = TotalOccupancyRate)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE) +
  labs(x = "Prop2Bed", y = "TotalOccupancyRate") +
  theme_minimal() +
  # add vertical line and annotation
  geom_vline(xintercept = x_intercept2, linetype = "dashed", color = "red") +
  geom_text(aes(x = x_intercept2, y = max(TotalOccupancyRate),
                label = paste0("Prop2Bed = ", round(x_intercept2, 3))),
            vjust = -1, color = "red", size = 4)


# Scatter plot for Prop3Bed
model3 <- lm(TotalOccupancyRate ~ Prop3Bed + I(Prop3Bed^2), data = df)
x_intercept3 <- -coef(model3)[2] / (2 * coef(model3)[3])
ggplot(df, aes(x = Prop3Bed, y = TotalOccupancyRate)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE) +
  labs(x = "Prop3Bed", y = "TotalOccupancyRate") +
  theme_minimal() +
  # add vertical line and annotation
  geom_vline(xintercept = x_intercept3, linetype = "dashed", color = "red") +
  geom_text(aes(x = x_intercept3, y = max(TotalOccupancyRate),
                label = paste0("Prop3Bed = ", round(x_intercept3, 3))),
            vjust = -1, color = "red", size = 4)

summary(model1)
summary(model2)
summary(model3)

x_intercept1 <- 1 - x_intercept2 - x_intercept3
#model <- lm(TotalOccupancyRate ~ Prop1Bed + Prop2Bed + I(Prop2Bed^2) + Prop3Bed + I(Prop3Bed^2), data = df)
#summary(model)
model <- lm(TotalOccupancyRate ~ Prop2Bed + Prop3Bed + I(Prop3Bed^2), data = df)
summary(model)

bed_predictor <- function(p2, p3) {
  rate <- coef(model)[1] + coef(model)[2]*p2 + coef(model)[3]*p3 + coef(model)[4]*p3^2
  return(rate)
}

df %<>%
  mutate(PredByBed = sapply(1:nrow(df), function(i) bed_predictor(df$Prop2Bed[i],
                                                                  df$Prop3Bed[i])))

# Use proportion to predict total occupancy rate
prop_deviation <- function(p1, p2, p3) {
  #y1p <- coef(model1)[1] + coef(model1)[2] * p1
  #y2p <- coef(model2)[1] + coef(model2)[2] * p2 + coef(model2)[3] * p2^2
  #y3p <- coef(model3)[1] + coef(model3)[2] * p3 + coef(model3)[3] * p3^2
  D <- sqrt((x_intercept1-p1)^2 + (x_intercept2-p2)^2 + (x_intercept3-p3)^2)
  return(D)
}

df %<>%
  mutate(PropDevi = sapply(1:nrow(df), function(i) prop_deviation(df$Prop1Bed[i],
                                                                  df$Prop2Bed[i],
                                                                  df$Prop3Bed[i])))

# Top 50% of Proportion Deviation Score
q50 <- quantile(df$PropDevi, 0.5)


df$IsLowPropDevi <- ifelse(df$PropDevi <= q50, "Yes", "No")
df$IsLowPropDevi %<>% as_factor()

ggplot(df, aes(x = TotalOccupancyRate, fill = IsLowPropDevi)) +
  geom_density(adjust = 1.5, alpha = 0.5) 

sorted_df <- data.frame(LogPropDevi = log(sort(df$PropDevi)))
logQ50 <- quantile(sorted_df$LogPropDevi, 0.5)
sorted_df$IsLowPropDevi <- ifelse(sorted_df$LogPropDevi <= logQ50, "Yes", "No")
sorted_df$IsLowPropDevi %<>% as_factor()
ggplot(sorted_df, aes(x = seq_along(LogPropDevi), y = LogPropDevi, color = IsLowPropDevi)) +
  geom_point() +
  scale_color_manual(values = c("#00C0B8","#FC717F")) +
  labs(x = "Index", y = "log(PropDevi)", title = "Log Transform Sorted PropDevi Values") +
  theme(legend.position = c(0.85, 0.1),text = element_text(size = 20))

ggplot(df, aes(x = TotalOccupancyRate, y = PropDevi, color = IsLowPropDevi)) +
  geom_point() +
  scale_color_manual(values = c("#FC717F", "#00C0B8")) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  theme(legend.position = c(0.85, 0.9),text = element_text(size = 28))

# TOC VS BEST BEDROOM PROPORTIONS ########################################

ls5 <- df %>%
  ggplot(aes(x = TotalOccupancyRate, y = Location)) +
  geom_point(aes(color = IsLowPropDevi), size = 2) +
  facet_wrap(~IsLowPropDevi) + 
  labs(x = "Total Occupancy Rate", y = "Location") +
  geom_density_ridges(aes(y = Location, fill = IsLowPropDevi), alpha = 0.5)

index_best <- which(df$IsLowPropDevi == "Yes")
t.test(df[index_best,]$TotalOccupancyRate, df[-index_best,]$TotalOccupancyRate)
# the total occupancy rate is statistically significant

# By company's definition
#df$OccupancyState <- ifelse(df$TotalOccupancyRate >= 0.85, "High",
#                             ifelse(df$TotalOccupancyRate >= 0.6,
#                                    "Medium",
#                                    "Low"))
#df$OccupancyState %<>% as_factor()

# Find the median of TotalOccupancyRate
medianRate <- median(df$TotalOccupancyRate)

df$IsHighOccupancy <- ifelse(df$TotalOccupancyRate >= medianRate, "Yes", "No")
df$IsHighOccupancy %<>% as_factor()

# TOC VS LOCATION & STATE ################################################

location_count <- df %>%
  group_by(Location) %>%
  summarise(count = n())

state_count <- df %>%
  group_by(State) %>%
  summarise(count = n())

# Bar chart of location
ls1 <- ggplot(location_count, aes(x = Location, y = count, fill = Location)) +
  geom_col() +
  geom_text(aes(label = count), vjust = 1.5) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# Bar chart of state
ls2 <- ggplot(state_count, aes(x = State, y = count)) +
  geom_col(fill = "grey") +
  geom_text(aes(label = count), vjust = 1.5) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# 100% stacked bar chart
ls3 <- df %>%
  ggplot(aes(State, fill = Location)) +
  geom_bar(position = "fill")+
  labs(x = "Location Proportion in State", y = "Percentage")

ls4 <-ggplot(df, aes(x = Location, y = PredByBed, color = IsLowPropDevi)) +
  geom_violin(trim = FALSE) 
  
# Display the plots
(ls1 / ls2 / ls3 | ls4 / ls5) +
  plot_layout(guides = "collect") &
  plot_annotation(tag_levels = 'a') &
  theme(text = element_text(size = 18),legend.position = "bottom")

ggplot(df, aes(x = Location, y = PredByBed, color = IsLowPropDevi)) +
  geom_violin(trim = FALSE)

# POPULARITY ###################################################################
# Create a contingency table of State and Location
cont_table <- table(df$State, df$Location)

# Convert the contingency table to a data frame
cont_df <- as.data.frame(cont_table)
cont_df %<>%
  rename(State = Var1, Location = Var2)

# Calculate average TotalOccupancyRate by state and location
avg_rate <- aggregate(TotalOccupancyRate ~ State + Location, data = df, FUN = mean)

# create a dataframe for the new rows
new_rows <- data.frame(State = c("ACT", "WA", "SA", "ACT"),
                       Location = c("City", "City", "City", "Coastal"),
                       TotalOccupancyRate = 0)
# bind the new rows to the original dataframe
avg_rate <- bind_rows(avg_rate, new_rows)
# rename the "TotalOccupancyRate" column
avg_rate %<>% 
  rename(AvgRate = TotalOccupancyRate)

# Merge the two data frames based on matching columns
merged_df <- merge(cont_df, avg_rate, by = c("State", "Location"))
merged_df$WeightedFreq <- round(merged_df$Freq * merged_df$AvgRate, 2)

#df %<>% select(-Freq.x, -Freq.y, -WeightedFreq.x, -WeightedFreq.y, -Popularity)

# Create a heat map of the contingency table
hm1 <- ggplot(data = cont_df, 
       aes(x = Location, y = State, fill = Freq, label = Freq)) +
  geom_tile() +
  geom_text(size = 5, color = "black") +
  scale_fill_gradient(low = "#00a9ff", high = "#f8766d") +
  labs(x = "Location", y = "State", title = "Frequency Heat Map") +
  theme(text = element_text(size = 20))
# Create a heat map of the contingency table
hm2 <- ggplot(data = merged_df, 
       aes(x = Location, y = State, fill = WeightedFreq, label = WeightedFreq)) +
  geom_tile() +
  geom_text(size = 5, color = "black") +
  scale_fill_gradient(low = "#00a9ff", high = "#f8766d") +
  labs(x = "Location", y = "State", title = "Weighted Frequency Heat Map")+
  theme(text = element_text(size = 20))

hm1 + hm2 +
  plot_layout(ncol=2, nrow=1)

# Add popularity score using the weighted frequency
df <- left_join(df, select(merged_df, State, Location, WeightedFreq), 
                           by = c("State", "Location"))

N_wf <- sum(merged_df$WeightedFreq)
df %<>%
  mutate(Popularity = (WeightedFreq+1) * PredByBed/(N_wf+24))
df_all_state <- copy(df)

ggplot(df, aes(x = MedianPrice, y = PropDevi, color = Popularity)) +geom_point()

#ggplot(df, aes(x = MedianPrice, y = PropConf, color = PopularityAdj)) +geom_point()

st1<- ggplot(df_all_state, aes(x = Popularity, y = TotalOccupancyRate, color = PropDevi)) +
  geom_point() + 
  stat_density_2d() +
  scale_color_gradient(low = "#f8766d", high = "#8494ff")

st2<-ggplot(df_all_state, aes(x = Popularity, y = TotalOccupancyRate, color = State)) +
  geom_point() + 
  stat_density_2d()
st3<-ggplot(df_all_state, aes(x = Popularity, y = TotalOccupancyRate, color = Location)) +
  geom_point() + 
  stat_density_2d()

# remove SA, WA, ACT and City, and VIC coastal
df_sub <- df_all_state[df_all_state$State %in% c("NSW", "QLD", "VIC"), ]
df_sub <- df_sub[df_sub$Location!='City',]
df_sub <- subset(df_sub, !(Location == "Coastal" & State == "VIC"))

st4<-ggplot(df_sub, aes(x = Popularity, y = TotalOccupancyRate, color = PropDevi)) +
  geom_point() + 
  stat_density_2d() +
  scale_color_gradient(low = "#f8766d", high = "#8494ff")

st1+st2+st3+st4+
  plot_layout(ncol=2, nrow=2) &
  plot_annotation(tag_levels = 'a') &
  theme(text = element_text(size = 18),legend.position = "bottom")


ggplot(df_sub, aes(x = Popularity, y = TotalOccupancyRate, color = PropDevi)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ log(x), se = TRUE) +
  scale_color_gradient(low = "#f8766d", high = "#8494ff")

summary(lm(df_sub$TotalOccupancyRate~df_sub$Popularity))

# MEDIAN PRICE VS LOCATION & STATE #############################################

# Box plot of Median Price by Location
ggplot(df_sub, aes(x = Location, y = MedianPrice, fill = Location)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1) +
  labs(title = "Median House Price by Location",
       x = "Location",
       y = "Median Price") +
  scale_fill_discrete(name = "Location") +
  theme(legend.position = "right")

# Box plot of Median Price by State
ggplot(df_sub, aes(x = State, y = MedianPrice, fill = State)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1) +
  labs(title = "Median House Price by State",
       x = "State",
       y = "Median Price") +
  scale_fill_discrete(name = "State") +
  theme(legend.position = "right")


df_sub %>%
  ggplot(aes(x = MedianPrice, fill = Location)) +
  geom_density(alpha = 0.5) +
  facet_grid(Location ~ .) +
  theme(legend.position = "none")



# TOC VS NEW HOME RATE & TOTAL HOUSE NO ########################################
ggplot(df_sub, aes(x = NewHomeRate , y = TotalHouseNo)) +
  geom_point() + geom_density_2d() +
  theme(text = element_text(size = 18))

df_sub$IsNewDominant <- ifelse(df_sub$NewHomeRate >= 0.5 & df_sub$TotalHouseNo >= 150, 1, 0)
df_sub$IsNewDominant %<>% as_factor()
# Fit the SVM model
svm_model <- svm(IsNewDominant ~ NewHomeRate * TotalHouseNo, data = df_sub, kernel = "linear")

# Create a grid of values to predict over
grid <- expand.grid(NewHomeRate = seq(min(df_sub$NewHomeRate), max(df_sub$NewHomeRate), length.out = 100),
                    TotalHouseNo = seq(min(df_sub$TotalHouseNo), max(df_sub$TotalHouseNo), length.out = 100))

# Predict over the grid
grid$IsNewDominant <- predict(svm_model, newdata = grid)

# Plot the decision boundary
nt2 <- ggplot(df_sub, aes(x = NewHomeRate, y = TotalHouseNo, color = IsNewDominant)) +
  geom_point() +
  geom_tile(data = grid, aes(fill = IsNewDominant), alpha = 0.3) +
  scale_color_manual(values = c("#00C0B8", "#FC717F")) +
  scale_fill_manual(values = c("#00C0B8", "#FC717F")) +
  theme_minimal()
dens1 <- ggplot(df_sub, aes(x = TotalHouseNo, fill = IsNewDominant)) +
  geom_density(alpha = 0.4) + 
  scale_fill_manual(values = c("#00C0B8", "#FC717F")) +
  theme_minimal() +   
  theme(legend.position = "none")
dens2 <- ggplot(df_sub, aes(x = NewHomeRate, fill = IsNewDominant)) + 
  geom_density(alpha = 0.4) + 
  scale_fill_manual(values = c("#00C0B8", "#FC717F")) +
  theme_minimal() + 
  theme(legend.position = "none") + 
  coord_flip()
dens1 + plot_spacer() + nt2 + dens2 + 
  plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))+
  plot_layout(guides = "collect") &
  theme(text = element_text(size = 18),legend.position = "bottom")

# Set up the 3D scatter plot
scatter3D(x = df_sub$NewHomeRate, y = df_sub$TotalHouseNo, z = df_sub$TotalOccupancyRate,
          colvar = df_sub$IsNewDominant, col = c("#FC717F", "#00C0B8"),
          pch = 16, type = "s", cex = 2,
          xlab = "NewHomeRate", ylab = "TotalHouseNo", zlab = "TotalOccupancyRate",
          main = "TOR ~ NewHomeRate & TotalHouseNo", alpha = 0.5)

df_sub$IsNewDominant %<>% as_factor()
ggplot(df_sub, aes(x = TotalHouseNo*NewHomeRate, y = TotalOccupancyRate, color = IsNewDominant)) +
  geom_boxplot() +
  theme(text = element_text(size = 18),legend.position = "bottom")

ggplot(df_sub, aes(x = TotalHouseNo*NewHomeRate, y = TotalOccupancyRate, color = IsNewDominant)) +
  geom_point()

# RESULT #######################################################################
#df_out <- select(df, -)

df_sub$IsHighOccupancy <- ifelse(df_sub$IsHighOccupancy=="Yes", 1, 0)
df_sub$IsLowPropDevi <- ifelse(df_sub$IsLowPropDevi=="Yes", 1, 0)
df_sub$IsNewDominant <- as.numeric(as.character(df_sub$IsNewDominant))

# Output
write.csv(df_sub, "communities_db.csv", row.names = FALSE)
