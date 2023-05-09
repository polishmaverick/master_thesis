#####Plot 1#####

#Choosing only numeric variables
numeric_cols = c(which(names(data) == "tot_household_income"), grep("_exp", names(data)))
data_numeric <- data[, numeric_cols]

#Calculating correlations
correlations <- cor(data_numeric)

#Defining color palette
my_palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)

#Creating corrplot
corrplot(correlations,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         col = my_palette,
         addCoef.col = "black",
         cl.pos = "n",
         diag = FALSE,
         number.cex = 0.8)

#Creating data frame with correlations
corr_df <- as.data.frame(as.table(correlations))
corr_df <- data.frame(var1 = corr_df$Var1,
                      var2 = corr_df$Var2,
                      cor = corr_df$Freq)

#Sorting correlation
corr_df <- corr_df[order(-corr_df$cor), ]
corr_df

#####Plot 2#####

#Calculating median for each region
median_by_region <- aggregate(data$tot_household_income, by=list(data$region), median)
median_by_region <- median_by_region[order(median_by_region$x), ]

ggplot(median_by_region, aes(x = reorder(Group.1, x), y = x)) +
  geom_bar(stat = "identity",
           fill = brewer.pal(n = 8, name = "Blues")[7],
           color = "black") +
  #scale_y_continuous(labels = comma) +
  labs(x = "Region",
       y = "Median household income",
       title = NULL,
       size = 5) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1,
                                   size = 24,
                                   color = "black"),
        axis.text.y = element_text(size = 24,
                                   color = "black"),
        axis.title = element_text(size = 24,
                                  color = "black",
                                  face = "bold"),
        panel.background = element_rect(fill = "white")) +
  coord_flip()

#####Plot 3######

#Calculating percentage share of each category
income_counts <- table(data$main_source_income)
income_percentages <- round(100*income_counts/sum(income_counts))

#Creating labels
income_labels <- paste0(names(income_counts), "\n(", income_percentages, "%)")

#Creating plot
pie(income_counts,
    col = brewer.pal(n = 3, name = "Blues"),
    cex = 3,
    labels = income_labels)

######Missing data plots######
as.data.frame(data_MNAR_list$'60'[1])%>% aggr(combined = FALSE, numbers = TRUE, only.miss = TRUE)
as.data.frame(data_MNAR_list$'60'[1]) %>% select(household_head_sex, tot_household_income) %>% spineMiss(col = c("steelblue", "forestgreen"))
as.data.frame(data_MNAR_list$'60'[1]) %>% mosaicMiss(highlight = "tot_household_income", plotvars = c("household_head_sex", "house_electricity"))

#####Plot - bootstrap confidence intervals#####
######MCAR######
m <- matrix(c(93, 92, 92, 92, 84, 88, 85, 80, 79, 75, 68, 62, 68, 61,
              95, 94, 92, 86, 84, 83, 72, 65, 65, 61, 57, 53, 40, 41,
              95, 92, 90, 83, 79, 71, 68, 70, 65, 58, 45, 49, 39, 34,
              97, 96, 93, 96, 92, 93, 93, 89, 86, 91, 85, 83, 78, 81,
              97, 96, 96, 96, 92, 93, 92, 88, 87, 92, 88, 81, 83, 82), 5, 14, byrow = TRUE)

t(m) -> m

colnames(m) <- c("MCAR hot-deck",
                 "MCAR kNN",
                 "MCAR regression",
                 "MCAR random forest",
                 "MCAR multiple")
rownames(m) <- paste(seq(5, 70, by = 5), "%")

df <- melt(m)
colnames(df) <- c("x", "y", "value")

# Plot the heatmap using the viridis color palette
ggplot(df, 
       aes(x = x, 
           y = y, 
           fill = value)) +
  geom_tile(color = "#203864") +
  geom_text(aes(label = value), 
            color = "white", 
            size = 10) +
  coord_fixed() +
  scale_fill_viridis(limits = c(0, 100),
                     direction = -1,
                     option = "mako", 
                     name = "Pokrycie wartości parametru\nprzez przedział ufności\n(% przypadków)") +
  scale_x_discrete(labels = rownames(m)) +
  labs(x = "Frakcja braków danych", 
       y = "Mechanizm powstania\nbraków i metoda imputacji") +
  theme(text = element_text(color = "black", 
                            size = 20),
        axis.text.x = element_text(color = "black", 
                                   angle = 45, 
                                   hjust = 1, 
                                   size = 20),
        axis.text.y = element_text(color = "black", 
                                   size = 20))

######MAR######
m <- matrix(c(95, 95, 93, 86, 89, 85, 84, 83, 84, 79, 60, 63, 51, 58,
              95, 94, 92, 83, 88, 78, 74, 66, 55, 57, 56, 45, 39, 31,
              96, 95, 95, 91, 87, 92, 83, 83, 80, 74, 74, 66, 75, 76,
              96, 96, 97, 95, 93, 87, 91, 91, 92, 92, 85, 82, 75, 76,
              95, 96, 95, 95, 97, 92, 91, 91, 90, 96, 88, 83, 79, 81),
            5, 14, byrow = TRUE)

t(m) -> m

colnames(m) <- c("MAR hot-deck",
                 "MAR kNN",
                 "MAR regression",
                 "MAR random forest",
                 "MAR multiple")
rownames(m) <- paste(seq(5, 70, by = 5), "%")

df <- melt(m)
colnames(df) <- c("x", "y", "value")

# Plot the heatmap using the viridis color palette
ggplot(df, 
       aes(x = x, 
           y = y, 
           fill = value)) +
  geom_tile(color = "#203864") +
  geom_text(aes(label = value), 
            color = "white", 
            size = 10) +
  coord_fixed() +
  scale_fill_viridis(limits = c(0, 100),
                     direction = -1,
                     option = "mako", 
                     name = "Pokrycie wartości parametru\nprzez przedział ufności\n(% przypadków)") +
  scale_x_discrete(labels = rownames(m)) +
  labs(x = "Frakcja braków danych", 
       y = "Mechanizm powstania\nbraków i metoda imputacji") +
  theme(text = element_text(color = "black", 
                            size = 20),
        axis.text.x = element_text(color = "black", 
                                   angle = 45, 
                                   hjust = 1, 
                                   size = 20),
        axis.text.y = element_text(color = "black", 
                                   size = 20))

######MNAR######
m <- matrix(c(93, 83, 70, 56, 24, 12, 3, 0, 0, 0, 0, 0, 0, 0,
              95, 95, 91, 92, 86, 80, 80, 63, 44, 28, 16, 2, 0, 0,
              94, 87, 75, 68, 54, 39, 25, 19, 13, 3, 3, 4, 0, 0,
              94, 91, 88, 81, 73, 65, 54, 29, 18, 5, 2, 0, 0, 0,
              95, 95, 90, 87, 79, 72, 58, 40, 34, 12, 10, 1, 1, 0),
            5, 14, byrow = TRUE)

t(m) -> m

colnames(m) <- c("MNAR hot-deck",
                 "MNAR kNN",
                 "MNAR regression",
                 "MNAR random forest",
                 "MNAR multiple")
rownames(m) <- paste(seq(5, 70, by = 5), "%")

# Transform the matrix in long format
df <- melt(m)
colnames(df) <- c("x", "y", "value")

# Plot the heatmap using the viridis color palette
ggplot(df, 
       aes(x = x, 
           y = y, 
           fill = value)) +
  geom_tile(color = "#203864") +
  geom_text(aes(label = value), 
            color = "white", 
            size = 10) +
  coord_fixed() +
  scale_fill_viridis(limits = c(0, 100),
                     direction = -1,
                     option = "mako", 
                     name = "Pokrycie wartości parametru\nprzez przedział ufności\n(% przypadków)") +
  scale_x_discrete(labels = rownames(m)) +
  labs(x = "Frakcja braków danych", 
       y = "Mechanizm powstania\nbraków i metoda imputacji") +
  theme(text = element_text(color = "black", 
                            size = 20),
        axis.text.x = element_text(color = "black", 
                                   angle = 45, 
                                   hjust = 1, 
                                   size = 20),
        axis.text.y = element_text(color = "black", 
                                   size = 20))