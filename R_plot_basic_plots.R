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