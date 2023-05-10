#####Plot 3.1#####

#Switch off scientific notation
options(scipen = 999)

#Calculating median for each region
median_by_region <- aggregate(data$tot_household_income, by=list(data$region), median)
median_by_region <- median_by_region[order(median_by_region$x), ]

ggplot(median_by_region, aes(x = reorder(Group.1, x), y = x)) +
  geom_bar(stat = "identity",
           fill = "#348fa7",
           color = "black") +
  labs(x = "Region",
       y = "Mediana dochodu gospodarstwa domowego",
       title = NULL,
       size = 5) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1,
                                   size = 26,
                                   color = "black"),
        axis.text.y = element_text(size = 26,
                                   color = "black"),
        axis.title = element_text(size = 30,
                                  color = "black"),
        panel.background = element_rect(fill = "white")) +
  coord_flip()

#####Plot 3.2#####

#Choosing only numeric variables
numeric_cols = c(which(names(data) == "tot_household_income"), grep("_exp", names(data)))
data_numeric <- data[, numeric_cols]

#Calculating correlations
correlations <- cor(data_numeric)

#Defining color palette with cividis and direction = -1
my_palette <- colorRampPalette(viridis(100, option = "mako", direction = -1))(100)

#Creating corrplot
corrplot(correlations,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         col = my_palette,
         addCoef.col = "white", #Change the color of the labels on the tiles to white
         cl.pos = "n",
         diag = FALSE,
         number.cex = 1,
         tl.cex = 1.3) #Increase the font size of the variable names

#Sorting correlation
corr_df <- corr_df[order(-corr_df$cor), ]
corr_df

#####Plot 3.3######

#Calculating percentage share of each category
income_counts <- table(data$main_source_income)
income_percentages <- round(100*income_counts/sum(income_counts))

#Creating labels
income_labels <- paste0(names(income_counts), "\n(", income_percentages, "%)")

#Creating plot
pie(income_counts,
    col = c("#37659e", "#348fa7", "#40b7ad"),
    cex = 2.5,
    labels = c("Działalność przedsiębiorcza\n (25%)", "Inne źródła dochodu\n(26%)", "Pensja\n (49%)"))

######Missing data plots######
as.data.frame(data_MNAR_list$'60'[1])%>% aggr(combined = FALSE, numbers = TRUE, only.miss = TRUE)
as.data.frame(data_MNAR_list$'60'[1]) %>% select(household_head_sex, tot_household_income) %>% spineMiss(col = c("steelblue", "forestgreen"))
as.data.frame(data_MNAR_list$'60'[1]) %>% mosaicMiss(highlight = "tot_household_income", plotvars = c("household_head_sex", "house_electricity"))