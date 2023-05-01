#####Plot example#####
#X axis labels
labels <- c("MCAR median",
            "MAR median",
            "MNAR median",
            "MCAR hot-deck",
            "MAR hot-deck",
            "MNAR hot-deck",
            "MCAR kNN",
            "MAR kNN",
            "MNAR kNN",
            "MCAR regression",
            "MAR regression",
            "MNAR regression",
            "MCAR random forest",
            "MAR random forest",
            "MNAR random forest",
            "MCAR multiple",
            "MAR multiple",
            "MNAR multiple")

hypothesis <- expand.grid(X = labels, Y = seq(5, 70, 5))

hypothesis <- transform(hypothesis, X = factor(X))
hypothesis$Z <- runif(nrow(hypothesis)) * 100

my_palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)

#Levelplot
levelplot(Z ~ X * Y,
          data = hypothesis,
          xlab = list(label = "Mechanizm powstania braków danych i technika imputacji", fontsize = 20),
          ylab = list(label = "Frakcja braków danych (w %)", fontsize = 20),
          scales = list(x = list(rot = 45, cex = 1.4), y = list(cex = 1.4)),
          col.regions = my_palette,
          colorkey = list(labels = list(cex = 1.4),
                          at = seq(0, 100, length.out = 100), 
                          title = "% przypadków odrzucenia hipotezy H0"))