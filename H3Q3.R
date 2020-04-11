library(ggplot2)

#We need to import the data on wine chemical properties and two other variables.
wine_df = read.csv("C:/Users/Nyarlathotep/Documents/Econ - Data Mining/Exercise 3/wine.csv")

#We need to see what our properties are
colnames(wine_df)

#We will subset the data so that only the chemical properties are considered.
wine_chem = wine_df[1:11]
colnames(wine_chem)

#normalize the data to adjust for scale.Otherwise, measuring distance between points
#will not be meaningful.
wine_chem_nm = scale(wine_chem, center = TRUE, scale = TRUE)

#we will also need to rescale later, so we will define these constants.
cluster_mu = attr(wine_chem_nm,"scaled:center")
cluster_sig = attr(wine_chem_nm, "scaled:scale")

#we want to seperate reds from whites, so we will form two clusters. Hopefully, we can
# use this as a basis to classify the wine colors.

#run k-means with 2 clusters and 80 starts.
clust1 = kmeans(wine_chem_nm, 2, nstart = 80)

# Now, we want to do some visualization, but neet to choose the
# proper method of doing so. Rather than a historam, let's do
# a scatter plot. To do this, for each chemical property, we
# need to choose one chemical property to compare the rest against.
# Ideally, the chosen property will have no correlation with color.

cor(wine_chem, as.numeric(wine_df$color))

# Note that here higher correlation means that the property is more closely
# associated with white wine.

# As we see, alcohol content has almost no correlation with color, as 
# we can futher see by the histogram below.

#Histogram of wine colors over alcohol content and other variables.
ggplot(wine_df, aes(x= alcohol, fill= color)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'fill') +
  scale_fill_manual(values=c("red", "white")) +
  labs(fill="")

#Now we rescale and use a scatter plot for the remaining chemical properties.
# qplot is in the ggplot2 library
qplot(alcohol, fixed.acidity, data = wine_df, color=factor(clust1$cluster))
qplot(alcohol, citric.acid, data=wine_df, color=factor(clust1$cluster))
qplot(alcohol, residual.sugar, data=wine_df, color=factor(clust1$cluster))
qplot(alcohol, chlorides, data=wine_df, color=factor(clust1$cluster))
qplot(alcohol, free.sulfur.dioxide, data=wine_df, color=factor(clust1$cluster))
qplot(alcohol, total.sulfur.dioxide, data=wine_df, color=factor(clust1$cluster))
qplot(alcohol, density, data=wine_df, color=factor(clust1$cluster))
qplot(alcohol, pH, data=wine_df, color=factor(clust1$cluster))
qplot(alcohol, sulphates, data=wine_df, color=factor(clust1$cluster))


# Let's come up with a model and see it's confusion matrix.
# confusionMatrix(predicted, actual)
