# Reading data
dataset <- read.csv("A1Ratings.csv")
movies.ratings <- dataset[, 2:21]

# Means
means.dataset <- colMeans(movies.ratings, na.rm = TRUE)
ranking.by.means <- means.dataset[order(means.dataset, decreasing = TRUE)][1:5]

# % of ratings 4+
ratings4 <- apply(movies.ratings, 2, function(x) sum(x >= 4, na.rm=TRUE) / length(which(!is.na(x))))
ranking.by.ratings4 <- ratings4[order(ratings4, decreasing = TRUE)][1:5]

# Rating count
ratings.count <- apply(movies.ratings, 2, function(x) length(which(!is.na(x))))
ranking.by.count <- ratings.count[order(ratings.count, decreasing = TRUE)][1:5]

# Association with Star Wars Episode IV
user.who.rated <- which(!is.na(dataset[, 2]))
association.with <- apply(movies.ratings[, 2:20], 2, function(x) length(which(!is.na(x[user.who.rated]))) / length(user.who.rated))
ranking.by.association <- association.with[order(association.with, decreasing = TRUE)][1:5]
