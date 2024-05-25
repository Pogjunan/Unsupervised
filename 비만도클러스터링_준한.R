obe_encoded_scaled <- scale(obe_encoded)

sil_width <- numeric(15)
for (i in 2:15) {
  km <- kmeans(obe_encoded_scaled, centers = i, nstart = 25)
  sil_width[i] <- mean(silhouette(km$cluster, dist(obe_encoded_scaled))[, 3])
}

# 결과 시각화
plot(1:15, sil_width, type = "b", xlab = "Number of Clusters", ylab = "Average Silhouette Width")
