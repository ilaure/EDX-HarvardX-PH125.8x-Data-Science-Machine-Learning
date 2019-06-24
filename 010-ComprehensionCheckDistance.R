# 3. Nearest Neighbors  ---------------------------------------------------------------------


# 3.1 Distance ------------------------------------------------------------

library(dslabs)
data("tissue_gene_expression")

dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
d <- dist(tissue_gene_expression$x)

#Question1
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

image(as.matrix(d))

#Question2
#Compare the distances between observations 1 and 2 (both cerebellum), observations 39 and 40 (both colon), and observations 73 and 74 (both endometrium).
#Distance-wise, are samples from tissues of the same type closer to each other?
  
  
#  ```{r}
d_matrix <- as.matrix(d)
d_matrix[1:2,1:2]
d_matrix[39:40,39:40]
d_matrix[73:74,73:74]

#```
#Answer : 
#Yes, the samples from the same tissue type are closest to each other


#Question3
#Make a plot of all the distances using the image function to see if the pattern you observed in Q2 is general.
#Which code would correctly make the desired plot?

#```{r}
image(as.matrix(d))
#image(d_matrix)[order(tissue_gene_expression$y):order(tissue_gene_expression$y)]
#```
