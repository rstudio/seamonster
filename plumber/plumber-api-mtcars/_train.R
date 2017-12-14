m <- lm(mpg ~ hp, data = mtcars)
saveRDS(m, "model.Rdat")
