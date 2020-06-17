### Medvedev Andrey

# Read data
varNumber <- 6
dependentVar <- 'precep'
independentVars <- c('asian', 'black', 'temper', 'cross', 'transp', 'degree')
data <- read.csv2("mult_reg.csv")

# Modelling
fit <- lm(precep~asian+black+temper+cross+transp+degree, data=data)
summary(fit)

fit$coefficients

# Calculate correlation coefficients

df <- data[independentVars]
cor(fit$residuals, df)

# Calculate partial correlation coefficients
targetCoefficients <- fit$coefficients[2:7]
targetCoefficients <- as.data.frame(t(targetCoefficients))

asianRes <- fit$residuals + targetCoefficients$asian * df$asian
blackRes <- fit$residuals + targetCoefficients$black * df$black
temperRes <- fit$residuals + targetCoefficients$temper * df$temper
crossRes <- fit$residuals + targetCoefficients$cross * df$cross
transpRes <- fit$residuals + targetCoefficients$transp * df$transp
degreeRes <- fit$residuals + targetCoefficients$degree * df$degree

allRes <- list(asianRes,
               blackRes,
               temperRes,
               crossRes,
               transpRes,
               degreeRes)

m <- matrix(0, nrow = varNumber, ncol = varNumber)
rownames(m) <- independentVars
colnames(m) <- independentVars

for (i in 1:nrow(m))
{
  for (j in i:ncol(m))
  {
    print(j)
    m[i, j] <- cor(allRes[[i]], allRes[[j]])
  }
}

m