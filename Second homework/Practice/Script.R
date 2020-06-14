## Medvedev Andrey
# Reading data
data_big <- read.csv("data_big.csv")
df<-data.frame(group=as.factor(data_big$weakness.1)
               ,X=as.numeric(data_big$headache.1))

# Chi-squared test
tbl <- table(data_big$weakness.1, data_big$headache.1);tbl
chisq.test(tbl) 

# Fisher test
fisher.test(tbl)

# Entropy values
entropy <- function (vals, n)
{
  result <- 0
  for (val in vals)
  {
    if (val > 0)
    {
      p.k = (val / n)
      result = result + p.k * log2(1 / p.k)
    }
  }
  
  return (result)
}

n = nrow(df);
entropy.a <- entropy(rowSums(tbl), n);entropy.a
entropy.b <- entropy(colSums(tbl), n);entropy.b
entropy.ab <- entropy(tbl, n);entropy.ab

entropy.a
entropy.b
entropy.ab

I.ab <- entropy.a + entropy.b - entropy.ab
J.ba <- I.ab / entropy.a * 100
J.ab <- I.ab / entropy.b * 100
J <- 2 * I.ab / (entropy.a + entropy.b) * 100


# McNemar's test
ctable = table(data_big$chest.pain.1,
               data_big$chest.pain.3)
ctable = matrix(ctable, nrow = length(unique(data_big$chest.pain.1)))
ctable[2,] = ctable[2,]+ctable[3,]
ctable = ctable[1:2,]
ctable

mcnemar.test(ctable)

# Cohran's test
data = matrix(c(data_big$chest.pain.1,
                data_big$chest.pain.2,
                data_big$chest.pain.3), 
              ncol = 3)
print(data)
data[data==2]=1

col_sums = colSums(data, na.rm= T )
row_sums = rowSums(data, na.rm= T )
n = sum(data, na.rm= T)

s = 3
top = sum(sapply(colSums(data, na.rm=T), function (i) (i-n/s)^2))
bottom = sum(sapply(rowSums(data, na.rm=T), function (i) i*(s-i)))
multiplier = top / bottom
cohranStat =  s * (s-1) * multiplier; cohranStat
cohranPValue=1-pchisq(cohranStat, s-1); cohranPValue
