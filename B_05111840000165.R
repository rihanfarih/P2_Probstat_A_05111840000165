
# Nomor 1

# a)
# Mencari peluang penyurvei bertemu x = 3 orang yang tidak hadir
# pada acara vaksinasi sebelum keberhasilan pertama; Jika terdapat 3 orang yang 
# tidak hadir pada acara vaksinasi sebelum keberhasilan pertama, maka orang
# keempat adalah yang hadir pada keberhasilan pertama
#
# Peluang orang yang hadir (p) = 0.20, peluang orang yang tidak hadir 
# (q) = p - 1 = 1 - 0.20 = 0.80

p <- 0.20
q <- 0.80
x <- 3 + 1

b <- p * q^(x - 1)
b # 0.1024

# Dengan fungsi dgeom

dgeom(x - 1, p) # 0.1024

# b)
# Mean distribusi geometrik dengan data random = 10000, p = 0.20, dan
# distribusi geometri acak X = 3

n <- 10000
p <- 0.20
x <- 3

res <- rgeom(n, p) == 3

mean(res)

# c)
# Dari hasil poin a dan b, dapat disimpulkan bahwa hasil dari a merupakan
# nilai eksak dari peluang distribusi geometrik, sedangkan hasil dari b
# merupakan nilai estimasi yang mendekati nilai dari a.

# d)

# e)
# Rataan

mean <- 1 / p
mean

# Varians

variance <- (1-p) / p^2
variance


# Nomor 2

# a)
# Peluang 4 pasien sembuh

n <- 20
p <- 0.2
x <- 4

P <- (factorial(n)/(factorial(n - x)*factorial(x))) * p^x * (1 - p)^(n - x)
print(P) # 0.2181994

# Dengan fungsi dbinom

dbinom(x, n, p) # 0.2181994

# b)

x <- 0:n
plot(x, dbinom(x, n, p),
     type='h',
     main='Distribusi Binomial n = 20, p = 0.2',
     ylab='Peluang',
     xlab='Pasien dengan peluang sembuh',
     lwd=4)

# c)
# Rataan

n <- 20
x <- 0:n
p <- 0.2

P <- dbinom(x, n, p)
mean <- sum(x * P)
mean

# Varian

varian <- sum(x^2 * P) - mean^2
varian


# Nomor 3
# a)

lambda = 4.5
X = 6
P = (exp(1)^(-lambda) * lambda^X) / factorial(X)
P

# Dengan fungsi dpois

dpois(X, lambda)

# b)
# Simulasi

data <- rpois(365, lambda)
data

plot(1:365, data,
     type='h',
     main='Distribusi Poisson Kelahiran 6 Bayi',
     ylab='Jumlah Bayi',
     xlab='Hari',
     lwd=4)

# c)

# d)
# Mean = lambda dan Varian = lambda

mean = lambda
mean

varian = lambda
varian


# Nomor 4
# a)

x <- 2
v <- 10

dchisq(x, v)

# b)

n <- 100
data <- rchisq(n, v)
plot(1:n, data,
     type='h',
     main='Distribusi Chi-Square dari v = 10',
     ylab='Distribusi Chi-Square',
     xlab='x',
     lwd=4)

# c)
# Rataan = v, varian = 2v
mean <- v
mean

varian <- 2*v
varian


# Nomor 5
# a)
# Dengan range 1-100

lambda <- 3

dexp(1:100, lambda)

# b)
         
x <- 10
data <- rexp(x, lambda)
plot(1:10, data,
     type='h',
     main='Distribusi Eksponensial dengan x = 10',
     ylab='Distribusi Eksponensial',
     xlab='x',
     lwd=4)

x <- 100
data <- rexp(x, lambda)
plot(1:100, data,
     type='h',
     main='Distribusi Eksponensial dengan x = 100',
     ylab='Distribusi Eksponensial',
     xlab='x',
     lwd=4)

x <- 1000
data <- rexp(x, lambda)
plot(1:1000, data,
     type='h',
     main='Distribusi Eksponensial dengan x = 1000',
     ylab='Distribusi Eksponensial',
     xlab='x',
     lwd=4)

x <- 10000
data <- rexp(x, lambda)
plot(1:10000, data,
     type='h',
     main='Distribusi Eksponensial dengan x = 10000',
     ylab='Distribusi Eksponensial',
     xlab='x',
     lwd=4)

# c)

set.seed(1) 

n <- 100
lambda <- 3

# Rataan = 1/lambda, varian = 1/lambda^2

mean <- 1 / lambda
mean

varian <- 1 / lambda^2
varian


# Nomor 6
# a)

n <- 100
mean <- 50
sd <- 8

# Membuat nilai acak untuk x
x <- runif(1, min = runif(1, max = mean), max = runif(1, min = mean, max = n))

# Mendapatkan z score
z_score <- (x - mean) / sd

data <- rnorm(n, mean, sd)
plot(1:n, data, type = 'h')

# b)

hist(data, breaks=50, 
     main = "5025201211_AzzuraMPM_Probstat_C_DNhistogram"
     )

# c)

var(data)

