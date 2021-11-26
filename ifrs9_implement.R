

library(haven)
library(expm)
library(markovchain)
library(expm)
library(ctmcd)

n.dim <- 10;

# own data
df <- read_sas("C:/Users/a96h4va/OneDrive - Erste Group/R/data/migmatrix_02.sas7bdat")
head(df)

df1 <- data.frame(df[ ,-2])
head(df1)

variabls <- df1$variable
dat <- df1[df1$variable==variabls[10], ];
MM_Banken <- data.frame(dat[ ,c("to_1","to_2","to_3","to_4","to_5","to_6","to_7","to_8","to_9","to_10")]);
MM_Banken

matrix_numeric <- as.matrix(MM_Banken)
generatormatrix_regul <- gm(matrix_numeric, te=1, method="QO",logmethod="Eigen")

par(mfrow=c(2,2))

plot(generatormatrix_regul)

generatormatrix_neu <- (generatormatrix_regul$par)
generatormatrix_neu

migration_matrix_basis <- expm(generatormatrix_neu)
migration_matrix_basis

MM_year_function_unt <- function(t) {
  if (t == 0.25) {
    x <- expm(generatormatrix_neu * c(0.25))
    return(x)
  }
  else if (t == 0.5) {
    x <- expm(generatormatrix_neu * c(0.5))
    return(x)
  }
  else if (t == 0.75) {
    x <- expm(generatormatrix_neu * c(0.75))
    return(x)
  }
  else if (t == 0.99) {
    x <- expm(generatormatrix_neu * c(1))
    return(x)
  }
  else{
    expm(generatormatrix_neu * c(t))
  }
}


Lifetime_PD_unt <- function(t, rk){
  x <- MM_year_function_unt(t)[rk, n.dim]
  return(x)
}


# Plotten der LtPDs:
RK_Output <- function(t, rk) {
  a <- vector("numeric", t)
  for (i in 1:t) {
    a[i] <- Lifetime_PD_unt(i, rk)
  }
  a
}

RK_Output_gesamt <- function(t) {
  z <-
    cbind(
      RK_Output(t, 1),
      RK_Output(t, 2),
      RK_Output(t, 3),
      RK_Output(t, 4),
      RK_Output(t, 5),
      RK_Output(t, 6),
      RK_Output(t, 7),
      RK_Output(t, 8),
      RK_Output(t, 9),
      RK_Output(t, 10)
    )
  colnames(z) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  return(z)
}


t.len <- 5;


x <- RK_Output_gesamt(t.len)


ts.plot(
  x,
  gpars = list(col = rainbow(n.dim)),
  ylim = c(0, 1),
  xlim = c(1, t.len),
  xlab = "Zeit",
  ylab = "LtPD",
  lwd = 2,
  main = "Lifetime-PD (LtPDs)"
)
legend(
  'topright',
  'groups',
  c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
  lty = 1,
  cex = 0.75,
  col = rainbow(t.len)
)


#--------------------------------------------------------------------- marginal pd -----------------------------------------------------------------;


marginale_PD_unt <- function(t, rk) {
  if (t == 0.25) {
    # print("1")
    Lifetime_PD_unt(0.25, rk)
  }
  else if (t == 0.5) {
    # print("2")
    Lifetime_PD_unt(0.5, rk) - Lifetime_PD_unt(0.25, rk)
  }
  else if (t == 0.75) {
    # print("3")
    Lifetime_PD_unt(0.75, rk) - Lifetime_PD_unt(0.5, rk)
  }
  else if (t == 0.99) {
    # print("4")
    Lifetime_PD_unt(1, rk) - Lifetime_PD_unt(0.75, rk)
  }
  else{
    # print("5")
    Lifetime_PD_unt(t, rk) - Lifetime_PD_unt(t - 1, rk)
  }
}


Lifetime_PD_m <- function(t, rk){
  x <- marginale_PD_unt(t,rk)
  return(x)
}


RK_Output_mPD <- function(t, rk) {
  a <- vector("numeric", t)
  for (i in 1:t) {
    a[i] <- Lifetime_PD_m(i, rk)
  }
  a
}

RK_Output_gesamt_mPD <- function(t) {
  z <-
    cbind(
      RK_Output_mPD(t, 1),
      RK_Output_mPD(t, 2),
      RK_Output_mPD(t, 3),
      RK_Output_mPD(t, 4),
      RK_Output_mPD(t, 5),
      RK_Output_mPD(t, 6),
      RK_Output_mPD(t, 7),
      RK_Output_mPD(t, 8),
      RK_Output_mPD(t, 9),
      RK_Output_mPD(t, 10)
    )
  colnames(z) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  return(z)
}

t.len <- 5;
x <- RK_Output_gesamt_mPD(t.len)
ts.plot(
  x,
  gpars = list(col = rainbow(n.dim)),
  ylim = c(-0.5, 0.7),
  xlim = c(1, t.len),
  xlab = "Zeit",
  ylab = "LtPD",
  lwd = 2,
  main = "Uncondit. marginal PD(unbedingte margin. PD)"
)
legend(
  'topright',
  'groups',
  c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
  lty = 1,
  cex = 0.75,
  col = rainbow(n.dim)
)


#-------------------------------------------------Incremental Default Probability pd -----------------------------------------------------------------;



bedingte_PD_unt <- function(t, rk) {
  if (t == 0.25) {
    marginale_PD_unt(0.25, rk)
  }
  else if (t == 0.5) {
    marginale_PD_unt(0.5, rk) / (1 - Lifetime_PD_unt(0.25, rk))
  }
  else if (t == 0.75) {
    marginale_PD_unt(0.75, rk) / (1 - Lifetime_PD_unt(0.5, rk))
  }
  else if (t == 0.99) {
    marginale_PD_unt(0.5, rk) / (1 - Lifetime_PD_unt(0.25, rk))
  }
  else{
    marginale_PD_unt(t, rk) / (1 - Lifetime_PD_unt(t - 1, rk))
  }
}

Marginal_be_PD <- function(t, rk){
  x <- bedingte_PD_unt(t,rk)
  return(x)
}

Marginal_be_PD(5,1)
# [1] 0.1261923

RK_Output_bePD <- function(t, rk) {
  a <- vector("numeric", t)
  for (i in 1:t) {
    a[i] <- Marginal_be_PD(i, rk)
  }
  a
}


RK_Output_gesamt_PD <- function(t) {
  z <-
    cbind(
      RK_Output_bePD(t, 1),
      RK_Output_bePD(t, 2),
      RK_Output_bePD(t, 3),
      RK_Output_bePD(t, 4),
      RK_Output_bePD(t, 5),
      RK_Output_bePD(t, 6),
      RK_Output_bePD(t, 7),
      RK_Output_bePD(t, 8),
      RK_Output_bePD(t, 9),
      RK_Output_bePD(t, 10)
    )
  colnames(z) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  return(z)
}


t.len <- 5;
x <- RK_Output_gesamt_PD(t.len)
ts.plot(
  x,
  gpars = list(col = rainbow(n.dim)),
  ylim = c(-0.1, 0.7),
  xlim = c(1, t.len),
  xlab = "Zeit",
  ylab = "LtPD",
  lwd = 2,
  main = "Incremental def. prob. h_k = p_k / (1 - q_{k-1})"
)
legend(
  'topright',
  'groups',
  c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
  lty = 1,
  cex = 0.75,
  col = rainbow(t.len)
)


