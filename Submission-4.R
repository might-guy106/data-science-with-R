
# Question no 1
A <- matrix(runif(1000000), nrow = 1000, ncol = 1000)
print(A)

# loop method
ti_l <- Sys.time()
  norms_loop <- numeric(ncol(A))
  for (i in 1:ncol(A)) {
    norms_loop[i] <- norm(A[, i], type = "2")
  }
tf_l <- Sys.time()
time_loop <- tf_l - ti_l
print(norms_loop)

# sapply method
ti_s <- Sys.time()
  norms_sapply <- sapply(A, norm, type = "2")
tf_s <- Sys.time()
time_sapply <- tf_s - ti_s
print(norms_sapply)

# HERE, SAPPLY METHOD (21s) TAKES FAR MORE TIME AS COMPARED TO THE LOOP METHOD(2s)
if (time_sapply > time_loop) {
  print("Sapply takes more time than Loop")
}


# Question no 2
n <- 1000
m <- 1000
A <- matrix(runif(n*m), nrow = n, ncol = m)
B <- matrix(rnorm(n*m), nrow = n, ncol = m)
x <- runif(m)
ABtx <- x %*% t(A %*% B)
# we have rearranged the multiplication order since
# the earlier code involved multiplication of two large matrices.

# Question no 3
load("C:/Users/kkart/Downloads/ques3.Rdata")
eigenvalues <- eigen(mat, only.values = TRUE)$values
det_A <- prod(eigenvalues)
trace_A <- sum(eigenvalues)
p <- 2
ans <- (det_A^(1/p)) * factorial(p) * ((2.7)^p) / trace_A * (p^p)
ans

# Question no 4
n <- 50
m <- 1e3
A <- matrix(runif(n*m), nrow = n, ncol = m)

# instead of using loops, we are directly using vectorized operations
# to perfom L2 norm.
norms <- sqrt(colSums(A^2))
p_vec <- norms / sum(norms)
chosen <- sample(1:m, size = 1, prob = p_vec)
chosen

# Question no 5
# even here we will avoid the loop using vectorized operations, to improve it
autoreg_fast <- function(n, rho) {
  errors <- rnorm(n) # here we generate all random errors at once
  out <- numeric(n)
  out[1] <- errors[1]
  out[-1] <- rho * out[-n] + errors[-1]
# this step will serve the purpose of loop. 
  return(out)
}

# Question no 6

sumsC <- function(mat,s) {
  col_sum <- colSums(mat[,1:s])
  #col_sum has p elements, each contains sum of elements in that column
  return(sum(col_sum))
}

sel_sums <- function(mat) {
  p <- ncol(mat)
  s <- sample(1:p, size=1) # Task-2
  output <- sumsC(mat,s) # Task-3
  return(output) # Task-4
}





