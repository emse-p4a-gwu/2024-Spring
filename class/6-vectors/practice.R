# ------------------------------------------------
# Quick code tracing 1

rep(c(TRUE, FALSE, "TRUE"), 2)

seq(FALSE, 3)

rep(c(seq(3), seq(2)), each = 2)





# ------------------------------------------------
# Quick code tracing 2

f <- function(x) {
    m <- x
    n <- sum(x + 4)
    m <- m + 5
    return(c(m, n))
}

# What will this return?

x <- c(1, 3)
f(x)


y <- c(TRUE, FALSE, 1)
f(y)




# ------------------------------------------------
# Practice 1

# Summing with vectors (not loops)

test_sumFromMToN <- function() {
    cat("Testing sumFromMToN()...")
    stopifnot(sumFromMToN(1, 5) == 1 + 2 + 3 + 4 + 5)
    stopifnot(sumFromMToN(5, 10) == (5 + 6 + 7 + 8 + 9 + 10))
    stopifnot(sumFromMToN(1, 1) == 1)
    cat("Passed!")
}

sumFromMToN <- function(m, n) {
    # Write code here
}

test_sumFromMToN()


# 2) sumEveryKthFromMToN(m, n, k): Write a function to sum every kth integer 
#    from m to n.

test_sumEveryKthFromMToN <- function() {
    cat("Testing sumEveryKthFromMToN()...")
    stopifnot(sumEveryKthFromMToN(1, 10, 2) == (1 + 3 + 5 + 7 + 9))
    stopifnot(sumEveryKthFromMToN(5, 20, 7) == (5 + 12 + 19))
    stopifnot(sumEveryKthFromMToN(0, 0, 1) == 0)
    cat("Passed!")
}

sumEveryKthFromMToN <- function(m, n, k) {
    # Write code here
}

test_sumEveryKthFromMToN()



# 3) sumOfOddsFromMToN(m, n): Write a function that sums
#    every *odd* integer between m and n.

test_sumOfOddsFromMToN <- function() {
    cat("Testing sumOfOddsFromMToN()...")
    stopifnot(sumOfOddsFromMToN(4, 10) == (5 + 7 + 9))
    stopifnot(sumOfOddsFromMToN(5, 9) == (5 + 7 + 9))
    cat("Passed!")
}

sumOfOddsFromMToN <- function(m, n) {
    # Write code here
}

test_sumOfOddsFromMToN()




# ------------------------------------------------
# Practice 2

# Re-write isPrime(n) from last week, but **without loops!**.
# Remember, isPrime(n) takes a non-negative integer, n, and
# returns TRUE if it is a prime number and FALSE otherwise.

test_isPrime <- function() {
    cat("Testing isPrime()...")
    stopifnot(isPrime(1) == FALSE)
    stopifnot(isPrime(2) == TRUE)
    stopifnot(isPrime(7) == TRUE)
    stopifnot(isPrime(13) == TRUE)
    stopifnot(isPrime(14) == FALSE)
    cat("Passed!")
}

isPrime <- function(n) {
    # Write code here
}

test_isPrime()




# ------------------------------------------------
# Quick code tracing 3

f <- function(x) {
    for (i in seq(length(x))) {
        x[i] <- x[i] + sum(x) + max(x)
    }
    return(x)
}

# What will this return?

x <- c(1, 2, 3)
f(x)

# Main: x <- c(10, 27, 70)

c(10, 27, 70)




# ------------------------------------------------
# Practice 3

# 1) reverse(x): Write a function that returns the vector
# in reverse order. You cannot use the rev() function.

test_reverseVector <- function() {
    cat("Testing reverseVector()...")
    stopifnot(all(reverseVector(c(5, 1, 3)) == c(3, 1, 5)))
    stopifnot(all(reverseVector(c('a', 'b', 'c')) == c('c', 'b', 'a')))
    stopifnot(all(reverseVector(c(FALSE, TRUE, TRUE)) == c(TRUE, TRUE, FALSE)))
    cat("Passed!")
}

# Loop solution
reverseVector <- function(x) {
    # Write code here
}

test_reverseVector()

# Vector solution
reverseVector <- function(x) {
    # Write code here
}

test_reverseVector()




# 2) alternatingSum(a): Write a function that takes a vector
# of numbers a and returns the alternating sum, where the
# sign alternates from positive to negative or vice versa.

test_alternatingSum <- function() {
    cat("Testing alternatingSum()...")
    stopifnot(alternatingSum(c(5,3,8,4)) == (5 - 3 + 8 - 4))
    stopifnot(alternatingSum(c(1,2,3)) == (1 - 2 + 3))
    stopifnot(alternatingSum(c(0,0,0)) == 0)
    stopifnot(alternatingSum(c(-7,5,3)) == (-7 - 5 + 3))
    cat("Passed!")
}

# Loop solution
alternatingSum <- function(a) {
    # Write code here
}

test_alternatingSum()

# Vector solution
alternatingSum <- function(a) {
    # Write code here
}

test_alternatingSum()

