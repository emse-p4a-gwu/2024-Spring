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

# Loop solution from last week:
sumFromMToN <- function(m, n) {
    total <- 0
    for (i in seq(m, n)) {
        total <- total + i
    }
    return(total)
}

# Vector solution 1:
sumFromMToN <- function(m, n) {
    return(sum(seq(m, n)))
}

# Vector solution 2:
sumToN <- function(n) {
    return(n*(n+1)/2)
}
sumFromMToN <- function(m, n) {
    return(sumToN(n) - sumToN(m-1))
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

# Loop solution from last week:
sumEveryKthFromMToN <- function(m, n, k) {
    total <- 0
    for (i in seq(m, n, by = k)) {
        total <- total + i
    }
    return(total)
}

# Vector solution:
sumEveryKthFromMToN <- function(m, n, k) {
    return(sum(seq(m, n, k)))
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

# Loop solution from last week:
sumOfOddsFromMToN <- function(m, n) {
    total <- 0
    for (i in seq(m, n)) {
        if ((i %% 2) == 1) {
            total <- total + i
        }
    }
    return(total)
}

# Vector solution:
sumOfOddsFromMToN <- function(m, n) {
    # if m is even, start sequence from m + 1
    if ((m %% 2) == 0) {
        m <- m + 1
    }
    return(sum(seq(m, n, 2)))
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

# Loop solution from last week
isPrime <- function(n) {
    if (n <= 1) {
        return(FALSE)
    }
    if (n == 2) {
        return(TRUE)
    }
    for (i in seq(2, n - 1)) {
        if ((n %% i) == 0) {
            return(FALSE)
        }
    }
    return(TRUE)
}

# Vector solution
isPrime <- function(n) {
    if (n <= 1) {
        return(FALSE)
    }
    if (n == 2) {
        return(TRUE)
    }
    x <- seq(2, n - 1) # Check remainders for each number from 2 to n-1
    y <- n %% x # Compute remainders
    if (any(y == 0)) {
        return(FALSE)
    }
    return(TRUE)
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
    y <- x
    for(i in 1:length(x)) {
        y[i] <- x[length(x) - i + 1]
    }
    return(y)
}

test_reverseVector()

# Vector solution

reverseVector <- function(x) {
    indices <- seq(length(x), 1, by = -1)
    return(x[indices])
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
    total <- 0
    for (i in 1:length(a)) {
        if ((i %% 2) == 1) {
            total <- total + a[i]
        } else {
            total <- total - a[i]
        }
    }
    return(total)
}

test_alternatingSum()

# Vector solution

alternatingSum <- function(a) {
    ones <- rep(c(1, -1), length(a))
    ones <- ones[1:length(a)]
    return(sum(ones*a))
}

test_alternatingSum()

