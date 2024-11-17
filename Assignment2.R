makeCacheVector <- function(x = numeric()) {
  cached_mean <- NULL
  set <- function(y) {
    x <<- y
    cached_mean <<- NULL
  }
  get <- function() x
  setMean <- function(mean) cached_mean <<- mean
  getMean <- function() cached_mean
  list(set = set, get = get, setMean = setMean, getMean = getMean)
}

cacheMean <- function(cacheVector, ...) {
  cached_mean <- cacheVector$getMean()
  if (!is.null(cached_mean)) {
    message("Getting cached mean")
    return(cached_mean)
  }
  data <- cacheVector$get()
  mean_value <- mean(data, ...)
  cacheVector$setMean(mean_value)
  mean_value
}

cachedVector <- makeCacheVector(c(1, 2, 3, 4, 5))
cacheMean(cachedVector)
cacheMean(cachedVector)
cachedVector$set(c(10, 20, 30, 40, 50))
cacheMean(cachedVector)
```