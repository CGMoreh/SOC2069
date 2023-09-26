
set.seed(123)
t <- data.frame(
  a = rnorm(100, 50, sd = 10),
  b = rnorm(100, 1000, sd = 150),
  c = sample(1:100, 100, replace = TRUE),
  f = rep(c("Group 1", "Group 2", "Group 3"), length.out = 100),
  g = sample(c(1, 2), 100, replace = TRUE),
  h = sample(c(0, 1, NA), 100, replace = TRUE, prob = c(90, 90, 10)),
  i = sample(c("Yep", "Nope"), 100, replace = TRUE),
  j = sample(c(TRUE, FALSE), 100, replace = TRUE),
  k = sample(c("No", "Sure"), 100, replace = TRUE),
  stringsAsFactors = FALSE ## default now = F
)

str(t)


## Variable types

t2 <- t

t2[, c("f", "g", "h", "i", "k")] <- lapply(t2[, c("f", "g", "h", "i", "k")], as.factor)


str(t2)



################# New columns ###################### 

## `with()` and `within()`

t$sum <- with(t,
              a*g)

# or #

t <- within(t,
            sum2 <- a*g)


t <- within(t,
            newvar <- rnorm(100, 10000, sd = 100))


############### Univariate plots ###################


plot(t$a)
# = #
with(t, plot(a))


plot(t$a,
     ylab = "Some label")



plot(t$a, type = "h")

points(t$a)




plot(t$i)
plot(factor(t$i))
plot(factor(t$i, labels = c("No", "Yes")))

### !!! DANGER ZONE !!! ###

plot(factor(t$i, labels = c(1, 0)))
plot(factor(t$i, labels = c("Yes", "No")))


### Labelling first


###

plot(t$j)
plot(factor(t$j))
plot(factor(t$i, labels = c("No", "Yes")))



## barplot



### Dotchart


dotchart(t$a)

dotchart(t$a, labels = row.names(t))


# select a random sample of 10 rows (i.e. cases) from `t`

# set.seed(123)
tsample <- t[sample(nrow(t), 10), ]

dotchart(tsample$a, row.names(tsample))

# select random sample of rows using `dplyr` and `tibble`

# set.seed(123)
tsample2 <- t |> 
  tibble::rownames_to_column("Row") |> 
  dplyr::slice_sample(n = 10) |> 
  tibble::column_to_rownames("Row")

dotchart(tsample2$a, rownames(tsample2))



## Histogram

hist(t$a, 
     xlab = "Label for 'a'", 
     main = "Title of the histogram")




############### Bivariate plots ###################

## Scatterplot

plot(t$a, t$b)




##### Piping and `within()`


# `{magrittr}` pipes for us to have
`%>%` <- magrittr::`%>%`        # pipe
`%$%` <- magrittr::`%$%`        # exposition pipe
`%<>%` <- magrittr::`%<>%`      # assignment pipe



### operations

summary(t$a)

t["b"] |> summary()

t[2] |> summary()

t[2, ] |> summary()


t |> dplyr::select(a) |> summary()

t %>% dplyr::select(a) %>% summary()


### Plotting

t |> plot(a, b)     # error

t %>% plot(a, b)    # error



# Isabela Velasquez explains how the pipes work: https://ivelasq.rbind.io/blog/understanding-the-r-pipe/

###### To do it in straight pipes

## With magrittr pipe:
t %>% {plot(.$a, .$b)}

## With the native pipe:

# verbosely
t |> (function(.) plot(.$a, .$b))()

# using the anonymous function shortcut, emulating the dot syntax
t |> (\(.) plot(.$a, .$b))()

# or if you prefer x to .
t |> (\(x) plot(x$a, x$b))()

# ... which is not much different to...

t |> (\(t) plot(t$a, t$b))()

# so... not really worth it


####### Straight piping `with()`

with(t,
     plot(a, b))

t |> with(
  plot(a, b))



###### Exposition piping

t %$% plot(a, b)    # works

