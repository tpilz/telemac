# t2d_opt object
opt_obj <- optionals(c("# test file", "col1 col2", "1 2", "3 4"), fname = "optional.txt")
opt_obj
str(opt_obj)

# values (lines of the file) as t2d_opt_LINES object
opt_obj$value
str(opt_obj$value)

# multiple optional files
opt_obj <- optionals(list(c("# test file", "col1 col2", "1 2", "3 4"),
                          c("# test file 2", "col1 col2", "5 6", "7 8")),
                     fname = c("optional.txt", "optional2.txt"))
print(opt_obj, n = 4)

# change individual values via list methods
opt_obj$value[[1]][3] <- c("10 20")
opt_obj
