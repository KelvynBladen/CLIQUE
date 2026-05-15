
and = read.csv("mae_and.csv")
c_v1 = read.csv("mae_v1.csv")
c_v2 = read.csv("mae_v2.csv")
reg = read.csv("mae_reg.csv")

summary(and[-1])
summary(c_v1[-1])
summary(c_v2[-1])
summary(reg[-1])

colMeans(and[-1])
1.96 * apply(and[-1], 2, FUN = function(x) sd(x)/sqrt(50))

colMeans(c_v1[-1])
1.96 * apply(c_v1[-1], 2, FUN = function(x) sd(x)/sqrt(50))

colMeans(c_v2[-1])
1.96 * apply(c_v2[-1], 2, FUN = function(x) sd(x)/sqrt(50))

colMeans(reg[-1])
1.96 * apply(reg[-1], 2, FUN = function(x) sd(x)/sqrt(50))

