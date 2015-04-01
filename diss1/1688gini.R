###	England 1688 gini allen p.50 from gregory king social table

library(ineq)

income <- rep(c(46.4,40.2,9.0,10.4,5.6,2.0),c(200.358,262.704,
	1190.552,1023.480,1970.895,1041.344))

ineq(income)

incomes <- spline(income)
incomesu <- unlist(incomes)
incomes1 <- incomesu[1:(length(incomesu/2))]

curve <- Lc(income,  n  =  rep(1,length(income)))

plot(curve,main="1688 English Lorenz curve",
	xlab="Cumulative share of population",
	ylab="Cumulative share of income",
	sub="Corrado Gini income coefficient = .429")
