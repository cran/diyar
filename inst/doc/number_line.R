## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE-------------------------------------------------------
plot_number_line <- diyar:::plot_number_line

## ----include=FALSE-------------------------------------------------------
library(diyar)
exp <- number_line(8, 1)

## ----message=FALSE, warning=FALSE----------------------------------------
library(diyar)

nl_a <- number_line(l=c(3,5), r = c(4,1))

# `number_line` objects
nl_a

# lower end of the range
start_point(nl_a)

# left part of the range
left_point(nl_a)

## ----echo=FALSE, fig.height=1.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(nl_a)

## ----message=FALSE, warning=FALSE----------------------------------------
# Only reverse decreasing `number_line` objects
nl_b <- reverse_number_line(nl_a, direction = "decreasing"); nl_b

## ----echo=FALSE, fig.height=1.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(nl_b)

## ----message=FALSE, warning=FALSE----------------------------------------
nl_c <- number_line(as.Date("04/04/2019", "%d/%M/%Y"),
                    as.Date("07/04/2019", "%d/%M/%Y"))

nl_c

# expand the `number_line` object by 2 days from both ends 
nl_d <- expand_number_line(nl_c, 2); nl_d

# expand the `number_line` object by 2 days from the lower end
nl_e <- expand_number_line(nl_c, 2, "start"); nl_e

# shrink the `number_line` object by 2 days from the upper end
nl_f <- expand_number_line(nl_c, -2, "end"); nl_f

# shrink the `number_line` object by 2 days from both ends
nl_g <- expand_number_line(nl_c, -2); nl_g

# reverse the direction of the `number_line`
nl_h <- reverse_number_line(nl_c); nl_h

# shift the `number_line` object towards the left of the number line by 2 days 
nl_i <- shift_number_line(nl_c, -2); nl_g

# shift the `number_line` object towards the right of the number line by 2 days 
nl_j <- shift_number_line(nl_c, 2); nl_i

## ----echo=FALSE, fig.height=2.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(rev(c(nl_c, nl_d, nl_e, nl_f, nl_g, nl_h, nl_i)))

## ----message=FALSE, warning=FALSE----------------------------------------
inv <- number_line(c(3,-3,3,-3), c(-6,6,6,-6)); inv

inv
invert_number_line(inv)
invert_number_line(inv, "left")
invert_number_line(inv, "start")
invert_number_line(inv, "right")
invert_number_line(inv, "end")


## ----message=FALSE, warning=FALSE----------------------------------------
nls <- c(nl_c, nl_d, nl_e, nl_f, nl_g, nl_h, nl_i, nl_j)

nls

number_line_sequence(nls, by=2)

number_line_sequence(nls, length.out = 3)

## ----message=FALSE, warning=FALSE----------------------------------------
exact <- c(number_line(1,2), number_line(1,2),
           # negative test - start_point() and end_point() 
           # for both must be identical to be an exact mactch
           number_line(3,4), number_line(4,3), 
           number_line(2.5,2.5), number_line(2.5,2.5))

# positive logical test
exact(exact[c(1,3,5)], exact[c(2,4,6)])
# negtaive logical test
across(exact[c(1,3,5)], exact[c(2,4,6)])
# check overlap methods as defined in `diyar`
overlap_method(exact[c(1,3,5)], exact[c(2,4,6)])

## ----echo=FALSE, fig.height=1.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(exact, show_overlap=T)

## ----message=FALSE, warning=FALSE----------------------------------------
inbetween <- c(number_line(5, 8), number_line(6, 7), 
               number_line(11, 10), number_line(9, 12), 
               number_line(13, 15), as.number_line(14))

# positive logical test
inbetween(inbetween[c(1,3,5)], inbetween[c(2,4,6)])
# negtaive logical test
across(inbetween[c(1,3,5)], inbetween[c(2,4,6)])
# check overlap methods as defined in `diyar`
overlap_method(inbetween[c(1,3,5)], inbetween[c(2,4,6)])

## ----echo=FALSE, fig.height=1.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(inbetween, show_overlap=T)

## ----message=FALSE, warning=FALSE----------------------------------------
across <- c(number_line(16, 19), number_line(18, 21), 
            number_line(22, 25), number_line(23, 26),
            number_line(27, 30), number_line(31, 29))

# positive logical test
across(across[c(1,3,5)], across[c(2,4,6)])
# negtaive logical test
inbetween(across[c(1,3,5)], across[c(2,4,5)])
# check overlap methods as defined in `diyar`
overlap_method(across[c(1,3,5)], across[c(2,4,6)])

## ----echo=FALSE, fig.height=1.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(across, show_overlap=T)

## ----message=FALSE, warning=FALSE----------------------------------------
chain <- c(number_line(32, 34), number_line(34, 36), 
            number_line(39, 37), number_line(41, 39), 
           # negative test - end_point() of one must lead to the start_point() 
           # to be considered a chain overlap
           number_line(42, 44), number_line(47, 44))

# positive logical test
chain(chain[c(1,3,5)], chain[c(2,4,6)])
# negtaive logical test
across(chain[c(1,3,5)], chain[c(2,4,6)])
# check overlap methods as defined in `diyar`
overlap_method(chain[c(1,3,5)], chain[c(2,4,6)])

## ----echo=FALSE, fig.height=1.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(chain, show_overlap=T)

## ----message=FALSE, warning=FALSE----------------------------------------
aligns_start <- c(number_line(45, 46), number_line(45, 47), 
            number_line(48, 49.5), number_line(48, 48),
            number_line(51, 50), number_line(51, 52))

# positive logical test
aligns_start(aligns_start[c(1,3,5)], aligns_start[c(2,4,6)])
# negtaive logical test
across(aligns_start[c(1,3,5)], aligns_start[c(2,4,6)])
# check overlap methods as defined in `diyar`
overlap_method(aligns_start[c(1,3,5)], aligns_start[c(2,4,6)])

## ----echo=FALSE, fig.height=1.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(aligns_start, show_overlap=T)

## ----message=FALSE, warning=FALSE----------------------------------------
aligns_end <- c(number_line(54, 55), number_line(53, 55), 
            number_line(56, 57.5), number_line(57.5, 57.5), 
            number_line(58, 59), number_line(60, 59))

# positive logical test
aligns_end(aligns_end[c(1,3,5)], aligns_end[c(2,4,6)])
# negtaive logical test
across(aligns_end[c(1,3,5)], aligns_end[c(2,4,6)])
# check overlap methods as defined in `diyar`
overlap_method(aligns_end[c(1,3,5)], aligns_end[c(2,4,6)])

## ----echo=FALSE, fig.height=1.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(aligns_end, show_overlap=T)

## ----message=FALSE, warning=FALSE----------------------------------------
a <- number_line(1,10); b <- number_line(5, 15)
c <- union_number_lines(a, b)
a; b; c

## ----echo=FALSE, fig.height=1.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(c(a,b,c), strata = c(1,1,0), show_overlap=F)

## ----message=FALSE, warning=FALSE----------------------------------------
c <- intersect_number_lines(a, b)
a; b; c

## ----echo=FALSE, fig.height=1.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(c(a,b,c), strata = c(1,1,0), show_overlap=F)

## ----message=FALSE, warning=FALSE----------------------------------------
c <- subtract_number_lines(a, b)
a; b; c

## ----echo=FALSE, fig.height=1.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(c(a,b,c$n1, c$n2), strata = c(1,1,0,0), show_overlap=F)

## ----message=FALSE, warning=FALSE----------------------------------------
cnl_a <- c(inbetween, exact); cnl_a

cmp_a <- compress_number_line(cnl_a, deduplicate = T); cmp_a

## ----echo=FALSE, fig.height=2.5, fig.width=8.5, message=FALSE, warning=FALSE----
f <- number_line(left_point(c(cnl_a, cmp_a)), right_point(c(cnl_a, cmp_a)), gid =c(cnl_a@gid, cmp_a@gid), id =c(cnl_a@id, cmp_a@id)  )
strata <- c(rep(0, length(cnl_a)), rep(1, length(cmp_a)))
plot_number_line(f, strata = strata, show_overlap=T)

## ----message=FALSE, warning=FALSE----------------------------------------
cnl_b <- c(across, chain); cnl_b

cmp_b <- compress_number_line(cnl_b, deduplicate = T); cmp_b

## ----echo=FALSE, fig.height=2.5, fig.width=8.5, message=FALSE, warning=FALSE----
f <- number_line(left_point(c(cnl_b, cmp_b)), right_point(c(cnl_b, cmp_b)), gid =c(cnl_b@gid, cmp_b@gid), id =c(cnl_b@id, cmp_b@id)  )
strata <- c(rep(0, length(cnl_b)), rep(1, length(cmp_b)))
plot_number_line(f, strata = strata, show_overlap=T)

## ----message=FALSE, warning=FALSE----------------------------------------
cnl_c <- c(aligns_start, aligns_end); cnl_c

cmp_c <- compress_number_line(cnl_c, deduplicate = T, methods ="aligns_start|exact"); cmp_c

## ----echo=FALSE, fig.height=2.5, fig.width=8.5, message=FALSE, warning=FALSE----
f <- number_line(left_point(c(cnl_c, cmp_c)), right_point(c(cnl_c, cmp_c)), gid =c(cnl_c@gid, cmp_c@gid), id =c(cnl_c@id, cmp_c@id))
strata <- c(rep(0, length(cnl_c)), rep(1, length(cmp_c)))
plot_number_line(f, strata = strata, show_overlap=T)

## ----message=FALSE, warning=FALSE----------------------------------------
nl_z <- number_line(l= c(1,2,4,5,6), r = c(3,4,8,8,7))
nl_z

## ----echo=FALSE, fig.height= 2, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(nl_z, show_overlap=T)

## ----message=FALSE, warning=FALSE----------------------------------------
cmp_z <- compress_number_line(nl_z, methods = "across|chain")

cmp_z

## ----echo=FALSE, fig.height= 2, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(cmp_z, show_overlap=T)

## ----message=FALSE, warning=FALSE----------------------------------------
nl_zb <- nl_z[c(2:5,1)]; nl_zb

cmp_zb <- compress_number_line(nl_zb, methods = "across|chain"); cmp_zb

## ----echo=FALSE, fig.height=1.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(cmp_zb, show_overlap=T)

## ----message=FALSE, warning=FALSE----------------------------------------
cmp_zba <- compress_number_line(nl_z, methods = "across|chain", collapse = T); cmp_zba

## ----echo=FALSE, fig.height=1.5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_number_line(cmp_zba, show_overlap=T)

