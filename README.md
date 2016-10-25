# propint

propint is an R package that implements a method for statistical inference for
proportions: to test for the difference of differences in proportions, i.e. the
interaction of proportions. The method that is used was developed by Newcombe (2001).

The method relies on confidence intervals for single proportions that are converted
to a confidence interval for the difference between differences of proportions. By
default, confidence intervals are used that are recommended in Newcombe (1998a method
3; 1998 method 10). However, confidence intervals that have been computed using a
different method can also be passed to the function that computes the interaction
confidence interval.

## Installation

```
library("devtools") # if not available: install.packages("devtools")
install_github("m-Py/propint")

# load the package via 
library("propint")
```

## Usage

The function `ci.indep.interaction()` computes the confidence interval for an
interaction of proportions. Specifically, the differences of the difference of two
respective proportions is compared, i.e. $(p1 - p2)$ and $(p3 - p4)$.

This example illustrates the usage (example from Newcombe, 2001):

 
> The Bristol Antenatal Care Study sought to evaluate the effectiveness of routine
antenatal visits by asking whether choice improves well-being. Women were randomized
to either a traditional or a Lexible schedule of antenatal care. As one of the
outcomes, 277 women who developed a problem were asked whether their problem could
have been recognized earlier than it was. Among nulliparous women, the proportions
answering yes in the two groups were 17/65 and 17/75. Among multiparae, the
proportions were 18/72 and 16/65. While clearly none of the differences between these
four proportions are conventionally statistically significant, it is of interest to
assess the degree to which the effect of the intervention differs between these two
subsets of the parturient population, which it was anticipated could be quite
different.

To test for this interaction we use:

```

> ci.indep.interaction(ci=95, a1=17, m1=65, b1=17, n1=75, a2=18, m2=72, b2=16, n2=65)

$d
[1] 0.03102564

$l
[1] -0.1686752

$u
[1] 0.2343009

```

We obtain the difference between the differences (p1 - p2) and (p3 - p4) = 0.031
and the lower (-0.169) and the upper bound (0.234) for the confidence interval of
this difference. In this case we do not find evidence for an interaction effect,
because the interval contains 0.

An alternative usage would be to compute confidence intervals for single proportions first, and the call `ci.indep.interaction()`:

```

# Alternative usage:
p1 <- ci.one.prop(95, 17, 65)
p2 <- ci.one.prop(95, 17, 75)
p3 <- ci.one.prop(95, 18, 72)
p4 <- ci.one.prop(95, 16, 65)
ci.indep.interaction(p1=p1$p, p2=p2$p, p3=p3$p, p4=p4$p, l1=p1$l, l2=p2$l, l3=p3$l, l4=p4$l,
                     u1=p1$u, u2=p2$u, u3=p3$u, u4=p4$u)

$d 
[1] 0.03102564

$l
[1] -0.1686752

$u
[1] 0.2343009

```

It is possible to pass the lower and upper bounds for the 4 proportions directly to
`ci.indep.interaction()`. In this case, `ci.one.prop()` is used to determine the
boundaries of the 95% confidence interval. `ci.one.prop()` is also part of the
`propint` package and is used as the default computation of confidence intervals of
single proportions (see Newcombe, 1998a).

## References 

Newcombe, R. G. (1998a). Two‐sided confidence intervals for the single
    proportion: comparison of seven methods. *Statistics in medicine, 17*(8),
    857-872.

Newcombe, R. G. (1998b). Interval estimation for the difference between
    independent proportions: comparison of eleven methods. *Statistics in medicine,
    17*(8), 873-890.

Newcombe, R. G. (2001). Estimating the difference between differences: measurement
    of additive scale interaction for proportions. *Statistics in medicine, 20*(19),
    2885-2893.

Wilson, E. B. (1927). Probable inference, the law of succession, and statistical
   inference. *Journal of the American Statistical Association, 22*(158), 209-212.
