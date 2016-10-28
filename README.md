# propint

propint is an R package that implements a method for statistical inference for
proportions: a test for the difference of differences in proportions, i.e. the
interaction of proportions. The method was developed by Newcombe (2001).

The method uses confidence intervals of single proportions that are converted to a
confidence interval that estimates the difference between differences of
proportions. By default, confidence intervals are used that have been recommended by
Newcombe (Method 3, 1998a; Method 10, 1998b). However, confidence intervals relying
on other computations can be passed to the function that computes the interaction
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
interaction of independent proportions. Specifically, the differences of the
difference of two respective proportions is compared, i.e. the contrast (p1 - p2) -
(p3 - p4).

`ci.indep.interaction()` can be used in two different ways:

1. Observed frequencies are directly passed  ("successes" and "total trials" for each
   proportion). Proportions and confidence intervals are computed.
2. Proportions and boundaries of a confidence interval (e.g. 95% confidence interval)
   are directly passed to the function, because they have already been computed.

The following example illustrates usage 1 (example from Newcombe, 2001):
 
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

# (the notation of a, m, b and n is taken from Newcombe (1998b))
> ci.indep.interaction(ci=95, a1=17, m1=65, b1=17, n1=75, a2=18, m2=72, b2=16, n2=65)

$d
[1] 0.03102564

$l
[1] -0.1686752

$u
[1] 0.2343009

```

We obtain the difference between the differences d = (p1 - p2) - (p3 - p4) = 0.031
and the lower bound (l = -0.169) and the upper bound (u = 0.234) for the 95%
confidence interval of this difference. Because the interval contains 0, there is no
evidence for an interaction.

An alternative usage of `ci.indep.interaction()` (usage 2) is to compute confidence
intervals for each proportion first, and then call `ci.indep.interaction()`:

```

> # Alternative usage:
> p1 <- ci.one.prop(95, 17, 65)
> p2 <- ci.one.prop(95, 17, 75)
> p3 <- ci.one.prop(95, 18, 72)
> p4 <- ci.one.prop(95, 16, 65)

> # it is also possible to pass otherwise obtained confidence intervals to ci.indep.interactio()
> # ci.one.prop() is the default computation in propint (see Newcombe, 1998a)

> ci.indep.interaction(p1=p1$p, p2=p2$p, p3=p3$p, p4=p4$p, l1=p1$l, l2=p2$l, l3=p3$l, l4=p4$l,
>                      u1=p1$u, u2=p2$u, u3=p3$u, u4=p4$u)					 

$d 
[1] 0.03102564

$l
[1] -0.1686752

$u
[1] 0.2343009

```

As the code shows, it is possible to pass the lower and upper bounds of the four
confidence intervals and the proportions directly to `ci.indep.interaction()`. In the
illustrated case above, `ci.one.prop()` is used to determine the boundaries of the
95% confidence interval. `ci.one.prop()` is also part of the `propint` package and is
used as the default computation of confidence intervals of single proportions (see
Newcombe, 1998a). Consequently, you can pass confidence intervals to
`ci.indep.interaction()` that you computed using some other method. Also note that we
do not pass the confidence level in this case; `ci.indep.interaction()` computes the
same confidence level that is implied in the lower and upper boundaries of the four
proportions.

## Other functions

`propint` can be used for a classical test of the difference between two
proportions. The function `ci.two.indep.props()` computes a confidence interval of
the difference of two independent proportions (relying on method 10 in Newcombe,
1998). It can be used in two ways analogue to `ci.indep.interaction()`:

```

> # Usage 1:
> ci.two.indep.props(ci=95, a=56, m=70, b=48, n=80)

$d
[1] 0.2

$l
[1] 0.05243147

$u
[1] 0.3338727

> # Usage 2:
> p1 <- ci.one.prop(ci=95, r=56, n=70)
> p2 <- ci.one.prop(ci=95, r=48, n=80)
> ci.two.indep.props(p1=p1$p, p2=p2$p, l1=p1$l, l2=p2$l, u1=p1$u, u2=p2$u)

$d
[1] 0.2

$l
[1] 0.05243147

$u
[1] 0.3338727

```

We thus obtain the difference between the two proportions and the lower and upper
boundary for the difference of the proportions.

## More help

Use the help functions for more help on `propint's` functions in your R concole:

```
> library("propint")
> ?ci.one.prop
> ?ci.two.indep.props
> ?ci.indep.interaction
```

## Questions and suggestions

If you have any questions or suggestions (which are greatly appreciated), just open
an issue at Github or contact me via martin.papenberg at hhu.de.

## References 

Newcombe, R. G. (1998a). Two-sided confidence intervals for the single
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
