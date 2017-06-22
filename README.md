# propint

`propint` is an R package that computes confidence intervals to test for 
interactions of independent or dependent proportions. It tests whether 
the difference between one pair of proportions is larger than the 
difference between a different pair of proportions. This method closely 
corresponds to a 2 x 2 ANOVA and was developed by Newcombe (2001).

`propint` can also be used to compute confidence intervals for simple 
pairs of (independent or dependent) proportions, and even to compute 
confidence intervals for higher order interaction (for example 
corresponding to a 2 x 2 x 2 ANOVA). All functions rely on confidence 
intervals of single proportions that are converted into "higher-order" 
confidence intervals using recommendations and developments by Newcombe 
(1998a, 1998b, 2001). Confidence intervals for single proportions are 
computed based on »Wilson's score« (1927) as recommended in Newcombe 
(Method 3, 1998c). 

## Installation

```R
library("devtools") # if not available: install.packages("devtools")
install_github("m-Py/propint")

# load the package via 
library("propint")
```

## Interaction of independent proportions

The function `ci.indep.interaction()` computes the confidence interval 
for an interaction of independent proportions. Specifically, a 
confidence interval is computed for the differences of the difference of 
two pairs proportions is compared, i.e. the contrast (p1 - p2) - (p3 - 
p4). If the confidence interval of this difference of differences does
not cover zero, we would assume an interaction.

`ci.indep.interaction()` can be used in two different ways:

1. Observed frequencies are directly passed  ("successes" and "total 
trials" for each of the four proportions).
2. Proportions and boundaries of a confidence interval (e.g. 95% 
confidence interval) are directly passed to the function, because they 
have already been computed.

The following example illustrates usage 1 (example from Newcombe, 2001):
 
> The Bristol Antenatal Care Study sought to evaluate the effectiveness 
of routine antenatal visits by asking whether choice improves 
well-being. Women were randomized to either a traditional or a Lexible 
schedule of antenatal care. As one of the outcomes, 277 women who 
developed a problem were asked whether their problem could have been 
recognized earlier than it was. Among nulliparous women, the proportions 
answering yes in the two groups were 17/65 and 17/75. Among multiparae, 
the proportions were 18/72 and 16/65. While clearly none of the 
differences between these four proportions are conventionally 
statistically significant, it is of interest to assess the degree to 
which the effect of the intervention differs between these two subsets 
of the parturient population, which it was anticipated could be quite 
different.

To test for this interaction we use:

```R
# (the notation of a, m, b and n is taken from Newcombe (1998b))
> ci.indep.interaction(ci=95, a1=17, m1=65, b1=17, n1=75, 
>                      a2=18, m2=72, b2=16, n2=65)

$d
[1] 0.03102564

$l
[1] -0.1686752

$u
[1] 0.2343009

```

We obtain the difference between the differences d = (p1 - p2) - (p3 - 
p4) = 0.031 and the lower bound (l = -0.169) and the upper bound (u = 
0.234) for the 95% confidence interval of this difference. Because the 
interval contains 0, there is no evidence for an interaction.

An alternative usage of `ci.indep.interaction()` is to compute confidence
intervals for each proportion first, and then call `ci.indep.interaction()`:

```R
> # Alternative usage:
> p1 <- ci.one.prop(95, 17, 65)
> p2 <- ci.one.prop(95, 17, 75)
> p3 <- ci.one.prop(95, 18, 72)
> p4 <- ci.one.prop(95, 16, 65)

> ci.indep.interaction(p1=p1$p, p2=p2$p, p3=p3$p, p4=p4$p, l1=p1$l, l2=p2$l, 
>                      l3=p3$l, l4=p4$l, u1=p1$u, u2=p2$u, u3=p3$u, u4=p4$u)

$d 
[1] 0.03102564

$l
[1] -0.1686752

$u
[1] 0.2343009

```

As the code shows, it is possible to pass the lower and upper bounds of 
the four confidence intervals and the proportions directly to 
`ci.indep.interaction()`. In the illustrated case above, `ci.one.prop()` 
is used to determine the boundaries of the 95% confidence interval. 
`ci.one.prop()` is also part of the `propint` package and is used as the 
default computation of confidence intervals of single proportions (see 
Newcombe, 1998c). Consequently, you can pass confidence intervals to 
`ci.indep.interaction()` that you computed using some other method. Also 
note that we do not pass the confidence level in this case; 
`ci.indep.interaction()` computes the same confidence level that is 
implied in the lower and upper boundaries of the four proportions.

## Interaction of dependent proportions ("mixed interaction")

To be written 

## Pairwise comparison of independent proportions

`propint` can be used for a classical test of the difference between two 
proportions. The function `ci.two.indep.props()` computes a confidence 
interval of the difference of two independent proportions (relying on 
method 10 in Newcombe, 1998b). It can be used in two ways analogue to 
`ci.indep.interaction()`:

```R
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

We thus obtain the difference between the two proportions and the lower 
and upper boundary for the difference of the proportions.

## Pairwise comparison of dependent proportions

To be written

## More help

Use the help functions for more help on `propint's` functions in your R 
console:
```R
> library("propint")
> ?ci.one.prop
> ?ci.two.indep.props
> ?ci.two.dep.props
> ?ci.indep.interaction
> ?ci.mixed.interaction
```

## Questions and suggestions

If you have any questions or suggestions (which are greatly 
appreciated), just open an issue at Github or contact me via 
martin.papenberg at hhu.de.

## References 

Newcombe, R. G. (1998a). Improved confidence intervals for the 
difference between binomial proportions based on paired data. 
*Statistics in medicine, 17*(22), 2635-2650.
    
Newcombe, R. G. (1998b). Interval estimation for the difference between 
independent proportions: comparison of eleven methods. *Statistics in 
medicine, 17*(8), 873-890.
    
Newcombe, R. G. (1998c). Two-sided confidence intervals for the single 
proportion: comparison of seven methods. *Statistics in medicine, 
17*(8), 857-872.

Newcombe, R. G. (2001). Estimating the difference between differences: 
measurement of additive scale interaction for proportions. *Statistics 
in medicine, 20*(19), 2885-2893.

Wilson, E. B. (1927). Probable inference, the law of succession, and 
statistical inference. *Journal of the American Statistical Association, 
22*(158), 209-212.
