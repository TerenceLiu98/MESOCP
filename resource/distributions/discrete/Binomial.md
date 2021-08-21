---
title: "Binomial"
author: "Terence, Jun-Jie Liu"
date: "6/4/2021"
---

$$\text{Binomial Distribution: } X\sim Bin(n, p)$$

* PDF and Support: $\displaystyle f(x) = {n \choose x} p^x (1 - p)^{n - x}, \; x \in \{0, 1, \ldots, n\}$
* CDF: $F(x) = B(1 - p; n - x, x + 1), \; x \in \{0, 1, \ldots, n\}$ where $B$ is the incomplete beta function
* parameter: $n \in \{1,2,3,\dots \}$, the number of trials
* parameter: $p \in [0, 1]$, the probability of success
* mean: $np$
* Variance: $np(1-p)$
