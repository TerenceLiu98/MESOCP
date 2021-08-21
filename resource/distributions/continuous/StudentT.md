---
title: "Student t distribution"
author: "Terence, Jun-Jie Liu"
date: "6/4/2021"
---

$\text{t distribution}$

* PDF and support: $f(x) = \frac{\Gamma \left(\frac{n + 1}{2} \right)} {\sqrt{n \pi} \Gamma \left(\frac{n}{2} \right)} \left(1 + \frac{x^2}{n} \right)^{-\frac{n + 1}{2}}, \; x \in (-\infty, \infty)$
* CDF: $F(x) = \frac{1}{2} + x \Gamma \left( \frac{n + 1}{2} \right) \frac{\,_2F_1 \left ( \frac{1}{2},\frac{n + 1}{2};\frac{3}{2}; - \frac{x^2}{n} \right)} {\sqrt{\pi n}\,\Gamma \left(\frac{n}{2}\right)}, \; x \in (-\infty, \infty)$
* parameter: $n \in (0, +\infty)$, degress of freedom
* mean: $0$ form $n > 1$
* variance $\frac{n}{n - 2}$, for $n > 2$