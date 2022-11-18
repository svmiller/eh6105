---
layout: page
title: Course Materials
permalink: /course-materials/
---

{% include image.html url="/_images/g-kw.png" caption="The Books for the Semester" width=400 align="right" %}

## Required Books

Gujarati, Damodar. 2014. *Econometrics by Example*. 2nd ed. Bloomsbury Academic.

Kellstedt, Paul M. and Guy D. Whitten. 2013. *The Fundamentals of Political Science Research*. 2nd ed. New York, NY: Cambridge University Press.

<!-- Best, Henning and Christof Wolf (eds.). 2014. [The SAGE Handbook of Regression Analysis and Causal Inference](https://www.amazon.com/Handbook-Regression-Analysis-Causal-Inference/dp/1446252442). London, UK: SAGE Publications, Ltd. -->

<br />
<br />
<br />
<br />
<br />

## A Comment on Assigned Quantitative Methods Books and Graduate-Level Methods Instruction

The decision on required books are made months in advance of the actual semester. I have the following two-pronged perspective to graduate-level methods instruction. One, the student should read and read widely. Two, no one reading will be sufficient to learn quantitative methods and students will need a bricolage approach to make sense of the material. The assigned books generally serve two purposes. Kellstedt and Whitten (2013) is a gentler text, aimed more at communicating the underlying concepts. Gujarati (2014) will be more about implementation.

I want to add a few *recommended* books the students may find useful, at least because I found them incredibly useful myself. I highly recommend [the Gelman and Hill (2007) book](https://www.amazon.com/Analysis-Regression-Multilevel-Hierarchical-Analytical/dp/B00QAV71GO) in particular. I owe much to this book (and my own stubborn persistence in teaching myself quantitative methods) even if much of the implementation they offer looks dated relative to recent advances in `R` (see also: [Gelman et al., 2020](https://www.cambridge.org/core/books/regression-and-other-stories/DD20DD6C9057118581076E54E40C372C)). I also strongly recommend reading the [Ziliak and McCloskey (2008)](https://www.amazon.com/Cult-Statistical-Significance-Economics-Cognition/dp/0472050079) book to better appreciate what quantitative methods cannot tell us and how easy it is to misinterpret what these methods are doing for us as researchers. Interested students should first look at these books at the library or on Google Preview and decide if they'd like to learn from these. I have e-versions of almost all these books that I can share as well.


## R and Rstudio

Lab sessions and problem sets (more in the next section) will all be done in the `R` programming language. Students should download this free software programming language at [cran.r-project.org](http://cran.r-project.org) and install it on their personal computer. Binaries are available for Windows and Mac (even Linux, if that is the weapon of choice for the student).

- The `R` scripts I provide are designed to work on the student’s computer with minimal maintenance. I will make this clear in each particular script.

- I *strongly* encourage students to contact me to learn about the language. I will assume that not discussing `R` with me means the student is fluent with the software.

- Consider getting a graphical user interface (GUI) front-end for `R` to learn it better. I recommend RStudio, available for free at [`posit.co`](https://posit.co/download/rstudio-desktop/). Do note there is a paid option of Rstudio that you *do **not** want.* The paid version is for servers. You want the basic open source integrated development environment (IDE). *This is **free***. Do not pay for anything related to R or Rstudio since you do not need whatever product is available for purchase.


I published a [beginner's guide to using `R`](http://svmiller.com/blog/2014/08/a-beginners-guide-to-using-r/) in 2014 when I first started to teach courses that forced students to use the `R` programming language. I have since streamlined the `R` requirements for this class, making that guide somewhat dated. You will need to install the following packages, which I illustrate here with the `R` commands to install them. 

```r
install.packages("tidyverse")      # for most things workflow
# ^ This is a huge installation. It should take a while.
install.packages("peacesciencer")  # some peace science data
install.packages("stevedata")      # for toy data sets to use in-class
install.packages("stevemisc")      # for some helper functions
install.packages("stevetemplates") # for preparing reports
install.packages("lmtest")         # for model diagnostics
```

The aforementioned `R` packages are not exhaustive of all packages a student may use this semester, and I reserve the right to ask students to install additional packages along the way (though these requests will ideally be rare). I will make this clear in each lab session and problem set.

