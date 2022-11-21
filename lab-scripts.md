---
layout: page
title: Lab Scripts/Problem Sets
permalink: /lab-scripts-problem-sets/
---

This page contains link to lab scripts throughout the semester. Clicking the title of the lab script will go directly to the ["spun"](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/) HTML document from the underlying R code. The bottom right icons link to the underlying R script (<i class="fab fa-r-project"></i>) and the HTML document (<i class="fas fa-file-code"></i>).


<ul id="archive">
{% for lectures in site.data.labscripts %}
      <li class="archiveposturl">
        <span><a href="{{ site.url }}/lab-scripts/{{ lectures.filename }}.html">{{ lectures.title }}</a></span><br>
<span class = "postlower">
<strong>tl;dr:</strong> {{ lectures.tldr }}</span>
<strong style="font-size:100%; font-family: 'Titillium Web', sans-serif; float:right; padding-right: .5em">

<a href="https://github.com/{{ site.githubdir}}/tree/master/lab-scripts/{{ lectures.filename}}.R"><i class="fab fa-r-project"></i></a>&nbsp;&nbsp;
<a href="{{ site.url }}/lab-scripts/{{ lectures.filename }}.html"><i class="fas fa-file-code"></i></a>
</strong> 
      </li>
{% endfor %}
</ul>

## Problem Sets

### [Problem Set #1](http://eh6105.svmiller.com/problem-sets/1/eh6105-ps1.pdf)

The first problem set makes use of the [Systemic Banking Crises Database II](http://svmiller.com/stevedata/reference/SBCD.html) in [`{stevedata}`](http://svmiller.com/stevedata) to learn about basic data summary, data exploration, and data manipulation.

### [Problem Set #2](http://eh6105.svmiller.com/problem-sets/2/eh6105-ps2.pdf)

The second problem set makes use of the [National Epidemiologic Survey on Alcohol and Related Conditions (NESARC)](http://svmiller.com/stevedata/reference/nesarc_drinkspd.html) data set in [`{stevedata}`](http://svmiller.com/stevedata) to learn about basic descriptive statistics, recoding things, and, importantly, how you should always read the codebook.


### [Problem Set #3](http://eh6105.svmiller.com/problem-sets/3/eh6105-ps3.pdf)

The third problem set makes use of some data available in [`{peacesciencer}`](http://svmiller.com/peacesciencer) to learn bivariate ordinary least squares (OLS) regression.


### [Problem Set #4](http://eh6105.svmiller.com/problem-sets/4/eh6105-ps4.pdf)

The fourth problem set makes use of [some simple (American) presidential election data](http://svmiller.com/stevedata/reference/election_turnout.html) in [`{stevedata}`](http://svmiller.com/stevedata) to learn about simple derivations of the OLS model (e.g. controls, fixed effects).

### [Problem Set #5](http://eh6105.svmiller.com/problem-sets/5/eh6105-ps5.pdf)

The final problem set makes use of [General Social Survey (GSS) data on attitudes about government spending](http://svmiller.com/stevedata/reference/gss_spending.html) in [`{stevedata}`](http://svmiller.com/stevedata) to learn about OLS model diagnostics. Students will also have the option of bootstrapping their regression model here if they want to go hardcore in the last question. Nothing in the course plan said I couldn't have you choose this path if I wanted.  😜