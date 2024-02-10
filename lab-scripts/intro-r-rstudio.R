#' ---
#' title: "An Intro to R, Rstudio, and `{tidyverse}`"
#' author: Steven V. Miller, [svmiller.com](http://svmiller.com)
#' date: 23 Nov. 2022
#' abstract: "This is a lab script for [EH6105](http://eh6105.svmiller.com), a graduate-level quantitative 
#' methods class that I teach at Stockholm University. It will not be the most sophisticated 
#' R-related write-up of mine---check [my blog](http://svmiller.com/blog) for those---but it should be useful 
#' for discussion around the associated R script for the week's 'lab' session."
#' output:
#'    html_document:
#'      css: lab-script.css
#'      toc: TRUE
#'      toc_float:
#'        collapsed: false
#'        smooth_scroll: false
#'      highlight: zenburn
#' ---

#' # Elsewhere in the R `"Steveverse"`
#' 
#' Some of what I offer here may be (aggressively) plagiarized from other resources I've made available.
#' In particular, check out [this near identical guide](http://post8000.svmiller.com/lab-scripts/intro-r-rstudio.html) 
#' I made available for graduate students at my previous employer. I have 
#' [a somewhat dated guide](http://svmiller.com/blog/2014/08/a-beginners-guide-to-using-r/) on my website too.
#' 
#' 
#' 
#' # Configure Rstudio
#' 
#' When you're opening R for the very first time, it'll be useful to just get a general sense of what's happening.
#' I have [a beginner's guide that I wrote in 2014](http://svmiller.com/blog/2014/08/a-beginners-guide-to-using-r/) 
#' (where did the time go!). Notice that I built it around [Rstudio](https://rstudio.com/products/rstudio/), 
#' which you should download as well. Rstudio desktop is free. 
#' Don't pay for a "pro" version.  You're not running a server. You won't need it. 
#' 
#' When you download and install Rstudio *on top* of R, you should customize it just a tiny bit to 
#' make the most of the graphical user interface. To do what I recommend doing, select "Tools" in the menu. 
#' Scroll to "global options" (which should be at the bottom). On the pop-up, select "pane layout." Rearrange it so that "Source" is top left, "Console"
#' is top right, and the files/plots/packages/etc. is the bottom right. Thereafter: apply the changes.
#' 
#' ![](http://post8000.svmiller.com/intro-r-rstudio/rstudio-global-options.png)
#' 
#' You don't have to do this, but I think you should since it better economizes space in Rstudio. The other pane 
#' (environment/history, Git, etc.) is stuff you can either learn to not need (e.g. what's in the environment) 
#' or will only situationally need at an advanced level (e.g. Git information). Minimize 
#' that outright. When you're in Rstudio, much of what you'll be doing leans on the script 
#' window and the console window. You'll occasionally be using the file browser and plot panes as well.
#' 
#' If you have not done so already, open a new script (Ctrl-Shift-N in Windows/Linux or Cmd-Shift-N in Mac) 
#' to open a new script.
#' 
#' # Get Acclimated in R
#' 
#' Now that you've done that, let's get a general sense of where you are in an R session. 
#' 
#' ## Current Working Directory
#' 
#' First, let's start with identifying the current working directory. You should know where 
#' you are and this happens to be where I am, given the location of this script.

getwd()

#' Of note: by default, R's working directory is the system's "home" directory. This is somewhat 
#' straightforward in Unix-derivative systems, where there is an outright "home" directory. 
#' Assume your username is "steve", then, in Linux, your home directory will be "/home/steve". In Mac, 
#' I think it's something like "/Users/steve". Windows users will invariably have something 
#' clumsy like "C:/Users/steve/Documents". Notice the forward slashes. 
#' R, like everything else in the world, uses forward slashes. The backslashes owe to 
#' Windows' derivation from DOS.
#' 
#' ## Create "Objects"
#' 
#' Next, let's create some "objects." R is primarily an "object-oriented" programming language. 
#' In as many words, inputs create outputs that may be assigned to objects in the workspace. 
#' You can go nuts here. Of note: I've seen R programmers use `=`, `->`, and `<-` 
#' interchangeably for object assignment, but I've seen instances where `=` doesn't work as 
#' intended for object assignment. `->` is an option and I use it for assignment
#' for some complex objects in a "pipe" (more on that later). 
#' For simple cases (and for beginners), lean on `<-`.
#' 

a <- 3
b <- 4 
A <- 7
a + b
A + b

# what objects did we create?
# Notice we did not save a + b or A + b to an object
# Also notice how a pound sign creates a comment? Kinda cool, right? Make comments to yourself.
ls()

#' Some caution, though. First, don't create objects with really complex names. To call them back 
#' requires getting every character right in the console or script.  Why inconvenience yourself? 
#' Second, R comes with some default objects that are kinda important and can seriously ruin 
#' things downstream. I don't know off the top of my head all the default objects in R, but there 
#' are some important ones like `TRUE`, and `FALSE` that you DO NOT want to overwrite. 
#' `pi` is another one you should not overwrite, and `data` is a function that serves a specific
#' purpose (even if you probably won't be using it a whole lot).
#' You can, however, assign some built-in objects to new objects.

this_Is_a_long_AND_WEIRD_objEct_name_and_yOu_shoUld_not_do_this <- 5
pi # notice there are a few built-in functions/objects
d <- pi # you can assign one built-in object to a new object.
# pi <- 3.14 # don't do this....

#' If you do something dumb (like overwrite `TRUE` with something), all hope is not lost. 
#' Just remove the object in question with the `rm()` command.
#' 
#' ## Install/Load Libraries
#' 
#' R depends on user-created libraries to do much of its functionality. This class will lean on just a 
#' few R libraries. The first, `{tidyverse}` is our workhorse for workflow. It'll also be 
#' the longest to install because it comes with lots of dependencies to maximize its 
#' functionality. [`{stevedata}`](http://svmiller.com/stevedata) contains toy 
#' data sets that I use for in-class instruction, and we'll make use of these 
#' data in these lab sessions (and in your problem sets). 
#' [`{stevemisc}`](http://svmiller.com/stevemisc) contains assorted helper functions
#' that I wrote for my research, which we'll also use in this class. 
#' [`{stevetemplates}`](http://svmiller.com/stevetemplates)  is not strictly necessary, 
#' but it will make doing your homeworks infinitely easier (even if you're not a LaTeX user).
#' `{lmtest}`, which is not a package I maintain, does various model diagnostics for OLS.
#' 
#' I *may*---and probably will, to be honest---ask you to install various other packages 
#' that I think you should have installed. Already, I can see that the last problem set
#' is going to be a "choose your adventure" at the end, and request that you have
#' either the `{fixest}` or `{modelr}` package installed. I hope to keep these
#' situations to a minimum.
#' 
#' If any of these result in a "non-zero exit status", that's R's way of saying "I
#' couldn't install this." For you Mac users, the answer to this situation is *probably*
#' "update [Xcode](https://developer.apple.com/xcode/)." Xcode is a developer tool suite for Apple,
#' and many of the `{tidyverse}` packages require access to developmental libraries that, 
#' to the best of my understanding, are available in Xcode. In all likelihood, you're a first-time
#' user who has not had to think about software development (and so you haven't updated Xcode since
#' you first got your Macbook). You might have to do that here.
#' 
#' For you Windows users: I think I've figured out what this may look like for you based on my recent
#' foray into the university's computer labs. The Windows corollary to Xcode is Rtools, which you *don't* have
#' installed by default (because it's not a Microsoft program, per se). You'll need to install it. 
#' First, take inventory of what version of R you have (for the university's computer labs, it should be 
#' 4.0.5). [Go to this website](https://cran.r-project.org/bin/windows/Rtools/) and download the 
#' version of Rtools that corresponds with the version of R you have. Just click through all the default
#' options so that it can install. Next, in Rstudio, open a new blank file and copy-paste the following code
#' into it.
#' 
# PATH="${RTOOLS40_HOME}\usr\bin;${PATH}"

#' I'll add the caveat that you should remove the hashtag and space preceding that line.
#' 
#' Next, save the file as `.Renviron` in your default working directory, which is probably where you are
#' if you are using Rstudio for the first time. The save prompt from Rstudio will advise you that this is 
#' no longer an `.R` file (and, duh, just tell it to save anyway). Afterwards, restart Rstudio and try
#' again. This *should* fix it, based on my recent trial run in the university's computer labs.
#' 
#' For you Linux users: you're awesome, have great hair, everyone likes you, and 
#' you don't need to worry about a thing, *except* the various developmental 
#' libraries you may have to install from your package repository. My flavor of 
#' Linux is in the Debian/Ubuntu family, so 
#' [here's an (incomplete) list of developmental libraries](http://svmiller.com/blog/2019/07/notes-to-self-new-linux-installation-r-ubuntu/) 
#' based on my experience. Helpfully, most R packages that fail this way will 
#' tell you what development library you need, whether in you're in the Debian 
#' or Red Hat family.
#' 
#' 
#' If you have yet to install these packages (and you almost certainly have not if you're opening R for the first time), install it as follows.
#' Note that I'm just commenting out this command so it doesn't do this when I compile this script on my end. 
# Take out the comment...
install.packages(c("tidyverse", "stevedata", "stevemisc", "stevetemplates", "lmtest"))

#' Once they're all installed, you can load the libraries with the `library()` command. 
#' Of note: you only need to install a package once, but you'll need to load the library 
#' for each R session. You won't really need to load `{stevetemplates}` for anything since
#' it's core functionality is its integration with Rstudio. Let's load `{tidyverse}` and
#' `{stevedata}` in this session, since it's what I'll typically use.
#' 
library(tidyverse)
library(stevedata)

#' For those of you that are having `{tidyverse}` installation issues because of
#' `{systemfonts}` needing some font-related development libraries, try this:
#' 
library(tibble)    # special data type we'll use
library(magrittr)  # pipe operator
library(dplyr)     # the workhorse
library(readr)     # for reading particular data types.
library(stevedata) # for data

#' These are the core packages that are in `{tidyverse}` that you should have 
#' installed. Having `{tidyverse}` loads all of these. It's basically a wrapper.
#' Here, you're just being explicit.

#' ## Load Data
#' 
#' Problem sets and lab scripts will lean on data I make available in `{stevedata}`. 
#' However, you may often find that you want to download a data set from 
#' somewhere else and load it into R. Example data sets would be stuff like 
#' European Values Survey, European Social Survey, or Varieties of Democracy, or 
#' whatever else. You can do this any number of ways, and it will depend on 
#' what is the file format you downloaded. Here are some 
#' commands you'll want to learn for these circumstances:
#' 
#' - `haven::read_dta()`: for loading Stata .dta files
#' - `haven::read_spss()`: for loading SPSS binaries (typically .sav files)
#' - `read_csv()`: for loading comma-separated values (CSV) files
#' - `readxl::read_excel()`: for loading MS Excel spreadsheets.
#' - `read_tsv()`: for tab-separated values (TSV) files
#' - `readRDS()`: for R serialized data frames, which are awesome for file compression/speed.
#' 
#' Notice that functions like `read_dta()`, `read_spss()`, and `read_excel()` 
#' require some other packages that I didn't mention. However, these other 
#' packages/libraries are part of the `{tidyverse}` and are just not loaded 
#' directly with them. Under these conditions, you can avoid directly loading a 
#' library into a session by referencing it first and grabbing the function 
#' you want from within it separated by two colons (`::`). Basically, 
#' `haven::read_dta()` could be interpreted as a command saying "using the 
#' `{haven}` library, grab the `read_dta()` command in it". 
#' 
#' These wrappers are also flexible with files on the internet. For example, these will work. 
#' Just remember to assign them to an object.

# Note: hypothetical data
Apply <- haven::read_dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")

# Let's take a look at these data.
Apply

#' ## Learn Some Important R/"Tidy" Functions
#' 
#' I want to spend most of our time in this lab session teaching you some basic commands 
#' you should know to do basically anything in R. These are so-called "tidy" verbs. We'll be using
#' some data available in `{stevedata}`. This is the `pwt_sample` data, which includes yearly
#' economic data for a handful of rich countries that are drawn from version 10.0 of the Penn
#' World Table. If you're in Rstudio, you can learn more about these data by typing the following
#' command.

?pwt_sample

#' I want to dedicate the bulk of this section to learning some core functions that are part 
#' of the `{tidyverse}`. My introduction here will inevitably be incomplete because 
#' there's only so much I can teach within the limited time I have. That said, 
#' I'm going to focus on the following functions available in the 
#' `{tidyverse}` that totally rethink base R. These are the "pipe" (`%>%`), 
#' `glimpse()` and `summary()`, `select()`, `group_by()`, 
#' `summarize()`, `mutate()`, and `filter()`.
#' 
#' ### The Pipe (`%>%`)
#' 
#' I want to start with the pipe because I think of it as the most 
#' important function in the `{tidyverse}`. The pipe---represented as `%>%`---allows 
#' you to chain together a series of functions. The pipe is especially useful 
#' if you're recoding data and you want to make sure you got everything 
#' the way you wanted (and correct) before assigning the data to 
#' another object. You can chain together *a lot* of `{tidyverse}` 
#' commands with pipes, but we'll keep our introduction here rather minimal 
#' because I want to use it to teach about some other things.
#' 
#' ### `glimpse()` and `summary()`
#' 
#' `glimpse()` and `summary()` will get you basic descriptions of your data. 
#' Personally, I find `summary()` more informative than `glimpse()` though `glimpse()` 
#' is useful if your data  have a lot of variables and you want to just peek 
#' into the data without spamming the R console without output. 
#' 
#' Notice, here, the introduction of the pipe (`%>%`). In the commands below,
#'  `pwt_sample %>% glimpse()` is equivalent to `glimpse(pwt_sample)`, but I like to 
#'  lean more on pipes than perhaps others would. My workflow starts with (data) 
#'  objects, applies various functions to them, and assigns them to objects. I think you'll get a lot of mileage
#' thinking that same way too.

pwt_sample %>% glimpse() # notice the pipe
pwt_sample %>% summary()

#' ### `select()`
#' 
#' `select()` is useful for basic (but important) data management. You can use it to grab 
#' (or omit) columns from data. For example, let's say I wanted to grab all the columns 
#' in the data. I could do that with the following command.

pwt_sample %>% select(everything())  # grab everything

#' Do note this is kind of a redundant command. You could just as well spit the entire data
#' into the console and it would've done the same thing. Still, here's if I wanted everything 
#' except wanted to drop the labor share of income variable.
#' 
pwt_sample %>% select(-labsh) # grab everything, but drop the labsh variable.

#' Here's a more typical case. Assume you're working with a large data object and you 
#' just want a handful of things. In this case, we have all these economic data on these 
#' 21 countries (ed. we really don't, but roll with it), but we just want the GDP data 
#' along with the important identifying information for country and year. Here's 
#' how we'd do that in the `select()` function, again with some assistance from the pipe.
pwt_sample %>% select(country, year, rgdpna) # grab just these three columns.


#' ### Grouping data for grouped functions (`group_by()`, or `.by=`)
#' 
#' I think the pipe is probably the most important function in the `{tidyverse}` even as 
#' a critical reader might note that the pipe is 1) a port from another package (`{magrittr}`) 
#' and 2) now a part of base R in a different terminology. Thus, the critical reader 
#' (and probably me, depending on my mood) may note that grouping functions---
#' whether through `group_by()` or `.by`---is probably the  most important 
#' component of the `{tidyverse}`. Basically, `group_by()` allows you to "split" 
#' the data into various subsets, "apply" various functions to them, and 
#' "combine" them into one output. You might see that terminology "split-apply-combine" 
#' as you learn more about the `{tidyverse}` and its development.
#' 
#' Here, let's do a simple `group_by()` exercise, while also introducing you to 
#' another function: `slice()`. We're going to group by country in `pwt_sample` and "slice"
#' the first observation for each group/country. Notice how we can chain these together 
#' with a pipe operator.

# Notice we can chain some pipes together
pwt_sample %>%
  # group by country
  group_by(country) %>%
  # Get me the first observation, by group.
  slice(1)

#' If you don't group-by the country first, `slice(., 1)` will just return the first 
#' observation in the data set.

pwt_sample %>%
  # Get me the first observation for each country
  slice(1) # womp womp. Forgot to group_by()

#' I offer one caveat here. If you're applying a group-specific function (that you 
#' need just once), it's generally advisable to "ungroup()" (i.e. `ungroup()`) as the 
#' next function in your pipe chain. As you build together chains/pipes, the intermediate 
#' output you get will advise you of any "groups" you've declared in your data. Don't
#' lose track of those. This is incidentally why the `{tidyverse}` effectively
#' "retired" the `group_by()` function for `.by` as an argument in these functions.
#' `.by` will always return un-grouped data whereas `group_by()` always returns
#' grouped data.
#' 
#' Observe:
pwt_sample %>%
  # group by country
  group_by(country) %>%
  # Get me the first observation, by group.
  slice(1)

pwt_sample %>%
  slice(1, .by=country)

#' ### `summarize()`
#' 
#' `summarize()` creates condensed summaries of your data, for whatever it is 
#' that you want. Here, for example, is a kind of dumb way of seeing how many observations
#' are in the data. `nrow(pwt_sample)` works just as well, but alas...

pwt_sample %>%
  # How many observations are in the data?
  summarize(n = n())

#' More importantly, `summarize()` works wonderfully with `group_by()` or `.by=`. 
#' For example, for each country (`group_by(country)`), let's get the 
#' maximum GDP observed in the data.

pwt_sample %>%
  group_by(country) %>%
  # Give me the max real GDP observed in the data.
  summarize(maxgdp = max(rgdpna, na.rm=T))

#' `.by` does the same here.

pwt_sample %>%
  # Give me the max real GDP observed in the data, .by country.
  summarize(maxgdp = max(rgdpna, na.rm=T), .by=country)

#' One downside (or feature, depending on your perspective) to `summarize()` is that it 
#' condenses data and discards stuff that's not necessary for creating the condensed output. 
#' In the case above, notice we didn't ask for what year we observed the maximum GDP 
#' for a given country. We just asked for the maximum. If you wanted something that would 
#' also tell you what year that particular observation was, you'll probably want a `slice()` command 
#' in lieu of `summarize()`.
#' 
#' Observe:
#' 

pwt_sample %>%
  group_by(country) %>%
  # translated: give me the row, for each country, in which real GDP is the max (ignoring missing values).
  slice(which(rgdpna == max(rgdpna, na.rm=T)))

# or...

pwt_sample %>%
  # translated: give me the row, for each country, in which real GDP is the max (ignoring missing values).
  slice(which(rgdpna == max(rgdpna, na.rm=T)), .by=country)

#' This is a convoluted way of thinking about `summarize()`, but you'll probably 
#' find yourself using it a lot.
#' 
#' ### `mutate()`
#' 
#' `mutate()` is probably the most important `{tidyverse}` function for data 
#' management/recoding.  It will allow you to create new columns while 
#' retaining the original dimensions of the data. Consider it the sister 
#' function to `summarize()`. But, where `summarize()` discards, 
#' `mutate()` retains.
#' 
#' Let's do something simple with `mutate()`. For example, the `rgdpna` column 
#' is real GDP in million 2017 USD. What if we wanted to convert that 
#' million to billions? This is simple with `mutate()`. Helpfully, you can create 
#' a new column that has both the original/raw data and a new/recoded variable. 
#' This is great for reproducibility in your data management. One thing I will 
#' want to reiterate to you through our sessions is you should *never* overwrite 
#' raw data you have. Always create new columns if you're recoding something.
#' 
#' Anyway, here's "Wonderw-"... sorry, here's that new real GDP in billions 
#' variable we wanted.


pwt_sample %>%
  # Convert rgdpna from real GDP in millions to real GDP in billions
  mutate(rgdpnab = rgdpna/1000) %>%
  # select just what we want for presentation
  select(country:year, rgdpna, rgdpnab)

#' Let's assume we wanted to create a dummy variable for observations in the 
#' data starting from the Great Recession forward? In other words, let's create 
#' a dummy variable for all observations that were in 2008 or later. 

pwt_sample %>%
  mutate(post_recession = ifelse(year >= 2008, 1, 0))  %>%
  select(country:year, post_recession)

#' Knowing these data go to 2019, we can do this another way as well.

pwt_sample %>%
  mutate(post_recession = ifelse(year %in% c(2008:2019), 1, 0)) %>%
  select(country:year, post_recession)

#' Economists typically care about GDP per capita, right? We can create that 
#' kind of data ourselves based on information that we have in `pwt_sample`.

pwt_sample %>%
  mutate(rgdppc = rgdpna/pop) %>%
  select(country:year, rgdpna, pop, rgdppc)

#' Notice that `mutate()` also works beautifully with `group_by()`. For example, 
#' you may recognize that these data are panel data. We have 21 countries 
#' (cross-sectional units) across 70 years (time units). If you don't believe 
#' me, check this out...
#' 
pwt_sample %>% 
  summarize(n = n(),
            min = min(year),
            max = max(year),
            .by=country) %>%
  data.frame

#' You might know---or should know, as you progress---that some panel methods 
#' look for "within" effects inside cross-sectional units by looking at the 
#' value of some variable relative to the cross-sectional average for that 
#' variable. Let's use the real GDP per capita variable we can create as an 
#' example. Observe what's going to happen here.

pwt_sample %>%
  mutate(rgdppc = rgdpna/pop) %>%
  select(country:year, rgdpna, pop, rgdppc) %>%
  mutate(meanrgdppc = mean(rgdppc),
         diffrgdppc = rgdppc - mean(rgdppc),
         .by=country) 

#' That `diffrgdppc` variable practically "centers" the real GDP per capita 
#' variable, and values communicate difference from the mean. This is a 
#' so-called "within" variable, or a transformation of a variable where
#' it now communicates changes of some variable "within" a cross-sectional unit.
#' 
#' ### `filter()`
#' 
#' `filter()` is a great diagnostic tool for subsetting your data to look at 
#' particular observations. Notice one little thing, especially if you're new to 
#' programming. The use of double-equal signs (`==`) is for making logical 
#' statements where as single-equal signs (`=`) is for object assignment or 
#' column creation. If you're using `filter()`, you're probably wanting to find 
#' cases where something equals something (`==`), is greater than something (`>`), 
#' equal to or greater than something (`>=`), is less than something (`<`), or 
#' is less than or equal to something (`<=`).
#' 
#' Here, let's grab just the American observations by filtering to where `isocode` == "USA".

pwt_sample %>%
  # give me just the USA observations
  filter(isocode == "USA")

#' We could also use `filter()` to select observations from the most recent year.

pwt_sample %>%
  # give me the observations from the most recent year.
  filter(year == max(year))


#' If we do this last part, we've converted the panel to a cross-sectional data set.
#' 
#' ## Don't Forget to Assign!
#' 
#' When you're done applying functions/doing whatever to your data, don't forget 
#' to assign what you've done to an object. For simple cases, and for beginners, 
#' I recommend  thinking "left-handed"  and using `<-` for object assignment 
#' (as we did above). When you're doing stuff in the pipe, my "left-handed" 
#' thinking prioritizes the starting data in the pipe chain. Thus, I tend to 
#' use `->` for object assignment at the end of the pipe.
#' 
#' Consider a simple example below. I'm starting with the original data 
#' (`pwt_sample`). I'm using a simple pipe to create a new variable 
#' (within `mutate()`) that standardizes the real GDP variable from millions to 
#' billions. Afterward, I'm assigning it to a new object (`Data`) with `->`.

pwt_sample %>%
  # convert real GDP to billions
  mutate(rgdpnab = rgdpna/1000) -> Data

Data