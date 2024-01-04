#' ---
#' title: "Extending OLS"
#' author: Steven V. Miller, [svmiller.com](http://svmiller.com)
#' date: 18 December 2022
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


#' # R Packages/Data for This Session
#' 
#' You should've already installed the R packages for this lab session. 
#' `{tidyverse}` will be for all things workflow and `{stevedata}` will have the 
#' data sets we'll be using today.

library(tidyverse)
library(stevedata)

# For those of you without `{tidyverse}`, do this:
# library(dplyr)
# library(tibble)
# library(magrittr)
# library(forcats)
# library(ggplot2)

#' # Building Toward Multiple Regression
#' 
#' Last time we had a lab, we just focused on bivariate linear model with fake 
#' data that we created. The goal of that exercise was to show you what the
#' linear model looked like when you had maximum control over the model 
#' parameters. We'll use some actual data this time. This is a survey data set 
#' from the United Kingdom in 2018-19, largely probing individual attitudes 
#' toward immigration. You can find out more about this data set by typing 
#' `?ESS9GB` into Rstudio. You can also 
#' [go here](http://svmiller.com/stevedata/reference/ESS9GB.html) for
#' a web version of the codebook.

ESS9GB

#' You can also see read some blog posts where I make considerable use of these
#' data here:
#' 
#' - [What Do We Know About British Attitudes Toward Immigration? A Pedagogical Exercise of Sample Inference and Regression](http://svmiller.com/blog/2020/03/what-explains-british-attitudes-toward-immigration-a-pedagogical-example/)
#' - [How Should You Think About Your Priors for a Bayesian Analysis?](http://svmiller.com/blog/2021/02/thinking-about-your-priors-bayesian-analysis/)
#' 
#' Understand that some of that is slightly more complex than what we'll be doing
#' here, but I'd rather inundate you with things to look at and read rather than
#' give you nothing at all.
#' 
#' Our primary dependent variable in this model will be the `immigsent` variable,
#' which is an additive index of three variables also included in the data set.
#' These prompts, verbatim, each range from 0 to 10 and ask 1) Would you say it 
#' is generally bad or good for the United Kingdom’s economy that people come to 
#' live here from other countries? (`imbgeco`), 2) And… would you say that the 
#' United Kingdom’s cultural life is generally undermined or enriched by people 
#' coming to live here from other countries? (`imueclt`), and 3) Is the United 
#' Kingdom made a worse or a better place to live by people coming to live here 
#' from other countries? (`imwbcnt`). The `immigsent` variable simply adds all
#' three together, resulting in a scale that ranges from 0 to 30. You can 
#' interpret 0 as being maximally anti-immigrant/immigration and 30 being 
#' maximally pro-immigrant/immigration. Higher values = more pro-immigration
#' sentiment.

#' Let's build toward multiple regression by starting in the simple bivariate, 
#' like we did in lecture. We'll start by exploring an uncontroversial argument 
#' that we should expect pro-immigration sentiment to be much lower in
#' North East than other parts of the United Kingdom. Why "North East"? This is
#' the region where Newcastle is. Newcastle, rightly or wrongly, has a reputation
#' of being ["Brexit Central".](https://bylinetimes.com/2019/07/17/calling-the-north-east-of-england-brexit-central-is-simplistic-and-snobbish/)
#' We have the means to explore this with the `region` variable. Let's take a 
#' look at it.

ESS9GB %>% distinct(region)

#' Perfect. I see I have 12 familiar regions to me in the data to explore, but
#' I have no "North East" dummy variable. No matter, it's a simple matter to 
#' create one like this.

ESS9GB %>%
  mutate(northeast = ifelse(region == "North East (England)", 1, 0)) -> Data

#' Note: I'm creating a new data frame here for convenience sake.
#' 
#' Now let's do a simple regression, regressing the immigration sentiment
#' variable on whether the respondent is in the North East.

M1 <- lm(immigsent ~ northeast, data=Data)
summary(M1)

#' By the way, in a super simple case like this where *y* is continuous and *x* 
#' is binary, this is basically what the *t*-test will tell you as well.

t.test(immigsent ~ northeast, Data)

#' In this simple difference of means, the mean immigration sentiment where
#' `northeast` is 0 is 17.02. The mean immigration sentiment where `northeast` 
#' is 1 is 14.65. The difference between those two numbers is basically the 
#' regression coefficient. It's the "North East" effect, which basically
#' decreases immigration sentiment by about 2.37. That effect is highly unlikely 
#' to be zero. You knew that because we went over statistical significance and 
#' null hypothesis testing. You also know that, one assumes, from just kind of
#' eye-balling the United Kingdom with a polite curiosity. Again, "rightly or
#' wrongly."
#' 
#' You can also do the same thing and look for differences between Scotland
#' and not-Scotland.
#' 

Data %>%
  mutate(scotland = ifelse(region == "Scotland", 1, 0)) -> Data

M2 <- lm(immigsent ~ scotland, Data)
summary(M2)

#' The model here suggests that pro-immigration sentiment in not-Scotland is
#' 16.73. The "Scotland" effect increases pro-immigration sentiment by 1.76. The 
#' *t* statistic suggest that if the true differences between Scotland and 
#' not-Scotland is 0, what we got would have been observed about .19% of the 
#' time, on average.
#' 
#' What if we wanted to tease out more information for all regions? This would
#' be a kind of "fixed effects" analysis, which is a somewhat misleading and 
#' confusing way of saying "take a categorical variable, and create a series of 
#' dummy variables from it." You could do this manually, or you can have R do it 
#' for you.
#' 
M3 <- lm(immigsent ~ factor(region), Data)
summary(M3)

#' I want to stop right here and explain what this procedure is doing. First, 
#' you have 12 regions, but you get 11 regression coefficients. In this 
#' procedure, something must be a baseline. Second, if you don't give R any 
#' guidance, the "baseline" becomes whatever is first in alphabetical order. In 
#' this case, that is "East Midlands (England)", which is the region where 
#' Leicester and Nottingham are (if Premier League squads may help orient you).
#' What this "fixed effects" communicates, then, is the difference between East 
#' of England (i.e. where Norwich, Essex and Cambridge are) relative to East 
#' Midlands (and if there is a discernible difference), the difference between
#' London and East Midlands (and if it's discernible), the difference between 
#' North East and East Midlands (and if it's discernible), and so on. Here, we'd
#' suggest discernible differences relative to the baseline of London, North 
#' East, Scotland, and South East (i.e. where Brighton, Canterbury, and Dover 
#' are). Pro-immigration sentiment is higher in London, South East, and Scotland 
#' relative to East Midlands and lower in North East relative to East Midlands.
#' 
#'  A few other things. For one, you could basically tease much of this out 
#'  through doing a comparison of means. This procedure doesn't demonstrate 
#'  "significance", but calculating standard errors and creating lower and 
#'  upper bounds around that mean will illustrate a little bit more about what 
#'  the regression model is telling you.
#'  
Data %>%
  summarize(mean = mean(immigsent, na.rm=T),
            sd = sd(immigsent, na.rm=T),
            n = n(),
            se = sd/sqrt(n()),
            lwr = mean - se,
            upr = mean + se,
            .by = region) %>%
  data.frame

#' Graphing it will help.

Data %>%
  summarize(mean = mean(immigsent, na.rm=T),
            sd = sd(immigsent, na.rm=T),
            n = n(),
            se = sd/sqrt(n()),
            lwr = mean - se,
            upr = mean + se,
            .by = region) %>%
  # The next two lines are super-hacky ways of making the plot read in order.
  arrange(desc(region)) %>%
  mutate(region = fct_inorder(region)) %>%
  # Notice below: region is x-axis, mean is y-axis, and I've outlined that I
  # want the `lwr` and `upr` columns to be mins and maxes for what's coming
  # next...
  ggplot(., aes(region, mean, ymin=lwr, ymax=upr)) + 
  # ...the point-range. This will create a dot at `mean` and bounds 
  # that correspond with the ymin and ymax variables.
  geom_pointrange() +
  # Put your coords down flip them and reverse them.
  coord_flip()

#' What if you wanted to make London the baseline? You could manually create 
#' your own dummy variables, or you can turn that "region" variable into a more 
#' informative factor through the use of the `{forcats}` package (which is 
#' loaded with `{tidyverse}`). Let's use `fct_relevel()` for this purpose. Using 
#' `fct_relevel()` in the way I do below, I'm basically telling R that this 
#' new `region2` variable is a factor and it's specific order is 1) London and 
#' 2) after that, I don't care, and just proceed alphabetically as you wish.
#' 

Data %>%
  mutate(region2 = fct_relevel(region, "London")) -> Data

Data %>% summary

#' Now, let's do this again.
#' 
M4 <- lm(immigsent ~ region2, Data)
summary(M4)

#' This time, you're benchmarking any comparisons to London. Just because 
#' there's no difference between London and Scotland here doesn't mean there's
#' no difference between Scotland and East Midlands. What you do with so-called 
#' "fixed effects" depends on what you want to accomplish. Does it matter that 
#' something is the baseline? If so, make it the baseline and compare everything 
#' else to it. Do you not care, and you're just doing this because you want to 
#' partial out these potential categorical effects while you focus on some other 
#' regressors? Then don't bother with the details, because it's mostly immaterial
#' to what you want to accomplish.
#' 
#' # Multiple Regression
#' 
#' Now, let's a do a multiple regression. Let's see if we can't regress the
#' immigration sentiment variable on the Scotland dummy variable, whether the
#' respondent is a woman, and the respondent's ideology on a 0-10 scale where 0
#' is the most to the right, and 10 is the most to the left. You knew how to do 
#' this in the bivariate case, but what do you do to the formula when you have 
#' multiple independent variables? Simple as this:
#' 
M5 <- lm(immigsent ~ scotland + female + lrscale, Data)
summary(M5)

#' In other words: separate however many *x* variables you want in the equation 
#' with a `+` sign. The formula above could be read as "using the `lm()` 
#' function, regress `immigsent` on `scotland` and `female` and `lrscale` from 
#' the `Data` data set." Let's walk through the basic takeaways from the model:
#' 
#' - The estimated pro-immigration sentiment for a man, not residing in Scotland,
#'   who is furthest to the political left is 20.38. In this case, the 
#'   y-intercept actually exists. This parameter isn't total nonsense, at least
#'   on paper.
#' - Partialing out everything else in the model ("ceteris paribus"), the effect
#'   of going from not-Scotland to "Scotland" increases pro-immigration sentiment
#'   by 1.45 points on our scale. If the true "Scotland effect" were 0, we'd
#'   expect to have observed this effect, on average, about 1.12% of the time.
#'   We'd suggest that "Scotland effect" is statistically significant.
#' - The partial effect of being a woman versus a man ("ceteris paribus") is to 
#'   decrease pro-immigration sentiment by -.43 points. However, the corresponding
#'   *t*-statistic and *p*-value suggests that such an effect would be observed
#'   about 19% of the time if there truly was no effect. Thus, the parameters are
#'   consistent with a null hypothesis of no relationship between pro-immigration
#'   sentiment and gender. We can't discern a difference between men and women
#'   exist here with respect to this outcome variable.
#' - Partialing out the Scotland effect and potential gender differences, we see
#'   an unsurprising effect of ideology on pro-immigration sentiment. Higher 
#'   levels of ideological affinity with the right coincide with lower levels of
#'   pro-immigration sentiment. The unit effect is -.63 and that effect is
#'   incidentally the most precise in the model. There is a clear ideology effect
#'   in our model.
#'   
#' # Interactive Effects
#' 
#' Let's walk through the issue of what to do when you think there might be an 
#' interactive relationship among *x* and *z* in their relationship with *y*. 
#' I'm going to keep this example simple. We noticed there were no gender 
#' differences, but is that because it depends on ideology? Let's see, and notice
#' how we're going to see it below. When you want to see if two (or more, but 
#' let's focus on just two) things interact, you put an asterisk (`*`) between 
#' the variables in the right-hand side of the equation. Like this:
#' 
M7 <- lm(immigsent ~ scotland + female*lrscale, Data)
summary(M7)

#' Notice that two variables we interacted produced three regression 
#' coefficients. The `female` coefficient is the gender difference when `lrscale`
#' is 0 (i.e. the respondent is furthest to the left). The  `lrscale` coefficient
#' is the effect of increasing ideological affinity to the right when `female` 
#' is 0 (i.e. the effect of ideology on men). The `female:lrscale` communicates 
#' a way of thinking about how the two interact with each other, and whether that
#' interaction is "significant". Here, it is (albeit at the .10 level). The 
#' "Scotland effect" variable is not affected by this and is still an "all else
#' equal" partial effect.
#' 
#' I want to use this as an illustrative case. *Be careful* in how you interpret
#' your interactions, and how you model them. We got lucky here that that the
#' data have naturally occurring 0s, but what is "0"---and whether there's a 0
#' at all---will influence what exactly the regression summary tells you. For 
#' example, let's center the data so that 0 is the people in the middle of the 
#' scale. Observe:
#' 

Data %>%
  mutate(c_lrscale = lrscale - 5) -> Data

Data %>%
  distinct(lrscale, c_lrscale) %>%
  arrange(lrscale)

#' Take very quick inventory of what this did. We took the raw data and 
#' subtracted 5 from it. Now, 0 are people who would place themselves in the
#' ideological middle. 5 are the people furthest to the right and -5 are the 
#' people furthest to the left. We didn't really "change" 0, as much as we 
#' shifted it.
#' 
#' Now, watch.
#' 
M8 <- lm(immigsent ~ scotland + female*c_lrscale, Data)
summary(M8)

#' Compare this to the output of `M7`. Notice how the gender dummy variable isn't
#' significant? Except, again, you have to be careful because the `female` dummy
#' is now the difference between men and women for those in the middle, not the
#' far left. The interactive coefficient didn't move, but the gender variable
#' did. The ideology constituent effect didn't move because men are still the
#' baseline. Just be careful about what's actually happening in an interaction.
#' 
#' I wanted to close with a caveat about interactions. Just because the overall 
#' interactive effect is insignificant, or just because component terms of the
#' interaction are insignificant, doesn't mean there isn't something potentially 
#' interesting happening in your interactions. If the interaction is 
#' statistically significant, you should *definitely* look at it to see what the 
#' interaction looks like. If it's insignificant, you should still look at it.
#' After all, you're the one that felt compelled to interact two things. Go look
#' at what you did just to see for yourself.
#' 
#' Here's what we'll do. We'll create a simple prediction grid. Let the ideology
#' variable be either 0 (furthest to the left), 5 (in the middle), or 10 
#' (furthest to the right). Let the `female` variable range from 0 to 1. Then, 
#' given the model we estimated, we're going to get predictions from the model 
#' and standard errors around those predictions. Let the Scotland effect be 0
#' for convenience sake. Then, we'll create conceivable lower and upper bounds 
#' around these estimates and graph them.
#' 
#' Note: it's killing me to not ask you to install `{modelr}` for this, but you
#' should absolutely install `{modelr}` for this. It makes it way less tedious.

tibble(lrscale = c(0, 5, 10, 0, 5, 10),
       female = c(0,0,0,1,1,1),
       scotland = 0) -> newdat

newdat # Look at our hypothetical data/prediction grid.

Preds <- predict(M7, newdata = newdat, se.fit = TRUE)

Preds # Look at our predictions.

Preds %>% as_tibble() %>% # Coerce to a tibble, and...
  # bind_cols to the prediction grid/hypothetical data, and...
  bind_cols(newdat, .) %>% 
  # select just what we want, to look at  it...
  select(lrscale:se.fit)

#' Since I can see that performed how I wanted it to. Let's make it pretty.

Preds %>% as_tibble() %>%
  bind_cols(newdat, .) %>% 
  select(lrscale:se.fit) %>%
  # Okay, there's going to be some hackiness (sic). First, we're going to create
  # lower and upper bounds that are 90% intervals around +/- the standard error.
  # Next, we're going to create a categorical variable from the female variable
  # in a way that's super obvious. Next. We're going to create a categorical
  # variable from the `lrscale` variable (ideo_lbl) that adds more intuitive 
  # labels. The \n you see there forces a linebreak for legibility in the label.
  # You'll see what it does. Using the fct_inorder() forces it to be ordered
  # because, otherwise, it would display 0, 10, 5 (i.e. furthest, furthest, 
  # middle) because of the alphabetical order. Because our data are already
  # ordered, this is just a shortcut/cheat code to do what we want to do. In
  # this case, it's because I know y'all might help to see the left in red and 
  # the right in blue.
  mutate(lwr = fit - 1.645*se.fit,
         upr = fit + 1.645*se.fit,
         gndr_lbl = ifelse(female == 0, "Men", "Women"),
         ideo_lbl = case_when(
           lrscale == 0 ~ "Furthest\nLeft",
           lrscale == 5 ~ "Middle",
           lrscale == 10 ~ "Furthest\nRight"
         ),
         ideo_lbl = fct_inorder(ideo_lbl)) %>%
  ggplot(.,aes(gndr_lbl, fit, ymin=lwr, ymax=upr, color=ideo_lbl)) +
  geom_pointrange(position = position_dodge(width = .1)) +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "Estimated Pro-Immigration Sentiment (with 90% Bounds)",
       color = "")


#' What this tells me: the effect of increasing ideology on pro-immigration 
#' sentiment is stronger for the men than it is the women. For women furthest
#' to the left, their mean pro-immigration sentiment is 19.3. Furthest to the 
#' right: 14.3. That's a min-max difference of about 5 points. For the men: the 
#' estimated pro-immigration sentiment for non-Scottish dudes furthest to the 
#' left is 21.2, which is interestingly higher than the women furthest to the
#' left. However, the estimated pro-immigration sentiment for the men furthest
#' to the right is 13.2. That's a min-max difference of 8 points. Kinda cool,
#' and kinda interesting as I'm doing this on the fly for the sake of this class.
#' 
#' There, is I suppose, an alternate interpretation that is largely consistent
#' with the data but communicates a different mechanism happening here. In the
#' above interpretation, I largely channeled the effect of ideology through men
#' and women. There could also be a story to tell about why there is an apparent
#' gender difference among those furthest to the left but no real gender 
#' differences among those furthest to the right. In this potential 
#' interpretation, there is a potential gender mechanism that's being clustered
#' on ideology, and why is it that the men and women are more like each other
#' on this matter (in this simple model) among the most right whereas they are
#' more different among those most to the left.
#' 
#' I'll defer to you to make sense of the findings as you see fit, and why you
#' think you see what you see (provided you treat such an obviously simple model
#' as gospel truth about the UK population at this point in time). No matter, 
#' always look at your data. You might find something cool in there.