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
#' You should've already installed the R packages for this lab session. `{tidyverse}` will be 
#' for all things workflow and `{stevedata}` will have the data sets we'll be using today. I should
#' be transparent that we'll just be walking through how I did the analyses I put into the relevant
#' lecture. `{stevemisc}` has a function for scaling things by a standard deviation (`r1sd()`)

library(tidyverse)
library(stevedata)
library(stevemisc)

#' # Building Toward Multiple Regression
#' 
#' Last time we had a lab, we just focused on bivariate OLS with fake data that we created.
#' The goal of that exercise was to show you what OLS looked like when you had maximum
#' control over the model parameters. We'll use some actual data this time, and data
#' that will be germane to your fourth problem set. This is the basic election-turnout
#' data set. You can find out more about this data set by typing `?election_turnout` into
#' Rstudio, or you can [go here](http://svmiller.com/stevedata/reference/election_turnout.html).

election_turnout

#' Let's build toward multiple regression by starting in the simple bivariate case, like we
#' did in lecture. We'll start by exploring an uncontroversial argument that voter turnout
#' tends to be higher in states where the split between Democrats and Republicans is a little
#' more even. The races become a little more competitive, and turnout increases as a result.
#' The data are somewhat dated---in essence: Ohio and Florida are now sunburn red states and no one should
#' think of them as swing states anymore---but we'll use FiveThirtyEight's identification of 
#' "swing states" around this time. Swing states are Colorado, Florida, Iowa, Michigan, Minnesota,
#' Nevada, New Hampshire, North Carolina, Ohio, Pennsylvania, Virginia, and Wisconsin. I already
#' created a dummy variable for these in the data, but let's retrace our steps. No harm in doing this
#' way if it shows you what I did previously.

election_turnout %>%
  mutate(ss = ifelse(state %in% c("Colorado", "Florida", "Iowa", "Michigan",
                                  "Minnesota", "Nevada", "New Hampshire",
                                  "North Carolina", "Ohio", "Pennsylvania",
                                  "Virginia", "Wisconsin"), 1, 0)) -> election_turnout

election_turnout %>%
  filter(ss == 1)

#' Now let's do a simple regression, regressing turnout for highest office in 2016
#' on whether these are swing states.
#' 

M1 <- lm(turnoutho ~ ss, election_turnout)
summary(M1)

#' By the way, in a super simple case like this where *y* is continuous and *x* is binary,
#' this is basically what the *t*-test will tell you as well.

t.test(turnoutho ~ ss, election_turnout)

#' In this simple difference of means, the mean turnout where swing state == 0 is 59.08 and
#' the mean turnout where swing state == 1 is 66.45. The difference between those two numbers
#' is basically the regression coefficient. It's the "swing state" effect, which basically
#' increases turnout by about 7.37 percentage points. That effect is highly unlikely to be 
#' zero. You knew that because we went over statistical significance and null hypothesis testing.
#' 
#' You can also do the same thing and look for differences between the South and the not-South.
#' Let's create a "South" dummy from the region variable.
#' 

election_turnout %>%
  mutate(south = ifelse(region == "South", 1, 0)) -> election_turnout

M2 <- lm(turnoutho ~ south, election_turnout)
summary(M2)

#' The model here suggests that turnout in the South is about 3.46% lower than the not-South.
#' The *t* statistic suggest that if the true differences between the South and the not-South
#' is 0, what we got would have been observed about 5.57% of the time, on average. Given so
#' few observations, suggesting a difference between the South and not-South that is "significant"
#' at the .10 level is adequate.
#' 
#' What if we wanted to tease out more information about the four regions in the United States?
#' You could do a so-called "fixed effects" analysis. This is a somewhat misleading and confusing
#' way of saying "take a categorical variable, and create a series of dummy variables from it."
#' You could do this manually, or you can have R do it for you.
#' 
M3 <- lm(turnoutho ~ factor(region), election_turnout)
summary(M3)

#' I want to stop right here and explain what this procedure is doing. First, you have four regions,
#'  but you get three regression coefficients. In this procedure, something must be a baseline. Second,
#'  if you don't give R any guidance, the "baseline" becomes whatever is first in alphabetical order.
#'  In this case, that is the North Central (Midwest). What this "fixed effects" communicates, then, 
#'  is the difference between the Northeast and Midwest (and if there is a discernible difference), 
#'  the difference between the South and Midwest (and if it's discernible), and the difference 
#'  between the West and Midwest (and if it's discernible). Here, we'd suggest there isn't much of 
#'  a difference between the Midwest and Northeast, but there appear to be discernible 
#'  differences between the South and Midwest, and the West and Midwest. In both
#'  those regions, turnout is lower than the Midwest.
#'  
#'  A few other things. For one, you could basically tease that out through doing a comparison of means.
#'  This procedure doesn't demonstrate "significance", but calculating standard errors and creating lower
#'  and upper bounds around that mean will illustrate a little bit more about what the regression model is
#'  telling you.
#'  
election_turnout %>%
  group_by(region) %>%
  summarize(mean = mean(turnoutho),
            sd = sd(turnoutho),
            se = sd/sqrt(n()),
            lwr = mean - se,
            upr = mean + se) %>%
  data.frame

#' What if you wanted to make the South the baseline? You could manually create your own dummy variables,
#' or you can turn that "region" variable into a more informative factor through the use of the `{forcats}`
#' package (which is loaded with `{tidyverse}`). Let's use `fct_relevel()` for this purpose. Using 
#' `fct_relevel()` in the way I do below, I'm basically telling R that this new `region2` variable is
#' a factor and it's specific order is 1) South and 2) after that, I don't care, and just proceed
#' alphabetically as you wish.
#' 

election_turnout %>%
  mutate(region2 = fct_relevel(region, "South")) -> election_turnout

election_turnout %>% summary

#' Now, let's do this again.
#' 
M4 <- lm(turnoutho ~ region2, election_turnout)
summary(M4)

#' The results are identical to what you saw in lecture, but notice, importantly, that the takeaways
#' are a little bit different. This time, you're benchmarking any comparisons to the South. Just because
#' there's no difference between the Northeast and the Midwest doesn't mean there's no difference between
#' the Northeast and the South. What you do with so-called "fixed effects" depends on what you want to accomplish.
#' Does it matter that something is the baseline? If so, make it the baseline and compare everything else to it.
#' Do you not care, and you're just doing this because you want to partial out these potential categorical effects
#' while you focus on some other regressors? Then don't bother with the details, because it's mostly immaterial
#' to what you want to accomplish.
#' 
#' # Multiple Regression
#' 
#' Now, let's a do a multiple regression. Let's see if we can't regress turnout on the swing state dummy,
#' the South dummy, and the percentage of the state having a college diploma. You knew how to do this in
#' the bivariate case, but what do you do to the formula when you have multiple independent variables?
#' Simple as this:
#' 
M5 <- lm(turnoutho ~ percoled + ss + south, election_turnout)
summary(M5)

#' In other words: separate however many *x* variables you want in the equation with a `+` sign. The formula
#' above could be read as "using the `lm()` function, regress `turnoutho` on `percoled` and `ss` and `south`
#' from the `election_turnout` data set." The takeaways are the same as they were in the lecture.
#' You could also outright specify fixed effects the same way. Here, let's drop the `ss` dummy for 
#' the `region2` factor we created.
#' 
M6 <-lm(turnoutho ~ percoled + ss + region2, election_turnout)
summary(M6)

#' Let's walk through the basic takeaways from the model:
#' 
#' - The estimated turnout in a non-swing state in the South (e.g. Alabama, Mississippi) where
#'   no one graduated from college is 47.46. There isn't much to do with this parameter since
#'   0 doesn't naturally occur in the `percoled` variable.
#' - Partialling out everything else in the model ("ceteris paribus"), a one-unit increase
#'   in the percentage of the state having a college diplooma increases turnout by .35
#'   percentage points. That is a significant effect too (i.e. unlikely to actually be 0).
#' - The partial effect of being a swing state ("ceteris paribus") is to increase turnout
#'   by about 6.54 percentage points (a significant effect).
#' - Partialling out education effects and swing state effects, there are slight regional
#'   differences compared to the South at the .10 level for the Midwest (North Central) and
#'   Northeast. There is no difference between the West and the South.
#'   
#' # Interactive Effects
#' 
#' Let's walk through the issue of what to do when you think there might be an interactive relationship
#' among *x* and *z* in their relationship with *y*. I'm going to keep this first example simple. Let's
#' use the `election_turnout` data to see if there's an interactive effect between the `percoled` variable
#' and the `ss` variable on turnout. When you want to see if two (or more, but let's focus on just two) things
#' interact, you put an asterisk (`*`) between the variables in the right-hand side of the equation. Like this:
#' 
M7 <- lm(turnoutho ~ ss*percoled, election_turnout)
summary(M7)

#' Notice that two variables produce three regression coefficients. The `ss` coefficient is the effect of being a 
#' swing state when `percoled` is 0. The `percoled` coefficient is the effect of a one-unit increase in the level
#' of college education (i.e. % of state having a college diploma) for a state that is not a swing state. The 
#' `ss:percoled` communicates a way of thinking about how the two interact with each other.
#' 
#' I want to use this as an illustrative case. It seems odd to think that the swing state variable is now negative,
#' if not signficant. *But be careful*. Whatever you want to do with that parameter needs to be mindful of the fact
#' there *is no 0* in the `percoled` variable. I use this to communicate the following: 
#' *if you want to interact two things, each of them must have a naturally occurring 0*. Otherwise, a basic
#' regression summary is going to be misleading. In this case, let's "standardize" the `percoled` variable by
#' scaling it by its standard deviation. This creates a *z* score of the `percoled` variable and centers its around
#' the mean.
#' 
election_turnout %>% mutate(z_percoled = r1sd(percoled)) -> election_turnout

#' Notice, when you do this, the rescaled variable has the same basic shape as the raw variable. It's just
#' been transformed to have a mean of 0, a standard deviation of 1, and where the same basic principles about
#' the area underneath the curve of the standard normal distribution apply. In other words, about 68% of the
#' 51 observations in the data set will be between a standard deviation on either side of the mean in this 
#' rescaled variable.
#' 
election_turnout %>%
  ggplot(.,aes(percoled)) +
  geom_density() +
  theme_steve()

election_turnout %>%
  ggplot(.,aes(z_percoled)) +
  geom_density() +
  theme_steve()

#' Btw: that massive outlier there is the District of Columbia.
#' 
#' Now, let's re-run our model, this time interacting the swing state variable with the standardized 
#' college education variable.
#' 
M8 <- lm(turnoutho ~ ss*z_percoled, election_turnout)
summary(M8)

#' We don't find an interactive effect yet, but notice a few things changed. The intercept now communicates
#' the estimated turnout for a non-swing state with the mean level of college education. The swing state
#' effect now communicates the swing state effect on states with the average level of college education.
#' The college education variable communicates the effect of a standard deviation change (i.e. across about
#' 34% of the data) in college education for a state that is not a swing state. The `ss:z_percoled` is the
#' interactive effect between the two. What changes is also a matter of "significance" for the swing state
#' effect. Just be careful about what's actually happening in an interaction.
#' 
#' I wanted to close with a caveat about interactions. Just because the overall interactive effect is
#' insignificant doesn't mean there isn't something potentially interesting happening in your interactions.
#' If the interaction is statistically significant, you should *definitely* look at it to see what the 
#' interaction looks like. If it's insignificant, you should still look at it.
#' 
#' Here's what we'll do. We'll create a simple prediction grid. There are seven values of the `z_percoled`
#' variable, approximating a reasonable range of the variable (since it's scaled to be on a standard
#' normal distribution). We're going to duplicate that variable, because we have that range for swing
#' states (`ss == 1`) and non-swing states (`ss == 0`). Then, given the model we estimated, we're going
#' to get predictions from the model and standard errors around those predictions. Then, we'll create
#' conceivable lower and upper bounds around these estimates and graph them.
#' 
tibble(z_percoled = rep(c(-1.5, -1, -.5, 0, .5, 1, 1.5), 2),
       ss = c(rep(0, 7), rep(1, 7))) -> newdat

newdat

Preds <- predict(M8, newdata = newdat, se.fit = TRUE)

newdat %>%
  mutate(pred = Preds$fit,
         se = Preds$se.fit,
         lwr = pred - p_z(.1)*se,
         upr = pred + p_z(.1)*se) %>%
  mutate(ss = ifelse(ss == 0, "Not Swing State", "Swing State")) %>%
  ggplot(.,aes(z_percoled, pred, ymin=lwr, ymax=upr, color=ss, fill=ss)) +
  geom_line() +
  geom_ribbon(alpha=.3) +
  theme_steve()


#' There isn't much of an interactive effect, to be fair. But notice the distance between non-swing
#' states and swing states starts to collapse for the least educated states? Notice how there's
#' greater distance (and greater precision/discernibility) for higher levels of education? Just
#' because an interactive effect is not significant doesn't mean the two variables aren't interacting
#' in a way that might be interesting.
#' 
#' Always look at your data.