Indonesia RASKIN Program (rice for the poor): a PCA Analysis
================

## Introduction

RASKIN (<em>beras miskin</em> or rice for the poor) is one of
Indonesia’s safety net programs with aims to strengthen food security
among poor and vulnerable households. The program has been started since
1998 and still ongoing. It provides a subsidized rice—staple food in
Indonesia—and distributed through local officials.

This writing makes a simple analysis using Principal Component Analysis
(PCA) to take a look at the RASKIN distribution. This writing is heavily
inspired by PCA analysis done by [Julia
Silge](https://juliasilge.com/blog/best-hip-hop/).

## Data preparation

Using PCA, I try to understand which households are more likely to ever
receive RASKIN. The set of characteristics I used in this analysis are
household location (urban or rural), does household only has 5 or less
members, house ownership, house characteristics (floor type, wall type,
roof type), water source, etc.. Every characteristic is coded as a
dummy, with 1 as characteristics that resemble wealthier households and
0 otherwise, such as protected water source as 1 and unprotected water
source as 0 (this categorization is subjected to me).

## Correlation between predictors

``` r
library(corrr)
```

``` r
hhchar %>% 
  select(urban:own_laptop) %>% 
  correlate() %>% 
  rearrange() %>% 
  rplot(colours = c("red3", "white", "green4"))
```

![](Bored_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Virtually every predictor is positively correlated to each other. This
is not surprising as a good aspect of household characteristics will be
complemented by another good aspect of household characteristics.

## Principal Component Analysis

To implement PCA, I use recipe from tidymodels. This package is helping
me a lot with my workflow (if you ever read this, thanks a lot
Ms. Silge\!).

``` r
library(tidymodels)
library(tidytext)
```

``` r
set.seed(123)

# Put each steps in the recipe
raskin_recipe <- recipe(raskin ~ ., data = hhchar) %>% 
  update_role(hhid, new_role = "id") %>% 
  step_pca(all_predictors())

# Apply each steps in the recipe
raskin_prep <- prep(raskin_recipe)

# Extract results from PCA
raskin_tidied_pca <- tidy(raskin_prep, 1)
```

``` r
# Extracting standard deviation from the extracted dataframe before
raskin_sdev <- raskin_prep$steps[[1]]$res$sdev

raskin_percent_variation <- raskin_sdev^2 / sum(raskin_sdev^2)

# Cumulative sum of variation plot
tibble(
  component = unique(raskin_tidied_pca$component),
  percent_var = cumsum(raskin_percent_variation)) %>%
  filter(component %in% c("PC1", "PC2", "PC3", "PC4", "PC5")) %>% 
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(component, percent_var)) +
  geom_col(fill = "green4") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Cumulative Sum of Variation",
       x = "Component", y = "Percentage of variation explained")
```

![](Bored_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

The bar plot above shows that almost 80 percent variation can be
explained by only 5 components. The first component is able to explain
more than 75 percent variation.

``` r
# Which terms contributed most to each component?
raskin_tidied_pca %>% 
  filter(component %in% c("PC1", "PC2")) %>% 
  mutate(terms = reorder_within(terms, abs(value), component)) %>% 
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, ncol = 1, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_manual(values = c("red3", "green4")) +
  labs(y = NULL, x = "Absolute values of contribution",
       fill = "Positive?")
```

![](Bored_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

I only show the first and the second component to break down each
contributor within each component. Surprisingly, all predictors from the
first component–which has the largest proportion in explaining the
variance–have negative directions, meaning that households with
characteristics resembling a wealthier household are less likely to
receive RASKIN.

``` r
# The same as above except only the top 5 contributors are showed
raskin_tidied_pca %>% 
  filter(component %in% c("PC1", "PC2")) %>% 
  group_by(component) %>% 
  top_n(5, abs(value)) %>% 
  ungroup() %>% 
  mutate(terms = reorder_within(terms, abs(value), component)) %>% 
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, ncol = 1, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_manual(values = c("red3", "green4")) +
  labs(y = NULL, x = "Values",
       fill = "Positive?")
```

![](Bored_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Focusing on the top 5 contributors, we can see that the first component
mostly about the size of the household, the physical characteristics of
the house, and its utility. While the second component is rather hard to
interpret, it is mostly about whether the households have a fridge or
not, and has a negative direction. So, the conclusion is pretty much the
same as the first component, although the terms whether the house is
only used for living seems contradicted (but, remember that the
variation explained by the second component is less than 5 percent).

How are the households distributed in the plane of the first components?

``` r
# Two dimensions plot of the second and the first component
juice(raskin_prep) %>% 
  mutate(raskin = if_else(raskin == 1, "Yes", "No")) %>% 
  ggplot(aes(PC1, PC2, color = as.factor(raskin))) +
  geom_jitter(alpha = 0.2) +
  scale_color_manual(values = c("red3", "green4")) +
  labs(color = "Receive RASKIN?")
```

![](Bored_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Let us focus on the direction of the first component. Wealthier
characteristics are to the left and poorer characteristics are to the
right. We can see that households that have ever received RASKIN tend to
be at the right side of the x-axis, although some of the wealthier
households also enjoy RASKIN. These results show suggestive evidence
that RASKIN is indeed distributed to poor households, but it is far from
perfect. While the selection process of RASKIN needs to be improved, the
fact that some poorer households never received RASKIN means that the
information of this program also has to be promoted more widely and more
inclusive.

This analysis is without limitations. Please note that this is my first
time using PCA. My intention is to apply what I’ve learned so far about
this tool, so don’t take anything here for granted. Suggestions,
critiques, and comments are welcomed\!
