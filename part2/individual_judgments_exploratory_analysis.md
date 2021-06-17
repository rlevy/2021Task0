Exploratory Analysis of Individual Judgments
================
Roger Levy

The approach here has the micro-averaged rank correlation performance
metric in mind, and asks how many of the ranks can we expect to be
correctly estimated as a function of how many observations per inflected
wordform are collected. The methods used here treat the stochastic
components of ratings (inter-participant differences and trial-level
variability) as normally distributed, which is surely wrong and affects
the estimates, but makes things simpler mathematically. Probably it
means that the resulting estimates of expected proportion correct are
overly optimistic. But hopefully the approach sketched out here is still
useful. The below is for English only but could be applied to data from
other languages too.

Ratings are on a \(0-7\) scale. First let’s look at the estimated
standard deviation \(\sigma\) of an individual-trial rating, including
both participant- and trial-level variability:

``` r
system.time(m <- lmer(past_rating ~ 0 + form + (1 | Participant),data=dat,REML=F))
```

    ##    user  system elapsed 
    ##   0.167   0.006   0.176

``` r
summary(m)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: past_rating ~ 0 + form + (1 | Participant)
    ##    Data: dat
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3952.8   4678.3  -1825.4   3650.8      751 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2046 -0.6814  0.0491  0.6962  3.1635 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 1.688    1.299   
    ##  Residual                3.009    1.735   
    ## Number of obs: 902, groups:  Participant, 36
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error t value
    ## formbaɪvd       4.6380     0.9692   4.785
    ## formbəʊv        5.5150     1.0836   5.089
    ## formblaɪk       1.9560     0.6853   2.854
    ## formblaɪkt      5.1500     1.0836   4.753
    ## formblɪt        4.1557     0.8191   5.073
    ## formblɪtɪd      2.2533     0.8848   2.547
    ## formbrɛmpt      4.7340     0.9692   4.884
    ## formbrɛndɪd     3.6100     1.0836   3.331
    ## formbrɛnt       4.7280     0.9692   4.878
    ## formbriːmd      5.8523     1.1983   4.884
    ## formbʊldɪd      6.4140     0.9692   6.618
    ## formbʊlt        1.9725     1.0836   1.820
    ## formdɛndɪd      1.1525     1.0836   1.064
    ## formdɛnt        2.4810     0.6853   3.620
    ## formdɪstraɪbd   4.7371     0.8191   5.783
    ## formdɪstrəʊb    2.1233     0.8848   2.400
    ## formdraɪnd      2.9010     0.6853   4.233
    ## formdrəʊd       4.7590     0.6853   6.944
    ## formdrəʊn       1.9575     1.0836   1.806
    ## formdruː        3.7025     1.0836   3.417
    ## formdʒəʊd       2.4430     0.6853   3.565
    ## formdʒɛndɪd     5.3275     1.0836   4.916
    ## formdʒɛnt       4.1560     0.9692   4.288
    ## formdʒɪŋkt      4.1250     0.6853   6.019
    ## formdʒuː        0.2575     1.0836   0.238
    ## formdʒʌŋk       0.2950     1.0836   0.272
    ## formfɪd         2.0330     0.6853   2.966
    ## formfɪdɪd       3.5450     1.0836   3.271
    ## formflɛd        5.2100     1.0836   4.808
    ## formflɛndɪd     2.9325     1.0836   2.706
    ## formflɛnt       3.3360     0.6853   4.868
    ## formfliːdɪd     6.4040     0.9692   6.607
    ## formfreɪk       1.9125     1.0836   1.765
    ## formfreɪkt      6.7460     0.9692   6.960
    ## formɡɑːst       2.8620     0.9692   2.953
    ## formɡɑːstɪd     5.9525     1.0836   5.493
    ## formɡeɪkt       5.9940     0.9692   6.184
    ## formɡraɪvd      3.3350     0.8848   3.769
    ## formɡreɪk       1.6514     0.8191   2.016
    ## formɡreɪkt      5.1567     0.8848   5.828
    ## formɡrəʊv       4.0429     0.8191   4.936
    ## formɡrɛmpt      4.6000     1.0836   4.245
    ## formɡrɛpt       3.0852     0.9417   3.276
    ## formɡriːmd      6.5440     0.9692   6.752
    ## formɡriːpt      4.4571     0.8191   5.441
    ## formɡrɔːt       2.4625     1.0836   2.272
    ## formɡrʌŋ        3.3990     0.6853   4.960
    ## formɡʊk         2.9075     1.0836   2.683
    ## formhaŋ         3.2971     0.8191   4.025
    ## formhaŋk        1.7617     0.8848   1.991
    ## formheɪkt       6.0020     0.9692   6.193
    ## formhəʊk        5.3750     1.0836   4.960
    ## formhɛndɪd      3.2776     1.1981   2.736
    ## formhɛnt        3.3730     0.6853   4.922
    ## formhɪŋd        5.5517     0.8848   6.275
    ## formhɪŋkt       5.0714     0.8191   6.191
    ## formɪmpleɪkt    2.5280     0.9692   2.608
    ## formɪmplʊk      5.3050     1.0836   4.896
    ## formklɛndɪd     3.1600     0.8191   3.858
    ## formklɛnt       2.4633     0.8848   2.784
    ## formkraɪtɪd     3.0850     0.8848   3.487
    ## formkraɪvd      4.4117     0.8848   4.986
    ## formkrəʊt       2.3757     0.8191   2.900
    ## formkrəʊv       1.8400     0.6753   2.725
    ## formkrɛndɪd     5.3850     1.0836   4.969
    ## formkrɛnt       5.7760     0.9692   5.959
    ## formkriːvd      5.2417     0.8848   5.924
    ## formkrɔːt       1.4071     0.8191   1.718
    ## formkrʌŋ        4.2800     0.8848   4.837
    ## formnaɪndɪd     1.9633     0.8848   2.219
    ## formnaʊnd       2.1086     0.8191   2.574
    ## formnɪd         2.8470     0.6853   4.154
    ## formnɪdɪd       0.1725     1.0836   0.159
    ## formpɛld        5.1983     0.8848   5.875
    ## formpɛlt        4.5514     0.8191   5.556
    ## formpɪd         2.1825     1.0836   2.014
    ## formpɪdɪd       3.2000     0.6853   4.669
    ## formpɪldɪd      2.4530     0.6853   3.579
    ## formpɪlt        4.6275     1.0836   4.270
    ## formpləʊd       5.4275     1.0836   5.009
    ## formplɛft       0.1325     1.0836   0.122
    ## formplɛndɪd     4.9743     0.8191   6.073
    ## formplɛnt       3.5750     0.8848   4.041
    ## formpliːvd      4.0242     0.7103   5.666
    ## formplɪŋkt      6.3067     0.8848   7.128
    ## formpluː        4.9550     0.6853   7.230
    ## formplʌŋk       2.5614     0.8191   3.127
    ## formpraɪtɪd     3.8350     0.8848   4.334
    ## formpraɪvd      3.8970     0.6853   5.686
    ## formprəʊt       2.4414     0.8191   2.981
    ## formprəʊv       2.2525     1.0836   2.079
    ## formseɪkt       2.7420     0.9692   2.829
    ## formsɛldɪd      2.4517     0.8848   2.771
    ## formsɛlt        3.0014     0.8191   3.664
    ## formsɪndɪd      2.1257     0.8191   2.595
    ## formsɪnt        3.5917     0.8848   4.059
    ## formskɛld       6.5000     0.9692   6.706
    ## formskɛlt       3.9300     1.0836   3.627
    ## formskɛt        3.7100     0.8191   4.529
    ## formskɛtɪd      4.5300     0.8848   5.120
    ## formskɪŋd       3.4120     0.9692   3.520
    ## formskɪŋkt      2.1650     1.0836   1.998
    ## formskrɪŋd      1.3083     0.8848   1.479
    ## formskrʌŋ       6.0100     0.8191   7.337
    ## formskʌŋ        3.7975     1.0836   3.504
    ## formskʌŋk       3.0080     0.6853   4.389
    ## formsmɪt        3.3532     0.9417   3.561
    ## formsmɪtɪd      3.3657     0.8191   4.109
    ## formsnaɪd       4.6200     0.8848   5.222
    ## formsnəʊd       2.3914     0.8191   2.919
    ## formsnɛld       4.6172     0.9417   4.903
    ## formsnɛlt       5.8243     0.8191   7.110
    ## formsnɛt        4.4800     0.9692   4.622
    ## formsnɛtɪd      4.1100     1.0836   3.793
    ## formspandɪd     5.5450     1.0836   5.117
    ## formspant       1.9540     0.9692   2.016
    ## formspəʊd       5.9400     1.0836   5.482
    ## formspəʊt       2.8300     0.6853   4.129
    ## formspɛt        3.5430     0.7103   4.988
    ## formspɛtɪd      2.8200     1.0836   2.602
    ## formspɪŋkt      1.5300     1.0836   1.412
    ## formspliːd      3.5371     0.8191   4.318
    ## formspliːdɪd    3.8100     0.8848   4.306
    ## formspraŋkt     4.8400     1.0836   4.467
    ## formsprɛpt      2.9433     0.8848   3.327
    ## formspriːpt     5.1943     0.8191   6.341
    ## formsprʌŋk      2.5100     0.6853   3.662
    ## formspʌŋk       2.8640     0.6853   4.179
    ## formstrɛd       1.7802     0.7103   2.506
    ## formstrɛdɪd     4.3425     1.0836   4.007
    ## formsʊk         5.6475     1.0836   5.212
    ## formswɪkt       6.5600     1.0836   6.054
    ## formswɪndɪd     4.7709     1.1981   3.982
    ## formswɪnt       2.4400     0.6853   3.560
    ## formswɪt        4.7040     0.9692   4.853
    ## formswɪtɪd      2.6650     1.0836   2.459
    ## formswʌk        4.2020     0.9692   4.335
    ## formtaɪndɪd     2.9683     0.8848   3.355
    ## formtaʊnd       1.9557     0.8191   2.388
    ## formtraɪmd      5.2933     0.8848   5.983
    ## formtrəʊm       1.3929     0.8191   1.700
    ## formtrɛpt       2.8825     1.0836   2.660
    ## formtriːpt      6.2820     0.9692   6.482
    ## formtʃɪd        2.7940     0.9692   2.883
    ## formtʃɪdɪd      5.1775     1.0836   4.778
    ## formvaɪndɪd     1.3270     0.6853   1.936
    ## formvaʊnd       6.1250     1.0836   5.652
    ## formzɛt         4.8329     0.8191   5.900
    ## formzɛtɪd       3.3183     0.8848   3.751

    ## 
    ## Correlation matrix not shown by default, as p = 149 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
#system.time(m1 <- lm(past_rating ~ 0 + form,data=dat.eng))
rating_sd <- sqrt(sum(as.data.frame(VarCorr(m))[,"vcov"]))
print(rating_sd)
```

    ## [1] 2.167223

and now, more crucially, let’s look at the distribution of absolute
difference scores in mean ratings between the regular and irregular
forms of each lemma:

``` r
averaged_rating_diffs <- dat %>%
  group_by(lemma, form) %>%
  summarize(rating=mean(past_rating,na.rm=TRUE)) %>%
  group_by(lemma) %>%
  summarize(ratingDiff=abs(rating[1]-rating[2]))
```

    ## `summarise()` has grouped output by 'lemma'. You can override using the `.groups` argument.

``` r
ggplot(averaged_rating_diffs,aes(x=ratingDiff)) + geom_histogram(binwidth=0.25) + theme_bw()
```

![](individual_judgments_exploratory_analysis_files/figure-gfm/abs_rating_diff_distribution-1.png)<!-- -->

Suppose that for each nonce lemma the plan is to collect \(n\) judgments
for each of the two candidate inflected forms. Each participant will
contribute one observation at most to judgments for forms of the lemma,
so the observations are IID with trial-level standard deviation
\(\sigma\) for a given form of as estimated above. If we call the
regular-form judgments \(\{r_j\}\) and the irregular-form judgments
\(i_j\), we are estimating the true difference score \(s\) by the
empirical difference of means:
\(\hat{s}=\frac{\sum_{j=1}^n r_j - \sum_{j=1}^n i_j}{n}\). Under the
assumption of IID normally-distributed errors we have
\(\hat{s} \sim N(s,\frac{2 \sigma}{\sqrt{n}})\). We can look up the
probability that the estimated direction of the difference score is the
same as the true difference score,
\(P(\text{sgn } \hat{s} = \text{sgn } s)\), from the normal cumulative
distribution function. And we can estimate the expected number of
differences scores that will go in the right direction by treating the
observed difference scores in the initial data sample as if they were
the true difference scores. This gives us the following curve of
expected number of lemmas for which the difference score goes in the
right direction as a function of \(n\):

``` r
prob_correct_sign <- function(true_abs_diff,n,rating_sd) {
  return(pnorm(true_abs_diff,mean=0,sd=2*rating_sd/sqrt(n)))
}
probs_correct_sign <- function(n) {
  return(sapply(averaged_rating_diffs$ratingDiff,function(x) prob_correct_sign(x,n,rating_sd)))
}

Ns <- seq(5,100,by=5)
ggplot(tibble(n=Ns,y=sapply(Ns,function(n) mean(probs_correct_sign(n)))),aes(x=n,y=y)) + 
  geom_line() + 
  theme_bw() + 
  xlab("Number of samples per form") +
  ylab("Expected proportion correct sign") +
  ylim(c(0,1))
```

![](individual_judgments_exploratory_analysis_files/figure-gfm/expected_correct_diff_sign_rate-1.png)<!-- -->

So, e.g. if one wants \(>90\%\) of judgments to go in the right
direction, maybe about 30 judgments per form is sufficient for English
(for German it’s similar; the Dutch data suggest we’d get to about 87%
with 30 judgments per person), but many more judgments would be
necessary to resolve the correct preference of the remaining forms. A
more adaptive approach, where more judgments per form are collected for
lemmas where the regular and irregular seem to have similar scores,
might not be a bad idea.

A larger potential point here, though, is that there is probably a lot
of information in the dataset that can be used to evaluate model quality
that would be lost under the micro-averaged correlation evaluation
approach.

### How predictive are lemma ratings of past-tense ratings?

This is an exploratory bit; “somewhat predictive” seems to be the
answer:

``` r
system.time(m_with_lemma_ratings <- lmer(past_rating ~ 0 + form + lemma_rating + (1 | Participant),data=dat,REML=F))
```

    ##    user  system elapsed 
    ##   0.158   0.004   0.164

``` r
anova(m,m_with_lemma_ratings)
```

    ## Data: dat
    ## Models:
    ## m: past_rating ~ 0 + form + (1 | Participant)
    ## m_with_lemma_ratings: past_rating ~ 0 + form + lemma_rating + (1 | Participant)
    ##                      npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
    ## m                     151 3952.8 4678.3 -1825.4   3650.8                     
    ## m_with_lemma_ratings  152 3944.5 4674.8 -1820.2   3640.5 10.292  1   0.001336
    ##                        
    ## m                      
    ## m_with_lemma_ratings **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
print(fixef(m_with_lemma_ratings)["lemma_rating"]) 
```

    ## lemma_rating 
    ##    0.1113945
