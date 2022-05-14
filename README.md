# Archery Rating (Using PlackettLuce)

Extract recurve and compound event scores from [Ianseo](https://ianseo.net/) and
builds a website containing the resulting ranks of all archers. The statistical
analysis was written in *R*, using the
[PlackettLuce](https://hturner.github.io/PlackettLuce/) package.

The extraction of scores from Ianseo was done using
[IanseoParse](https://github.com/Alcalol/IanseoParse). This was written in
*Java* and is a submodule of this repository.

Contribution, using or forking this repository is welcomed. I work on this in my
spare time and I am a novice in *R*. I may be available for further calibration,
please find my contact details on my
[GitHub profile](https://github.com/shermanlo77). Please see `LICENSE` and cite
any forthcoming publications where appropriate.

## [Main Website](https://shermanlo77.github.io/archeryratingweb/)

## How to Use (Linux Recommended)

- Compile the *Java* code using *Maven*, for example using

```Shell
mvn -f IanseoParse package
```

- Extract Ianseo scores and save them as `.csv` files. See
[IanseoParse](https://github.com/Alcalol/IanseoParse) for further information.
The following examples are provided:

```Shell
java -jar IanseoParse/target/IanseoParse-x.x.x.jar -t uk_2019.txt
```

to extract UK events for 2019

```Shell
java -jar IanseoParse/target/IanseoParse-x.x.x.jar -t uk_2021.txt
```

to extract UK events for 2021.

- For the above example, run the scripts `uk_2019.R` and `uk_2021.R` to build
the respective websites. This is computationally intensive, especially if there
are ties. Multiple threads and at least 16 GB of RAM is recommended.

- To build a website using other events, see the procedure `archeryRatingHtml()`
in `archeryrating.R`.

## More Information

The Plackett-Luce ranking is based on pairwise comparisons, who you win and lose
against to. Each archer starts with 1440 points (to try and be similar to the
handicap score in the UK). Each archer wins and lose points based on who they
win and lose to, but also based on their opponents’ future performance.

The points are updated to reflect the statistical property of each archer. More
specifically, it estimates the probability that you will beat an archer in a
format where it is either matchplay or a WA 720 chosen at random. The formula
for the probability that archer A beats archer B is
$$
P(\text{A beats B}) = \dfrac{1}{1+10^{(R_B-R_A)/400}}
$$
where $R_A$ and $R_B$ are the points for archer A and archer B respectively.

This has numerous advantages over previous ranking systems. Score based ranking
systems (add up your best 5 scores) do not take into consideration bad weather
scores because they are typically low and are not used. Position based ranking
systems do not take into account the playing field, the 1st place in a
competition may be more meaningful compared to the 1st place in another
competition.

The Plackett-Luce ranking system overcomes these problems by using pairwise
comparisons. In bad weather, it is who you beat which matters, not your score.
In tournaments of different scales, you are rewarded or punished depending on
who you win or lose against to which takes into account the playing field. A
disadvantage of this ranking system is that it is complicated compared to
previous ranking systems. It is not very clear how you can improve your rank
impart from beating archers who are ranked higher than you.

The method uses the maximum likelihood. This means that points will fluctuate
rapidly at first but steadies out as more and more archery events are attended.
This is similar to estimating the probability of a coin flip landing heads. With
one coin flip, your estimate is either 0% or 100%, but with more and more coin
flips your estimate will almost surely be 50%. Therefore, to exploit this
ranking method, win against the best archer once and stop playing.

To tackle this, uncertainty has been provided, if available, which quantifies
the possible fluctuation in estimation. A smaller uncertainty suggests that the
archer has competed in more events.

Similar ranking systems include the Elo rating system (used in chess) and the
Bradley–Terry model. However, it should be noted that the number of tournaments
an archer enters is significantly fewer compared to, for example, the number of
matches a chess player plays online. Ranking systems should be developed and
tuned to the game/sport used.

## References and Acknowledgement

- [Turner, H.L., van Etten, J., Firth, D. and Kosmidis, I., 2020. Modelling rankings in R: The PlackettLuce package. *Computational Statistics*, pp.1-31.](https://link.springer.com/article/10.1007/s00180-020-00959-3)
- The results were extracted from [Ianseo](https://www.ianseo.net) with the help
from Tony Sze using [IanseoParse](https://github.com/Alcalol/IanseoParse).
- Helpful discussions with students and academic staff at the University of
Warwick.

## Issues

- Variation on names including typos, for example Chris and Christopher, are
treated as different people. For the WA example, the names are cleaned in
`cleannames.R`. Otherwise, it is a matter of data cleaning.
- Two different people with the same name may have their results merged, causing
unexpected results.
- There are issues with scaling to WA events, eg lots of ties in qualification
will make the computation harder. The WA 2019 example is still in development.

## Potential Further Development

- Use of JavaScript technology for the website
- A move away from *R* and use the source code of PlackettLuce directly and more
efficiently

## Licenses and Other Information

- Copyright (c) 2019-2020 Sherman Lo. See `LICENSE` for further information.
- GPL-3.0 License (it should be noted that the
[PlackettLuce](https://hturner.github.io/PlackettLuce/) package uses the GPL-3.0
License too).
