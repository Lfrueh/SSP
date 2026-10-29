> This project is currently being restructured. Existing code may not follow these conventions yet — apply them to new code and only refactor existing code when explicitly asked.

# Overview

As much as possible, try to be concise and explain concepts in simple language. If you ever have questions or unsure, either because I've underspecified something in the prompt, ask me. For me, data projects tend to be iterative, so think of all the organizational stuff as the result of lots of testing and back and forth (something to strive for). Comments should explain briefly what code does and then *why* a decision was made.

Any time that I ask for code and data examples, I'd like those to be in R (currently v4.5.3) unless otherwise specified. For wrangling tasks, code should be written using `tidyverse` packages and conventions. Whenever we save a script, it should be hit with the `air` code formatter (Posit's R formatter).

-   For **inferential / descriptive modeling** tasks, I prefer to use the `easystats` framework unless otherwise specified.
-   For **predictive tasks**, I prefer to use the `tidymodels` framework.

Any time you're implementing something like a standard error or model estimation problem from scratch, double check with me that some package or function doesn't already exist for solving the problem. In general, do your best to surface all subjective decisions that you're making as you implement a given modeling approach.

For general modern R syntax and related skills, I've installed a suite of Skills from <https://github.com/ab604/claude-code-r-skills/tree/main>

# Script Structures

R scripts should be organized as follows:

``` r
#' Brief 2 - 3 sentence description of purpose
#' Any dependencies that need to be run before hand 
#' ================================================

#' Libraries up top
 

 #' Imports from wherever the problem dataset points 
 
 #' Some code that does something (filter, joins, transformations here),
 
 #' Some code that does other stuff 
 
 #' Exports | written whenever we plan and discuss
```

1.  Don't stick a dependency in the middle of a script (always up top)
2.  Scripts should be modular, meaning that they should be broken up in pieces (1 script -- 1 task). We can develop specifics through planning and chat about what 1 tasks means. As a very loose example, one script flow might be:

-   **00_get_data_from_source.R** (e.g. pull data down from a database)
-   **01_prep_data:** Transform data for the given tasks (pivots, joins, filters and such)
-   **02_explore_data:** Derive some descriptive and make some plots
-   **03_make_models:** Fits a model on data
-   **04_assess_models:** Export and asset results

This is one crude example. Modularity of this type will depend on the task that we're working on and will probs require iteration and discussion.

3.  Separate sections of code with the following convention:

``` r
## ===============================================
#' Section 1 ----
## ===============================================

# Header 1 ----
## Header 2 ----
### Header 3 ----

## ===============================================
#' Section 2 ----
## ===============================================

# Header 1 ----
## Header 2 ----
### Header 3 ----

## ===============================================
#' Section n -----
## ===============================================

# Header 1 ----
## Header 2 ----
### Header 3 ----
```

Sections should be titled with brief and clear descriptions. Keep purpose statements to a minimum, and code comments precise. As much as you can, strive to tell me why you made the choices that you did, not what the code does. When I have questions related to how things work, and you can use the chat features to get at that sort of thing. Single dashes should be long enough that they trigger the outline behaviors in positron / r studio. (I'd usually do this by hitting ctrl + shift + r and backspacing (Equal signs fencing off sections should be that shortcut length))

# Project Structure

In general, I try to implement the following structure:

```         
project-root/
├── .git/
├── .gitignore
├── readme.md / Overview of the project (descriptions, script_flow, sources etc )
├── .renv artifacts (renv, if being used, last workflow thought so we're not constantly adding stuff as needed)
├── documentation (Where users can find additional info about datasets, code etc. that we've prepared or downloaded)
├── R/  Code created for our analysis 
├── data/  Data generated from our script structuring and intermediate objects 
├── results/ Stuff like typsts artifacts, pngs for plots, tables for papers 
```

I'll often use sub-folders to separate tasks further. So, for example, the data sub-folder might be split into `data/raw`, `data/clean`, `shapefiles` etc. Or they might be broken down by the actual concept or thing they describe (e.g. `data/prism`, `data/ipums` etc.). We'll figure these out as we work and through back and forth. Depending on the task, stuff like this would also apply to the other sub-folders.

In `R`, we should always have a `helper_functions` sub-folder where we can source any large functions that we've created to run in different scripts.

# Function Conventions

In general, I'll often ask to pull out things that can be generalized as R functions. Functions should be their own scripts stored in a sub-folder called `R/helper_functions`.

-   Never reference libraries in the function scripts. Everything should be namespaced.
-   Follow the same purpose convention as I described in the scripts section and the modern R skills I've loaded.
-   Where we've broken functions apart in one script, these functions should all help accomplish the same task and be broken where they reduce complexity and increase readability.
-   Feel free to suggest places where you think that they might be helpful

# Reproducibility

At a minimum, we strive for finished products that can be run all together from source or the command line, meaning our scripts should be coded portably to allow collaborators to run things. Where this isn't possible because data can't be shared or stored locally, we need to highlight these pain points in readme files accompanying the project.

I'm trying to be better about incorporating `renv`, but only after we've ensured that nothing else needs to be added to the project so that we can re-run things fresh, capture the dependencies and call it day. I think of it as a renv last workflow.

# Git & Github

I will always be the one to commit and comment. I'll ask you to make branches where needed. Don't make any commits or do any Git actions unless asked.
