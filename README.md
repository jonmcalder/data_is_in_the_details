# The Data is in the Details

## What is this repo?
This repo is for people who want to run the code chunks contained in [this blog post](). 
If you are one of these people, clone the repo and go wild! 

## The TL:DR
As part of a hobby project, I want to analyse data from a recent by-election that took place on 11 November 2020, in 95 municipal wards across South Africa.
Unfortunately for me, the IEC [provides the raw data](https://www.elections.org.za/content/Elections/Municipal-by-elections-results/) for results of each ward's by-election in a single `.xls` file containing both the ward details and the results in human-readable format.
Each file must be manually downloaded to disk and then imported into `R`.
This is where things get interesting.
When I opened the downloaded files I noticed that each file contains numerous blank rows, merged rows, merged columns, and cells that are actually variable names in rows other than the first. 
This human-readable formatting prevents me from simply using the `read*` functions in `R`. 

## Mission Impossible?

I need to import the results contained in each of the 95 `.xls` files into `R`, and construct them as tidy tibbles.
In my former life, I would have considered capturing the data manually. 
Now that I have limited time (and more wisdom), I just wrote a function that takes the garbled chaos of each file, processes it and spits out a complete, tidy tibble for use in my analysis.  
Does that sound impossible?  
I thought it might be at first...

 

