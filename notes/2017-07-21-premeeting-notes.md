---
output:
  pdf_document: default
  html_document: default
---
# Thoughts before meeting on 2017-07-26 (Robyn, Elise, Sean)

## Next steps based on recommendations from the tiered approach workshop:

- start a Technical Working Group
- find out more and finish scorecard
- identify management objectives and performance metrics 
- test data-stripping and compare models systematically (similar to Rosenberg et al. FAO?)
- develop prioritization scheme (similar to Methot NOAA document)
- multi-species management systems?

## Possible separate subprojects involved here:

Testing performance of various management procedures and estimation models on operating models that are reasonable representations of data-limited West Coast fisheries. Are there commonalities to the operating models where various methods work well? How consistent are the various methods? Is there a general trend in advice or estimates when we move from more data rich to more data port methods? 

Developing a prioritization scheme and weights possibly similar to the NOAA framework described in Rick Methot's report. This is largely a collaborative and consensus building exercise. 

Multi-species management systems? Greg seems particularly interested in this. I believe is this partly about how can we produce CSAS documents that report on multiple species at once. Used to do this? Only in Res. Docs?

Identifying management objectives and performance metrics. This could be taken from existing objectives and metrics commonly used in DFO MSEs for more data rich species. Or this might involve discussion and meetings to come up with new ones relevant to these data-limited species. 

The scorecard. What exactly is this? I think we could write something that dynamically pulls from the databases and generates reports on each species. This could either be in the form of RMarkdown into a PDF or HTML document. Could also be an internal Shiny app.

Starting a Technical Working Group. This seems like a first priority. What is the process to go about this?

Testing of DLMtool. We need to have faith that the code is right. Unfortunately, hard to fully test the operating model code. Fairly monolithic and hard to unit test. At the very least we can test that the various estimation models are coded correctly by coding our own or comparing to an existing implementation elsewhere.

A user's guide to DLMtool for use by DFO. The existing user guide is great as a very high-level introductory guide. For anyone to use this effectively, they will need much more extensive descriptions and best practices for building operating models and understanding the various parameters. Would also be good to develop a framework RMarkdown document to document the process of generating the operating model. We may want to plot out more of the internal operating model to have full faith in what's going on under the hood.

## Random thoughts:

- table of all of the included management procedures?
- merge the GitHub repositories?
- what successful outcomes would look like for this project?
- what is our order of priorities and order of attack?
- how much of a role do you (Robyn) want to take in this?
- what CSAS documents and papers should we be aiming for?
