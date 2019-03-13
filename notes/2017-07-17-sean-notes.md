# Thoughts on 2017-07-17

Uniform distributions seem less than ideal but perhaps easier than arguing about shapes of distributions and means.

Other distributions can be used through the cpars (custom parameters) slot by specifying vectors of data to draw from. 

Hard part will be coming to consensus on operating model.

We would need multiple stocks for a prioritization example but only one or a few stocks as an example of a suggested framework for using DLMtool to choose a management procedure.

What happens if at a future assessment a different management procedure becomes more ideal? Presumably this would be a fluid choice and the management procedure could change from assessment to assessment? 

Need to talk with Tom or Adrian about specifics of how some of the operating model parameters are integrated. Or dig through the code, which has been helpful so far. 

One good place to start would be to write some of our own simulation verification code to make sure the operating models and management procedures are correctly coded and that true values can be recovered without bias given near perfect information. (Actually, somewhat hard to do because most estimation models are not estimating direct input parameters of the operating model.)

Would be useful to implement some ggplot functions for some of the main plots. For example, the various trajectory plots. Need to use a better color scheme that is also colorblind proof. Viridis? RColorBrewer::brewer.pal Dark2?

Is there anyway we can use information from multiple reasonable models? Using the pure means of multiple models could be dangerous, but using the spread of the models as a measure of model uncertainty could be useful. 

Perhaps there is some innovative graph that could show uncertainty across multiple models? A Kobe plot? Trajectory plots?

Need to dig into how to extract and plot out more elements of operating model trajectories. 

The DLMtool implementation can really only be a part of the entire framework. E.g. have a look back at the Methot NOAA document (I think). Would be useful to sketch out a diagram of how it could fit into the entire process.

Where possible, should maintain elements of DFO East Coast framework. For consistency. And helps us avoid reinventing the wheel.

I would find it useful to create a table of all of the included management procedures.

Are there any major other assessment measures or management procedures we should consider implementing? What about things like CMSY? May very well want to develop more management procedures with existing estimation models.

A full research document would probably include a much more verbose description of all of the slots available in the model objects.

Need to generate clean starting spreadsheets that are up to date with the latest version of the package.

Should be relatively trivial to try peeling away information starting with something like Pacific ocean perch or a simulated operating model. Similar to the operating model more ideal in that we know the truth. 

Need to look into code to figure out exactly what is plotted in the DFO plots. Perhaps I could make a pull request with fleshed out versions of the descriptions.

Perhaps I could wrap a number of ggplot functions that work on these classes into their own R package.

## Things to discuss with Elise

Things that will be helpful:

- Table summarizing the available methods, data requirements, original citation, notes on its use perhaps with some example citations, pros and cons, any papers that have evaluated its efficacy with simulated data (or real data?)
- What data is available in our databases on the various groundfish species of interest? Is there anything we can use in developing the operating models? This is both in biology and in terms of fisheries characteristics.
- What are the latest DFO documents available on the species of interest? 
- For a very limited number of them, is there useful data in the literature or on FishBase that we can use?
- Some of this might involve us trying to generate a reasonable operating model for one of these species and then figuring out where the stumbling blocks are.

## Things to discuss with Robyn

- Let's discuss what successful outcomes would look like for this project 
- Where should we start? 
- How involved should Elise and I be vs. Robyn?
- What various CSAS documents might there be and what papers might there be
- Could even be as simple as some closed-loop simulations showing performance of using simulation testing to choose an approach versus some default approach in the context of species managed by DFO and using precautionary approach goals benchmarks etc.
