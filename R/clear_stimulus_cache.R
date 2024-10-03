#'Clear cache used by stimulus.plot() to re-use resampling results from identical previous calls.
#'
#'When stimulus.plot() type='effects' is run, the resamples are saved to the local environment and if 
#'an identical call is ran again (same data, same dv, same condition, same number of simulations, etc)
#'the results are loaded instead of re-calculated. You can force recalculation by deleting the cache with this function.
#'@export

clear_stimulus_cache=function ()
{
   .GlobalEnv$.stimulus.cache=list()
   if (length(.GlobalEnv$.stimulus.cache)==0) message2("Cache for stimulus.plot() has been cleared.")
   if (length(.GlobalEnv$.stimulus.cache)>0) message("Failed to clear cache for stimulus.plot() .")
}