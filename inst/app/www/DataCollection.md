----
title: Data Collection
----

# Data Collection 

Before explaining the strategy behind each stage of data collection we introduce
notation so that the technical reader can identify consistent terms 
across all stages of data collection and methods.

Consider the $i$th PGH resident, $(i=1,...,N)$ who performs each of $K$ trips 
on a regular basis, say a typical week. We assume, to start, that the mode of 
transit is fixed per trip - a person **only** walks (for example) to the 
grocery store. The mode of transit for the $i$th resident is denoted as $m_i$,
where $m_i \in \{1,...,M\}$ and $M$ is the total possible modes of transit. 
This resident may also have certain characteristics, such as race, income, etc.
that may be relevant to understanding their mode choice. We denote the 
vector of these $p$ characteristics as $Z_i \in \mathbb{R}^{p}$. Similarly,
we measure route-specific characteristics, $X_i \in \mathbb{R}^q$, which 
may include the difference in expected travel time to a given destination
between biking and driving.

With this preliminary notation aside, we now describe how these data are 
collected or generated in each of the three stages constituting the PTPT's 
development.