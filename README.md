---
output: html_document
---
FDBeye
====

This is a collection of tools to ease working with eyetracker
data. I'm slowly migrating stuff from my junkpile of project specific
code into this package. FDBeye is about post-acquisition data
processing. What's here now is focussed on my own workflows involving
data from SRR Eyelink systems, although some stuff may be more
generally useful. Functionality is a bit thin at present. Pitch in and
help if you like (see [Contributing][]).

Before you try FDBeye, you might want to look at what else is
available. See the list of [Other Eye Tracking Tools][] below.

Installing FDBeye
-----------------
First you should install <https://github.com/davebraze/FDButils>.

Then, to get the very latest from *FDBeye*, install it via devtools::install_github()
```R
install.packages("devtools")    ## if you don't already have it
library(devtools)
install_github("davebraze/FDBeye")
```

On the other hand, if you want a (possibly) more stable
experience. Download the most current release from here:
<https://github.com/davebraze/FDBeye/releases>. Then install it like
this

```R
install.packages(pathToFile, repos = NULL, type="source")
```

Where pathToFile is the full path and file name for the file you
downloaded.

License
-------

FDBeye is released under the MIT open source license. See the
[LICENSE](https://github.com/davebraze/FDBeye/blob/master/LICENSE)
file for details.

Contributing
------------

Contributions are welcome, but you might want to send me an email
before wading in too deep (<davebraze@gmail.com>).

_IMPORTANT_: In opening a pull request to FDBeye, (a) you affirm that
you are the copyright owner for the material contained in the pull
request, and (b) you agree to apply the
[MIT license](https://opensource.org/licenses/MIT) to the material
contained in the pull request. Contributors retain copyright to their
own code.

Other Eye Tracking Tools
------------------------

I've kept this list short, including only tools whose focus is
post-acquisition summarization and analysis, and where the source code
can be freely downloaded (i.e., you don't have to ask for it, but some
of the stuff I've linked to lacks an explicit Open Source
License). I'm sure the list is incomplete. If you know of something
that seems like it ought to be included, let me know. Better yet, fork
this repository, make the changes yourself, and send me a pull
request.

### *R* packages ###

  * ***bdots*** provides techniques for analyzing eyetracking data, as
    well as other types of highly correlated data that consist of many
    consecutive tests. We explain how to set the data up in a fashion
    for the package to use, how to analyze the data using the built in
    functions, and how to check the t to the data. On github at
    <https://github.com/MichaelSeedorff/bdots>, and CRAN at
    <https://cran.rstudio.org/web/packages/bdots/index.html>.
  * ***edfR***: R package for reading SRR *edf files
    directly. Codebase is both R and C++, and requires SRR's
    edfapi. Source available on github
    <https://github.com/jashubbard/edfR>. Linux and OSX only (no
    Windows), as of May 2016.  Alternate download at SRR help forum
    <https://www.sr-support.com/showthread.php?1416-Reading-EDF-files-in-GNU-R>
    (requires login via free account).
  * ***em2***: "package for computing reading time measures for
    psycholinguistics," i.e. regional reading time measures. R
    package, underlying code mostly C++. Source available on github
    <https://github.com/cran/em2>.
  * ***emov***: Functions for fixation/saccade detection. On CRAN
    <https://cran.r-project.org/web/packages/emov/>. Source is
    available on github <https://github.com/schw4b/emov>.
  * ***ETRAN***: Fixation detection, plotting. Available from this web
    site <https://sites.google.com/site/azhegallo/>, and written up in
    the journal _Perception_ here:
    <http://pec.sagepub.com/content/early/2015/08/14/0301006615594944.full>.
  * ***eyelinker***: Tool for converting SRR *ASC files to
    data.frames. Available on github
    <https://github.com/dahtah/eyelinker>. Not clearly open source; no
    license statement at all.
  * ***eyetracking***: Compute pixel-to-pixel and eye-to-screen
    distances. On CRAN
    <https://cran.r-project.org/web/packages/eyetracking/>.
  * ***eyetrackingR***: Package oriented toward post-acquisition
    workflow, mostly for 'visual world' style studies. On CRAN
    <https://cran.r-project.org/web/packages/eyetrackingR/>. It also
    has its own web site here: <http://www.eyetracking-r.com/>. The
    source is available on github
    <https://github.com/jwdink/eyetrackingR>.
  * ***fix_align.R***: automatically adjusts y coordinate of fixations
    in multi-line reading paradigm so that each falls on a specific
    line of text. As written, code is specific to SRR systems
    (requires SRR ASC files as input). At UMass
    <http://www.psych.umass.edu/PACLab/resources/>. Described in
    *Behav Res*: <http://people.umass.edu/eyelab/CohenBRM.pdf>. Source
    file (v0p92) includes the notice "(c) 2012, University of
    Massachussetts, All rights reserved". The latter bit seems to
    entail that the code may not be modified, redistributed, or even
    used without first obtaining explicit permission from UMass. Odd.
  * ***gazepath***: Functions for fixation/saccade detection. On CRAN
    <https://cran.r-project.org/web/packages/gazepath/>. Source on
    github <https://github.com/cran/gazepath>.
  * ***gazetools***: Functions for processing and classifying eye gaze
    data (into blinks, fixations, etc.). Centered on SMI trackers, but
    includes some generally useful stuff. Available on github
    <https://github.com/RyanHope/gazetools>.
  * ***itrackR***: Read and analyze data from SRR eyelink systems. On
    github <https://github.com/jashubbard/itrackR>.
  * ***lookr*** provides a set of tools for analyzing
    looking-while-listening eye-tracking experiments performed by the
    Learning to Talk project. Development is ongoing. On github at
    <https://github.com/tjmahr/lookr>.
  * ***saccades***: Functions for fixation/saccade detection. On CRAN
    <https://cran.r-project.org/web/packages/saccades/>. Source on
    github <https://github.com/tmalsburg/saccades>.
  * ***ScaSim***: Tools for analyzing scanpaths
    <https://github.com/tmalsburg/scanpath>
  * ***pdata***: Purports to "Process data analysis and visualization
    for large scale educational assessments" but also includes some
    tools for visualization of gaze data. See heatmapanalysis() and
    scanpathPlot(). Available on github at
    <https://github.com/garyfeng/pdata>.
  * ***VWPre***: Provides functions for preparing visual world
    eye-tracking data for statistical analysis and plotting in R.
    Designed for handling data from SRR Eyelink trackers using Sample
    Reports created in SRR Data Viewer. Available on CRAN:
    <https://cran.r-project.org/web/packages/VWPre/index.html>, and
    also on github <https://github.com/cran/VWPre>.

### *Python* packages ###

  * ***cili***: on github <https://github.com/beOn/cili>. Reduce
    overhead in working with SRR Eyelink data. Extracts events from
    SRR ASC files and provides as pandas DataFrames.
  * ***DynAOI***: on launchpad
    <https://launchpad.net/dynaoi>. Described in *Behav Res*
    <https://www.iwm-tuebingen.de/cybermedia/dynaoi/PapenmeierHuffBRM.pdf>. Analysis
    of EM in relation to moving objects. Python codebase, relies on
    Blender for some functions. Last update in 2011.
  * ***EMDAT***: A library for processing eye gaze data exported from
    Tobii studio software. Available from authors' web site
    <https://www.cs.ubc.ca/~skardan/EMDAT/>. Last update in 2012.
  * ***eventdetect***: on github
    <https://github.com/gian/eventdetect>. Repository of event
    detection algorithms. Python codebase. Last update in 2015.
  * ***EyeSimplify***: web site at
    <http://www.cl.uni-heidelberg.de/seminare/studpro06/eyesimplify/>. Convenience
    wrapper with automation for UMass tools (*EyeDoctor*
    etc.). Possibly superceded by *RoboDoc.py*. Python codebase. Last
    update in 2007.
  * ***GazeParser***: "an open-source library for low-cost eye
    tracking and data analysis; it includes libraries for data
    recording and analysis." On sourceforge at
    <http://gazeparser.sourceforge.net/>. Also written up in *Behav
    Res* here
    <http://link.springer.com/article/10.3758/s13428-012-0286-x/fulltext.html>. System
    designed to record EM with commercially available cameras. Not
    clear that included analysis tools have more general utility.
  * ***ocupy***: on github <https://github.com/nwilming/ocupy>. Python
    codebase. "tools for analyzing eye-tracking data". last update in
    2015.
  * ***pyarbus***: on github
    <https://github.com/ivanov/pyarbus>. Python codebase. Major
    functionality seems to be for extracting sample and message info
    from SRR *asc files. Last update in 2013.
  * ***pyeparse***: on github
    <https://github.com/pyeparse/pyeparse>. Python
    codebase. "analyzing eye tracking data from cogntive sciences
    experiments," SRR eyelink trackers. Last update in 2015. Seems
    active.
  * ***PyGaze***: on github
    <https://github.com/esdalmaijer/PyGaze>. Python codebase. Focussed
    on data collection "programming of eyetracking experiments". Web
    site at <http://www.pygaze.org/>. Also see BRM paper
    <http://link.springer.com/article/10.3758%2Fs13428-013-0422-2>. Actively
    developed.
  * ***PyGazeAnalyser***: Post-acquisition processing tools associated
    with PyGaze. On GitHub at
    <https://github.com/esdalmaijer/PyGazeAnalyser>. Parses SRR ASC
    files and SensoMotoric IDF ASCII files. Plots heatmaps, scanpaths,
    fixation locations.
  * ***Samara***: on github
    <https://github.com/m-macaskill/Samara>. Python codebase. Saccade
    analysis, SMI iView trackers. Last update in 2013.

### *Matlab* toolkits ###

  * ***calib_adjust_code*** web page at
    <http://langcog.stanford.edu/materials/calib.html>. Method for
    offline calibration adjustment of Tobii eyetrackers, specifically
    geared toward work with infants. Additional description in the
    journal _Infancy_
    <http://langcog.stanford.edu/papers/FVS-infancy2012.pdf>.
  * ***EALab***: web site <https://ealab-matlabtoolbox.rhcloud.com/>.
    Described in *Neuroinform*
    <http://dx.doi.org/10.1007/s12021-015-9275-4>. Matlab
    toolbox. Classification and extraction of eye
    activity/events. Seems current.
  * ***ETML***: on github <https://github.com/jwdink/ETML>. Matlab
    codebase. "Framework for running eyetracking experiments in
    MATLAB," with SRR eyelink systems. Seems active.
  * ***EyeMMV***: on github
    <https://github.com/krasvas/EyeMMV>. Matlab
    toolbox. post-collection eye movement analysis. Last update
    in 2014. Described in J. of *Eye Movement Research*
    <http://www.jemr.org/online/7/1/1>.
  * ***eyetrack-tools***: on github
    <https://github.com/racheldenison/eyetrack-tools>. "Tools for
    interfacing between EyeLink 1000 and Psychtoolbox (Matlab)". No
    updates since April 2014.
  * ***FieldTrip***: Primarily geared toward EEG/MEG data work, but
    also includes some functions for working with SRR ET
    data. Developed at the
    [Donders Institute in Nijmegen](http://www.ru.nl/donders), it can
    be downloaded from <http://www.fieldtriptoolbox.org>.
  * ***GazeAlyze***: Purports to handle data from ISCAN, SMI,
    Arrington Research, and Tobii devices. Deals with both static and
    dynamic stimuli. Last release in Dec. 2015. Code available on
    SourceForge
    <https://sourceforge.net/projects/gazealyze/files/gazealyze170.zip/download>.
  * ***iEye***: Matlab toolbox providing GUI based interface for
    interacting with eye-movement data. On Github
    <https://github.com/wemackey/iEye>. Install instructions on
    developer's web site <http://wemackey.github.io/iEye/>. Uses SRR
    edf2asc command line utility to convert *edf files to *asc
    files. Reads data from SRR *asc files.
  * ***iMap***: author's website
    <http://perso.unifr.ch/roberto.caldara/index.php?page=3>. Fixation
    pattern analysis and visualization. Described in a couple of
    journal articles: *Behav Res*
    <http://perso.unifr.ch/roberto.caldara/pdfs/caldara_11.pdf> and *J
    of Vision*
    <http://jov.arvojournals.org/article.aspx?articleid=2433901>. Matlab
    toolbox. Seems current.
  * ***iTrack***: on github
    <https://github.com/jashubbard/iTrack>. "Matlab toolbox for
    importing and processing eye tracking data from EyeLink eye
    trackers".
  * ***ScanMatch***: web page
    <http://seis.bris.ac.uk/~psidg/ScanMatch/>. Described in *Behav
    Res* <http://seis.bris.ac.uk/~psidg/ScanMatch/CMTG2010.pdf>. Scan
    path analysis. Matlab toolbox. Last update 2009.
  * ***TimeStudio***: A general purpose workflow system that includes
    some components for processing gaze data from Tobii ET
    systems. Project URL is <http://timestudioproject.com/>. Also
    written up in *Behav Res*
    <http://link.springer.com/article/10.3758/s13428-015-0616-x>.

### Other languages ###

  * ***CARPE***: on google code
    <https://code.google.com/archive/p/dynamic-images-and-eye-movements/>. visualization
    and collection of gaze patterns over dynamic images. Project home
    page on wordpress <https://thediemproject.wordpress.com/>. Looks
    fairly polished, but no recent development (since 2010).
  * ***EyeMap***: on sourceforge
    <http:/openeyemap.sourceforge.net/>. Java codebase. Looks like
    reasonably good tools for analysis of EM over print. Supports data
    from both SRR and Tobii systems. It's not clear to me that there
    are means to specify interest areas other than for single
    words. Last update in 2012.
  * ***EyePatterns***: on sourceforge
    <http://sourceforge.net/projects/eyepatterns/>. Looks for
    patterns/similarities in fixation sequences. Java codebase. Last
    update in 2013.
  * ***eyetracker***: on github
    <https://github.com/stefan-k/eyetracker>. C++ codebase. Last
    update in 2012.
  * ***EyeTrack***, ***EyeDoctor***, ***EyeDry***, ***RoboDoc.py***:
    Designed for reading studies and for use with SRR ET
    systems. Available from Staub lab at UMass
    <https://blogs.umass.edu/eyelab/software/>. C++ codebase, some
    Python. Seems current.
  * ***OGAMA***: on codeplex <http://www.ogama.net/>. Data collection,
    fixation detection, scanpath analysis, and heatmaps. Supports
    Tobii and Alea Tech ET systems only. Also does mouse tracking. C#
    codebase. Seems current.
  * ***VizFix***: A "tool for visualizing gaze fixations and other
    data from eye tracking experiments." Seems to be focussed on data
    from LC Tech devices. Objective-C codebase requires Mac OS X 10.5
    or better. Download source at Google Code:
    <https://code.google.com/archive/p/vizfix/source>. Authors' web
    site is here:
    <https://www.cs.uoregon.edu/research/cm-hci/VizFix/>. Last update
    in 2010.
