GameDistr.exe
copyright 2002 - tangotiger@yahoo.com

Installation
-------------
Ensure that all your files appear in a folder called
c:\GameDistr


Purpose
-------
Given the runs scored / game of 2 teams, what is the expected win% outcome?


Input File
----------
Use the input file runmatch.txt to fill in your data.  There are 5 fields:
runs per game of team 1
runs per game of team 2
control value of team 1
control value of team 2
full report  (set to 1 if you want to create rundistrOutput.html, set to 0 for no output of this file)

You can put in multiple lines.  Make sure the last line shows
0 0 0 0 0

Make sure that all of your data is separated by TABS or SPACES.


Considerations
--------------
I suggest you keep the control value fixed at .760 to get the proper runs/inning distribution.  Set the control value to .852 to get the proper win% matchups.  More discussion on the control value can be found here
http://www.tangotiger.net/files/tangodist.zip


Output
------
The output will be found in the following files
winrecord.txt - final win%, along with the RPG of the 2 teams
rundistrOutput.html - the actual runs and win distribution for the 2 teams
