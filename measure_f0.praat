#############################################################
## measure_f0.praat
## 
## This script measures f0 at a user-specified number of points
## in all intervals on a user-specified tier from all files
## in a directory. 
## 
## It also collects information from up to two other tiers. 
## 
## A static floor and ceiling can be specified in the opening dialog box,
## or an ancillary file specifying speaker-specific parameters
## can be used. If so, the individual sound files should begin with the
## speaker code, directly followed by an underscore. 
##
## The following measurements are taken:
## - duration
## - information from up to two other tiers 
##   - (whatever label is present at the start point of the relevant interval)
## - f0 values at all measurement points
##
## This will process all .TextGrid/sound files in a user-specified directory.
##
## jessamyn.schertz@utoronto.ca
## August 2019 
#############################################################

clearinfo

beginPause: "User input"
	word: "dataDir", "/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/SB/output/sounds/"
	comment: "Tier including intervals to be measured"
	real: "f0Tier", 1
	comment: "Number of measurement points, including beginning and end"
	comment: "(3 = midpoint, 4 = every 1/3, 5 = every 1/4)"
	positive: "numPoints", 9
	comment: "Other tier to collect info from (0 if not relevant)"
	real: "otherInfoTier1", 0
	comment: "Yet another tier to collect info from (0 if not relevant)"
	real: "otherInfoTier2", 0
	word: "outputFile", "f0_measurements.txt"
	comment: "File specifying pitch-specific parameters (leave blank if you want to use the same values for all files)"
	word: "pitchInfo", ""
	comment: "Pitch floor and ceiling (this is overriden by pitch parameters file if specified above)"
	positive: "floor", 75
	positive: "ceiling", 320
endPause: "Continue", 1
 
files = Create Strings as file list: "", dataDir$+"*.TextGrid"
numFiles = Get number of strings

@createHeader

if pitchInfo$ <> ""
	pitchInfo = Read Table from tab-separated file: pitchInfo$
endif

# loop through these files
for file from 1 to numFiles
	select files
	filename$ = Get string: file
	filename$ = filename$-".TextGrid"
	ind = index(filename$, "_")
	speaker$ = left$(filename$, ind-1)

	if pitchInfo$ <> ""
		select pitchInfo
		row = Search column: "speaker", speaker$
		floor = Get value: row, "floor"
		ceiling = Get value: row, "ceiling"
	endif

	# Read in textgrid and sound file
	tg = Read from file: dataDir$+filename$+".TextGrid"
 	sound = Read from file: dataDir$+filename$

	# Create a pitch object
	pitch = To Pitch: 0, floor, ceiling

	# Loop through TextGrid and get measurements
	select tg
	numInt = Get number of intervals: f0Tier
	for syllInt from 1 to numInt
	
		select tg

		syll$ = Get label of interval: f0Tier, syllInt
		if syll$ == ""

			select tg

			start = Get start point: f0Tier, syllInt
			end = Get end point: f0Tier, syllInt
			dur = end-start

			# Get other info

			other1$ = "NA"
			other2$ = "NA"

			if otherInfoTier1 <> 0
				other1Int = Get interval at time: otherInfoTier1, start
				other1$ = Get label of interval: otherInfoTier1, other1Int
			endif
			
			if otherInfoTier2 <> 0
				other2Int = Get interval at time: otherInfoTier2, start
				other2$ = Get label of interval: otherInfoTier2, other2Int
			endif

			@getValues
			@writeToOutput

		endif
	endfor

	# Clean up
	select pitch
	plus tg
	plus sound
	Remove

endfor

procedure createHeader
	colsF0$ = ""
	for i from 1 to numPoints
		colsF0$ = colsF0$+"f0_"+"'i'"+tab$
	endfor
	header$ = "filename	speaker	syll	"
	header$ = header$+colsF0$-tab$
	writeFileLine: outputFile$, header$
endproc

procedure writeToOutput
	appendInfoLine: filename$
	newLine$ = filename$+tab$+speaker$+tab$+syll$+tab$
	newLine$ = newLine$+allF0$-tab$
	appendFileLine: outputFile$, newLine$
endproc

procedure getValues
	select pitch

	allF0$ = ""
	for i from 0 to numPoints-1
		measureTime = start+i*(dur/(numPoints-1))
		value = Get value at time: measureTime, "Hertz", "Linear"
		allF0$ = allF0$+"'value'"+tab$
	endfor
	
endproc
