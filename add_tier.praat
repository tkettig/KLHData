#Adds a tier for all textgrids in a directory

form Add tier
   sentence Directory_name: /Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/IN/output/sounds
endform

Create Strings as file list... list 'directory_name$'/*.TextGrid
num = Get number of strings
for ifile to num
	select Strings list
	fileName$ = Get string... ifile
	Read from file... 'directory_name$'/'fileName$'
	textGridID = selected("TextGrid")
	Insert interval tier: 1, ""
	Save as text file... 'directory_name$'/'fileName$'
endfor
select all
Remove
