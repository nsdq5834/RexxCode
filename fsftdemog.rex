/* Rexx program

   fsftdemog.rex
   
   Base code	05/22/2019
   Revision 1   05/29/2019
   Revision 2   05/30/2019
   Revision 3   05/31/2019
   Revision 4   06/02/2019
   Revision 5   06/02/2019
   Revision 6   06/16/2019
   Revision 7   06/17/2019
   
   This is a simple homegrown utility program that determines the storage
   drives that are available on the system. Once we have a list of all the 
   storage devices, we will obtain the contents of the file system on each
   device and do a simple demographic analysis of the file types that are
   found.
   
   File System File Type Demographics --> fsftdemog

   See if we were passed an argument.  If so, see if it is equal to the
   word debug. If it is, set a logic flag that we will use to control the
   messages we will write to our log file.
   
   potential keyword adds:
   output=csv or text
   drivetype=used etc
   detail=full or summary
   debug=     not sure on this one yet.
   
   consider a simple function to break down the options.
   possibly use an array to house the keywords and their status.
   
*/

/* Define some variables and initialize them.                                */

debugFlag = 0
driveOption = 'USED'
Drives = .stem~new
extName = .stem~new
extCntr = .stem~new

parmFile = 'fsftdemog.parm'

call setParameters

arg passedValues
numParms = words(passedValues)

if numParms = 0 then
  do
    say 'All parameters set to their default values'
	say parmValues[1,1] '=' parmValues[1,2]
	say parmValues[2,1] '=' parmValues[2,2]
	say parmValues[3,1] '=' parmValues[3,2]
	say parmValues[4,1] '=' parmValues[4,2]
  end

cntParm = 0

loop label cntParm while cntParm < numParms & ,
	   numParms > 0
  cntParm = cntParm + 1
  subParm = word(passedValues,cntParm)
  call IsoParms  
end cntParm	   
  
exit

/*
  The IsoDrives procedure is used to locate drives of a particular type that
  are accessible to the system. Use the SysDriveMap function to obtain the list
  of drives. Take the returned information string and break it down into the 
  drive letters and place them into the Drives. stem variable.
*/
 
call IsoDrives

if Drives.0 = 0 then exit

say Drives.0 driveOption 'drives detected'

if Drives.0 = 0 then
  do
    say 'No drives of type' driveOption 'were detected'
	exit
  end

/*	Process all of the drives that we were able to detect.                   */

do drivePointer = 1 to Drives.0
  
  filePattern = Drives.drivePointer || '\*.*'
  sftRC = SysFileTree(filePattern,retFiles,'FS')
  
  if retFiles.0 = 0 then iterate

/*	Process all of the files that SysFileTree has returned in retFiles stem  */
  
  do filePointer = 1 to retFiles.0
  
    parse var retFiles.filePointer sflDate sflTime sflSize sflAttrib sflFname
	extType = IsoExtension(sflFname)
	
/*	See if we have what appears to be a valid file extension.                */

	if extType = '*BOGUS*' then iterate
	
/*
	The following if statement determines if we have already encountered the
	extension value that is in extType.  If we have, we will increment the
	count for that extension.  If we have not, we will add the new extension
	value to the extName stem, and then set the count to 1 in extCntr.
*/

	if extName~hasItem(extType) then
	  do
	    curIdx = extName~index(extType)
		extCntr[curIdx] = extCntr[curIdx] + 1
	  end
	else
      do
	    NextIndex = extName~items
		NextIdx = (extName~items) + 1
		extName[NextIdx] = extType
		extCntr[NextIdx] = 1
      end
	
  end filePointer
  
/*
	extName~items indicates the number of values in the extName stem.        
	
	GapElements is the upper limit for the gap or increments values we will
	need to implement the Shell or diminishing increment sort.
*/ 

  TotalElements = extName~items
  GapElements = TotalElements / 3 
 
/* Set up some variables so we can calculate our gap or increment values.    */

  ii = 0
  incVal = 0
  shellInc. = null
  
/*	Compute some increment or gap values using a formula suggested by Knuth. */
  
  do until incVal > GapElements
    ii = ii + 1
    incVal = ((3 ** ii) - 1) / 2
    shellInc.ii = incVal
  end


/*
	The following resources were used to code the Shell sort.

	https://rosettacode.org/wiki/Sorting_algorithms/Shell_sort#ooRexx
	https://en.wikipedia.org/wiki/Shellsort

	From the previous do loop, ii points at the last calculated increment or
	gap value that we calculated. The last element will be greater that the
	upper limit that we specified in GapElements. We will set our increment
	starting index to ii - 1.
*/

  incStart = ii - 1
  
/*
	The Shell sort consists of three loop structures. The first loop steps
	through the calulated gaps from the largest value down to a gap value of
	1. The other loop structures are the outer loop and the inner loop. The 
	current gap value dictates how we process through the outer/inner loops.
*/

  do gapPoint = incStart to 1 by -1
  
    currentGap = shellInc.gapPoint
	
    do outerP = currentGap to TotalElements - 1
	
	  tempName = extName[outerP]
	  tempCntr = extCntr[outerP]
	  innerP = outerP
	  
	  loop label innerP while innerP >= currentGap & ,
	   extCntr[innerP - currentGap] < tempCntr
	   
		extName[innerP] = extName[innerP - currentGap]
		extCntr[innerP] = extCntr[innerP - currentGap]
		innerP = innerP - currentGap
		
	  end innerP
	  
	  extName[innerP] = tempName
	  extCntr[innerP] = tempCntr
	  
	end outerP
	
  end gapPoint
  
/*
	Currently echoing out the counts for the top 15 file types by count that
	were observed on the current drive. May change this at some point to able
	different option or options for outputting the data.
*/
  
  say 'Breakdown for Drive ' Drives.drivePointer
  say ' '
  
  do kkk = 1 to 15
    say extName[kkk] ' <--> ' extCntr[kkk]
  end kkk
  
/*	Clear our two stem variables.                                            */
  
  extName~empty
  extCntr~empty
  
end drivePointer

exit

IsoDrives: procedure EXPOSE driveOption Drives.

UsedDrives = strip(SysDriveMap(,driveOption),'B') || ' '
NumUsedDrives = length(UsedDrives) / 3

drivePoint = 1 

do curDrive = 1 to NumUsedDrives
  Drives.curDrive = substr(UsedDrives,drivePoint,2)
  drivePoint = drivePoint + 3
end curDrive

Drives.0 = curDrive - 1

return

/*
  The IsoExtension is used to isolate what appears to be a file's extension
  value. 
*/

IsoExtension: procedure

arg FullFileName
OnlyFileName = ''

/*	Clean up the passed in file name, and then obtain it's length.           */

FullFileName = strip(FullFileName,'B')
lengthFFN = length(FullFileName)
lengthFFN1 = lengthFFN
periodPos = lastpos('.',FullFileName)

/*	Eliminate files with no period and any files with period in position 1.  */

if periodPos == 0 then OnlyFileName = '*BOGUS*'

if periodPos < 2 then OnlyFileName = '*BOGUS*'

/*	Arbitrary cut off value for the size of the file extension.              */

if lengthFFN - periodPos > 10 then OnlyFileName = '*BOGUS*'

OnlyFileName = substr(FullFileName,periodPos+1,lengthFFN-periodPos)

return OnlyFileName

/*
	Simple procedure to process any passesd parameters.
*/

IsoParms: procedure EXPOSE parmValues subParm

eqPos = pos('=',subParm,1)

if eqPos = 0 then
  do
    say 'Badly formed parameter passed at invocation -->' subParm
	subParm = '##'
	return
  end	
  
parse var subParm sP1 '=' sP2

spIndex = parmValues~index(sP1)

if .nil = parmValues~index(sP1) then
  do
    say 'Unrecognized parameter value passed at invocation -->' sP1
	subParm = '##'
	return
  end

return

/*
	The following procedure is used to set up values that can be used to test
	parameters that may be passed to the program at invocation time. The pro-
	cedure is coded so that it can be used in multiple programs. The variable
	parmFile is set to the name of a parameter file that contains the parameter
	settings.  Each line if the parameter file consists if the parameter and
	all of the possible values it may be set to.  These are all space delimited
	so that they can be parsed.
*/

setParameters: PROCEDURE EXPOSE parmFile parmValues

/*	Make sure the parameter file exists.                                     */

if \ SysFileExists(parmFile) then
  do
    say 'Unable to locate ' parmFile
	say 'Terminating program execution.'
	exit sfeRC
  end

/*
	Create a handle for the parameter file and then open the file. The first
	line of the file should be a comment. We will determine the total number
	of lines in the file and then subtract 1 to account for the comment line.
	Next we read in the first line and check to be sure it is a comment line.
	If it isn't we are going to terminate the execution of the program.
*/

parmFilehandle = .stream~new(parmFile)
parmFilehandle~open('READ')

parmFilelines = (parmFilehandle~lines) - 1

parmValues = .array~new(parmFilelines,2)

inBuff = strip(parmFilehandle~linein,'B')

if substr(inBuff,1,1) \= '*' then
  do
    say 'Parameter file error. First line not a comment'
	say 'Terminating program execution'
	exit 1000
  end

/*
	Process the remaining lines in the parameter file.  Each line begins with
	the parameter itself followed by the allowable values for the parameter.
	We are going to place the values into the parmValues array. This array is
	going to be an N x 2 array where N is the number of lines/records that we
	read from the parameter file. The [N,1] element is the parameter itself,
	and the [N,2] element is a one dimensional array consisting of the allow-
	able values.
*/

pCnt = 0

do while parmFilehandle~lines \= 0

  inBuff = strip(parmFilehandle~linein,'B')
  pCnt = pCnt + 1
  numWords = words(inBuff)
  parmValues[pCnt,1] = word(inBuff,1)
  
/*	Create a one dimensional temporary array to hold the parameter values.   */

  tempArray =.array~new(numWords - 1)

/*	Process the parameter values saving them in tempArray.                   */

  do pvCnt = 2 to numWords
    tempArray[pvCnt - 1] = word(inBuff,pvCnt)
  end pvCnt

/*	Save tempArray in the current [N,2] element of parmValues.               */

  parmValues[pCnt,2] = tempArray
  drop tempArray
  
end

parmFilehandle~close
  
return