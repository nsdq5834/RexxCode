/* Rexx program

   fsftdemog.rex
   
   Base code	05/22/2019
   Revision 1   05/29/2019
   Revision 2   05/30/2019
   Revision 3   05/31/2019
   
   This is a simple homegrown utility program that determines the storage
   drives that are available on the system. Once we have a list of all the 
   storage devices, we will obtain the contents of the file system on each
   device and do a simple demographic analysis of the file types that are
   found.
   
   File System File Type Demographics --> fsftdemog

   See if we were passed an argument.  If so, see if it is equal to the
   word debug. If it is, set a logic flag that we will use to control the
   messages we will write to our log file.   
   
*/

/* Define some variables and initialize them.                                */

debugFlag = 0
driveOption = 'USED'
Drives = .stem~new
extName = .stem~new
extCntr = .stem~new

arg passedValue

if passedValue = 'DEBUG' then
  debugFlag = 1

/*
  The IsoDrives procedure is used to locate drives of a particular type that
  are accessible to the system. Use the SysDriveMap function to obtain the list
  of drives. Take the returned information string and break it down into the 
  drive letters and place them into the Drives. stem variable.
*/
 
call IsoDrives

if Drives.0 = 0 then exit

say Drives.0 driveOption 'drives detected'

/*s	Process all of the drives that we were able to detect.                   */

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
  
  say 'Breakdown for Drive ' Drives.drivePointer
  say ' '
  
  do kkk = 1 to 15
    say extName[kkk] ' <--> ' extCntr[kkk]
  end kkk
  
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
