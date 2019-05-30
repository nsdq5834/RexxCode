/* Rexx program

   fsftdemog.rex
   
   Base code	05/22/2019
   Revision 1   05/29/2019
   
   This is a simple homegrown utility program that determines the storage
   drives that are available on the system. Once we have a list of all the 
   storage devices, we will obtain the contents of the file system on each
   device and do a simple demographic analysis of the file types that are
   found.
   
   File System File Type Demographics --> fsftdemog

   See if we were passed an argument.  If so, see if it is equal to the
   word debug. If it is, set a logic flag that we will use to control the
   messages we will write to our log file.   

   Next identify the input file that contains the list of the base
   directories/folders that we want to make backups from. Create a unique
   file name that we can use for our log file that will track the programs
   execution.
   
*/

debugFlag = 0
driveOption = 'USED'
Drives. = ''

arg passedValue

if passedValue = 'DEBUG' then
  debugFlag = 1

/*
  The IsoDrives procedure is used to locate drives of a particular type that
  are accessible to the system. Use the SysDriveMap function to obtain the list
  of drives. Take the returned information string and break it down into the 
  drive letters and place them into the Drives. stem variable.
*/

ExtensionValues = .array~new(0,2)
ExtensionValues[1,1] = 'INI'
ExtensionValues[1,2] = 1
 
call IsoDrives

if Drives.0 = 0 then exit

say Drives.0 driveOption 'drives detected'

do drivePointer = 1 to 1
  
  filePattern = 'D:\*.*'
  sftRC = SysFileTree(filePattern,retFiles,'FS')
  
  if retFiles.0 = 0 then iterate
  
  do filePointer = 1 to retFiles.0
  
    parse var retFiles.filePointer sflDate sflTime sflSize sflAttrib sflFname
	extType = IsoExtension(sflFname)

	if extType = '*BOGUS*' then iterate

	if ExtensionValues~hasItem(extType) then
	  do
	    IndexValues = ExtensionValues~index(extType)
		curIdx = IndexValues[1]
		ExtensionValues[curIdx,2] = ExtensionValues[curIdx,2] + 1
	  end
	else
      do
	    NextIndex = ExtensionValues~last
		NextIdx = NextIndex[1] + 1
		ExtensionValues[NextIdx,1] = extType
		ExtensionValues[NextIdx,2] = 1
      end
	
  end filePointer
 
/*
	ExtensionValues~items returns the total number of items that are in the
	array. Since our array is [N,2], dividing the total number of items by
	2 gives us the number of rows in the array.
	
	GapElements is the upper limit for the gap or increments values we will
	need to implement the Shell or diminishing increment sort.
*/ 

  TotalElements = (ExtensionValues~items) / 2
  GapElements = TotalElements / 3 
  
do ii = 1 to TotalElements
 say ExtensionValues[ii,1] ' <---> 'ExtensionValues[ii,2]
end ii  
 
/* Set up some variables so we can calculate our gap or increment values.    */

  ii = 0
  incVal = 0
  shellInc. = null
  
  do until incVal > GapElements
    ii = ii + 1
    incVal = ((3 ** ii) - 1) / 2
    shellInc.ii = incVal
  end

/*  Decrement ii by 1 to point to last usable gap value */

  incStart = ii - 1
  
  do gapPoint = incStart to 1 by -1
    currentGap = shellInc.gapPoint
    do outerP = currentGap to TotalElements - 1
	  tempVal1 = ExtensionValues[outerP,1]
	  tempVal2 = ExtensionValues[outerP,2]
	  innerP = outerP + 1
	  loop label innerP while innerP >= currentGap
	    tPoint = innerP - currentGap
		if \(ExtensionValues[tPoint,2] > tempVal2) then leave innerP
		ExtensionValues[innerP,1] = ExtensionValues[tPoint,1]
		ExtensionValues[innerP,2] = ExtensionValues[tPoint,2]
		innerP = innerP - currentGap
	  end innerP
	  ExtensionValues[innerP,1] = tempVal1
	  ExtensionValues[innerP,2] = tempVal2
	end outerP
  end gapPoint
  
end drivePointer



/* https://rosettacode.org/wiki/Sorting_algorithms/Shell_sort#ooRexx 

  loop label inc while inc > 0
    loop i_ = inc to n - 1
      temp = ra~get(i_)
      j_ = i_
      loop label j_ while j_ >= inc
        if \(ra~get(j_ - inc) > temp) then leave j_
        ra~set(j_, ra~get(j_ - inc))
        j_ = j_ - inc
        end j_
      ra~set(j_, temp)
      end i_
    inc = format(inc / 2.2,, 0) -- rounding
    end inc
*/

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
  The IsoFname function takes a fully qualified file name and strips off the
  path information so we are left with the simple file name.
*/

IsoExtension: procedure

arg FullFileName
OnlyFileName = ''

FullFileName = strip(FullFileName,'B')
lengthFFN = length(FullFileName)
lengthFFN1 = lengthFFN

do pointFFN = lengthFFN to 1 by -1

  if substr(FullFileName,pointFFN,1) = '.' then
    do
	  lengthFFN1 = lengthFFN1 - pointFFN
	  pointFFN1 = pointFFN + 1
	  OnlyFileName = substr(FullFileName,pointFFN1,lengthFFN1)
	  leave
	end
	
end pointFFN

if pointFFN < 2 then OnlyFileName = '*BOGUS*'

return OnlyFileName
