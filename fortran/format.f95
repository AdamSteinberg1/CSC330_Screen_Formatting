program format
  character(60) :: currLine, maxLine, minLine
  character(100) :: word
  character(100) :: cla
  integer :: counter, currLineNum, maxLineNum, minLineNum
  character (1) :: input

  currLine = ""
  currLineNum = 1
  maxLine = ""
  minLine = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" !60 characters
  maxLineNum = -1
  minLinenum = -1

  call get_command_argument(1, cla)
  if(len_trim(cla) == 0) then
      print *, "Error: There must be one command line argument"
      call exit()
  end if
  open (unit=5,status="old",access="direct",form="unformatted",recl=1,file=cla)

  word = ""
  counter=1
  100 read (5,rec=counter,err=200) input
      if (input == " " .or. input == achar(10) .or. input == achar(13)) then !if input character is just whitespace, i.e. this is the end of the word
        if(word /= "") then
          !process word
          call processWord(removeNumbers(word), currLine, currLineNum, maxLine, maxLineNum, minLine, minLineNum)
          word = ""
        end if
      else
        word = trim(word) // input
      end if
      counter=counter+1
      goto 100
  200 continue
  close (5)

  !Process the last word
  call processWord(removeNumbers(word), currLine, currLineNum, maxLine, maxLineNum, minLine, minLineNum)

  !print the last line
  print "(I8 A)", currLineNum, "  " // trim(currLine)


  if (len_trim(currLine) <= len_trim(minLine)) then
    minLine = currLine
    minLineNum = currLineNum
  end if
  if (len_trim(currLine) >= len_trim(maxLine)) then
    maxLine = currLine
    maxLineNum = currLineNum
  end if

  print * !newline
  print "(A7 A13 A)", "LONG   ", str(maxLineNum), maxLine;
  print "(A7 A13 A)", "SHORT  ", str(minLineNum), minLine;

contains

  subroutine processWord(word, currLine, currLineNum, maxLine, maxLineNum, minLine, minLineNum)
    integer :: currLineNum, maxLineNum, minLineNum
    character(*) :: word, currLine, maxLine, minLine

    if(len_trim(currLine) == 0) then
      currLine = trim(word)
      return
    end if

    if(len_trim(currLine) + len_trim(word) + 1 <= 60) then
      currLine = trim(currLine) // " " // trim(word)
    else
      print "(I8 A)", currLineNum, "  " // trim(currLine)

      if (len_trim(currLine) <= len_trim(minLine)) then
        minLine = currLine
        minLineNum = currLineNum
      end if
      if (len_trim(currLine) >= len_trim(maxLine)) then
        maxLine = currLine
        maxLineNum = currLineNum
      end if
      currLineNum = currLineNum + 1
      currLine = trim(word)
    end if


  end subroutine processWord

	function removeNumbers(in) result(out)
		character(*), intent(in) :: in
		character(:), allocatable :: out
		character(60) :: temp
		integer :: i
		temp = ""
		do i = 1, LEN_TRIM(in)
		    if (in(i:i) /= '0' .and. &
            in(i:i) /= '1' .and. &
            in(i:i) /= '2' .and. &
            in(i:i) /= '3' .and. &
            in(i:i) /= '4' .and. &
            in(i:i) /= '5' .and. &
            in(i:i) /= '6' .and. &
            in(i:i) /= '7' .and. &
            in(i:i) /= '8' .and. &
            in(i:i) /= '9') then
          temp = trim(temp) // in(i:i)
        end if
		end do
    out = trim(temp)
	end function removeNumbers

  character(20) function str(num)
    !Convert an integer to string.
    integer, intent(in) :: num
    write (str, *) num
    str = adjustl(str)
  end function str

end program format
