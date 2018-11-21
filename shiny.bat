@echo off
setlocal
setlocal enabledelayedexpansion
rem throw away everything except the IPv4 address line 
for /f "usebackq tokens=*" %%a in (`ipconfig ^| findstr /i "ipv4"`) do (
  rem we have for example "IPv4 Address. . . . . . . . . . . : 192.168.42.78"
  rem split on : and get 2nd token
  for /f delims^=^:^ tokens^=2 %%b in ('echo %%a') do (
    rem we have " 192.168.42.78"
    rem split on . and get 4 tokens (octets)
    for /f "tokens=1-4 delims=." %%c in ("%%b") do (
      set _o1=%%c
      set _o2=%%d
      set _o3=%%e
      set _o4=%%f
      rem strip leading space from first octet
      set _3octet=!_o1:~1!.!_o2!.!_o3!.!_o4!
      #echo !_3octet!
	  set ip=!_3octet!
      )
    )
  )
rem add additional commands here
pushd "C:\Users\Roy\Documents\R_board2"
echo Running Shiny
echo ip: %ip%:8964
Rscript --vanilla shiny.R %ip%


endlocal