@echo off
call ..\env.bat

if not exist Zahed.class (
  echo  Compiling Zahed.java
  javac Zahed.java
)

java Zahed

pause
