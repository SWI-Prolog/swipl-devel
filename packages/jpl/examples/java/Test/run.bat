@echo off
call ..\env.bat

if not exist Test.class (
  echo  Compiling Test.java
  javac Test.java
)

java Test

pause
