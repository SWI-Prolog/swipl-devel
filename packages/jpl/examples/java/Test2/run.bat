@echo off
call ..\env.bat

if not exist Test2.class (
  echo  Compiling Test2.java
  javac Test2.java
)

java Test2

pause
