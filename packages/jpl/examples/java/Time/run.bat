@echo off
call ..\env.bat

if not exist Time.class (
  echo  Compiling Time.java
  javac Time.java
)

java Time

pause
