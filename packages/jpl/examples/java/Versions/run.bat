@echo off
call ..\env.bat

if not exist Versions.class (
  echo  Compiling Versions.java
  javac Versions.java
)

java Versions

pause
