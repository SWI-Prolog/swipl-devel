@echo off
call ..\env.bat

if not exist Exceptions.class (
  echo  Compiling Exceptions.java
  javac Exceptions.java
)

java Exceptions

pause
