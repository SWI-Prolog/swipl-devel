@echo off
call ..\env.bat

if not exist Exceptions2.class (
  echo  Compiling Exceptions2.java
  javac Exceptions2.java
)

java Exceptions2

pause
