@echo off
call ..\env.bat

if not exist Family.class (
  echo  Compiling Family.java
  javac Family.java
)

java Family

pause
