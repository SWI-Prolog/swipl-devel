@echo off
call ..\env.bat

if not exist FamilyMT.class (
  echo  Compiling FamilyMT.java
  javac FamilyMT.java
)

java FamilyMT

pause
