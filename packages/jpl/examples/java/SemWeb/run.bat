@echo off
call ..\env.bat

if not exist SemWeb.class (
  echo  Compiling SemWeb.java
  javac SemWeb.java
)

java SemWeb

pause
