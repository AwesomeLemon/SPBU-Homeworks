@echo off

cls

".nuget\NuGet.exe" "Install" "FAKE" "-OutputDirectory" "packages" "-ExcludeVersion"
".nuget\NuGet.exe" "Install" "NUnit" "-OutputDirectory" "packages" "-ExcludeVersion"
pause