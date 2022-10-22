@echo off
echo League of Legends hotkey bug fixer


echo Killing Riot Processes
taskkill /f /im "league of legends.exe"
taskkill /f /im "riotclientservices.exe"
taskkill /f /im "leagueclient.exe"

echo Waiting two seconds
ping 127.0.0.1 -n 2 > nul

echo Deleting input.ini
del "C:\Riot Games\League of Legends\Config\input.ini" /Q

echo Restarting League Client
cd C:\Riot Games\Riot Client
start RiotClientServices.exe --launch-product=league_of_legends --launch-patchline=live

echo Finished
pause