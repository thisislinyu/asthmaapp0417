@echo off
cd "e:/2023/0324asthma registry/asthmaapp0417"
if "%1"=="h" goto begin
start mshta vbscript:createobject("wscript.shell").run("""%~nx0"" h",0)(window.close)&&exit
:begin
wsl cd "/mnt/e/2023/0324asthma registry/asthmaapp0417"
wsl nohup  R -e 'shiny::runApp(host = getOption("shiny.host","0.0.0.0"),port = 1017)' 