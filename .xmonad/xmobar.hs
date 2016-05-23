-- xmobar config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

Config {
    font = "xft:SFNS Display:size=11",
    additionalFonts = ["xft:FontAwesome:size=11"],
    alpha = 0,
    --bgColor = "#575757",
    --fgColor = "#dcdccc",
    bgColor = "#2f343f",
    fgColor = "#2f343f",
    position = TopSize C 100 24,
    lowerOnStart = True,
    commands = [
        Run Weather "KPAO" ["-t","<tempF>F <skyCondition>","-L","64","-H","77","-n","#CEFFAC","-h","#FFB6B0","-l","#96CBFE"] 36000,
        -- cpu activity monitor
        Run MultiCpu [
        "--template" , "Cpu: <total0>% | <total1>% | <total2>% | <total3>%",
        "--Low", "50",        -- units: %
        "--High", "85",       -- units: %
        "--normal", "darkorange",
        "--high", "darkred"] 10,
        -- battery monitor
        Run Battery [
            "--template" , "<acstatus>",
            "--Low"      , "10",       -- units: %
            "--High"     , "50",       -- units: %
            "--low"      , "darkred",
            "--normal"   , "darkorange",
            "--", -- battery specific options
            -- discharging status
            "-o"	, "<leftipat>  <left>%",
            -- AC "on" status
            "-O"	, "<leftipat>  <left>%",
            -- charged status
            "-i"	, "<fn=1>\xf240</fn>",
            "--off-icon-pattern", "<fn=1>\xf1e6</fn>",
            "--on-icon-pattern", "<fn=1>\xf1e6</fn>"
            ] 50,
        Run Volume "default" "Master" [
            "-t", "<status>  <volume>%",
            "--",
            "-o", "<fn=1>\xf026</fn>",
            "-O", "<fn=1>\xf028</fn>",
            "-c", "#2f343f",
            "-C", "#2f343f"] 10,
        Run Wireless "wlp2s0" [
            "-t", "<fn=1>\xf1eb</fn>  <essid>",
            "-x", "Not Connected"] 10,
        Run Date "%a %_d %b %H:%M" "date" 10,
        Run StdinReader,
    Run UnsafeStdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "  %UnsafeStdinReader% } %date% { %wlp2s0wi%    %default:Master%    %battery%    <action=`oblogout` button=1><fn=1><raw=1:/></fn></action>  "
}
