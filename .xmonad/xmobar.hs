-- xmobar config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

Config {
    font = "xft:Fira Code Retina:size=12",
    additionalFonts = ["xft:Font Awesome 5 Free Solid:size=10", "xft:Font Awesome 5 Brands:size=10"],
    alpha = 200,
    bgColor = "#424242",
    fgColor = "#E0E0E0",
    position = TopSize C 100 24,
    lowerOnStart = True,
    commands = [
      Run Battery [
        "--template" , "<acstatus>",
        "--Low"      , "10",       -- units: %
        "--High"     , "50",       -- units: %
        "--low"      , "darkred",
        "--normal"   , "darkorange",
        "--", -- battery specific options
        -- discharging status
        "-o"	, "<leftipat> <left>%",
        -- AC "on" status
        "-O"	, "<leftipat> <left>%",
        "-i"	, "<leftipat> <left>%",
        -- charged status
        --"-i"	, "<fn=1>\xf240</fn>",
        "--off-icon-pattern", "<fn=1>\xf240</fn>",
        "--on-icon-pattern", "<fn=1>\xf0e7</fn>",
        "--idle-icon-pattern", "<fn=1>\xf0e7</fn>"
      ] 50,
      -- Run Date "%a %_d %b %H:%M" "date" 10,
      Run Date "<action=`show-clock` button=1><fn=1>\xf073</fn> %a %_d %b</action>" "date" 10,
      Run UnsafeStdinReader,
      Run Volume "default" "Master" [
        "-t", "<status> <volume>%",
        "--",
        "-o", "<fn=1>\xf026</fn>",
        "-O", "<fn=1>\xf028</fn>",
        "-c", "#E0E0E0",
        "-C", "#E0E0E0"
      ] 10,
      Run Wireless "wlp2s0" [
        "-t", "<action=`sudo -A systemctl restart wpa_supplicant.service` button=1><fn=1>\xf1eb</fn> <essid></action>",
        "-x", "Not Connected"
      ] 10,
      Run UnsafeXPropertyLog "focus",
      Run UnsafeXPropertyLog "pom-timer"
    ],
    sepChar = "%",
    alignSep = "}{",
    template = " %UnsafeStdinReader% } %date%  |  <action=`focus What\'s your new focus?` button=1><fn=1><raw=1:/></fn> %focus%</action>  |  <fn=1><raw=1:/></fn> %pom-timer%{ %wlp2s0wi%  %default:Master%  %battery%  <action=`oblogout` button=1><fn=1><raw=1:/></fn></action> "
}
