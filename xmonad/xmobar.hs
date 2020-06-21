Config { font         = "-misc-fira code retina-medium-r-normal--*-*-*-*-*-*-*-*"
       , bgColor      = "#282a36"
       , fgColor      = "#b45bcf"
       , position     = Top L 100
       , lowerOnStart = True
       , hideOnStart  = False
       , persistent   = True
       , allDesktops  = True
       , iconRoot     = "/home/balbi/.xmonad/"
       , commands     = [ Run Weather "EFHK" [ "-t","<station>: <tempC>C",
                                               "-L","5","-H","20",
                                               "--normal", "green",
                                               "--high", "red",
                                               "--low", "lightblue"] 36000
                        , Run Cpu [ "-L", "3", "-H","50",
                                    "--low","green",
                                    "--normal", "orange",
                                    "--high","red"] 10
                        , Run Wireless "wlp12s0" [ "-L", "40", "-H", "70",
                                                   "--low", "red",
                                                   "--normal", "orange",
                                                   "--high", "green"] 10
                        , Run Memory ["-t","Mem: <usedratio>%"] 10
                        , Run Swap [] 10
                        , Run Date "%a %b %_d %H:%M" "date" 10
                        , Run StdinReader
                        ]
       , sepChar      = "%"
       , alignSep     = "}{"
       , template     = " <icon=haskell.xpm/> %StdinReader% }{ %cpu% | %wlp12s0wi% | %memory% * %swap% | <fc=#ee9a00>%date%</fc> | %EFHK%"
       }