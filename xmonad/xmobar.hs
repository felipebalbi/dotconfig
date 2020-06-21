Config { font = "-misc-fira code retina-medium-r-normal--*-*-*-*-*-*-*-*"
       , bgColor = "black"
       , fgColor = "grey"
       , position = TopW L 100
       , allDesktops = True
       , commands = [ Run Weather "EFHK" [ "-t","<station>: <tempC>C",
                                           "-L","5","-H","20",
                                           "--normal","green",
                                           "--high","red",
                                           "--low","lightblue"] 36000
                    , Run Cpu [ "-L","3","-H","50",
                                "--normal","green","--high","red"] 10
                    , Run Wireless "wlp12s0wi" [ "-L", "0", "-H", "32",
                                                 "--normal", "green",
                                                 "--high", "red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %b %_d %H:%M" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %cpu% | %wlp12s0wi% | %memory% * %swap% | <fc=#ee9a00>%date%</fc> | %EFHK%"
       }
