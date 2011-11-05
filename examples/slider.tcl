package require Tk

wm title . "Manipulate"
grid [ttk::frame .c]
grid [ttk::scale .v -command output -length 400]

proc output {val} { puts $val }
