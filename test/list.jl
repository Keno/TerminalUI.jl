using TerminalUI
import TerminalUI: ListWidget

listw = Border(ListWidget([1:100;]),"List")
tty = Base.Terminals.TTYTerminal("xterm",STDIN,STDOUT,STDERR)
wait(FullScreenDialog(listw,tty))
