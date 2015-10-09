using TerminalUI

myform = form(Pair[
     :name => ask("Enter your name:", AbstractString),
     :address => ask("Enter your address:", AbstractString),
     :age => ask("Enter your age:", Int)
   ],style = :incremental)

# At the REPL displaying the form would allow you to enter things
if isdefined(Base,:active_repl)
    display(myform)
else
    # Otherwise you should create a dialog
    tty = Base.Terminals.TTYTerminal("xterm",STDIN,STDOUT,STDERR)
    wait(FullScreenDialog(myform,tty))
end

# Retrieve the answers
@show answers(myform)
