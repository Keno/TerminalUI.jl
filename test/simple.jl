using TerminalUI
using Reactive
using VT100

import TerminalUI: Border, TightCentering, SimpleSlider,
    DoubleBufferedTerminalScreen, draw, CheckBox, WidgetStack,
    Gauge, RowLayout, MultiLineInput, IOBufferView, ScrollableChain
import Base.Terminals: CSI

slider = SimpleSlider(10,5)
checkboxes = [
    CheckBox("Pineapple"),
    CheckBox("Onion"),
    CheckBox("Cheese"),
    CheckBox("Tomato Saunce")
]

tty = Base.Terminals.TTYTerminal("xterm",STDIN,STDOUT,STDERR)

function run_repl(repl)
    repl_channel = Channel(1)
    response_channel = Channel(1)
    backend = Base.REPL.start_repl_backend(repl_channel, response_channel)
    repl.mi.backendref = Base.REPL.REPLBackendRef(repl_channel,response_channel)
    backend
end

gauges = [WidgetStack([Border(Gauge(0,100,20*i),"Label")]) for i = 1:4]

repl = Base.REPL.LineEditREPL(tty).mi
we = TerminalUI.WidgetREPL()
we.mi = repl
interface = Base.REPL.setup_interface(we,repl)

we.widget = w = TerminalUI.WidgetForREPL(interface,we)
run_repl(we)
b = WidgetStack([RowLayout(gauges,[3 for i = 1:4],zeros(Int64,4),12),
    Border(w,"REPL")])
    #=Border(historyview,"History"),
    Border(,"REPL")])=#
import TerminalUI: children
TerminalUI.initialize!(b)
TerminalUI.focus(children(children(w)[2])[2])
s = TerminalUI.DoubleBufferedTerminalScreen(Base.size(tty))

function resized(size)
    # Throw away all state, since we have no idea what the resize did
    global s = TerminalUI.DoubleBufferedTerminalScreen(size)
    draw(s,b)
    #TerminalUI.render(STDOUT,s)
end

#lift(resized,TerminalUI.monitor_resize(tty))

function redraw(args...)
    TerminalUI.redraw(s,b)
    TerminalUI.render(STDOUT,s)
end

function update_silder(args...)
    slider.cur = mod1(slider.cur + 1, slider.numcells)
end

function on_click(button, x, y)
    TerminalUI.dispatch_mouse(s, button, x, y)
    #ss, sspos = TerminalUI.subscreen_for_pos(s,s.subscreens,pos)
    #TerminalUI.swrite(ss,sspos[1],sspos[2],' ',bg = :red)
    redraw()
end

function create_input_loop()
    @schedule begin
        result = :ok
        # eof needs to go second here, because it may block
        while result != :done && !eof(STDIN)
            result = Base.LineEdit.match_input(b.ctx.focuss.keymap,nothing,tty)(nothing,nothing)
        end
    end
end

function embed_here()
    try
        Base.reseteof(tty)
        Base.Terminals.raw!(tty,true)
        write(STDOUT,CSI,"?25l")
        write(STDOUT,CSI,"?6n")
        t = create_input_loop()
        wait(TerminalUI.curspos_condition)
        pos = TerminalUI.curspos
        global s
        s = TerminalUI.DoubleBufferedTerminalScreen((5,50); offset = pos, fullsize = Base.size(tty))
        redraw()
        wait(t)
        TerminalUI.afterembed(STDOUT,s)
    catch e
        rethrow(e)
    finally
        Base.Terminals.raw!(tty,false)
        write(STDOUT,CSI,"?25h")
    end
end

if !Base.isinteractive()
    try
        Base.reseteof(tty)
        Base.Terminals.raw!(tty,true)
        write(STDOUT,CSI,"?25l")
        write(STDOUT,CSI,"?1002h")
        write(STDOUT,CSI,"?1005h")
        lift(resized,TerminalUI.monitor_resize(tty))
        lift(redraw,TerminalUI.invalidated)
        #=lift(every(1.0)) do args...
            redraw()
            return true
        end=#
        wait(create_input_loop())
    finally
        Base.Terminals.raw!(tty,false)
        write(STDOUT,CSI,"?25h")
        write(STDOUT,CSI,"?1002l")
    end
end
