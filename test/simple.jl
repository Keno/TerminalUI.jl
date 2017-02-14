using TerminalUI
using Reactive
using VT100
using Compose

import TerminalUI: Border, TightCentering, SimpleSlider,
    DoubleBufferedTerminalScreen, draw, CheckBox, WidgetStack,
    Gauge, RowLayout, MultiLineInput, IOBufferView, ScrollableChain,
    ImageWidget
import Base.Terminals: CSI

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

sl = SimpleSlider{Float64}(0.,10.,5.)
slider = Border(sl,"Slider")

imgout = ImageWidget()
imgw = Border(imgout, "Plot")

repl = Base.REPL.LineEditREPL(tty).mi
we = TerminalUI.WidgetREPL()
we.mi = repl
interface = Base.REPL.setup_interface(we,repl)

we.widget = w = TerminalUI.WidgetForREPL(interface,we)
run_repl(we)
b = WidgetStack([ slider, imgw, #RowLayout(gauges,[3 for i = 1:4],zeros(Int64,4),12),
    Border(w,"REPL")])
    #=Border(historyview,"History"),
    Border(,"REPL")])=#
import TerminalUI: children
TerminalUI.initialize!(b)
#TerminalUI.focus(children(children(w)[2])[2])
TerminalUI.focus(sl)
s = TerminalUI.DoubleBufferedTerminalScreen(Base.size(tty))

function resized(size)
    # Throw away all state, since we have no idea what the resize did
    #global s = TerminalUI.DoubleBufferedTerminalScreen(size)
    TerminalUI.redraw(s,b)
    TerminalUI.render(STDOUT,s)
end

#lift(resized,TerminalUI.monitor_resize(tty))

function redraw(args...)
    TerminalUI.redraw(s,b)
    TerminalUI.render(STDOUT,s)
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

function pixel_for_cell_size(height, width)
    (height*34,width*14)
end

import VT100: Cell

function plot_to_imgout(p)
    buf = IOBuffer()
    size = (TerminalUI.optheight(imgout),TerminalUI.width(s))
    psize = pixel_for_cell_size(size...)
    # Macbook Pro
    dpi = 220.53
    png = PNG(buf,psize[2]/dpi*inch,psize[1]/dpi*inch; dpi = round(dpi))
    png.emit_on_finish = false
    #p = deepcopy(p)
    if isa(p,Gadfly.Plot)
        p.theme = Gadfly.Theme(
        panel_fill=color("black"), default_color=color("orange"), major_label_color=color("white"), minor_label_color=color("white"), key_label_color=color("white"), key_title_color=color("white")
       )
    end
    Compose.draw(png,p)
    imgout.data = take!(buf)
    if imgout.screenchar.content != 0
        push!(s.dropped, Int(imgout.screenchar.content) - 0xF0000)
    end
    imgout.screenchar = Cell(Char(0))
end

using Gadfly
lift(sl.val) do t
    plot_to_imgout(plot(x->sin(x+t),0,4,
        Scale.x_continuous(minvalue=0,maxvalue=4),
        Scale.y_continuous(minvalue=-1,maxvalue=1),
        Guide.xlabel(nothing),
        Guide.ylabel(nothing)))
    redraw()
end

#plot_to_imgout(compose(context(),rectangle(),fill("red")))

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
