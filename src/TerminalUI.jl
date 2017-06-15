VERSION >= v"0.4.0-dev+6641" && __precompile__()

module TerminalUI

using Compat

import Base.Terminals: width, height,
    cmove, cmove_col, cmove_line_up, cmove_line_down,
    cmove_up, cmove_down, UnixTerminal, CSI

import Base: UnitRange, size
import Base.Multimedia: display

# Libuv signal support to support SIGWINCH
import Base: wait, close, _uv_hook_close

const _sizeof_uv_signal = Base.uv_sizeof_handle(Base.UV_SIGNAL)

function __init__()
    global uv_jl_signalcb = cfunction(uv_signalcb,Void,(Ptr{Void},Cint))
end

type SignalListener
    handle::Ptr{Void}
    c::Condition
    let _check = function(err)
            err < 0 ? throw(Base.UVError("signal",err)) : nothing
        end
        function SignalListener(signum)
            global uv_jl_signalcb
            this = new(Libc.malloc(_sizeof_uv_signal), Condition())
            Base.associate_julia_struct(this.handle, this)
            Base.preserve_handle(this)
            _check(ccall(:uv_signal_init,Cint,(Ptr{Void},Ptr{Void}),
                Base.eventloop(), this.handle))
            _check(ccall(:uv_signal_start,Cint,(Ptr{Void},Ptr{Void},Cint),
                this.handle, uv_jl_signalcb::Ptr{Void}, signum))
            this
        end
    end
end

function uv_signalcb(handle::Ptr{Void},signum)
    signal = @Base.handle_as handle SignalListener
    notify(signal.c)
    nothing
end

wait(s::SignalListener) = wait(s.c)
close(s::SignalListener) = if s.handle != C_NULL
    ccall(:jl_close_uv, Void, (Ptr{Void},), s.handle)
end

function _uv_hook_close(s::SignalListener)
    Base.unpreserve_handle(s)
    Base.disassociate_julia_struct(s)
    s.handle = C_NULL
    notify(s.c)
    nothing
end

# Things missing from Base.Terminals
cmove(t::UnixTerminal,row,col) = write(t.out_stream,"$(CSI)$(row);$(col)H")


using Reactive

@compat abstract type Widget end
@compat abstract type Screen end

function draw(::Screen,::Void)
end

function draw(::Void,::Any)
end

# subsreen

@compat abstract type Subscreen <: Screen end
istracking(s::Subscreen) = false

immutable SimpleSubscreen <: Subscreen
    parent::Screen
    rows::UnitRange
    cols::UnitRange
end

immutable TrackingSubscreen <: Subscreen
    _s::SimpleSubscreen
    children::Vector{TrackingSubscreen}
    scroll::Bool
    topwidget::Widget
end
simple(s::TrackingSubscreen) = s._s
istracking(s::TrackingSubscreen) = true
topwidget(s::TrackingSubscreen) = s.topwidget
subscreens(s::TrackingSubscreen) = s.children

TrackingSubscreen(_s::SimpleSubscreen,scroll::Bool,w::Widget) =
    TrackingSubscreen(_s, Vector{Screen}(), scroll, w::Widget)

function add_child(s::TrackingSubscreen, ss::TrackingSubscreen)
    push!(s.children, ss)
end

width(s::SimpleSubscreen) = length(s.cols)
height(s::SimpleSubscreen) = length(s.rows)
size(s::SimpleSubscreen) = (height(s),width(s))

topscreen(s::SimpleSubscreen) = topscreen(s.parent)
topscreen(s) = s

for f in (:width,:height,:size,:topscreen)
    @eval ($f)(s::TrackingSubscreen) = ($f)(simple(s))
end

swrite(s::TrackingSubscreen,args...;kwargs...) = swrite(simple(s),args...; kwargs...)
swrite_image(s::TrackingSubscreen,args...;kwargs...) = swrite_image(simple(s),args...; kwargs...)
clear(s::TrackingSubscreen,args...) = clear(simple(s),args...)

function translate(s::SimpleSubscreen, rows, cols)
    rows = (first(s.rows)+first(rows)-1):min(last(s.rows), first(s.rows)+last(rows)-1)
    cols = (first(s.cols)+first(cols)-1):min(last(s.cols), first(s.cols)+last(cols)-1)
    (rows,cols)
end

function translate⁻¹(s::SimpleSubscreen, rows, cols)
    rows = (first(rows)-first(s.rows)+1):(last(rows)-last(s.rows)+1)
    cols = (first(cols)-first(s.cols)+1):(last(cols)-last(s.cols)+1)
    (rows,cols)
end

function translate⁻¹(s::SimpleSubscreen, p::Tuple{Int,Int})
    r = translate⁻¹(s,p[1]:p[1],p[2]:p[2])
    (first(r[1]),first(r[2]))
end

translate⁻¹(s::TrackingSubscreen, p::Tuple{Int,Int}) = translate⁻¹(simple(s),p)

function clear(s::SimpleSubscreen, rows=1:length(s.rows), cols=1:length(s.cols))
    clear(s.parent,translate(s,rows,cols)...)
end

function swrite(s::SimpleSubscreen, rows, cols, char; args...)
    swrite(s.parent,translate(s,rows,cols)...,char; args...)
end

function swrite_image(s::SimpleSubscreen, rows, cols, img; args...)
    swrite_image(s.parent,translate(s,rows,cols)..., img; args...)
end

function subscreen(s::Screen, rows, cols, widget = nothing; scroll = false)
    if !(first(rows) >= 1 && last(rows) <= height(s))
        error("Tryed to create subscreen with rows $(rows) of a $(1:height(s)) screen")
    end
    if !(first(cols) >= 1 && last(cols) <= width(s))
        error("Tryed to create subscreen with columns $(cols) of a $(1:width(s)) screen")
    end
    ss = SimpleSubscreen(s, rows, cols)
    if scroll && istracking(s)
        @assert widget !== nothing
        ss = TrackingSubscreen(ss, true, widget)
        add_child(s,ss)
    end
    ss
end


# Unbuffered write directly to the terminal
# Quite inefficient but useful for debugging
type DirectTerminalScreen <: Screen
    tty::Base.Terminals.TTYTerminal
    # Buffer the size to make sure it doesn't change midway through the draw operation
    size::Tuple{Int,Int}
end
width(s::DirectTerminalScreen) = s.size[2]
height(s::DirectTerminalScreen) = s.size[1]
size(s::DirectTerminalScreen) = s.size
resize!(s, size) = s.size = size

function swrite(s::DirectTerminalScreen, rows, cols, char)
    cmove(s.tty,1,1)
    cur_row = 1
    for r in rows
        cmove(s.tty,r,1)
        for c in cols
            cmove_col(s.tty,c)
            write(s.tty,char)
        end
    end
end


# Screensize widget

immutable Screensize <: Widget
end

function draw(s::Screen,::Screensize)
    screensize = size(topscreen(s))
    towrite = string(typeof(topscreen(s)),':',screensize[2],'x',screensize[1])
    swrite(s,div(height(s),2),div(width(s),2)-div(sizeof(towrite),2),towrite)
end

# Trees
using AbstractTrees
immutable Tree
    t::Tree
end

# Most terminals use $(CSI)2J to indicate that the current contents
# should be scrolled into the history buffer, even though this is not
# super evident from the spec.
function fresh_screen(s::DirectTerminalScreen)
    w = width(s)
    h = height(s)
    write(s.tty.out_stream,"$(CSI)H$(CSI)2J")
end

function clear_screen(s::DirectTerminalScreen)
    w = width(s)
    h = height(s)
    cmove(s.tty,h,w)
    write(s.tty.out_stream,"$(CSI)1J")
end

const SIGWINCH = 28

function monitor_resize(tty)
    last_size = Base.displaysize(tty)
    s = Signal(Tuple{Int,Int}, last_size)
    signal = TerminalUI.SignalListener(TerminalUI.SIGWINCH)
    @schedule while true
        # First check if the terminal size changed on us while
        # we were processing the request. This is possible if we
        # have to re-enter the event loop while drawing, in which
        # case the signal would have been delivered with nobody
        # waiting for it. If this condition is true, we are guaranteed
        # that any resize will at least ping the event loop again.
        if Base.displaysize(tty) == last_size
            wait(signal)
        end
        last_size = Base.displaysize(tty)
        push!(s,last_size)
    end
    s
end

import VT100: readdec

global curspos = (0,0)
global curspos_condition = Condition()

function parse_terminal_reply(stream)
    (c1,f1) = readdec(stream)
    if c1 == ';'
        (c2,f2) = readdec(stream)
        if c2 == 'R'
            global curspos
            curspos = (f1,f2)
            notify(curspos_condition)
        end
    else
        error("Unrecognized reply message")
    end
end

invalidated = Signal(Any, nothing)
invalidated2 = Signal(Any, nothing)

include("focus.jl")
include("render.jl")
include("widgets.jl")
include("dialog.jl")
include("inputs.jl")

end # module
