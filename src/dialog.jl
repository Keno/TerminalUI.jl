import Base.REPL: outstream
export FullScreenDialog

type InlineDialog
    w::Widget
    tty
    was_invalidated::Bool
    t
    do_redraw
    InlineDialog(w::Widget,tty) = new(w,tty,false)
end

function enable_settings(tty; query_cursor = false)
    Base.reseteof(tty)
    Base.Terminals.raw!(tty,true)
    write(tty,CSI,"?25l")
    query_cursor && write(tty,CSI,"?6n")
    write(tty,CSI,"?1002h")
    write(tty,CSI,"?1005h")
end

function disable_settings(tty)
    Base.Terminals.raw!(tty,false)
    write(tty,CSI,"?25h")
    write(tty,CSI,"?1002l")
    write(tty,CSI,"?1005l")
end

type FullScreenDialog
    w::Widget
    tty
    was_invalidated::Bool
    t
    do_redraw
    FullScreenDialog(w::Widget,tty) = new(w,tty,false)
end
invalidate(dialog::Union{InlineDialog, FullScreenDialog}) = dialog.was_invalidated = true

function create_input_loop(i,tty,s)
    @schedule begin
        try
            result = :ok
            # eof needs to go second here, because it may block
            while result != :done && !eof(tty)
                result = Base.LineEdit.match_input(i.w.ctx.focuss.keymap,nothing,tty)(s,nothing)
                i.was_invalidated && i.do_redraw()
            end
        catch e
            if !isa(e,InterruptException)
                rethrow(e)
            end
        end
    end
end

immutable IOBufferTerminal
    buf::IOBuffer
end
size(tty::IOBufferTerminal) = (80,24)
Base.displaysize(tty::IOBufferTerminal) = size(tty)
width(tty::IOBufferTerminal) = size(tty)[1]
height(tty::IOBufferTerminal) = size(tty)[2]
Base.write(tty::IOBufferTerminal,args...) = write(buf,args...)

function debug(w::Widget, emdebug = false)
    buf = IOBuffer()
    i = InlineDialog(w, IOBufferTerminal(buf))
    s = setup_screen(i)
    s.offset = (1,1)
    s.fullsize = Base.displaysize(i.tty)
    focus(i.w)
    redraw(s, i.w)
    render(buf, s)
    em = VT100.ScreenEmulator()
    em.debug = emdebug
    seekstart(buf)
    VT100.parseall!(em, buf)
    em
end

function set_widget_dialog!(w, i)
    !isdefined(w, :ctx) && return
    w.ctx.dialog = i
    for nw in children(w)
        set_widget_dialog!(nw, i)
    end
end

function setup_screen(i)
    full = isa(i,FullScreenDialog)
    TerminalUI.initialize!(i.w)
    set_widget_dialog!(i.w, i)
    local s
    if full
        s = TerminalUI.DoubleBufferedTerminalScreen(Base.displaysize(i.tty))
    else
        inline_height = min(optheight(i.w),height(i.tty)-4)
        s = TerminalUI.DoubleBufferedTerminalScreen((inline_height,width(i.tty)))
    end
    s
end

function wait(i::Union{FullScreenDialog,InlineDialog})
    full = isa(i,FullScreenDialog)
    local s = setup_screen(i)
    t = i.t = create_input_loop(i,i.tty,s)
    try
        i.do_redraw = function(args...)
            redraw(s,i.w)
            render(i.tty.out_stream,s)
        end
        global curspos
        curspos = (0,0)
        enable_settings(i.tty; query_cursor = !full)
        focus(i.w)
        if !full
            curspos == (0,0) && wait(TerminalUI.curspos_condition)
            pos = curspos
            # Leave 1 line between the prompt and the dialog, otherwise it looks
            # squished.
            s.offset = (pos[1]+1,pos[2])
            s.fullsize = Base.displaysize(i.tty)
        end
        map(size->(resized(s,size); i.do_redraw()),monitor_resize(i.tty))
        map((args...)->render(i.tty.out_stream,s),TerminalUI.invalidated2)
        wait(t)
        afterembed(i.tty.out_stream,s)
    catch e
        bt = catch_backtrace()
        @show e
        Base.show_backtrace(STDERR, bt)
        rethrow(e)
    finally
        disable_settings(i.tty)
        close(i)
    end
end

function change_widget!(i::Union{FullScreenDialog,InlineDialog}, w)
    i.w = w
    TerminalUI.initialize!(i.w)
    set_widget_dialog!(i.w, i)
    focus(i.w)
    invalidate(i.w)
    i.do_redraw()
end

function close(d::Union{FullScreenDialog,InlineDialog})
    if !istaskdone(d.t)
        Base.throwto(d.t, InterruptException())
    end
end

function print_snapshot(i)
    full = isa(i,FullScreenDialog)
    local s = setup_screen(i)
    t = i.t = create_input_loop(i,i.tty,s)
    try
        enable_settings(i.tty; query_cursor = !full)
        if !full
            wait(TerminalUI.curspos_condition)
            pos = TerminalUI.curspos
            # Leave 1 line between the prompt and the dialog, otherwise it looks
            # squished.
            s.offset = (pos[1]+1,pos[2])
            s.fullsize = Base.displaysize(i.tty)
        end
        focus(i.w)
        redraw(s,i.w)
        render(i.tty.out_stream,s)
    finally
        disable_settings(i.tty)
        close(i)
    end
end
