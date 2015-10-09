import Base.REPL: outstream
export FullScreenDialog

type InlineDialog
    w::Widget
    tty
    t
    InlineDialog(w::Widget,tty) = new(w,tty)
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
    t
    FullScreenDialog(w::Widget,tty) = new(w,tty)
end

function create_input_loop(focuss,tty,s)
    @schedule begin
        try
            result = :ok
            # eof needs to go second here, because it may block
            while result != :done && !eof(STDIN)
                result = Base.LineEdit.match_input(focuss.keymap,nothing,tty)(s,nothing)
            end
        catch e
            if !isa(e,InterruptException)
                rethrow(e)
            end
        end
    end
end


function wait(i::Union{FullScreenDialog,InlineDialog})
    full = isa(i,FullScreenDialog)
    TerminalUI.initialize!(i.w)
    local s
    if full
        s = TerminalUI.DoubleBufferedTerminalScreen(Base.size(i.tty))
    else
        inline_height = min(optheight(i.w),height(i.tty)-4)
        s = TerminalUI.DoubleBufferedTerminalScreen((inline_height,width(i.tty)))
    end
    t = i.t = create_input_loop(i.w.ctx.focuss,i.tty,s)
    try
        function do_redraw(args...)
            redraw(s,i.w)
            render(i.tty.out_stream,s)
        end
        enable_settings(i.tty; query_cursor = !full)
        if !full
            wait(TerminalUI.curspos_condition)
            pos = TerminalUI.curspos
            # Leave 1 line between the prompt and the dialog, otherwise it looks
            # squished.
            s.offset = (pos[1]+1,pos[2])
            s.fullsize = Base.size(i.tty)
        end
        focus(i.w)
        lift(size->(resized(s,size); do_redraw()),monitor_resize(i.tty))
        lift(w->do_redraw(),TerminalUI.invalidated)
        lift((args...)->render(i.tty.out_stream,s),TerminalUI.invalidated2)
        wait(t)
        afterembed(i.tty.out_stream,s)
    catch e
        bt = catch_backtrace()
        @show e
        Base.show_backtrace(STDERR, bt)
        rethrow(e)
    finally
        disable_settings(i.tty)
        istaskdone(t) || Base.throwto(t,InterruptException())
    end
end

function close(d::Union{FullScreenDialog,InlineDialog})
    istaskdone(d.t) || Base.throwto(d.t,InterruptException())
end
