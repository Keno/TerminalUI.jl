import Base.REPL: outstream

immutable InlineDialog
    w::Widget
    tty
end

function enable_settings(tty)
    Base.reseteof(tty)
    Base.Terminals.raw!(tty,true)
    write(tty,CSI,"?25l")
    write(tty,CSI,"?6n")
    write(tty,CSI,"?1002h")
    write(tty,CSI,"?1005h")
end

function disable_settings(tty)
    Base.Terminals.raw!(tty,false)
    write(tty,CSI,"?25h")
    write(tty,CSI,"?1002l")
    write(tty,CSI,"?1005l")
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


function wait(i::InlineDialog)
    TerminalUI.initialize!(i.w)
    inline_height = min(optheight(i.w),height(i.tty)-4)
    s = TerminalUI.DoubleBufferedTerminalScreen((inline_height,width(i.tty)))
    t = create_input_loop(i.w.ctx.focuss,i.tty,s)
    try
        function do_redraw(args...)
            redraw(s,i.w)
            render(i.tty.out_stream,s)
        end
        enable_settings(i.tty)
        wait(TerminalUI.curspos_condition)
        s.offset = TerminalUI.curspos
        s.fullsize = Base.size(i.tty)
        focus(i.w)
        lift(size->(resized(s,size); do_redraw()),monitor_resize(i.tty))
        lift(w->do_redraw(),TerminalUI.invalidated)
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
