import Base.LineEdit: KeyAlias

type FocusState
    stack::Vector{Widget}
    keymap
end
FocusState() = FocusState(Vector{Widget}(),nothing)

type WidgetContext
    visible::Bool
    parent
    focuss
    dialog
    WidgetContext(visible::Bool) = new(visible)
end
WidgetContext() = WidgetContext(true)
isvisible(w::Widget) = (isdefined(w,:ctx) && w.ctx !== nothing) ? w.ctx.visible : true
hide(w::Widget) = w.ctx.visible = false
makevisible(w::Widget) = w.ctx.visible = true

parentwidget(w::Widget) = w.ctx.parent

root_keymap(tty) = Dict(
    "^C" => (s,p,c) -> (return :done),
    # Mouse Tracking
    "\e[M" => (topscreen,o...)->begin
        a = read(tty,Char)
        b = read(tty,Char)
        c = read(tty,Char)
        dispatch_mouse(topscreen,map(x->x-32,map(Int,(a,b,c)))...)
    end,
    "\e\x7f" => KeyAlias("\e\b"),
    # Ctrl-Left Arrow
    "\e[1;5D" => KeyAlias("\eb"),
    # Ctrl-Left Arrow on rxvt
    "\eOd" => KeyAlias("\eb"),
    # Ctrl-Right Arrow
    "\e[1;5C" => KeyAlias("\ef"),
    # Ctrl-Right Arrow on rxvt
    "\eOc" => KeyAlias("\ef"),
    # Terminal queries
    "\e[?" => (args...)->parse_terminal_reply(tty),
    # Force Redraw
    "^L" => (topscreen,o...)->(clear(topscreen);invalidate(topscreen)),
    # Tab
    '\t' => nothing
)

function focus(s::FocusState, w::Widget)
    p = w
    newstack = Vector{Widget}()
    # First find the greatest common ancestor
    # of the new widgets and the existing widget stack
    while p !== nothing && !(p in s.stack)
        push!(newstack,p)
        p = parentwidget(p)
    end
    if p !== nothing
        i = findfirst(s.stack, p)
    else
        i = 0
    end
    # Now defocus all widgets that will no longer be in the
    # keymap
    for oldw in s.stack[(i+1):end]
        defocused(oldw)
    end
    deleteat!(s.stack,(i+1):endof(s.stack))
    reverse!(newstack)
    for neww in newstack
        focused(neww)
    end
    append!(s.stack,newstack)
    # TODO: If this takes too long, cache intermediate results
    s.keymap = Base.LineEdit.keymap(vcat(Dict[keymap(widget) for widget in reverse(s.stack)],Dict[root_keymap(w.ctx.dialog.tty)]))
    s
end

function focus_child(w::Widget)
    for w in children(w)
        if focusable(w)
            focus(w)
            return
        end
    end
    focus(w.ctx.focuss,w)
end

invalidate(w::Widget) = isdefined(w,:ctx) && isdefined(w.ctx,:dialog) && invalidate(w.ctx.dialog)

focus(w::Widget) = focus(w.ctx.focuss,w)
isfocused(w::Widget) = w in w.ctx.focuss.stack

function initialize!(w; parent = nothing, focuss = FocusState())
    w = value(w)
    if !isdefined(w,:ctx) || isdefined(w.ctx,:focuss)
        return
    end
    w.ctx.parent = parent
    w.ctx.focuss = focuss
    for c in children(w)
        initialize!(c, parent = w, focuss = focuss)
    end
end

const KEY_SCROLL = 64
const BUTTON_RELEASE = 3
function dispatch_mouse(topscreen::Screen, button, x, y)
    target, targetpos = subscreen_for_pos(topscreen, subscreens(topscreen), (y,x))
    targetw = topwidget(target)
    if (button & KEY_SCROLL) != 0
        if (button & 1) != 0
            scroll_up(targetw, targetpos)
        else
            scroll_down(targetw, targetpos)
        end
    elseif (button & BUTTON_RELEASE) == BUTTON_RELEASE
        mouse_release(targetw, targetpos)
    else
        mouse_press(targetw, targetpos)
    end
end
