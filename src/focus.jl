type FocusState
    stack::Vector{Widget}
    keymap
end
FocusState() = FocusState(Vector{Widget}(),Dict{Any,Any}())

type WidgetContext
    parent
    visible::Bool
    focuss
end
WidgetContext() = WidgetContext(nothing,true,FocusState())
isvisible(w::Widget) = isdefined(w,:ctx) && w.ctx !== nothing ? w.ctx.visible : true
hide(w::Widget) = w.ctx.visible = false
makevisible(w::Widget) = w.ctx.visible = true

parentwidget(w::Widget) = w.ctx.parent

const root_keymap = Dict(
    "^C" => (s,p,c) -> (return :done),
    # Mouse Tracking
    "\e[M" => (topscreen,o...)->begin
        a = read(STDIN,Char)
        b = read(STDIN,Char)
        c = read(STDIN,Char)
        dispatch_mouse(topscreen,map(x->x-32,map(Int,(a,b,c)))...)
    end
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
    s.keymap = Base.LineEdit.keymap(vcat(Dict[keymap(widget) for widget in reverse(s.stack)],Dict[root_keymap]))
    s
end

focus(w::Widget) = focus(w.ctx.focuss,w)

function initialize!(w; ctx = WidgetContext())
    if isdefined(w,:ctx)
        return
    end
    w.ctx = ctx
    for c in children(w)
        initialize!(c,ctx = WidgetContext(w,true,ctx.focuss))
    end
end

function invalidate(w::Widget)
    push!(invalidated,w)
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
