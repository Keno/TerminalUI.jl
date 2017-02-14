# Imports
import Base: start, next, done, getindex, length, show
import Base.LineEdit: KeyAlias, AnyDict

export Border, ListWidget, WidgetStack, IOBufferView, MenuWidget

const LazyWidget = Union{Widget,Signal}
value(w::Widget) = w

# Defaults
children(w::Widget) = ()
focused(w::Widget) = nothing
defocused(w::Widget) = nothing
keymap(w::Widget) = Dict{Any,Any}()
isscrollable(w::Widget) = false
scroll_up(w::Widget, it) = nothing
scroll_down(w::Widget, it) = nothing
mouse_release(w::Widget, pos) = nothing
mouse_press(w::Widget, pos) = nothing
show(io::IO, w::Widget) = print(io,typeof(w))
focusable(w::Widget) = false
optheight(w::Widget) = 1
optwidth(w::Widget) = 10

focusable{T<:Widget}(l::Signal{T}) = focusable(value(l))
optheight{T<:Widget}(l::Signal{T}) = optheight(value(l))
focus{T<:Widget}(l::Signal{T}) = focus(value(l))

# ScrollableWidget
@compat abstract type ScrollableWidget <: Widget end
isscrollable(w::ScrollableWidget) = true
cur_start_line(w::ScrollableWidget) = 1

function draw_scrollable(s::Screen, w::Widget)
    it = start_it = cur_top(w)
    clear(s)
    cur_row = 1
    done = false
    while cur_row <= height(s) && !element_done(w,it)
        nrows = draw_element_line(subscreen(s,cur_row:height(s),1:width(s)), w, it)
        cur_row += nrows
        it = element_next(w,it)
    end
    draw_scroll_indicators(s,w, up = it == element_start(w), down = !element_done(w,it))
end

function draw(s::Screen, w::Widget)
    if isvisible(w)
        if isscrollable(w)
            draw_scrollable(s, w)
        else
            draw_regular(s, w)
        end
    else
        clear(s)
    end
end

# Label
immutable Label <: Widget
    text::AbstractString
    # Doesn't get a widget context, because it's only used as an aux widget
    # by other widgets
end
optwidth(l::Label) = strwidth(l.text)

function draw_regular(s::Screen, l::Label; fg = :default, bg = :default)
    swrite(s,1,1,Base._truncate_at_width_or_chars(l.text,width(s)); fg = fg, bg = bg)
end

# SimpleSlider
type SimpleSlider{T} <: Widget
    min::T
    max::T
    val::Signal{T}
    last_sliderwidth::Int
    ctx::WidgetContext
    (::Type{SimpleSlider{T}}){T}(min::T,max::T,val::T) = new{T}(min,max,Signal(T, val),0)
end
optheight(s::SimpleSlider) = 1

# ─';'─';'⎼
function draw_regular(s::Screen,slider::SimpleSlider)
    textwidth = max(strwidth(repr(slider.min)),strwidth(repr(slider.max)))
    sliderwidth = max(0,width(s)-textwidth-1)
    fraction = ((slider.val.value - slider.min)/(slider.max - slider.min))
    fwidth = floor(Int,fraction*sliderwidth)
    if fwidth >= 1
        swrite(s,1,1:(fwidth-1),'═', fg = :red)
        swrite(s,1, fwidth+1         ,' ')
        swrite(s,1, fwidth           ,'⬤', fg = :red)
        swrite(s,1,(fwidth+2):sliderwidth,'═', fg = :default)
    end
    slider.last_sliderwidth = sliderwidth
    draw(subscreen(s,1:1,(1+sliderwidth+1):width(s)),Label(repr(slider.val.value)))
end

function slider_inc(s::SimpleSlider)
    step = (s.max-s.min)/s.last_sliderwidth
    push!(s.val,min(s.val.value + step, s.max))
    #invalidate(s)
end

function slider_dec(s::SimpleSlider)
    step = (s.max-s.min)/s.last_sliderwidth
    push!(s.val,max(s.val.value - step, s.min))
    #invalidate(s)
end

function keymap(s::SimpleSlider)
    Dict(
    # Right Arrow
    "\e[C" => (o...)->(slider_inc(s); invalidate(s)),
    # Left Arrow
    "\e[D" => (o...)->(slider_dec(s); invalidate(s))
    )
end

function mouse_release(s::SimpleSlider, pos)
    row, col = pos
    if col > s.last_sliderwidth
        push!(s.val,s.max)
    else
        push!(s.val,s.min + (s.max-s.min)*(col-1)/s.last_sliderwidth)
    end
    invalidate(s)
end
mouse_press(s::SimpleSlider, pos) = mouse_release(s, pos)
scroll_up(s::SimpleSlider, _) = slider_inc(s)
scroll_down(s::SimpleSlider, _) = slider_dec(s)

# Gauge
type Gauge{T} <: Widget
    min::T
    max::T
    val::T
    ctx::Void
    (::Type{Gauge{T}}){T}(min,max,val) = new{T}(min,max,val)
end
Gauge{T}(min::T,max::T,val::T) = Gauge{T}(min,max,val)

minheight(g::Gauge) = 1
optheight(g::Gauge) = 3


function draw_regular(s::Screen, g::Gauge)
    fraction = ((g.val - g.min)/(g.max - g.min))
    swrite(s,1:height(s),1:round(Integer,fraction*width(s)),' '; bg = :magenta)
    percentage = round(Integer,fraction*100)
    swrite(s,ceil(Integer,height(s)/2),floor(Integer,width(s)/2),"$(percentage) %";
        bg = nothing)
end

# CheckBox
type CheckBox <: Widget
    checked::Bool
    label::Label
    highlighted::Bool
    ctx::WidgetContext
    CheckBox(checked,label,highlighted) = new(checked,label,highlighted)
end
CheckBox(s::AbstractString) = CheckBox(false,Label(s),false)
focused(c::CheckBox) = c.highlighted = true
defocused(c::CheckBox) = c.highlighted = false

function draw_regular(s::Screen, c::CheckBox)
    swrite(s,1,1,c.checked ? "◉ " : "◯ ")
    draw(subscreen(s,1:height(s),3:width(s)), c.label; fg = c.highlighted ? :yellow : :default)
end

minheight(c::CheckBox) = 1

keymap(c::CheckBox) = Dict(
    ' '     => (args...)->(c.checked = !c.checked; invalidate(c))
)

# WidgetStack
type WidgetStack <: Widget
    widgets::Vector{Widget}
    curidx
    ctx::WidgetContext
    WidgetStack(widgets,curidx=1) = new(widgets,curidx,WidgetContext())
end
WidgetStack(widgets::Vector) = WidgetStack(Widget[w for w in widgets], 1)
children(w::WidgetStack) = w.widgets
focus(w::WidgetStack) = focus_child(w)

function draw_regular(s::Screen,w::WidgetStack)
    cumheight = 0
    for w in w.widgets
        height = optheight(w)
        starth = 1+cumheight
        draw(subscreen(s,starth:(starth+height-1),1:width(s), w; scroll = true),w)
        cumheight += height
    end
end

optheight(w::WidgetStack) = sum(map(optheight,w.widgets))

function update_index(w::WidgetStack,i)
    if i == w.curidx
        return
    end
    focus(w.widgets[i])
    w.curidx = i
    invalidate(w)
end

keymap(w::WidgetStack) = Dict(
    "\e[A"      => (s,p,c)->(update_index(w,mod1(w.curidx - 1,length(w.widgets)))),
    "\e[B"      => (s,p,c)->(update_index(w,mod1(w.curidx + 1,length(w.widgets)))),
    '\t'        => "\e[A"
)

# ScrollableChain
# TODO: Use the chain iterator somehow?
type ScrollableChain <: ScrollableWidget
    children::Vector{Widget}
    cur_widget::Int
    ctx::WidgetContext
    function ScrollableChain(children)
        for c in children
            if !isscrollable(c)
                error("Child of type $(typeof(c)) is not scrollable")
            end
        end
        new(children,1,WidgetContext())
    end
end
children(s::ScrollableChain) = s.children
optheight(s::ScrollableChain) = sum(map(optheight,s.children))
focusable(s::ScrollableChain) = any(map(focusable,s.children))
focus(s::ScrollableChain) = focus_child(s)


function next_valid(s::ScrollableChain, it)
    while element_done(s.children[it[1]],it[2])
        if it[1] == length(s.children)
          return (it[1]+1,)
        end
        it = (it[1]+1,element_start(s.children[it[1]+1]))
    end
    it
end

cur_top(s::ScrollableChain) = next_valid(s,(s.cur_widget,cur_top(s.children[s.cur_widget])))

function element_start(s::ScrollableChain)
    next_valid(s,(1,element_start(s.children[1])))
end
function element_next(s::ScrollableChain, idx)
    next_valid(s,(idx[1],element_next(s.children[idx[1]],idx[2])))
end
function element_prev(s::ScrollableChain, idx)
    it = (idx[1],element_prev(s.children[idx[1]],idx[2]))
    while it[2] != element_start(s.children[it[1]])
        if it[1] == 1
          return it
        end
        it = (it[1]-1,element_start(s.children[it[1]-1]))
    end
    it
end
element_done(s::ScrollableChain, idx) = idx[1] > length(s.children)

function set_cur_top(s::ScrollableChain, it)
    s.cur_widget = it[1]
    set_cur_top(s.children[it[1]],it[2])
end

draw_element_line(s::Screen, c::ScrollableChain, it) =
    isvisible(c.children[it[1]]) && draw_element_line(s, c.children[it[1]], it[2])

function scroll_up(s::ScrollableChain, pos)
    cur = cur_top(s)
    if element_done(s, cur)
        return
    end
    n = element_next(s, cur)
    set_cur_top(s, n)
end
function scroll_down(s::ScrollableChain, pos)
    cur = cur_top(s)
    if element_start(s) == cur
        return
    end
    n = element_prev(s, cur)
    set_cur_top(s, n)
end

function currently_focused(w::ScrollableChain)
    # Search the the focus stack for our widget
    i = findfirst(w.ctx.focuss.stack,w)
    @assert i != 0
    if i == length(w.ctx.focuss.stack)
        return (0,w)
    else
        child = w.ctx.focuss.stack[i+1]
        idx = findfirst(w.children,child)
        @assert idx != 0
        return (idx,child)
    end
end
function focus_next(w::ScrollableChain; show_invisible = false)
    # show_visible = false, should skip over hidden, not implemented
    next = currently_focused(w)[1]+1
    if next > length(w.children)
        return false
    end
    if show_invisible
        makevisible(w.children[next])
    end
    focus(w.children[next])
    invalidate(w)
    return true
end

keymap(w::ScrollableChain) = Dict(
    '\t' => (args...)->focus_next(w)
)

child_ok(w::Widget) = true
child_ok{T}(w::Signal{T}) = T <: Widget

init_child(w::Widget,this::Widget) = this
function init_child{T<:Widget}(child::Signal{T},this::Widget)
    lift(child) do w
        w.ctx.parent = this
        if isdefined(this.ctx,:focuss)
            w.ctx.focuss = this.ctx.focuss
        end
        invalidate(this)
        nothing
    end
    return this
end

# Border
type Border <: Widget
    child::LazyWidget
    label::Label
    ctx::WidgetContext
    function Border(child::LazyWidget,label::Label)
        @assert child_ok(child)
        init_child(child,new(child,label,WidgetContext()))
    end
end
# first one is deprecated
Border(child::LazyWidget,label::AbstractString="") = Border(child,Label(label))
Border(label::AbstractString,child::LazyWidget) = Border(child,Label(label))
children(b::Border) = (b.child,)
focusable(b::Border) = focusable(b.child)
focus(b::Border) = focus(b.child)

minheight(b::Border) = 2
optheight(b::Border) = 2 + optheight(b.child)
optwidth(b::Border) = max(4 + optwidth(b.label), optwidth(b.child))

function draw_regular(s::Screen,b::Border)
    color = isfocused(b) ? :green : :default
    swrite(s,1              ,1             ,'┌', attrs = IsACS, fg = color) # Top Left
    swrite(s,1              ,2:(width(s)-1),'─', attrs = IsACS, fg = color) # Top
    swrite(s,1              ,   width(s)   ,'┐', attrs = IsACS, fg = color) # Top Right
    swrite(s,2:(height(s)-1),1             ,'│', attrs = IsACS, fg = color) # Left
    swrite(s,2:(height(s)-1),   width(s)   ,'│', attrs = IsACS, fg = color) # Right
    swrite(s,   height(s)   ,1             ,'└', attrs = IsACS, fg = color) # Bottom Left
    swrite(s,  (height(s)  ),2:(width(s)-1),'─', attrs = IsACS, fg = color) # Bot
    swrite(s,   height(s)   ,   width(s)   ,'┘', attrs = IsACS, fg = color) # Bottom Right
    # Label
    opt = optwidth(b.label)
    draw(subscreen(s,1:1,3:(min(2+opt,width(s)-1))),b.label)
    # Child
    draw(subscreen(s,2:(height(s)-1),2:(width(s)-1), value(b.child); scroll = true),value(b.child))
end

# TightCentering
# Centers the widget to the size given by its `minbounds`
type TightCentering <: Widget
    w::Widget
    ctx::WidgetContext
    TightCentering(w) = new(w,WidgetContext())
end
children(t::TightCentering) = (t.w,)
focus(t::TightCentering) = focus(t.w)

function draw_regular(s::Screen, w::TightCentering)
    cw = w.w
    bounds = (optheight(cw),optwidth(cw))
    starth = div(height(s)-bounds[1],2)
    startw = div(width(s)-bounds[2],2)
    draw(subscreen(s,starth:(starth+bounds[1]),startw:(startw+bounds[2])),cw)
end

# Layout
type RowLayout <: Widget
    cols::Vector{Widget}
    spans::Vector{Int}
    fixedwidths::Vector{Int}
    span::Int
    ctx::WidgetContext
    function RowLayout(cols,spans,fixedwidths,span)
        new(cols,spans,fixedwidths,span,WidgetContext())
    end
end
children(r::RowLayout) = r.cols
cur_top(r::RowLayout) = map(cur_top,r.cols)
focusable(r::RowLayout) = any(map(focusable,r.cols))
focus(r::RowLayout) = focus_child(r)

element_start(r::RowLayout) = map(element_start,r.cols)
function element_next(r::RowLayout,i)
    map(zip(i,r.cols)) do x
        j, w = x
        if !element_done(w,j)
            return element_next(w,j)
        else
            return j
        end
    end
end
function element_done(r::RowLayout,i)
    all(zip(i,r.cols)) do x
        j, w = x
        element_done(w,j)::Bool
    end
end

# A row layout is scrollable if all its children are
isscrollable(r::RowLayout) = all(map(isscrollable,children(r)))

function optheight(rl::RowLayout)
    maximum([optheight(w) for w in rl.cols])
end

function compute_widths(rl::RowLayout,totalwidth)
    # The extra width will be given to the last column
    curwidth = 0
    numfixed = 0
    totalfixed = 0
    for i in eachindex(rl.fixedwidths)
        if rl.fixedwidths[i] != 0
            numfixed += rl.spans[i]
            totalfixed += rl.fixedwidths[i]
        end
    end
    widths = Array{UnitRange}(0)
    sizehint!(widths,length(rl.cols))
    for i = eachindex(rl.cols)
        c = rl.cols[i]
        span = rl.spans[i]
        fixed = rl.fixedwidths[i]
        if fixed == 0
            w = floor(Integer,span*(totalwidth-totalfixed)/(rl.span-numfixed))
        else
            w = fixed
        end
        startw = 1+curwidth
        curwidth += w
        if i == endof(rl.cols)
            endw = totalwidth
        else
            endw = startw + w - 1
        end
        push!(widths,startw:endw)
    end
    widths
end

function draw_regular(s::Screen,rl::RowLayout)
    for (w,r) in zip(rl.cols,compute_widths(rl,width(s)))
        draw(subscreen(s,1:height(s),r, w; scroll = true),w)
    end
end

function draw_element_line(s::Screen,rl::RowLayout,it)
    for (w,r,i) in zip(rl.cols,compute_widths(rl,width(s)),it)
        r = draw_element_line(subscreen(s,1:height(s),r),w,i)::Int
        @assert r == 1
    end
    return 1
end

# CellDisplay
# Just a bunch of terminal cells
type CellDisplay <: ScrollableWidget
    rows::Vector{Vector{Cell}}
    cur_top::Int
    ctx::WidgetContext
    CellDisplay(rows) = new(rows,1,WidgetContext())
end
optheight(cd::CellDisplay) = length(cd.rows)
cur_top(cd::CellDisplay) = cd.cur_top
element_start(cd::CellDisplay) = 1
element_next(cd::CellDisplay, i) = i+1
element_done(cd::CellDisplay, i) = i > length(cd.rows)

function draw_element_line(s::Screen, d::CellDisplay, row)
    ncells = min(width(s),length(d.rows[row]))
    for col in 1:ncells
        c = d.rows[row][col]
        swrite(s, 1, col, c)
    end
    return 1
end

function draw_regular(s::Screen, d::CellDisplay)
    for row in 1:min(height(s),length(d.rows))
        draw_element_line(s, d, row)
    end
end

#IOBufferView
type IOBufferView <: ScrollableWidget
    buf::IOBuffer
    do_show_cursor::Bool
    may_show_cursor::Bool
    draw_empty_last_line::Bool
    cur_top::UnitRange
    ctx::WidgetContext
    function IOBufferView(buf,may_show_cursor=true,draw_empty_last_line=true)
        this = new(buf,false,may_show_cursor,draw_empty_last_line,0:0,WidgetContext())
        this.cur_top = element_start(this)
        this
    end
end
optheight(view::IOBufferView) = 10
cur_top(view::IOBufferView) = view.cur_top
set_cur_top(view::IOBufferView, it::UnitRange) = view.cur_top = it
focusable(view::IOBufferView) = true

function scroll_up(l::IOBufferView, pos)
    it = element_next(l, l.cur_top)
    if !element_done(l, it)
        l.cur_top = it
        invalidate(l)
    end
end
function scroll_down(l::IOBufferView, pos)
    if l.cur_top != element_start(l)
        l.cur_top = element_prev(l, l.cur_top)
        invalidate(l)
    end
end

function didwrite(view::IOBufferView,at,nmoved)
    at = at+1
    #@show (at,view.cur_top)
    if at < first(view.cur_top)
        # Need to move the start of the range by the number of bytes inserted
        view.cur_top = (first(view.cur_top)+nmoved):(last(view.cur_top)+nmoved)
    elseif first(view.cur_top) <= at && (
        # If we inserted into the current line
        at <= last(view.cur_top) ||
        # Or the buffer was empty
        last(view.cur_top) == -1 ||
        # Or this is the last line
        sizeof(view.buf.data)-nmoved == last(view.cur_top))
        # Naively, we'd just have to add nmoved to the end, but we may have
        # writeen one or more newlines, so we need to look for that
        idx = findnext(view.buf.data,UInt8('\n'),at)
        start = first(view.cur_top)
        view.cur_top = (start == 0 ? 1 : start):(idx == 0 ? endof(view.buf.data) : idx-1)
    end
    #@show view.cur_top
end

buffer(view::IOBufferView) = view.buf
focused(w::IOBufferView) = w.may_show_cursor && (w.do_show_cursor = true)
defocused(w::IOBufferView) = w.may_show_cursor && (w.do_show_cursor = false)

# 0:-1 is used as the invalid marker, but next is arranged in such
# a way that next of invalid is the first valid line. Since done is called
# before next in normal application code, this is not a problem.
element_start(view::IOBufferView) = element_next(view,0:-1)
function element_next(view::IOBufferView, cur_range)
    # Special case: An empty buffer with draw_empty_last_line
    # will show an empty line with a cursor. This state is represented as 1:0
    if view.draw_empty_last_line && isempty(view.buf.data) && cur_range == 0:-1
        return 1:0
    end
    if cur_range == 1:0 || last(cur_range) == sizeof(view.buf.data) ||
        isempty(view.buf.data)
        return 0:-1
    end
    # Cur range will have ended on the character before a \n, so the point
    # to start searching is the character after the \n (i.e. +2)
    idx = findnext(view.buf.data,UInt8('\n'),last(cur_range)+2)
    if idx == 0
        # The last line is empty
        if !view.draw_empty_last_line &&
            (last(cur_range)+2 >= sizeof(view.buf.data))
            return 0:-1
        else
            return  (last(cur_range)+2):sizeof(view.buf.data)
        end
    else
        return (last(cur_range)+2):(idx-1)
    end
end
function element_prev(view::IOBufferView, cur_range)
    idx = findprev(view.buf.data,'\n',first(cur_range)-2)
    if idx == 0
        return element_start(view)
    end
    return (idx+1):(first(cur_range)-2)
end
element_done(view::IOBufferView,range) = range == 0:-1

# This implementation is significantly simpler than the LineEdit.jl
# implementation, because it doesn't have to deal with a)
#   A) Calculating movement commands
#   B) Worry About the Prompt
#   C) Position the actual cursor correctly
# `offsets` are offsets into the IOBuffer
function draw_element_line(s::Screen, input::IOBufferView, offsets)
    cols = width(s)
    buf = buffer(input)
    text = @compat(String)(buf.data)
    buf_pos = position(buf)
    curs_pos = buf_pos+1
    row = 1
    col = 1
    offset = first(offsets)
    justwrapped = false
    em = LineEmulator(Cell(Cell('\0'),fg=9,bg=9))
    seek(buf,first(offsets)-1)
    while position(buf)+1 <= last(offsets) && text[position(buf)+1] != '\n'
        iscurs = (offset == curs_pos) && input.do_show_cursor
        #@assert !iscurs
        c = parse_cell!(em,buf)
        if (c.content == '\0')
            break
        end
        # For now, just consider a tabstop every 4 cells
        if c.content == '\t'
            nexttab = 4*(div(col-1,4)+1)+1
            swrite(s, row, col:(nexttab-1), Cell(c,content=' '))
            col = nexttab
        else
            swrite(s, row, col, Cell(Cell(c),
                fg = iscurs ? :black : :default,
                bg = iscurs ? :white : :default))
            col += 1
        end
        if col == cols+1
            row = row + 1
            col = 1
            justwrapped = true
        end
        offset = nextind(text,offset)
    end
    seek(buffer(input),buf_pos)
    lastoffset = offset
    ((offset <= last(offsets) && text[offset] == '\n') ||
        (offset > endof(text))) && (offset += 1)
    # Render the cursor if it's after the end of the text
    if curs_pos == lastoffset && input.do_show_cursor
        swrite(s, row, col, ' ', bg = :blue)
    end
    return row
end


function draw_scroll_indicators(s::Screen, input; up = true, down = true)
    up   && swrite(s,1        , width(s), '▲', fg=:blue)
    down && swrite(s,height(s), width(s), '▼', fg=:blue)
end

# TextInput
# Base on LineEdit.jl's multi line input widget
import Base.LineEdit: edit_insert, edit_backspace,
    edit_move_right, edit_move_left, move_input_end, move_line_end

type TextInput <: ScrollableWidget
    view::IOBufferView
    on_enter::Function
    on_done::Function
    multiline::Bool
    tabcomplete::Bool
    optheight::Int
    ctx::WidgetContext
    TextInput(buf=IOBuffer(),multiline=false,tabcomplete=false,draw_empty_last_line=true) =
        new(IOBufferView(buf,true,draw_empty_last_line),
            (args...)->true,(args...)->nothing,tabcomplete,multiline,10,
            WidgetContext())
end

children(i::TextInput) = (i.view,)
optheight(e::TextInput) = e.optheight
draw(s::Screen, input::TextInput) = draw(s,input.view)
focusable(w::TextInput) = true
focus(w::TextInput) = focus(w.view)

element_start(w::TextInput)     = element_start(w.view)
element_next(w::TextInput, idx) = element_next(w.view, idx)
element_done(w::TextInput, idx) = element_done(w.view, idx)
cur_top(w::TextInput)           = cur_top(w.view)
draw_element_line(s::Screen, w::TextInput, idx) =
    draw_element_line(s, w.view, idx)

# LineEdit support
buffer(w::TextInput) = w.view.buf
function edit_insert(w::TextInput,args...)
    pos = position(w.view.buf)
    n = edit_insert(buffer(w),args...)
    didwrite(w.view,pos,n)
    invalidate(w)
end
function edit_backspace(w::TextInput)
    pos = position(w.view.buf)
    edit_backspace(buffer(w))
    didwrite(w.view, pos, -1)
    invalidate(w)
end
edit_move_left(w::TextInput) = edit_move_left(buffer(w)) && invalidate(w)
edit_move_right(w::TextInput) = edit_move_right(buffer(w)) && invalidate(w)
move_input_end(w::TextInput) = (move_input_end(buffer(w)); invalidate(w))
move_line_end(w::TextInput) = (move_line_end(buffer(w)); invalidate(w))

function keymap(w::TextInput)
    d = AnyDict(
    # Enter
    '\r' => (s,o...)->begin
        line = String(take!(copy(buffer(w))))
        if w.on_enter(line)
            w.on_done(line)
        end
    end,
    '\n' => KeyAlias('\r'),
    # Backspace/^H
    '\b' => (s,o...)->edit_backspace(w),
    127 => KeyAlias('\b'),
    # Meta Backspace
    "\e\b" => (s,o...)->edit_delete_prev_word(w),
    # ^D
    "^D" => (s,o...)->begin
        if buffer(s).size > 0
            edit_delete(w)
        else
            return :abort
        end
    end,
    "^B" => (s,o...)->edit_move_left(w),
    "^F" => (s,o...)->edit_move_right(w),
    # Meta B
    "\eb" => (s,o...)->edit_move_word_left(w),
    # Meta F
    "\ef" => (s,o...)->edit_move_word_right(w),
    # Meta Enter
    "\e\r" => (s,o...)->(edit_insert(w, '\n')),
    "\e\n" => "\e\r",
    # Simply insert it into the buffer by default
    "*" => (s,data,c)->(edit_insert(w, c)),
    "^U" => (s,o...)->edit_clear(w),
    "^K" => (s,o...)->edit_kill_line(w),
    "^Y" => (s,o...)->edit_yank(w),
    "^A" => (s,o...)->(move_line_start(w); invalidate(w)),
    "^E" => (s,o...)->(move_line_end(w); invalidate(w)),
    # Try to catch all Home/End keys
    "\e[H"  => (s,o...)->(move_input_start(w); invalidate(w)),
    "\e[F"  => (s,o...)->(move_input_end(w); invalidate(w)),
    #"^L" => (s,o...)->(Terminals.clear(terminal(s)); refresh_line(s)),
    "^W" => (s,o...)->edit_werase(w),
    # Meta D
    "\ed" => (s,o...)->edit_delete_next_word(w),
    "^C" => (s,o...)->begin
        try # raise the debugger if present
            ccall(:jl_raise_debugger, Int, ())
        end
        move_input_end(w)
        invalidate(w)
        #transition(s, :reset)
        #refresh_line(s)
        return :done
    end,
    "^Z" => (s,o...)->(return :suspend),
    # Right Arrow
    "\e[C" => (s,o...)->edit_move_right(w),
    # Left Arrow
    "\e[D" => (s,o...)->edit_move_left(w),
    # Delete
    "\e[3~" => (s,o...)->edit_delete(w),
    "^T" => (s,o...)->edit_transpose(w),
)
    if w.tabcomplete
        d['\t'] = (s,o...)->begin
            buf = buffer(w)
            # Yes, we are ignoring the possiblity
            # the we could be in the middle of a multi-byte
            # sequence, here but that's ok, since any
            # whitespace we're interested in is only one byte
            i = position(buf)
            if i != 0
                c = buf.data[i]
                if c == '\n' || c == '\t' ||
                   # hack to allow path completion in cmds
                   # after a space, e.g., `cd <tab>`, while still
                   # allowing multiple indent levels
                   (c == ' ' && i > 3 && buf.data[i-1] == ' ')
                    edit_insert(w, " "^4)
                    return
                end
            end
            complete_line(w)
            invalidate(w)
        end
    end

    if w.multiline
        merge!(d,AnyDict(
            # Up Arrow
            "\e[A" => (s,o...)->edit_move_up(w),
            # Down Arrow
            "\e[B" => (s,o...)->edit_move_down(w),
        ))
    end

    d
end

Base.Terminals.beep(t::TextInput) = nothing
Base.println(t::TextInput) = error()

# REPLWidget
import Base: LineEdit
import Base.LineEdit: ModalInterface
import Base.REPL: AbstractREPL

if isdefined(Base.REPL,:MIRepl)
    import Base.REPL.MIRepl
else
    const MIRepl = Base.REPL.LineEditREPL
end

#=
type REPLWidget
    repl::ModalInterface
    widget::Widget
    ctx::WidgetContext
    REPLWidget(repl,widget) = new(repl,widget)
end=#

type WidgetREPL <: AbstractREPL
    widget::Widget
    inputarea::Widget
    historybuf::IOBuffer
    historyview::IOBufferView
    mi::MIRepl
    WidgetREPL() = new()
end
Base.REPL.reset(repl::WidgetREPL) = nothing
Base.REPL.backend(repl::WidgetREPL) = Base.REPL.backend(repl.mi)
Base.LineEdit.deactivate(a,s,c, ::TerminalUI.TextInput) = s
Base.LineEdit.activate(a,b,c, ::TerminalUI.TextInput) = nothing
Base.LineEdit.commit_changes(::TerminalUI.TextInput, _) = nothing
function Base.REPL.prepare_next(repl::WidgetREPL)
    makevisible(repl.inputarea)
end

immutable WidgetREPLDisplay
    repl::WidgetREPL
end

function write_history(repl::WidgetREPL, tempbuf::IOBuffer)
    pos = position(repl.historybuf)+1
    didwrite(repl.historyview,pos,write(repl.historybuf,take!(tempbuf)))
end

function display(d::WidgetREPLDisplay, ::MIME"text/plain", x)
    statebuf = d.repl.historybuf
    tempbuf = IOBuffer()
    writemime(tempbuf, MIME("text/plain"), x)
    println(tempbuf)
    write_history(d.repl,tempbuf)
end
display(d::WidgetREPLDisplay, x) = display(d, MIME("text/plain"), x)

function Base.REPL.print_response(repl::WidgetREPL, val::ANY, bt, show_value::Bool, have_color::Bool)
    while true
        try
            if bt !== nothing
                tempbuf = IOBuffer()
                Base.REPL.display_error(tempbuf, val, bt)
                println(tempbuf)
                write_history(repl,tempbuf)
                iserr, lasterr = false, ()
            else
                if val !== nothing && show_value
                    try
                        display(WidgetREPLDisplay(repl),val)
                    catch err
                        println(repl.historybuf, "Error showing value of type ", typeof(val), ":")
                        rethrow(err)
                    end
                end
            end
            break
        catch err
            if bt !== nothing
                tempbuf = IOBuffer()
                println(tempbuf, "SYSTEM: show(lasterr) caused an error")
                println(tempbuf, err)
                write_history(repl,tempbuf)
                break
            end
            val = err
            bt = catch_backtrace()
        end
    end
end


function WidgetForREPL(repl::ModalInterface,wr::WidgetREPL)
    inputbuf = IOBuffer(b"", true, true)
    wr.historybuf = IOBuffer(b"",true,true)
    wr.historyview = IOBufferView(wr.historybuf,false,false)
    input = TextInput(inputbuf,true,true,false)

    mistate = LineEdit.init_state(input, repl)

    lift(input.content) do line
        if LineEdit.on_enter(mistate) && line != ""
            tempbuf = IOBuffer()
            write(tempbuf,"\033[1m\033[32mjulia> \033[1m\033[37m",line,"\033[0m\n")
            write_history(wr, tempbuf)
            buf = input.view.buf
            b = IOBuffer()
            input.view.buf = b
            input.view.cur_top = element_start(input.view)
            hide(wr.inputarea)
            isdefined(Main,:redraw) && Main.redraw()
            mistate.mode_state[mistate.current_mode].p.on_done(mistate,buf,true)
            isdefined(Main,:redraw) && Main.redraw()
            nothing
        end
    end

    cd = TerminalUI.CellDisplay(
        Vector{VT100.Cell}[[
            VT100.Cell(TerminalUI.dummycell(:green,:default,0),content = c)
            for c in "julia> "]])

    wr.inputarea = RowLayout([cd,input],[1,1],[7,0],2)
    b = ScrollableChain([wr.historyview,wr.inputarea])
end

import Base.LineEdit: refresh_multi_line
function refresh_multi_line(termbuf::LineEdit.TerminalBuffer,
    w::TextInput, s::LineEdit.ModeState)
    #invalidate(w)
end
function refresh_multi_line(w::TextInput, args...)
    #invalidate(w)
end

# ImageWidget
type ImageWidget <: Widget
    data::Vector{UInt8}
    optheight::Int
    # Screen char if allocated
    screenchar::Cell
    ctx::WidgetContext
    ImageWidget() = new(Vector{UInt8}(),10,Cell(Char(0)))
end
optheight(w::ImageWidget) = w.optheight

function draw(s::Screen,img::ImageWidget)
    clear(s)
    if img.screenchar.content != Char(0)
        swrite_image(s, 1:height(s), 1:width(s), img.screenchar)
    elseif !isempty(img.data)
        img.screenchar = swrite_image(s, 1:height(s), 1:width(s), img.data)
    end
end

# ListWidget
type ListWidget <: Widget
    item::Any
    draw_indicies::Bool
    highlight_idx::Int
    highlighted::Signal{Any}
    cur_top
    ctx::WidgetContext
    function ListWidget(item)
        this = new(item,true,start(item),Signal(Any, start(item)),start(item),WidgetContext())
        map(this.highlighted) do it
            invalidate(this)
        end
        this
    end
end
optheight(l::ListWidget) = isa(Base.iteratorsize(l.item),Union{Base.HasLength,Base.HasShape}) ? length(l.item) : 10
cur_top(l::ListWidget) = l.cur_top
isscrollable(l::ListWidget) = true

element_start(l::ListWidget) = start(l.item)
element_next(l::ListWidget,it) = next(l.item,it)[2]
element_prev(l::ListWidget,it) = prevind(l.item,it)
element_done(l::ListWidget,it) = done(l.item,it)
function draw_element_line(s::Screen, l::ListWidget, it)
    if l.draw_indicies
        # Try to figure out how much space we'll need for writing the index
        lidx = endof(l.item)
        maxwidth = strwidth(sprint(print,lidx))
        swrite(s, 1, 1, '[')
        swrite(s, 1, 2 + maxwidth, ']')
        draw(subscreen(s,1:height(s),2:(2+maxwidth)),Label(sprint(print,it)))
        s = subscreen(s,1:height(s),(4+maxwidth):width(s))
    end
    highlight = l.highlight_idx == it
    clear(s)
    if highlight
        swrite(s, 1:height(s), 1:width(s), ' ', fg = :black, bg = :yellow)
    end
    draw_regular(s,Label(sprint(print,l.item[it]));
        fg = highlight ? nothing : :default, bg = highlight ? nothing : :default)
    1
end

function scroll_up(l::ListWidget, pos)
    it = element_next(l, l.cur_top)
    if !element_done(l, it)
        l.cur_top = it
        invalidate(l)
    end
end
function scroll_down(l::ListWidget, pos)
    if l.cur_top != element_start(l)
        l.cur_top = element_prev(l, l.cur_top)
        invalidate(l)
    end
end


keymap(l::ListWidget) = Dict(
    "\e[A"      => (s,p,c)->begin
        if l.highlight_idx != element_start(l)
            l.highlight_idx = element_prev(l, l.highlight_idx)
            push!(l.highlighted, l.highlight_idx)
            invalidate(l)
        end
    end,
    "\e[B"      => (s,p,c)->begin
        it = element_next(l, l.highlight_idx)
        if !element_done(l, it)
            l.highlight_idx = it
            push!(l.highlighted, it)
            invalidate(l)
        end
    end
)

# Like a list Widget, but one of the items can be selected (has an on_done callback),
# and selections can be made by numeric entry
type MenuWidget <: Widget
    on_done::Any
    list_widget::ListWidget
    ctx::WidgetContext
    function MenuWidget(on_done, item)
        new(on_done, ListWidget(item),WidgetContext())
    end
end
children(w::MenuWidget) = (w.list_widget,)
focusable(w::MenuWidget) = true
focus(w::MenuWidget) = focus(w.list_widget)
isscrollable(W::MenuWidget) = true
cur_top(w::MenuWidget) = cur_top(w.list_widget)
optheight(w::MenuWidget) = optheight(w.list_widget)

element_start(l::MenuWidget) = element_start(l.list_widget)
element_next(l::MenuWidget,it) = element_next(l.list_widget,it)
element_prev(l::MenuWidget,it) = element_prev(l.list_widget,it)
element_done(l::MenuWidget,it) = element_done(l.list_widget,it)

draw_element_line(s::Screen, l::MenuWidget, it) =
    draw_element_line(s, l.list_widget, it)

keymap(widget::MenuWidget) = Dict(
    '\r' => (s,p,c) -> widget.on_done(widget.list_widget.highlight_idx)
)
