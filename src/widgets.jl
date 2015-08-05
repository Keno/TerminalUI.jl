    # Imports
import Base: start, next, done, getindex, length, show
import Base.LineEdit: KeyAlias, AnyDict

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

# ScrollableWidget
abstract ScrollableWidget <: Widget
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
    swrite(s,1,1,Base._truncate_at_width_or_chars(l.text,width(s)*height(s)); fg = fg, bg = :default)
end

# SimpleSlider
type SimpleSlider{T} <: Widget
    min::T
    max::T
    val::Input{T}
    last_sliderwidth::Int
    ctx::WidgetContext
    SimpleSlider(min::T,max::T,val::T) = new(min,max,Input{T}(val),0)
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
    Gauge(min,max,val) = new(min,max,val)
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
CheckBox(s::String) = CheckBox(false,Label(s),false)
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
    WidgetStack(widgets,curidx=1) = new(widgets,curidx)
end
WidgetStack(widgets::Vector) = WidgetStack(Widget[w for w in widgets], 1)
children(w::WidgetStack) = w.widgets

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
    "\e[B"      => (s,p,c)->(update_index(w,mod1(w.curidx + 1,length(w.widgets))))
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
        new(children,1)
    end
end
children(s::ScrollableChain) = s.children
optheight(s::ScrollableChain) = 20

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


# Border
type Border <: Widget
    child
    label::Label
    ctx::WidgetContext
    Border(child,label) = new(child,label)
end
Border(child,label::AbstractString="") = Border(child,Label(label))
children(b::Border) = (b.child,)

minheight(b::Border) = 2
optheight(b::Border) = 2 + optheight(b.child)

function draw_regular(s::Screen,b::Border)
    swrite(s,1              ,1             ,'┌', attrs = IsACS) # Top Left
    swrite(s,1              ,2:(width(s)-1),'─', attrs = IsACS) # Top
    swrite(s,1              ,   width(s)   ,'┐', attrs = IsACS) # Top Right
    swrite(s,2:(height(s)-1),1             ,'│', attrs = IsACS) # Left
    swrite(s,2:(height(s)-1),   width(s)   ,'│', attrs = IsACS) # Right
    swrite(s,   height(s)   ,1             ,'└', attrs = IsACS) # Bottom Left
    swrite(s,  (height(s)  ),2:(width(s)-1),'─', attrs = IsACS) # Bot
    swrite(s,   height(s)   ,   width(s)   ,'┘', attrs = IsACS) # Bottom Right
    # Label
    opt = optwidth(b.label)
    draw(subscreen(s,1:1,3:(min(2+opt,width(s)-1))),b.label)
    # Child
    draw(subscreen(s,2:(height(s)-1),2:(width(s)-1), b.child; scroll = true),b.child)
end

# TightCentering
# Centers the widget to the size given by its `minbounds`
type TightCentering <: Widget
    w::Widget
    ctx::WidgetContext
    TightCentering(w) = new(w)
end
children(t::TightCentering) = (t.w,)

function draw_regular(s::Screen, w::TightCentering)
    cw = w.w
    bounds = minbounds(cw)
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
    RowLayout(cols,spans,fixedwidths,span) = new(cols,spans,fixedwidths,span)
end
children(r::RowLayout) = r.cols

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
    widths = Array(UnitRange,0)
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
    ctx::WidgetContext
    CellDisplay(rows) = new(rows)
end
optheight(cd::CellDisplay) = length(cd.rows)
element_start(cd::CellDisplay) = 1
element_next(cd::CellDisplay, i) = i+1
element_done(cd::CellDisplay, i) = i > length(cd.rows)

function draw_element_line(s::Screen, d::CellDisplay, row)
    for col in 1:min(width(s),length(d.rows[row]))
        swrite(s, row, col, d.rows[row][col])
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
    function IOBufferView(buf,may_show_cursor=false,draw_empty_last_line=true)
        this = new(buf,false,may_show_cursor,draw_empty_last_line)
        this.cur_top = element_start(this)
        this
    end
end
optheight(view::IOBufferView) = 10
cur_top(view::IOBufferView) = view.cur_top
set_cur_top(view::IOBufferView, it::UnitRange) = view.cur_top = it

function didwrite(view::IOBufferView,at,nmoved)
    at = at+1
    if at < first(view.cur_top)
        # Need to move the start of the range by the number of bytes inserted
        view.cur_top = (first(view.cur_top)+nmoved):(last(view.cur_top)+nmoved)
    elseif first(view.cur_top) <= at && (at <= last(view.cur_top) || last(view.cur_top) == -1)
        # Naively, we'd just have to add nmoved to the end, but we may have
        # writeen one or more newlines, so we need to look for that
        idx = findnext(view.buf.data,'\n',at)
        start = first(view.cur_top)
        view.cur_top = (start == 0 ? 1 : start):(idx == 0 ? endof(view.buf.data) : idx-1)
    end
end

buffer(view::IOBufferView) = view.buf
focused(w::IOBufferView) = w.may_show_cursor && (w.do_show_cursor = true)
defocused(w::IOBufferView) = w.may_show_cursor && (w.do_show_cursor = false)

# 0:-1 is used as the invalid marker, but next is arranged in such
# a way that next of invalid is the first valid line. Since done is called
# before next in normal application code, this is not a problem.
element_start(view::IOBufferView) = element_next(view,0:-1)
function element_next(view::IOBufferView, cur_range)
    if last(cur_range) == sizeof(view.buf.data)
        return 0:-1
    end
    # Cur range will have ended on the character before a \n, so the point
    # to start searching is the character after the \n (i.e. +2)
    idx = findnext(view.buf.data,'\n',last(cur_range)+2)
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
function draw_element_line(s::Screen, input::IOBufferView, offsets)
    cols = width(s)
    text = UTF8String(buffer(input).data)
    curs_pos = position(buffer(input))+1
    row = 1
    col = 1
    offset = first(offsets)
    justwrapped = false
    while offset <= last(offsets) && text[offset] != '\n'
        iscurs = (offset == curs_pos) && input.do_show_cursor
        c = text[offset]
        swrite(s, row, col, c,
            fg = iscurs ? :black : :default,
            bg = iscurs ? :white : :default)
        justwrapped = false
        col += 1
        if col == cols+1
            row = row + 1
            col = 1
            justwrapped = true
        end
        offset = nextind(text,offset)
    end
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

# MultiLineInput
# Base on LineEdit.jl's multi line input widget
import Base.LineEdit: edit_insert, edit_backspace, edit_move_right, edit_move_left

type MultiLineInput <: ScrollableWidget
    view::IOBufferView
    content::Input{UTF8String}
    ctx::WidgetContext
    MultiLineInput(buf) = new(IOBufferView(buf,false),Input{UTF8String}(""))
end

children(i::MultiLineInput) = (i.view,)
optheight(e::MultiLineInput) = 10
draw(s::Screen, input::MultiLineInput) = draw(s,input.view)

element_start(w::MultiLineInput)     = element_start(w.view)
element_next(w::MultiLineInput, idx) = element_next(w.view, idx)
element_done(w::MultiLineInput, idx) = element_done(w.view, idx)
draw_element_line(s::Screen, w::MultiLineInput, idx) =
    draw_element_line(s, w.view, idx)

# LineEdit support
buffer(w::MultiLineInput) = w.view.buf
function edit_insert(w::MultiLineInput,args...)
    edit_insert(buffer(w),args...)
    invalidate(w)
end
edit_backspace(w::MultiLineInput) = (edit_backspace(buffer(w)); invalidate(w))
edit_move_left(w::MultiLineInput) = edit_move_left(buffer(w)) && invalidate(w)
edit_move_right(w::MultiLineInput) = edit_move_right(buffer(w)) && invalidate(w)

keymap(w::MultiLineInput) = AnyDict(
    # Tab
    '\t' => (s,o...)->begin
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
    end,
    # Enter
    '\r' => (s,o...)->begin
        line = takebuf_string(copy(buffer(w)))
        push!(w.content,line)
    end,
    '\n' => KeyAlias('\r'),
    # Backspace/^H
    '\b' => (s,o...)->edit_backspace(w),
    127 => KeyAlias('\b'),
    # Meta Backspace
    "\e\b" => (s,o...)->edit_delete_prev_word(w),
    "\e\x7f" => "\e\b",
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
    # Ctrl-Left Arrow
    "\e[1;5D" => "\eb",
    # Ctrl-Left Arrow on rxvt
    "\eOd" => "\eb",
    # Ctrl-Right Arrow
    "\e[1;5C" => "\ef",
    # Ctrl-Right Arrow on rxvt
    "\eOc" => "\ef",
    # Meta Enter
    "\e\r" => (s,o...)->(edit_insert(w, '\n')),
    "\e\n" => "\e\r",
    # Simply insert it into the buffer by default
    "*" => (s,data,c)->(edit_insert(w, c)),
    "^U" => (s,o...)->edit_clear(w),
    "^K" => (s,o...)->edit_kill_line(w),
    "^Y" => (s,o...)->edit_yank(w),
    "^A" => (s,o...)->(move_line_start(w); refresh_line(w)),
    "^E" => (s,o...)->(move_line_end(w); refresh_line(w)),
    # Try to catch all Home/End keys
    "\e[H"  => (s,o...)->(move_input_start(w); refresh_line(w)),
    "\e[F"  => (s,o...)->(move_input_end(w); refresh_line(w)),
    #"^L" => (s,o...)->(Terminals.clear(terminal(s)); refresh_line(s)),
    "^W" => (s,o...)->edit_werase(w),
    # Meta D
    "\ed" => (s,o...)->edit_delete_next_word(w),
    "^C" => (s,o...)->begin
        try # raise the debugger if present
            ccall(:jl_raise_debugger, Int, ())
        end
        move_input_end(s)
        refresh_line(s)
        print(terminal(s), "^C\n\n")
        transition(s, :reset)
        refresh_line(s)
    end,
    "^Z" => (s,o...)->(return :suspend),
    # Right Arrow
    "\e[C" => (s,o...)->edit_move_right(w),
    # Left Arrow
    "\e[D" => (s,o...)->edit_move_left(w),
    # Up Arrow
    "\e[A" => (s,o...)->edit_move_up(w),
    # Down Arrow
    "\e[B" => (s,o...)->edit_move_down(w),
    # Delete
    "\e[3~" => (s,o...)->edit_delete(w),
    "^T" => (s,o...)->edit_transpose(w),
)

Base.Terminals.beep(t::MultiLineInput) = nothing
Base.println(t::MultiLineInput) = error()

# REPLWidget
import Base: LineEdit
import Base.LineEdit: ModalInterface
import Base.REPL: AbstractREPL, MIRepl

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
Base.LineEdit.deactivate(a,s,c, ::TerminalUI.MultiLineInput) = s
Base.LineEdit.activate(a,b,c, ::TerminalUI.MultiLineInput) = nothing
Base.LineEdit.commit_changes(::TerminalUI.MultiLineInput, _) = nothing
function Base.REPL.prepare_next(repl::WidgetREPL)
    makevisible(repl.inputarea)
end

immutable WidgetREPLDisplay
    repl::WidgetREPL
end

function write_history(repl::WidgetREPL, tempbuf::IOBuffer)
    pos = position(repl.historybuf)+1
    didwrite(repl.historyview,pos,write(repl.historybuf,takebuf_array(tempbuf)))
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
    input = MultiLineInput(inputbuf)

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
    w::MultiLineInput, s::LineEdit.ModeState)
    #invalidate(w)
end
function refresh_multi_line(w::MultiLineInput, args...)
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
