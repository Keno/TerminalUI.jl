using VT100
import VT100: Line, Cursor, Cell
import Base: getindex, convert, show
using VT100.Flags
using VT100.Attributes

import VT100: get_image_cell, pos_for_image_cell, RGB8

using Colors
using FixedPointNumbers

import Base: ==, hash

immutable CellLoc
    row::Int
    col::Int
end
convert(::Type{CellLoc},t::Tuple{Int,Int}) = CellLoc(t...)
==(x::CellLoc,y::CellLoc) = x.row == y.row && x.col == y.col

immutable CellRect
    height::Int
    width::Int
end
convert(::Type{CellRect},t::Tuple{Int,Int}) = CellRect(t...)
==(x::CellRect,y::CellRect) = x.height == y.height && x.width == y.width

immutable CellBox
    topleft::CellLoc
    size::CellRect
end
==(x::CellBox, y::CellBox) = x.topleft == y.topleft && x.size == y.size


const LocOrRect = Union{CellLoc,CellRect}

==(x::LocOrRect,y::Tuple{Int,Int}) = x == (typeof(x))(y...)
==(x::Tuple{Int,Int},y::LocOrRect) = (typeof(y))(x...) == y

getindex(x::LocOrRect,i) = getfield(x,i)

# Used to minimize the number of characters to be written.
# The written characters themselves still have to be buffered,
# which is done by an IOBuffer in the `render` method.
type DoubleBufferedTerminalScreen <: Screen
    # If this is empty, the screen state is unkown and should be cleared first
    have::Vector{Line}
    want::Vector{Line}
    wantcursor::Cursor
    # Technically these are available by looking
    # at the above, but to be explicit, let's save
    # it here.
    size::CellRect
    # For mouse tracking
    subscreens::Vector{TrackingSubscreen}
    topwidget::Nullable{Widget}
    # For iterm2 image support
    images::Vector{Tuple{Vector{UInt8},CellRect}}
    freelist::IntSet     # Which indicies to recycle
    # Which indicies were dropped by the widget and can
    # be recycled if they are no longer in the image
    dropped::IntSet
    # For embedding support
    offset::CellLoc
    fullsize::CellRect
end
istracking(s::DoubleBufferedTerminalScreen) = true
topwidget(s::DoubleBufferedTerminalScreen) = get(s.topwidget)
subscreens(s::DoubleBufferedTerminalScreen) = s.subscreens
show(io::IO,s::DoubleBufferedTerminalScreen) =
    print(io,"DoubleBufferedTerminalScreen(",s.size[2],"x",s.size[1],")")

function resized(s::DoubleBufferedTerminalScreen,size)
    s.have = Vector{Line}()
    s.want = Vector{Line}()
    s.wantcursor = Cursor(1,1)
    if isfullscreen(s)
        s.size = size
    end
    s.fullsize = size
    empty!(s.subscreens)
    s.topwidget = Nullable{Widget}()
    s.want = emptybuffer(s)
    s
end

function clear(s::DoubleBufferedTerminalScreen)
    s.have = Vector{Line}()
    s.want = emptybuffer(s)
end

function invalidate(s::DoubleBufferedTerminalScreen)
    s.have = Vector{Line}()
    push!(invalidated2,s)
end

function allocate_char_for_image(s::DoubleBufferedTerminalScreen, data::Vector{UInt8}, size)
    if !isempty(s.freelist)
        idx = pop!(s.freelist)
        s.images[idx] = (data, size)
    else
        push!(s.images, (data, size))
        idx = length(s.images)
    end
    return Cell(Cell(Char(0xF0000+idx)), flags = CELL_IS_IMG)
end

function subscreen_for_pos(s::Screen,subscreens::Vector{TrackingSubscreen}, pos)
    for ss in subscreens
        if pos[1] in simple(ss).rows && pos[2] in simple(ss).cols
            return subscreen_for_pos(ss, ss.children, translate⁻¹(ss,pos))
        end
    end
    return (s, pos)
end

function add_child(s::DoubleBufferedTerminalScreen, ss::TrackingSubscreen)
    push!(s.subscreens, ss)
end

function redraw(s::DoubleBufferedTerminalScreen, w::Widget)
    empty!(s.subscreens)
    s.topwidget = w
    draw(s,w)
end

width(s::DoubleBufferedTerminalScreen) = s.size[2]
height(s::DoubleBufferedTerminalScreen) = s.size[1]
size(s::DoubleBufferedTerminalScreen) = s.size

isfullscreen(s::DoubleBufferedTerminalScreen) = s.offset == (1,1) && s.size == s.fullsize

function DoubleBufferedTerminalScreen(size; offset = (1,1), fullsize = size)
    s = DoubleBufferedTerminalScreen(
        Vector{Line}(),
        Vector{Line}(),
        Cursor(1,1),
        size,
        Vector{TrackingSubscreen}(),
        Nullable{Widget}(),
        Vector{Tuple{Vector{UInt8},CellRect}}(),
        IntSet(), IntSet(),
        offset,fullsize)
    s.want = emptybuffer(s)
    s
end

function invalidbuffer(s::DoubleBufferedTerminalScreen)
    Line[Line(Cell[Cell(0) for _ in 1:s.size[2]]) for _ in 1:s.size[1]]
end

function emptybuffer(s::DoubleBufferedTerminalScreen)
    Line[Line(Cell[Cell(' ') for _ in 1:s.size[2]]) for _ in 1:s.size[1]]
end

for f in (:cmove, :cmove_col, :cmove_line_up, :cmove_line_down,
    :cmove_up, :cmove_down)
    @eval function $(f)(s::DoubleBufferedTerminalScreen,args...)
        s.wantcursor = $(f)(s.wantcursor,args...)
    end
end

fresh_screen(s::DoubleBufferedTerminalScreen) = empty!(s.have)

function resize!(s::DoubleBufferedTerminalScreen,size)
    error("DoubleBufferedTerminalScreen cannot be resized. Create a new screen instead")
end

# Will be filled in in swrite write previous color
ascellparams(color::Void) = (0,RGB8(0,0,0),false)
ascellparams(symbol::Symbol) = (VT100.colorlist[symbol],RGB8(0,0,0),false)
ascellparams(c::Color)  = (0,convert(RGB8,c),true)

function dummycell(fg,bg,attrs = 0)
    fg, fg_rgb, fg_is_rgb = ascellparams(fg)
    bg, bg_rgb, bg_is_rgb = ascellparams(bg)
    Cell(Char(0),0 | (fg_is_rgb ? FG_IS_RGB : 0) |
        (bg_is_rgb ? BG_IS_RGB : 0),fg,bg,attrs,fg_rgb,bg_rgb)
end

function update_cell!(line,idx,dcell,char,fg,bg)
    oldcell = line[idx]
    cell = Cell(dcell; content = char)
    fg === nothing && (cell = Cell(cell; fg = oldcell.fg, fg_rgb = oldcell.fg_rgb))
    bg === nothing && (cell = Cell(cell; bg = oldcell.bg, bg_rgb = oldcell.bg_rgb))
    line[idx] = cell
end

function swrite(s::DoubleBufferedTerminalScreen, rows, cols, cell::Cell)
    for r in rows
        line = s.want[r]
        for c in cols
            line[c] = cell
        end
    end
end

function swrite(s::DoubleBufferedTerminalScreen, rows, cols, char::Char; fg = :default, bg = :default, attrs = 0)
    if attrs & IsACS != 0
        i = findfirst(VT100.LineDrawing,char)
        @assert i != -1
        char = Char(i-1)
    end
    dcell = dummycell(fg,bg,attrs)
    for r in rows
        line = s.want[r]
        for c in cols
            update_cell!(line,c,dcell,char,fg,bg)
        end
    end
end

function swrite_image(s::DoubleBufferedTerminalScreen, rows, cols, data::Vector{UInt8})
    c = allocate_char_for_image(s,data,CellRect(length(rows),length(cols)))
    swrite_image(s,rows,cols,c)
    c
end

function swrite_image(s::DoubleBufferedTerminalScreen, rows, cols, c::Cell)
    @assert (c.flags & CELL_IS_IMG) != 0
    for row in rows
        line = s.want[row]
        for col in cols
            line[col] = get_image_cell(c,row-first(rows)+1,col-first(cols)+1)
        end
    end
    c
end

function clear(s::DoubleBufferedTerminalScreen, rows, cols)
    swrite(s, rows, cols, ' ')
end

function swrite(s::DoubleBufferedTerminalScreen, rows, cols, string::AbstractString;
        fg = :default, bg = :default, attrs = 0)
    @assert attrs & IsACS == 0
    dcell = dummycell(fg,bg,attrs)
    for r in rows
        line = s.want[r]
        for c in cols
            for (i,char) in enumerate(string)
                update_cell!(line,c+i-1,dcell,char,fg,bg)
            end
        end
    end
end

function do_erase(buf,n)
    if n <= 5
        for i = 1:n
            write(buf,' ')
        end
    else
        write(buf,CSI,string(n),'X') # ECH
        do_move(buf,n)
    end
end

function do_clear_rect(buf,box::CellBox)
    tl = box.topleft
    top = tl.row
    left = tl.col
    bot = top + box.size.height
    right = left + box.size.width

    # For terminals that support DECERA
    # write(buf,CSI,top,';',left,';',bot,';',right,"\$z")
    for i in top:bot
        do_absmove(buf,i,left)
        write(buf,CSI,string(right-left),'X') # ECH
    end
end

function do_absmove(buf,row,col)
    write(buf, CSI, string(row), ';', string(col), 'H') # CUP
end

do_absmove(buf,s,row,col) = do_absmove(buf,s.offset[1]+row-1,s.offset[2]+col-1)

function do_move(buf,n)
    write(buf, CSI, string(n), 'C') # CUF
end

function change_color(buf, flags, color, rgb, is_256, is_rgb)
    if (flags & is_256) != 0
        write(buf,"8;5;",string(color))
    elseif (flags & is_rgb) != 0
        write(buf,"8;2;",string(rgb.r.i),';',
                          string(rgb.g.i),';',
                          string(rgb.b.i))
    else
        write(buf,string(color))
    end
    write(buf,'m')
end

function change_fg_color(buf, cell)
    write(buf,CSI,'3')
    change_color(buf, cell.flags, cell.fg, cell.fg_rgb, FG_IS_256, FG_IS_RGB)
end

function change_bg_color(buf, cell)
    write(buf,CSI,'4')
    change_color(buf, cell.flags, cell.bg, cell.bg_rgb, BG_IS_256, BG_IS_RGB)
end

function change_attrs(buf, want, have)
    # If we are clearing any bits
    if (want & IsACS) != (have & IsACS)
        write(buf,(want & IsACS) == 0 ? "\e(B" : "\e(0")
    end
    if (have & ~want) != 0
        write(buf,CSI,"0m")
        have = 0
    end
    if (want & Bright) != 0
        write(buf,CSI,"1m")
    end
end

function render_cell(buf,wantc,attrs)
    (wantc.bg != attrs.bg || wantc.bg_rgb != attrs.bg_rgb) && change_bg_color(buf,wantc)
    (wantc.fg != attrs.fg || wantc.fg_rgb != attrs.fg_rgb) && change_fg_color(buf,wantc)
    (wantc.attrs != attrs.attrs) && change_attrs(buf,wantc.attrs,attrs.attrs)
    write(buf,wantc.content)
    wantc
end

same_attributes(havec,wantc) = Cell(havec; content = '\0') == Cell(wantc; content = '\0')

include("iterm2.jl")

"""
Core screen rendering functionality. Loop through the screen and update all
the cells that have changed since the last time we wenth through (in the most
efficient way possible).

- We do images first because they need to be written out as a chunk, and it
  is not worth the incresed complexity to go back and figure out exactly
  which areas we'd have to reprocess.

"""
function render(s::DoubleBufferedTerminalScreen)
    buf = IOBuffer()
    # For now use a cell to hold the attributestate
    attributestate = dummycell(:default,:default)
    full = isfullscreen(s)
    if !full
        # If our subscreen extends off the screen we may have to scroll first
        if s.offset.row + s.size.height > s.fullsize.height
            difference = s.offset.row + s.size.height-s.fullsize.height
            write(buf,CSI,string(difference),'S')
            s.offset = CellLoc(s.offset.row-difference,s.offset.col)
        end
    end
    change_fg_color(buf,attributestate)
    change_bg_color(buf,attributestate)
    # Make sure everything gets reset
    change_attrs(buf,0,0xFF)
    if isempty(s.have)
        s.have = emptybuffer(s)
        if full
            write(buf,CSI,'H',CSI,"2J")
        else
            do_clear_rect(buf,CellBox(s.offset,s.size))
        end
    end
    # Stage 1: Render images
    images_to_draw = Dict{Tuple{Int,Int},Int}()
    for (row,(havel,wantl)) in enumerate(zip(s.have,s.want))
        for (j,(havec,wantc)) in enumerate(zip(havel,wantl))
            if (wantc.flags & CELL_IS_IMG) == 0 ||
                havec == wantc
                continue
            end
            cur_pos = pos_for_image_cell(wantc)
            start_pos = (row-cur_pos[1]+1,j-cur_pos[2]+1)
            if !haskey(images_to_draw,start_pos)
                images_to_draw[start_pos] = wantc.content - 0xF0000
            end
        end
    end

    todelete = copy(s.dropped)
    for (pos,image) in images_to_draw
        # Drop this image from the todelete list if it was on there
        pop!(todelete, image, image)
        data, size = s.images[image]
        do_absmove(buf,s,pos...)
        display_file(buf, data, height = size.height, width = size.width, inline = true)
        #@show (pos,size)
        for row in pos[1]:(pos[1]+size.height-1)
            line = s.have[row]
            for col in pos[2]:(pos[2]+size.width-1)
                line[col] = get_image_cell(Cell(Char(image+0xF0000)),row-pos[1]+1,col-pos[2]+1)
            end
        end
    end
    for image in todelete
        s.images[image] = (Vector{UInt8}(),CellRect(0,0))
        pop!(s.dropped,image)
        push!(s.freelist,image)
    end

    # Stage 2: Every thing else
    for (row,(havel,wantl)) in enumerate(zip(s.have,s.want))
        didmoverow = false
        nerase = 0
        nmove = 0
        # Look through the cells
        for (j,(havec,wantc)) in enumerate(zip(havel,wantl))
            # Update the have buffer in place
            havel[j] = wantc
            # Now do the rendering
            if havec == wantc
                if nerase != 0
                    do_erase(buf, nerase)
                    nerase = 0
                end
                nmove += 1
                continue
            end
            # First we need to move to the right spot
            if nmove != 0
                if !didmoverow
                    do_absmove(buf,s,row,nmove+1)
                    didmoverow = true
                else
                    do_move(buf, nmove)
                end
                nmove = 0
            end
            if wantc.content == ' ' && same_attributes(wantc,attributestate)
                # This can be handled by an erase
                nerase += 1
                continue
            else
                @assert (wantc.flags & CELL_IS_IMG) == 0
                if !didmoverow
                    do_absmove(buf,s,row,1)
                    didmoverow = true
                end
                if nerase != 0
                    do_erase(buf, nerase)
                    nerase = 0
                end
                attributestate = render_cell(buf,wantc,attributestate)
            end
        end
    end
    change_attrs(buf,0,0xFF)
    buf
end

function afterembed(io::IO,s::DoubleBufferedTerminalScreen)
    # Go to the line after the embedding
    # However, if we are at the end, we might have to scroll a line
    if s.offset.row + s.size.height == s.fullsize.height
        write(io,CSI,"1S")
    end
    write(io,CSI,"$(s.offset.row + s.size.height);1H")
end

function render(io::IO,s::DoubleBufferedTerminalScreen)
    write(io,take!(render(s)))
end
