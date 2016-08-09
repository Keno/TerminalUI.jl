using TerminalUI; using VT100

const thisdir = dirname(@__FILE__)

function _compare(output, outbuf)
    result = outbuf == output
    if !result
        println("Test failed. Expected result written to expected.out,
            actual result written to failed.out")
        open("failed.out","w") do f
            write(f,outbuf)
        end
        open("expected.out","w") do f
            write(f,output)
        end
    end
    return result
end

function compare(em, output, decorator = nothing)
    buf = IOBuffer()
    decoratorbuf = IOBuffer()
    VT100.dump(buf,decoratorbuf,em)
    outbuf = takebuf_array(buf)
    decoratorbuf = takebuf_array(decoratorbuf)
    println(String(decoratorbuf))
    _compare(output, outbuf) || return false
    decorator === nothing && return true
    _compare(decorator, decoratorbuf)
end

function test_against(em,file)
    output = open(read,joinpath(thisdir,file))
    compare(em, output)
end

function load_outputs(file)
    outputs = String[]
    decorators = String[]
    is_output = true
    for line in readlines(file)
        if line[1] == '+'
            push!(outputs,"")
            is_output = true
            continue
        elseif line[1] == '-'
            push!(decorators,"")
            is_output = false
            continue
        elseif line[1] == '|'
            array = is_output ? outputs : decorators
            array[end] = string(array[end],isempty(array[end])?"":"\n",line[2:end-1])
        else
            error("Unrecognized first character \"$(line[1])\"")
        end
    end
    outputs, decorators
end

function do_compare(em, i, s, output, decorator)
    TerminalUI.redraw(s, i.w)
    isa(output, String) && (output = output.data)
    isa(decorator, String) && (decorator = decorator.data)

    outbuf = IOBuffer()
    TerminalUI.render(outbuf, s)
    seekstart(outbuf); VT100.parseall!(em, outbuf)
    compare(em, output, decorator)
end

function interact(w, inputs, file)
    outputs, decorators = load_outputs(joinpath(thisdir,file))

    outbuf = IOBuffer()
    i = TerminalUI.InlineDialog(w,
        TerminalUI.IOBufferTerminal(outbuf))
    s = TerminalUI.setup_screen(i)
    s.offset = (1,1)
    s.fullsize = Base.displaysize(i.tty)
    em = VT100.ScreenEmulator()
    TerminalUI.focus(i.w)

    (length(decorators) < length(outputs)) &&
        (decorators = Any[decorators; Vector{Void}(length(outputs))])
    @test do_compare(em, i, s, outputs[1], decorators[1])
    @assert length(outputs) >= length(inputs)+1
    # Run over inputs
    for (input, output, decorator) in zip(inputs, outputs[2:end], decorators[2:end])
        inbuf = IOBuffer(input)
        seekstart(inbuf)
        result = :ok
        while result != :done && !eof(inbuf)
            result = Base.LineEdit.match_input(i.w.ctx.focuss.keymap,nothing,inbuf)(s,nothing)
        end
        @test do_compare(em, i, s, output, decorator)
    end
end

# Test cellblock
let
    cells = Vector{Cell}[
      [Cell(TerminalUI.dummycell(:blue,:red); content = 'x') for i = 1:5]
    for i = 1:5];
    w = TerminalUI.CellDisplay(cells);
    em = TerminalUI.debug(w, false)
    @test test_against(em,"outputs/cellblock.out")
end

# Test simple form
let
    myform = form(Pair[
         :name => ask("Enter your name:", AbstractString),
         :address => ask("Enter your address:", AbstractString),
         :age => ask("Enter your age:", Int)
       ],style = :incremental);
    interact(myform.chain, ["The"," Wizard\n","Oz\n","10"], "outputs/form.multiout")
end

# Test simple list
let
    fruit = ["Apple","Pear","Kiwi"]
    w = ListWidget(fruit)
    interact(w, ["\e[B","\e[B"], "outputs/list.multiout")
end
