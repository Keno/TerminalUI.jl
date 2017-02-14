using TerminalUI; using VT100

const thisdir = dirname(@__FILE__)

include(Pkg.dir("VT100","test","TerminalRegressionTests.jl"))
using TerminalRegressionTests: load_outputs, compare

function do_compare(em, i, s, output, decorator)
    TerminalUI.redraw(s, i.w)
    isa(output, String) && (output = Vector{UInt8}(output))
    isa(decorator, String) && (decorator = Vector{UInt8}(decorator))

    outbuf = IOBuffer()
    TerminalUI.render(outbuf, s)
    seekstart(outbuf); VT100.parseall!(em, outbuf)
    compare(em, output, decorator)
end

function test_against(em,file)
    output = open(read,joinpath(thisdir,file))
    compare(em, output)
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
