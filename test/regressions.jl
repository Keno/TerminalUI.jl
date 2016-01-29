using TerminalUI; using VT100

const thisdir = dirname(@__FILE__)

function test_against(em,file)
    output = open(readbytes,joinpath(thisdir,file))
    buf = IOBuffer()
    VT100.dump(buf,DevNull,em)
    outbuf = takebuf_array(buf)
    result = outbuf == output
    if !result
        println("Test failed. Actual result written to failed.out")
        open("failed.out","w") do f
            write(f,outbuf)
        end
    end
    return result
end

cells = Vector{Cell}[
  [Cell(TerminalUI.dummycell(:blue,:red); content = 'x') for i = 1:5]
for i = 1:5];
w = TerminalUI.CellDisplay(cells);
em = TerminalUI.debug(w)
@test test_against(em,"outputs/cellblock.out")
