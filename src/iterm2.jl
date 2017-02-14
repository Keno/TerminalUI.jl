# This code is duplicated from TerminalExtensions
# Maybe they will be reunified in the future

function prepare_display_file(buf;filename="Unnamed file", size=nothing, width=nothing, height=nothing, preserveAspectRatio::Bool=true, inline::Bool=false)
    q = "\e]1337;File="
    options = String[]
    filename != "Unnamed file" && push!(options,"name=" * base64encode(filename))
    size !== nothing && push!(options,"size=" * dec(size))
    height !== nothing && push!(options,"height=" * dec(height))
    width !== nothing && push!(options,"width=" * dec(width))
    preserveAspectRatio !== true && push!(options,"preserveAspectRatio=0")
    inline !== false && push!(options,"inline=1")
    q *= join(options,';')
    q *= ":"
    write(buf,q)
end

function display_file(buf,data::Vector{UInt8}; kwargs...)
    prepare_display_file(buf;kwargs...)
    write(buf,base64encode(data))
    write(buf,'\a')
end

#=
# Incomplete list. Will be extended as necessity comes up
const iterm2_mimes = ["image/png", "image/gif", "image/jpeg", "application/pdf", "application/eps"]

for mime in iterm2_mimes
    @eval begin
        function display(d::InlineDisplay, m::MIME{symbol($mime)}, x)
            prepare_display_file(;filename="image",inline=true)
            buf = IOBuffer()
            pipe = Base64EncodePipe(buf)
            writemime(pipe,m,x)
            close(pipe)
            write(STDOUT, take!(buf))
            write(STDOUT,'\a')
        end
    end
end
=#
