import Base.REPL: REPLDisplay
export Query, ask, form, answers, make_widget, on_done

immutable Query{T}
    w::Widget
end

function (::Type{Query{Int}})()
    t = TextInput(IOBuffer(copy(b"1234"),true,true))
    t.optheight = 1
    Query{Int}(t)
end

function on_done(f::Function,i::Query{Int})
    i.w.on_done = str->f(parse(Int,str))
end


function (::Type{Query{AbstractString}})()
    t = TextInput()
    t.optheight = 1
    Query{AbstractString}(t)
end
on_done(f::Function,i::Query{AbstractString}) = i.w.on_done = f

function display(d::REPLDisplay, w::Widget)
    wait(InlineDialog(w,d.repl.t))
end

immutable Question
    text::AbstractString
    input::Query
end

function ask(question, answerT)
    Question(question,Query{answerT}())
end

make_widget(i::Query) = i.w
function make_widget(q::Question)
    cd = TerminalUI.CellDisplay(
        Vector{VT100.Cell}[[
            VT100.Cell(TerminalUI.dummycell(:default,:default,0),content = '['),
            VT100.Cell(TerminalUI.dummycell(:green,:default,0),content = '?'),
            VT100.Cell(TerminalUI.dummycell(:default,:default,0),content = ']')]])

    cd2 = TerminalUI.CellDisplay(
        Vector{VT100.Cell}[[
        VT100.Cell(TerminalUI.dummycell(:default,:default,0),content = c)
        for c in q.text]])

    RowLayout([cd,cd2,q.input.w],[1,1,1],[4,strwidth(q.text)+1,0],3)
end

display(d::REPLDisplay, q::Question) = display(d,make_widget(q))

immutable Form
    questions
    chain::ScrollableChain
    keyidxs
    answers
end

function form(questions; style = :incremental)
    widgets = map(x->make_widget(x[2]),questions)
    if style == :incremental
        for i = 2:length(widgets)
            hide(widgets[i])
        end
    end
    f = Form(questions,ScrollableChain(widgets),
        Dict(k => i for (i,(k,_)) in enumerate(questions)),
        Dict{Symbol,Any}())
    for (k,q) in questions
        on_done(q.input) do v
            answer(f, k, v)
            focus_next(f.chain, show_invisible = true) || (invalidate(f.chain); return :done)
        end
    end
    f
end

display(d::REPLDisplay,f::Form) = display(d,f.chain)
FullScreenDialog(f::Form,tty) = FullScreenDialog(f.chain, tty)

function answer(form::Form, key, value)
    # First record the answer
    form.answers[key] = value
    # Create a widget to permanently display the answer
    str = sprint(print,value)
    cd = TerminalUI.CellDisplay(
        Vector{VT100.Cell}[[
        VT100.Cell(TerminalUI.dummycell(:cyan,:default,Bright),content = c)
        for c in str]])
    # Replace the question widget with an answer widget
    form.chain.children[form.keyidxs[key]].cols[3] = cd
end

answers(f::Form) = f.answers

make_widget(w::Widget) = w
make_widget(s::AbstractString) = Label(s)
function make_widget(a::Array)
    if ndims(a) == 1
        ScrollableChain(map(make_widget,a))
    elseif ndims(a) == 2
        if size(a)[1] > 1
            widgets = [make_widget(a[:,i]) for i = 1:size(a)[2]]
        else
            widgets = map(make_widget,vec(a))
        end
        RowLayout(widgets,[1 for _ in widgets],[0 for _ in widgets],length(widgets))
    else
        error("Higher dimensional widgets only supported for
        higher dimensional terminals")
    end
end
