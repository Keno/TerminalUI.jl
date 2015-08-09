import Base.REPL: REPLDisplay
export Query, ask, form

immutable Query{T}
    w::Widget
end

function call(::Type{Query{Int}})
    t = TextInput(IOBuffer(copy(b"1234"),true,true))
    t.optheight = 1
    Query{Int}(t)
end

function on_done(f::Function,i::Query{Int})
    i.w.on_done = str->f(parse(Int,str))
end


function call(::Type{Query{String}})
    t = TextInput()
    t.optheight = 1
    Query{String}(t)
end
on_done(f::Function,i::Query{String}) = i.w.on_done = f

function display(d::REPLDisplay, w::Widget)
    wait(InlineDialog(w,d.repl.t))
end

immutable Question
    text::String
    input::Query
end

function ask(question, answerT)
    Question(question,Query{answerT}())
end

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
    f = Form(questions,ScrollableChain(widgets),
        [k => i for (i,(k,_)) in enumerate(questions)],
        Dict{Symbol,Any}())
    for (k,q) in questions
        on_done(q.input) do v
            answer(f, k, v)
            focus_next(f.chain)
        end
    end
    f
end

display(d::REPLDisplay,f::Form) = display(d,f.chain)

function answer(form::Form, key, value)
    # First record the answer
    form.answers[:key] = value
    # Create a widget to permanently display the answer
    str = sprint(print,value)
    cd = TerminalUI.CellDisplay(
        Vector{VT100.Cell}[[
        VT100.Cell(TerminalUI.dummycell(:cyan,:default,Bright),content = c)
        for c in str]])
    # Replace the question widget with an answer widget
    form.chain.children[form.keyidxs[key]].cols[3] = cd
end
