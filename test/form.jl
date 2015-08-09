using TerminalUI

form(Pair[
  :name => ask("Enter your name:", String),
  :address => ask("Enter your address:", String),
  :age => ask("Enter your age:", Int)
],style = :plain)
