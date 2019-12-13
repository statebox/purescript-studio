# brick diagrams

![](https://i.imgur.com/vNNzqbd.png)

wires 2:29 AM
the math is not that crazy, but it's a bit subtle, anyway it finds a decomposition of the grid on the left, and that is the tree on the right. this is a term of a type theory for moncat and right now i only count the wires, but you see the resulting type above the tree
right now it always splits horizontally first and then vertically, so it does not work for bricks that are oriented vertically

nigredo 2:31 AM
What is the role of constants and their types?

wires 2:31 AM
it's the bindings of the labels 1 2 3
of the 'bricks', so the type of the box inside that brick

![](https://i.imgur.com/yKrh4Tf.png)

nigredo 2:32 AM
Ok. And does the typechecker consider them for typechecking?

wires 2:32 AM
yes, but atm only the size not the types
but it is clear how to extend the type theory for that
so you could write `1: (typdefs type) => (typedefs type)`, e.g. `1: int*int -> bool+string`
actually it already check this, i just need to port it over from another file

nigredo 2:34 AM
This is nice
I think these diagrams will be great to represent executions, especially if we find a nice way to represent symmetries

wires 2:35 AM
oh and btw it is fully nestable AND you can navigate in the tree (i need to patch that up as well)

nigredo 2:35 AM
Yes, the nesting thing is a bit tricky, because I don't know exactly how to deal with operad nesting atm
So I'm working on adapting christina's stuff to automata
I've found out that the example they provide in the paper is quite ad hoc and they hid a lot of stuff under a lot of carpets, but it's not unfixable

wires 2:36 AM
yeah, and btw the symmetries sit nicely as "cement" boxes, I intend to implement this mode shift where you see the wires and use that for swaps etc

nigredo 2:36 AM
Yes

wires 2:36 AM
oh thats great

nigredo 2:36 AM
But the whole point is that the wiring diagrams are a bit more complicated
since they include feedback loops stuff
So, about this, i'd like to have a video chat at some point
What they really do is they say "A morphism from X to Y is basically a lens"

wires 2:37 AM
okay let's schedule i'm interested, this is also related to koko's work namely and goi

nigredo 2:38 AM
Precisely they say `X = (Xin Xout) Y = (Yin Yout)` and then they say a morphism `X -> Y` is the usual couple of functions `Xin -> Xout + Yin, Xout -> Yout`

This can basically be represented as a wiring diagram (operad style) but my conjecture is that if you choose other types of optics you can get different types of wirings
From my point of view I really don't give a flying fuck about how we wire things together, there's absolutely no difference from my perspective, but since lenses and optics are popular i'd like to keep using them for everyone's peace of mind

wires 2:40 AM
yeah there is not really a huge difference, but the optics part is more interesting beyond just the "hype", because nesting and scoping is a very natural thing to do

nigredo 2:40 AM
Also, for automata what we do is *really* easy: Last time we generated free categories from graphs. So you have basically a couple of functors Graphs <-> Free cats. What we are really doing is wiring diagrams of the underlying graphs of the free categories.

wires 2:41 AM
and function composition is just nesting of a singleton pair ( !! sorry), but graphically laid flat out horizontally or vertically

nigredo 2:41 AM
So imagine graphs with ports, which is literally graphs co-span style: Some vertexes are marked as inputs and some as outputs. Inputs and outputs can be wired together using wiring diagrams (hence nesting etc), and when you do this what you are actually doing is quotienting things together
but since our graphs are all finite you can do this quotienting without really quotienting, but inductively
So you do this thing and then you consider the free category on the resulting graph

wires 2:42 AM
@nigredo this is exactly what happens in stbx-core-js or the reasonml thing running on the api
This allows us to obtain the executions of the "completely wired automaton" without really doing any quotient whatsoever

nigredo 2:42 AM
but since our graphs are all finite you can do this quotienting without really quotienting, but inductively

nigredo 2:42 AM
Exactly, I guessed in the end it would have been something similar
Also this connects with some stuff Ryan was saying ages ago, namely "In CQL we fiddle with presentations of categories and not with categories directly"
This is exactly the same, morally

wires 2:44 AM
yeah so i just represent the resulting quotient thing to the user and then i map back and forth between _that_ and (the original inductive defition and the decomposed set states, one for each net)

nigredo 2:44 AM
Yes, exactly
Also, conjecture: I am pretty sure we can do the same for petri nets, using hypegraphs. The problem is that we'll need probably HoTT to build executions from hypergraphs

wires 2:45 AM
okay I'm nice that you come to the same conclusions haha (so you can do category theory in javascript)

nigredo 2:45 AM
But, think about it this way: A finite state machine is basically a graph that you are interpreting as a machine
similarly, a Petri net is an hypergraph that you are interpreting as a machine

wires 2:45 AM
i mean. it kind of already works for petri nets in general, not just for workflow

nigredo 2:45 AM
So graphs/hypegraphs are the underlying topological structures of these models of computation
And it is these topological structure that are actually important to generate executions, the net/automaton itself is just a glorified version of the topology with some extra metadata on it
