# Introduction to modern React and Reactix

Prerequisite knowledge:

* Javascript: some, ideally some react.
* Purescript: basics, typeclasses, row types, monads (including `do`)

## Background

React is a component-based user interface library for the web written
in javascript. It provides a declarative API for building a Single
Page (web) Application (SPA). React is the primary way we handle user
interfaces in the `purescript-gargantext` codebase.

React exposes two APIs:
* The traditional `object` component model, where each component is a class
* The new `hooks` model where each component is a function that follows some rules.

Almost all of our UI code is written with the `Reactix` purescript
library, a quite simple wrapper over the `hooks` api only. This is a
pragmatic choice: we believe that it results in simpler, more
understandable purescript code that is just more fun to maintain.

This choice is not without downside, in particular some of those rules
that components must follow *cannot be automatically enforced by the
compiler for you*. Thus we lose one of the benefits a lot of people
associate with languages with good type systems - "if it compiles, it
will work".

To put it another way, you are essentially left with only the
guarantees react provides - that it will work if you follow the rules,
*some of which you will have to enforce yourself*. To some degree,
it's *as if you're writing javascript with a type system*.

We appreciate some people might be uncomfortable with this tradeoff,
but on the whole we think it's worked out pretty well for our needs.

## Let's learn Reactix!

Let's start off by looking at some simple reactix code. It's kinda
like writing html in purescript:

```purescript
import Reactix as R
import Reactix.DOM.HTML as H

-- Rendering html elements is easy:

helloWorld :: R.Element -- <div class="hello">Hello, world!</div>
helloWorld = H.div { className: "hello" } [ H.text "Hello, world" ]

-- We can write functions for abstraction:

greet :: String -> String -> R.Element
greet className greeting = H.div { className } [ H.text greeting ]

-- The same as before, written differently.
helloWorld' :: R.Element
helloWorld' = greet "hello" "Hello, world!"

-- Much like html, some elements don't take children.
img :: R.Element
img = H.img { src: "kitten.png" }
```

You can see the general pattern: call the appropriate function passing
in your choice of attributes and children (where supported).

You may notice the `class` attribute in html is spelled `className` in
the example. While *most* attribute names are the same as the DOM
properties, react uses the DOM API convention, meaning sometimes the name
changes. Full details [here](https://reactjs.org/docs/dom-elements.html).

### Our first component

Another option for abstracting markup is to write a component. Using
components is much like using the html functions from the last
section: we will pass it properties and (if appropriate) children.

Let's rework our example from before to be a component!

```purescript
import Reactix as R
import Reactix.DOM.HTML as H

-- We will accept these properties

type Greet = ( className :: String, greeting :: String )
  
-- Our constructor will use React's `createElement` to instantiate
-- our component with the provided properties. It will not take children.

greet :: Record Greet -> R.Element
greet props = R.createElement greetCpt props []

-- We use it similarly to html functions!
helloWorld :: R.Element
helloWorld = greet { className: "hello", greeting: "Hello, world!" }

-- Now the component itself!

greetCpt :: R.Component Greet
greetCpt = R.hooksComponent "greet" cpt where
  cpt { className, greeting } _children =
    pure $ H.div { className } [ H.text greeting ]
```

There are three important things going on in the component definition:

* The component has a name, `greet`. In debug builds, This will appear
  in the React devtools for your browser if you have them installed.
* The component is based around a function that takes the properties
  and any children.
* We have added a `pure`, because the body of a component function is
  monadic (specifically the `Reactix.Hooks` monad).

While an appropriate name in the react devtools is not to be sniffed
at, our last example really only scratches the surface of
components. At risk of opening pandora's box, we can do *much* more!

### Adding interactivity: our first hook

So far, we've made mild improvements over basic html. Since it's a bit
dumb to force purescript on someone for something html could do, we
should probably do something it couldn't: interaction!

We will make a component that simply counts how many times a button
has been clicked.

```purescript
import Data.Tuple.Nested ( (/\) )
import Reactix as R
import Reactix.DOM.HTML as H

helloCounter :: R.Element
helloCounter = counter { initialCount: 0 }

type Counter = ( initialCount :: Int )

counter :: Record Counter -> R.Element
counter props = R.createElement counterCpt props []

counterCpt :: R.Component Counter
counterCpt = R.hooksComponent "counter" cpt where
  cpt { initialCount } _children = do
    (count /\ setCount) <- R.useState' initialCount
    pure $ H.div {}
      [ H.button { on: { click: \_event -> setCount (_ + 1) } }
        [ H.text "Don't click me" ]
      , H.text ("Clicked " <> show count <> " times!") ]
```

There's a bit more going on in the component this time:

* The `R.useState'` hook is called with the `initialCount`.
* The resulting tuple is unpacked into a value and a setter.
* We render a button with a click handler.
* The click handler increments the count. Event handlers have a
  general type: `event -> Effect Unit`.
* We display how many times it has been clicked.

How all of that actually happens under the hood is a little
complicated, but let's pick off the easy bits.

The `on` property is treated specially by the html constructors - it
plumbs purescript functions into being and react event handlers. In
this case, the `click` entry becomes an `onClick` handler. We ignore
the provided event and call the setter function with an increment (to
be applied to the current value).

All of this is interesting, but it doesn't fundamentally explain why
it works. Unfortunately, that's quite complicated...

### Secrets of `createElement`

The true type of` createElement` is a bit difficult to read, but you've
seen from usage that it's essentially:

`createElement :: component -> Record props -> Array R.Element -> R.Element`

One thing you may immediately notice is that it's a pure function - it
doesn't live in a monad. In fact, when we render some HTML, we aren't
rendering a DOM at all - we're rendering a *virtual DOM* - a tree of
DOM-like data objects. At the appropriate time, React will `mount`
these - turn them into real DOM elements in the document.

But if everything is pure and declarative, how could hooks - *a way of
having side-effects* - work at all?! The 'trick' relies on two things:

1. Component rendering must produce an `Element`.
2. The only way to do that is by calling `createElement` (possibly
   indirectly through a wrapper function).

This isn't much structure, but it's enough to impose a lifecycle upon
component instances. With a lifecycle, we can meaningfully have
*instances* of a component. With instances, we can meaningfully have
*per-instance state*. With per-instance state, we have interactivity!

### Per-instance state: the root of interactivity

Let's take our `counter` component from earlier as an example. Here's
the constructor function and how we use it:

```purescript
counter :: Record Counter -> R.Element
counter props = R.createElement counterCpt props []

helloCounter :: R.Element
helloCounter = counter { initialCount: 0 }
```

Straight away we see `counter` calling the crucial `createElement`
function. In `helloCounter`, we pass it the sensible initial count
of 0. Now let's look at the component itself again:

```purescript
counterCpt :: R.Component Counter
counterCpt = R.hooksComponent "counter" cpt where
  cpt { initialCount } _children = do
    (count /\ setCount) <- R.useState' initialCount
    pure $ H.div {}
      [ H.button { on: { click: \_event -> setCount (_ + 1) } }
        [ H.text "Don't click me" ]
      , H.text ("Clicked " <> show count <> " times!") ]
```

We know from before that the `initialCount` we are passing to
`useState'` is zero. Here is the type of `useState'`:

```purescript
type State state = Tuple state ((state -> state) -> Effect Unit)

useState' :: forall s. s -> Hooks (State s)
```

`Hooks` is the Monad used in components. Only in `Hooks` are we able
to use, er... hooks. `useState'` returns (in `Hooks`) a `Tuple` of a
value and a setter function. The setter function is structured such
that you provide a function which receives the current value and must
return a new value. So it's kind of a weird way of having a variable
in the scope of the component instance.

During the initial render, the `count` returned from `useState'` will
be zero, because that is the value we provided it. If we never clicked
the button, that would be the end of the story.

Of course you want to click the button. And when you do that, a whole
chain of events sets in motion:

* The`click` handler is called, which...
* Calls the setter function, which...
* Increments the counter state, which...
* Makes react rerender the component, which...
* Calls the render function again, BUT...
* The `count` returned by `useState'` will be 1 this time!
* A new tree of elements is produced with the correct count.

So not only does `useState'` give us a "variable", it makes sure that
when it's updated, the component is rerendered! 

If you're wondering why `useState'` has a `'` (pronounced "prime") in
its name, this is a common haskell convention. Appending prime to a
name indicates that it is a modified version of the non-prime
version.

`useState` indeed exists and is much like `useState'`, but where that
takes an initial value, `useState` takes a function to calculate an
initial value. In our case, we have the value already, so `useState'`.

### Going in circles: rerendering by changing props

We have already explored one way of getting a component to rerender in
response to the world - the `useState'` hook. Another way is to change
the properties we call it with. Let's separate our last example out
into three components so we can demonstrate:

```purescript
import Reactix as R
import Reactix.DOM.HTML as H

helloCounter :: R.Element
helloCounter = counter { initialCount: 1 }

type Counter = ( initialCount :: Int )

counter :: Record Counter -> R.Element
counter props = R.createElement counterCpt props []

counterCpt :: R.Component Counter
counterCpt = R.hooksComponent "counter" cpt where
  cpt { initialCount } _children = do
    (count /\ setCount) <- R.useState' initialCount
    pure $ H.div {}
      [ button { setter: setCount }
      , clicks { clicks: count } ]

type Button = ( setter :: R.Setter Int )

button :: Record Button -> R.Element
button props = R.createElement buttonCpt props []

buttonCpt :: R.Component Button
buttonCpt = R.hooksComponent "button" cpt where
  cpt { setter } _children =
    pure $ H.button { on: { click: \_event -> setter (_ + 1) } }
      [ H.text "Don't click me" ]

type Clicks = ( clicks :: Int )

clicks :: Record Clicks -> R.Element
clicks props = R.createElement clicksCpt props []

clicksCpt :: R.Component Clicks
clicksCpt = R.hooksComponent "clicks" cpt where
  cpt { clicks } _children =
    pure $ H.text $ "Clicked " <> show clicks <> " times"
```

In our new `counter`, the `useState'` hook works exactly as before,
but this time, we render our new `button` and `clicks` components. Now
when we click the button, *almost* the same sequence of events as
before happens. This time though, the last step is replaced with more
steps:

* We pass 1 to `clicks`, which...
* Causes react to notice the properties have changed, which...
* Triggers a rerender of clicks, which...
* Renders an updated text element

*phew*. That sounds like a lot to do a simple thing! Perhaps a better
way to think about it is "look at all the stuff react has done for
us!". Developing GUI software has always involved a lot of
complication, at least react will help us with some of it

### All good things must come to an end

By changing just the `counterCpt` from the last example, we can
demonstrate the final stage of the lifecycle - unmounting.

```purescript
counterCpt :: R.Component Counter
counterCpt = R.hooksComponent "counter" cpt where
  cpt { initialCount } _children = do
    count <- R.useState' initialCount
    pure $ H.div {} (children count)
  children (count /\ setCount) =
    if count < 5 then
      [ button { setter: setCount }
      , clicks { clicks: count } ]
    else [ H.text "Enough." ]
```

The first 4 clicks work much as before here, but now when you click
for the fifth time, the structure of the returned elements changes
completely! What does that mean for our lifecycle?

When an element wrapping one component is replaced by an element
wrapping a different component, it triggers the unmounting of the
existing component instance. Aside from ultimately removing the
element from the document, hooks-related cleanup may be performed. In
the case of state, this just frees up the associated storage.

A hook that demonstrates this better is `useEffect` - a hook for
executing effectful code (i.e. code in `Effect`):

```purescript
import DOM.Simple.Console (log)

lifecycleCpt :: R.Component ()
lifecycleCpt = R.hooksComponent "lifecycle" cpt where
  cpt _props _children = do
    R.useEffect $ do
      log "Component mounted!"
      pure $ log "Component unmounting!"
    pure $ H.div {} []
```

As you can see, we don't produce any useful html output here, but we
do print to the browser console twice: on mounting and on unmounting.

The trick to this lies in the type of `useEffect`:

`useEffect :: Effect (Effect Unit) -> Hooks Unit`

The actual `Effect` type is implemented in purescript as a 0-arity
javascript function, thus it represents the *delaying* of performing
the associated effect until the correct moment. This is why we are
able to compose `Effect`s with pure functions.

The contract of `useEffect` is that the provided `Effect` is executed
when the component has *mounted* and the `Effect` that it returns is
not executed until the component will *unmount*. So with one hook we
can both initialise and clean up after ourselves. Neat!

And with that, we've covered the entire lifecycle:

* render - for the first time
* mount - when the virtual DOM becomes the real DOM.
* rerender - when something has changed
* unmount - cleanup and remove from the DOM.

And all of that controlled by just a render function!

## Handling state with Toestand

Dealing with state storage in React can seem a bit daunting at
first. There are too many options and it's not always obvious which
you need. On top of that, if you change later, you have to rewrite a
lot of code.

Toestand is a new purescript library built on top of Reactix. It aims
to provide one extremely flexible state type, `Toestand.Box a` that is
suitable for the majority of usecases.

Passed as a property, a `Box` is like a Reactix Ref - irrelevant for
prop diffing purposes. Changing the value stored in the box does not
cause a rerender by itself.

With the `Toestand.useLive` hook, a component can opt in to rerender
when the value changes (like a `State`, but sharing the same
type!). While most of the time you will want to check equality, you
can provide a custom predicate and only rerender when you want to.

### My first box

Let's pick up our counter from earlier and rewrite it to use
Toestand. Not much code actually changes, it's pretty similar to
before:

```purescript
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

counterCpt :: R.Component Counter
counterCpt = R.hooksComponent "counter" cpt where
  cpt { initialCount } _children = do
    -- Create the box with the initial value
    box <- T.useBox initialCount
    -- Subscribe to a live count when it changes
    count <- T.useLive T.unequal box
    pure $ H.div {}
      [ button { box }
      , clicks { clicks: count } ]

-- The button now takes a box instead of a setter function.
type Button = ( setter :: T.Box Int )

buttonCpt :: R.Component Button
buttonCpt = R.hooksComponent "button" cpt where
  cpt { box } _children =
    pure $ H.button { on: { click } } [ H.text "Don't click me" ] where
      -- Increment the value in the box
      click _event = T.modify_ (_ + 1) } box

-- Everything else is identical and here for completeness.

helloCounter :: R.Element
helloCounter = counter { initialCount: 1 }

type Counter = ( initialCount :: Int )

counter :: Record Counter -> R.Element
counter props = R.createElement counterCpt props []

button :: Record Button -> R.Element
button props = R.createElement buttonCpt props []

type Clicks = ( clicks :: Int )

clicks :: Record Clicks -> R.Element
clicks props = R.createElement clicksCpt props []

clicksCpt :: R.Component Clicks
clicksCpt = R.hooksComponent "clicks" cpt where
  cpt { clicks } _children =
    pure $ H.text $ "Clicked " <> show clicks <> " times"
```

### Core Toestand API: a closer look

`useBox` has a fairly straightforward type signature:

```purescript
-- Creates a new Box inside a component from an initial value
useBox :: forall b. b -> R.Hooks (Box b)
```

`modify_` is slightly more complicated. `ReadWrite` is a typeclass
that `Box` implements. This constraint is basically saying a `v` can
be read from and written to a `box`. So given a function that takes
and returns a value and a box, modify the value by applying that
function to the current value of the box:

```purescript
modify_ :: forall box v. ReadWrite box v => (v -> v) -> box -> Effect Unit
```

As you might have guessed, `modify` exists too, and returns the newly set value:

```purescript
modify :: forall c v. ReadWrite c v => (v -> v) -> c -> Effect v
```

The `ReadWrite` class is actually methodless, it is a shorthand way of
referring to both the `Read` and `Write` classes:

```purescript
class (Read box val, Write box val) <= ReadWrite box val | box -> val

instance readWrite :: (Read box val, Write box val) => ReadWrite box val
```

When you don't care about the current value, you can use `write`, the
singular method of the `Write` typeclass. Its effective type is
similar to `modify` but simpler, reflecting our lack of need to read
from it:

```purescript
write :: forall box v. Write box v => v -> box -> Effect v
```

There is also the corresponding `write_` for when you want a Unit return:


```purescript
write_ :: forall box v. Write box v => v -> box -> Effect Unit
```

`read` is the most important method of the `Read` typeclass. Its effective type is:

```purescript
read :: forall box v m. Read box v => MonadDelay m => box -> m v
```

`MonadDelay` is implemented by two types we already know:

* `Effect`
* `Hooks`

This means reading can be done in either monad.

### Live updates

The astute reader may notice that `useLive` was used in our first
example but not covered in our last section. Explaining it will take a
little longer...

Boxes have another functionality: a hook to call a function when the
value is written. Registering one of these is the purpose of the other
function in the `Read` typeclass, `listen`, which has the following
effective type:

```purescript
listen :: forall box v. Read box v => Listener v -> box -> Effect (Effect Unit)
```

We'll take that in two bites, first, the `Listener`:

```purescript
forall box v. Read box v => Listener v
```

Here's how `Listener` (and the `Change` that it mentions) are defined:


```purescript
-- | A summary of a change in value
type Change c = { new :: c, old :: c }

-- | An Effect function which is provided the new and old values.
type Listener c = Change c -> Effect Unit
```

So to listen, we provide an effectful `Listener`, which receives the
new and old values. Whenever someone calls `write` (or a function that
wraps it) on a `Box`, our callback will be executed.

The type of `listen` ends thus:

```purescript
Effect (Effect Unit)
```

Remember that an `Effect` is internally a 0-arity function used to delay the execution of some code.

The inner `Effect Unit` is a means of cancelling the subscription we
established. The outer `Effect` is used to return it without executing
it.

So, you provide a listener (which can execute effects) and you get
back a means of cancelling when you no longer need to listen. Neat!

Now we just need one more type before we can look at `useLive`:

```purescript
-- | An effect function which determines whether notifications should be sent.
type ShouldReload c = Change c -> Effect Boolean
```

Toestand ships with just one function of this type, `unequal`, which
does what you'd expect (i.e. it's `Prelude.notEq`, but in Effect):

```purescript
unequal :: forall v. Eq v => Change v -> Effect Boolean
```

And finally, we're ready to study `useLive`:

```purescript
useLive :: forall box b. Read box b => ShouldReload b -> box -> R.Hooks b
```

Wondering how it works?

* It uses `useState` to create a counter. 
* It registers a listener with `listen` to hear when writes are performed.
* When a write is performed, the `ShouldReload` callback is executed.
* If it returns true, the counter.

Once you know how it works, it's not actually so mysterious :)

The nice thing about `useState` is it pushes the choice about whether
to refresh to the component that uses it. Because it even allows you
to customise the logic, it is incredibly flexible.

### Focused boxes and the single source of truth

By now, we hope you think Toestand is as cool as we do. But it's not done yet!

Sometimes, you'd like to have a box containing a data structure of
state (say a record, for example) and only pass a part of it on to a
child component, as another Box.

That was a bit of a mouthful, let's look at an example:

```purescript
use Reactix as R
use Toestand as T

type Bigger = ( count :: Int, start :: Int )

useCountBox :: Box (Record Bigger) -> R.Hooks (T.Box Int)
useCountBox box = R.useFocused reader writer box where
  reader :: Record Bigger -> Int
  reader {count} = count
  writer :: Int -> Record Bigger -> Record Bigger
  writer count old = old { count = count }
```

We have overannotated the types to be clearer here. `reader` is a
function that can look up `count` in a `Bigger` record and return
it. `write` is a function that can set a new `count` in a `Bigger`.

The `Box` that `useCountBox` takes is linked to the new `Box` it
returns. When the value inside the original `Box` changes, the value
inside the focused box may also appear to be changed, depending on the
read function. You can even write to the returned Box and have it
update the original, you just have to pass the right writer function!

If you are a haskell programmer, you may recognise the reader and
writer together as being a van Laarhoven-style lens, as used by most
of the haskell lens libraries. Indeed it is, but this is as complex as
ours get - no prisms or anything fancy.

The particular case of turning a `Box (Record a)` into `focused field
boxes` is in fact so common that we ship it in Toestand as
`useFocusedFields`:

```purescript
import Toestand as T

type Bigger = ( count :: Int, start :: Int )
type Smaller = ( count :: T.Box Int, start :: T.Box Int )

useSmaller :: T.Box (Record Bigger) -> R.Hooks (Record Smaller)
useSmaller box = T.useFocusedFields box {}
```

The final argument, {} is the base record to add the cursors to.

## Advanced Reactix

<!-- ### Reactix is a bad purescript library -->

<!-- Reactix is one of those libraries that was written because we needed -->
<!-- it and that would be great if it got all the time and attention other -->
<!-- things get. -->

<!-- This isn't to say Reactix sucks, on the contrary we're generally quite -->
<!-- fond of it, but using it effectively means understanding its -->
<!-- limitations and figuring out how to make peace with its requirements -->
<!-- of you. -->

<!-- Firstly, it's not finished. It was hastily written and we've really -->
<!-- only maintained it as much as we need to and no more. -->

<!-- In attempting to be a lightweight wrapper over modern React, Reactix -->
<!-- does not always behave the way you might expect a purescript library -->
<!-- to. In -->


<!-- ### Limitations of Reactix -->

<!-- Reactix is meant to be a relatively barebones binding to modern -->
<!-- React. Where purescript and react differ, react wins. -->

<!-- What do I mean by this? The `Hooks` monad is literally a newtype over -->
<!-- `Effect`. We take a very liberal view of this - when you are writing -->
<!-- code in `Hooks`, you are choosing to invert control to React and its -->
<!-- way of doing things. -->

<!-- It's not necessarily against the rules of purescript to do this, but -->
<!-- it does mean that some of the properties we often take for granted in -->
<!-- writing purescript code do not necessarily apply. -->


### Reconciliation and the rules of hooks

So far, we've largely glossed over the part that links the virtual DOM
with the real DOM. This is react's [reconciliation algorithm.](https://reactjs.org/docs/reconciliation.html).

This is probably a good time to mention one of the major current
limitations of Reactix: the properties provided to html constructor
functions are not (yet) properly type checked. Be careful!

The [rules of hooks](https://reactjs.org/docs/hooks-rules.html) are
the fundamental set of rules that dictate what we're allowed to do in
our components. There are only two of them!

Nonetheless, i think it still takes experience to understand the full
meaning of these rules, so let's go our own way, dear comrade in code!

#### New rule: always execute the same hooks

Rerendering the same component should cause the same sequence of hook
calls to execute. That is, you should:

* Call the same hooks
* In the same order
* With the same arguments

That sounds pretty inflexible, and it is! We have to change the way we
work to meet this requirement.

### The unstated rule: Unless your hands are tied

We hope you never need to venture down the path of the fool, but
sometimes you might not have a choice. In any case, understanding what
happens if you do is quite enlightening, so read on.


Let's look at some of the ways I found us already getting it wrong during a refactor in early 2021!


### Case studies

#### Getting it wrong - conditional hook invocation

Here is an egregious example of hiding hook calls behind branches:

```purescript
nodeActionsCpt :: R.Component NodeActionsProps
nodeActionsCpt = here.component "nodeActions" cpt where
  cpt { id, session, triggerRefresh, nodeType: GT.Graph } _ =
    useLoader id (graphVersions session) $ \gv ->
      nodeActionsGraph { graphVersions: gv, session, id, triggerRefresh }
  cpt { id, nodeType: GT.NodeList, session, triggerRefresh } _ =
    useLoader { nodeId: id, session } loadCorpusWithChild $ \{ corpusId } ->
      nodeActionsNodeList
      { listId: id, nodeId: corpusId, session, triggerRefresh
      , nodeType: GT.TabNgramType GT.CTabTerms }
  cpt _ _ = pure $ H.div {} []

  graphVersions session graphId = GraphAPI.graphVersions { graphId, session }
```

Depending on the type of the component, one of these happens:

* `useLoader` is called with one set of arguments.
* `useLoader` is called with another set of arguments.
* No hooks are called at all.

I love this example! Reasons include:

* It clearly breaks the rules.
* It appears to work correctly in production.
* It contains a subtle bug that's difficult to diagnose.

#### Analysis

The reason this appears to work correctly is that the type of a node
will not (typically) change once it has been created. You could be
forgiven for thinking therefore that the code works correctly.

So when does it not work? Imagine you want to render this component
many times because you have a list of them. What happens if you delete
one or rearrange them? According to the
[holy book](https://reactjs.org/docs/reconciliation.html#component-elements-of-the-same-type):

> When a component updates, the instance stays the same, so that state
> is maintained across renders.

Oh dear. The type of the node being rendered *could* change after all!

#### Resolution

In this particular case, a simple fix will do the job - adding a
unique 'key' property as mentioned in next section of the the same
[reconciliation docs](https://reactjs.org/docs/reconciliation.html#keys).

While this is probably enough to fix the problem (and is recommended
for performance), we can do better! Before applying it, I:

* Extracted the `useLoader` calls into new components.
* Rendered a different component depending on the type.

The new component does not use any hooks at all. No hooks, no problem!

```purescript
nodeActionsCpt :: R.Component NodeActionsProps
nodeActionsCpt = here.component "nodeActions" cpt where
  cpt props _ = pure (child props.nodeType) where
    nodeActionsP = SProxy :: SProxy "nodeType"
    childProps = Record.delete nodeActionsP props
    child GT.NodeList = listNodeActions childProps
    child GT.Graph = graphNodeActions childProps
    child _ = H.div {} []
```

And the two new components should look pretty familiar:

```purescript
graphNodeActionsCpt :: R.Component NodeActionsCommon
graphNodeActionsCpt = here.component "graphNodeActions" cpt where
  cpt { id, session, triggerRefresh } _ =
    useLoader id (graphVersions session) $ \gv ->
      nodeActionsGraph { graphVersions: gv, session, id, triggerRefresh }
  graphVersions session graphId = GraphAPI.graphVersions { graphId, session }

listNodeActionsCpt :: R.Component NodeActionsCommon
listNodeActionsCpt = here.component "listNodeActions" cpt where
  cpt { id, session, triggerRefresh } _ =
    useLoader { nodeId: id, session } loadCorpusWithChild $ \{ corpusId } ->
      nodeActionsNodeList
      { listId: id, nodeId: corpusId, session, triggerRefresh
      , nodeType: GT.TabNgramType GT.CTabTerms }
```

These simple components clearly obey the rules of hooks. We should
still apply the 'key' property to improve performance though.
