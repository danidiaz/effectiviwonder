# effectiviwonder

[Faemino y Cansado - Excursión a Segovia](https://www.youtube.com/watch?v=Iov8_6a46Fg).

An experiment with the [ReaderT
design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern) and
[extensible records](http://hackage.haskell.org/package/red-black-record).

The capabilities are named. 

See the "tests.hs" file in the repo for some exmaples.

## How to add a new capability?

3 steps:

- Create a record-of-functions parameterized by a monad.
- Create some helper methods that can be called directly on a ReaderT.
- Create one or more implementations that create a record-of-functions for some monad.

Notice that the helper methods require `-XTypeApplications`, `-XDataKinds` and
`-XAllowAmbiguousTypes`. Their first type parameter is the name (passed as a
[`Symbol`](http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeLits.html#t:Symbol))
that identifies the capability in the record-of-capabilities. There are no
"anonymous" capabilities.

### What about capabilities that depend on lower-level ones?

Just as before, but the creation of implementations is more involved.

The following property is desirable: implementing the logic for an "capability"
shouldn't be substantially different from implementing the top-level logic.
(This is something that OO dependency injection gets right.)

I arrived at the following pattern: the function that creates the
implementation receives a record-of-capabilities carrying the lower-level
effects.  See the `mkUsers` function in "test.hs" for an example.

### Doesn't that mean that to build a member of the record-of-capabilities we need the record-of-capabilities itself?

Yes, we have to tie the knot. See the `fixRecord` function in module
`Effectiviwonder`.

That function takes an "open"
[`Record`](http://hackage.haskell.org/package/red-black-record-2.0.2.2/docs/Data-RBR.html#t:Record)
where each field is wrapped in a function that accepts the "closed" version of
the record, and returns the "closed" version of the `Record`. (Wrapping each
field of a record in some type constructor is sometimes called the
"Higher-Kinded-Data" pattern.)

Yes, this is an easy way of getting an infinite loop.

## Pending questions

Right now capabilities whose creation involves some effect (like adquiring some
resource) have to be created outside of the record. Is there a way of keeping
them in the record, and perform an "effecful" knot tie? 

## See also

Eric Torreborre's post [A balancing
act](https://medium.com/barely-functional/a-balancing-act-c869e1f4fea4) got me
thinking about dependency injection.

> I think that the ReaderT design pattern is also too limited because it only
> allows us to switch leaves in our “application graph”

See also the [following comment](https://medium.com/@etorreborre_99063/sure-when-you-use-the-readert-pattern-you-are-generally-putting-in-your-environment-low-level-971135e1823f):

> When you use the ReaderT pattern you are generally putting in your
> environment low-level functions or “capabilities”: a logger, the HTTP
> manager, a database connection, a S3 client etc… However you generally don’t
> put there other “capabilities” which depend on the low-level ones: a Client
> repository, a ChatStorage, an OrderExecution component. So if you have a
> large application with different levels of abstraction, it becomes harder to
> replace full sub-systems at once

and the [registry](http://hackage.haskell.org/package/registry) package,
possibly a more convenient method of managing dependency injection.
