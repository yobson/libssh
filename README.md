# libssh Haskell bindings

> This library is in very early stages and is not tested

This repository aims to bind as much of the [libssh](https://www.libssh.org) library
as possible. The bindings will follow the following convention:

- Raw bindings will have the same name, but returing through pointer arguments should be replaced with tuples
- All types named like `ssh_` shall have their names changed to `SSH_...`

Once this is done, I plan on making a more haskelly wrapper that will take care of memory management

## Plan
- [ ] Implement all functions used in the tutorials
- [ ] Implement a memory pool like monad for memeory management
- [ ] Implement wrapping functions so that users of the library don't need to use CTypes
- [ ] Implement a monad that does all allocation, freeing and error handling automatically.

### Design of memeory pool monad.
Pretty simple. We create a typeclass for a freeable type that has a free function inside (and maybe the allowcation function)
We create a monad based on the state monad where all allocations cause a pointer to be added to a list in the state
and all manual calls to free remove it from the list and then after running the monad, all values still in the list have
their free function called.
