# L-static-analyzer

Project was initially intended to play with different type systems imposed on the language and static analyses. 

Now there are 2 implementations of language 
1) [Simple implementation](src/Intermediate), that does not improse such requirements as for example - requirement for the function to return values of the same type: 

```
def fun(a, b) {
    if (a > b) {
        return a;
    } else {
        write(b);
    }
}
```

2) [GADT implementation](src/Typed), that prohibits such code exploting GADTs and some simple Static analyses.

Examples of programs are [here](examples/valid_programs)

Project checks programs, runs interpter and saves them into jsons (which are then used to compile code to [LLVM](https://github.com/alex28sh/LLVM/))

Run analyzer + interpreter by: 
```
stack run
```