# amt-word64

`Word64Map` offers a similar API as GHC's `Word64Map` but is based on array-mapped tries, similar to `HashMap` from `unordered-containers`.

The goal is achieve better performance than GHC's `Word64Map` (and `Word64Set`).



## Development



### Formatting



This project uses `fourmolu` for code formatting. To format the code, run:



```bash

fourmolu --mode inplace src test

```
