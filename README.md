# FFI-to-LLS

Automatically turns LuaJIT FFI declarations into valid Lua-Language-Server meta files.

## Dependencies
- LuaJIT
- LLVM (for `libclang`)
- C compiler (to compile [`utilities.c`](./utilities.c))

## Usage

First, compile [`utilities.c`](./utilities.c)

```bash
cc $(llvm-config --cflags) utilities.c -shared -o utilities.so -L$(llvm-config --libdir) -lclang
```

Then, run the script:
```bash
luajit ffi-to-lls.lua <input defs.h> [-o <output file>] [--remove-prefix <prefix>]
```

### Options

- `<input defs.h>`: The header file containing the FFI declarations.
- `-o <output file>`: The output file to write the LLS meta file to. If not specified, the output will be written to stdout.
- `--remove-prefix <prefix>`: Remove the specified prefix from the function names in the output. This is useful when the FFI declarations are prefixed with a common string, such as `lua_` or `lj_`.