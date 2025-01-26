# cmex

The C programming language macro extension: use Rust-like macros in C code!

## Usage

```sh
cargo run -- ./path-to-file.cmex
```

## Examples

```c
/* see examples/vector.cmex */

macro_rules! vec {
    [$($x:expr),*] => {
        ({
            struct vec v;
            vec_init(&v);
            $(vec_push(&v, $x);)*
            v;
        })
    }
}

int main(void) {
    struct vec a = vec![1, 2, 3];
    vec_print(&a);
    return 0;
}
```
