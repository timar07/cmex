# cmex

The C programming language macro extension: use Rust-like macros in C code!

## Usage

```sh
cargo run -- ./path-to-file.cmex
```

## Examples

```c
macro_rules! vec {
    [$($x:expr),*] => {
        ({
            vec_t *v = alloc_vec();
            $(push_vec($x));*
            v;
        })
    }
}

int main(void) {
    vec_t *v = vec![1, 2, 3];
    return 0;
}
```
