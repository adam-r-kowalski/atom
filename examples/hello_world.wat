(module

    (import "wasi_unstable" "fd_write" (func $fd_write (param i32) (param i32) (param i32) (param i32) (result i32)))

    (memory 1)
    (export "memory" (memory 0))

    (data (i32.const 0) "Hello, World!\n")
    (data (i32.const 15) "Goodbye, World!")

    (global $arena (mut i32) (i32.const 30))

    (func $print (param $text i32)
        (local $_ i32)
        (local.set $_
            (call $fd_write
                (i32.const 1)
                (local.get $text)
                (i32.const 1)
                (i32.const 200))))

    (func $start
        (local $0 i32)
        (local $1 i32)
        (call $print
            (block (result i32)
                (local.set $0
                    (global.get $arena))
                (i32.store
                    (local.get $0)
                    (i32.const 0))
                (i32.store
                    (i32.add
                        (local.get $0)
                        (i32.const 4))
                    (i32.const 15))
                (global.set $arena
                    (i32.add
                        (local.get $0)
                        (i32.const 8)))
                (local.get $0)))
        (call $print
            (block (result i32)
                (local.set $1
                    (global.get $arena))
                (i32.store
                    (local.get $1)
                    (i32.const 15))
                (i32.store
                    (i32.add
                        (local.get $1)
                        (i32.const 4))
                    (i32.const 15))
                (global.set $arena
                    (i32.add
                        (local.get $1)
                        (i32.const 8)))
                (local.get $1))))

    (export "_start" (func $start)))