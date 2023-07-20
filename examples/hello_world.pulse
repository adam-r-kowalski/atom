fd_write = foreign_import(
    "wasi_unstable",
    "fd_write",
    (fd: i32, iovs: str, iovs_len: i32, mut nwritten: i32) i32
)

stdout: i32 = 1

print = (text: str) void {
    mut nwritten = undefined
    _ = stdout |> fd_write(text, 1, mut nwritten)
}

start = () void {
	print("Hello World!\n")
	print("Goodbye!")
}
