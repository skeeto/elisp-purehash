# Pure Emacs Lisp Hash Tables

This is a hash table implementation written purely in Emacs Lisp. It
has the same functionality as native hash tables, just ten times
slower. What purpose does it serve? I don't know.

### Differences

 * Any comparison function is valid for `:test`.
 * `purehash-map` (i.e. `maphash`) collects and returns its results.
 * No special support in `cl-loop`.
 * For Emacs versions earlier than 23.3, it's readable.
