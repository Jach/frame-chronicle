# Frame Chronicle

Just a simple lib I wrote to help me debug some of my game code projects.
The idea is you specify which frames you want to enable recording for, and then
put record statements around your code. Later on you run queries to get it out
and inspect. Example:

```lisp
; in your defpackage:
  (:local-nicknames (#:fc #:com.thejach.frame-chronicle))

; somewhere in an init or main, collect for the first 10 frames
(fc:make-store :sample-on-frames (loop for i below 10 collect i))

; at the start of a game loop tick:
(fc:frame-tick)

; anywhere else:
(fc:record :obj-new-speed (list vx vy))

; later, typically in the REPL:
(fc:query :obj-new-speed :frames 5)
```

See the test code for more examples and how the query result is structured.

# License

This library is licensed under the Unlicense and as such is in the public
domain.
